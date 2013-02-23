;; src/vix/db.clj functionality that provides database interaction
;; Copyright 2011-2013, Vixu.com, F.M. (Filip) de Waard <fmw@vixu.com>.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns vix.db
  (:use [slingshot.slingshot :only [try+ throw+]]
        [clojure.set])
  (:require [clojure.java.io :as io]
            [com.ashafa.clutch :as clutch]
            [vix.util :as util])
  (:import [org.apache.commons.codec.binary Base64]))

(def database-connection-error
  {:type ::database-socket-error
   :message (str "Can't connect to the database. Check "
                 "config.clj, if the server is running and "
                 "if the database exists.")})

(defn load-view [path]
  (slurp (io/resource path)))

(def views {:by_feed
            {:map (load-view "database-views/map_document_by_feed.js")}
            :by_slug
            {:map (load-view "database-views/map_document_by_slug.js")}
            :by_username
            {:map (load-view "database-views/map_user_by_username.js")}
            :feeds
            {:map (load-view "database-views/map_feeds.js")}
            :feeds_overview
            {:map (load-view "database-views/map_feeds_overview.js")}
            :feeds_by_default_document_type
            {:map (load-view
                   "database-views/map_feeds_by_default_document_type.js")}
            :events_by_feed
            {:map (load-view "database-views/map_events_by_feed.js")}
            :subscribers
            {:map (load-view
                   "database-views/map_newsletter_subscribers.js")}
            :languages
            {:map (load-view "database-views/map_languages.js")
             :reduce (load-view "database-views/reduce_languages.js")}})

(defn create-views [database design-doc views]
  "Creates the provided views in given database and saves them in
   design-doc."
  (try+
   (clutch/save-view database design-doc [:javascript views])
   (catch java.net.SocketException _
     (throw+ database-connection-error))))

(defn get-view
  "Wraps around clutch/get-view in order to automatically create views
   if they don't exist yet."
  [database design-doc view-key & [query-params-map post-data-map :as args]]
  (try+
   (clutch/get-view database
                    design-doc
                    view-key
                    query-params-map
                    post-data-map)
   (catch java.net.SocketException _
     (throw+ database-connection-error))
   (catch java.io.IOException _
     (create-views database design-doc views)
     (get-view database
               design-doc
               view-key
               query-params-map
               post-data-map))))

(defn- clean
  "Internal fn to extract value from a CouchDB result and remove the
   internal :current-state value. Also turns the :action value into a
   keyword."
  [clutch-result]
  (update-in (dissoc (:value clutch-result) :current-state)
             [:action]
             keyword))

(defn list-feeds
  "Returns a sequence of the most recent states of the feeds in the
   given database, optionally restricting them to the provided
   language. Omits deleted feeds."
  [database & [language]]
  (map clean
       (get-view database
                 "views"
                 :feeds_overview
                 (if language
                   {:descending true
                    :startkey [language {}]
                    :endkey [language nil]}
                   {:descending true}))))

(defn list-feeds-by-default-document-type
  "Returns a sequence of the most recent states of the feeds in the
   given database, optionally restricting them to the provided
   language, limited to the provided default-document-type.
   Omits deleted feeds."
  [database default-document-type & [language]]
  (map clean
       (get-view database
                 "views"
                 :feeds_by_default_document_type
                 {:descending true
                  :startkey [default-document-type
                             (or language {})
                             {}]
                  :endkey [default-document-type
                           language
                           nil]})))

(defn get-feed
  "Returns a sequence of states for the feed with the given language
   and name from the provided database. Returns all available states
   by default, but accepts an optional numeric limit argument. Omits
   the :current-state key (which makes the output immutable from the
   viewpoint of this API)."
  [database language name & [limit]]
  (map clean
       (get-view database
                 "views"
                 :feeds
                 (if (number? limit)
                   {:limit limit
                    :include_docs true
                    :startkey [language name {}]
                    :endkey [language name nil]
                    :descending true}
                   {:include_docs true
                    :startkey [language name {}]
                    :endkey [language name nil]
                    :descending true}))))

(def invalid-action
  {:type ::invalid-action
   :message (str "The provided :action value is invalid "
                 "(or \"action\" if JSON). The actions "
                 ":create, :update & :delete are allowed "
                 "(or \"create\", \"update\" & \"delete\" if JSON).")})

(def feed-update-conflict
  {:type ::feed-update-conflict
   :message "This feed map doesn't contain the most recent :previous-id."})

(def feed-already-exists-conflict
  {:type ::feed-already-exists-conflict
   :message "The provided feed already exists."})

(def feed-already-deleted-error
  {:type ::feed-already-deleted
   :message "This feed has already been deleted."})

(defn append-to-feed
  "Accepts a map describing a feed state to be stored in the given
   database. The feed map must contain the following
   keys: :action (i.e. :create, :update, :delete),
   :previous-id. The :previous-id value is the CouchDB document ID for
   the document describing the previous state and is only required
   only for :update and :delete actions. If the :previous-id doesn't
   refer to the last state of the feed, a :vix.db/feed-update-conflict
   exception is thrown.

   Alternatively, it would be possibly to implement this function
   without the :previous-id value by passing the whole state history
   as a sequence. This is arguably cleaner, but requires more data to
   be passed between the server and client through XMLHttpRequest.
   This isn't too problematic for feeds, but document state histories
   can get quite large. So the current implementation uses of this fn
   :previous-id to make it consistent with the append-to-document
   implemention.

   Internally, the :current-state key is set to false on the previous
   state if a new one is added. This is hidden from view and the key
   is stripped from the output of append-to-feed and get-feed. The
   reason for this :current-state hack is that it makes it a lot
   easier to retrieve e.g. a list of the most current feeds from
   CouchDB. From the viewpoint of this API the output is totally
   immutable."
  
  [database {:keys [action language name previous-id] :as feed}]
  (let [action (keyword action)
        previous-state (first (get-feed database language name 1))
        datestamp (util/now-rfc3339)]
    (cond
     (not (some #{action} [:create :update :delete]))
     (throw+ invalid-action)
     (and (= action :create)
          (some #{(:action previous-state)} [:create :update]))
     (throw+ feed-already-exists-conflict)
     (and (not (= action :create))
          (not (= (:_id previous-state) previous-id)))
     (throw+ feed-update-conflict)
     (and (= action :delete) (= (:action previous-state) :delete))
     (throw+ feed-already-deleted-error)
     :default
     (do
       (clutch/bulk-update database
                           (filter
                            #(not (nil? %))
                            ;; new state
                            [(assoc (if (= action :create)
                                      (assoc feed
                                        :created datestamp)
                                      (assoc (dissoc feed :_rev :_id)
                                        :created (:created previous-state)))
                               :type "feed"
                               :current-state true
                               :datestamp datestamp)
                             ;; update for previous state
                             (when previous-state
                               (assoc previous-state
                                 :current-state false))]))
       (get-feed database language name)))))

(defn get-most-recent-event-documents
  "Retrieves the most recent events for an event feed identified by
   language and feed-name from database, optionally capping results at
   limit."
  [database language feed-name & [limit]]
  (let [feed [language feed-name]
        options {:include_docs true
                 :startkey [feed {}]
                 :endkey [feed nil]
                 :descending true}]
    (map clean
         (get-view database
                   "views"
                   :events_by_feed
                   (if (nil? limit)
                     options
                     (assoc options :limit limit))))))

(defn get-documents-for-feed
  "Returns documents for language and feed-name from the given database.
   Accepts limit, startkey and startkey_docid as optional pagination
   arguments. Returns a map with the documents associated to
   :documents. When there is a next page, the result map also contains
   a :next key pointing to a map with a :startkey_docid and :published
   key. To retrieve the documents for the next page, simply call this
   fn with the particular limit and the startkey_docid from the :next
   map of the current page, with the :published value as startkey."
  [database language feed-name & [limit startkey startkey_docid]]
  (let [docs (map
              clean
              (get-view database
                        "views"
                        :by_feed
                        (merge {:endkey [[language feed-name] nil]
                                :startkey [[language feed-name]
                                           (or startkey "2999")]
                                :include_docs true
                                :descending true}
                               (when startkey_docid
                                 {:startkey_docid startkey_docid})
                               (when limit
                                 {:limit (inc limit)}))))]
    (if (or (nil? limit) (<= (count docs) limit))
      ;; does not have more pages
      {:next nil
       :documents docs}
      ;; does have additional pages
      {:next {:startkey_docid (:_id (last docs))
              :published (:published (last docs))}
       :documents (butlast docs)})))

(defn get-attachment-as-base64-string
  "Returns Base64 encoded string or nil (if the attachment wasn't
   found) for the attachment with the given attachment-key in
   document-id and database."
  
  [database document-id attachment-key]
  (when-let [a (clutch/get-attachment database document-id attachment-key)]
    (let [byte-array-os (java.io.ByteArrayOutputStream.)]
      (io/copy a byte-array-os)
      (Base64/encodeBase64String (.toByteArray byte-array-os)))))

(defn get-document
  "Retrieves all available states of the document identified by
   slug (the URI) from the given database. It is possible to pass a
   map with CouchDB query options as an optional third argument (e.g.
   {:limit 1}). Note that when working with documents that have
   attachments, each state will contain a Base64 encoded attachment.
   This can be demanding in terms of bandwidth, so it is advisable to
   use a query limit by default for documents that may contain
   attachments."
  [database slug & [couchdb-options]]
  (let [states
        (vec ;; turn into vector to enable assoc-in/get-in
         (map (fn [raw-state]
                (let [state (clean raw-state)]
                  (if (contains? state :_attachments)
                    (assoc (dissoc state :_attachments)
                      :attachments
                      {:original
                       {:type (get-in state [:_attachments
                                             :original
                                             :content_type])
                        :data (get-attachment-as-base64-string database
                                                               (:_id state)
                                                               :original)
                        :length (get-in state [:_attachments
                                               :original
                                               :length])}})
                    state)))
              (get-view database
                        "views"
                        :by_slug
                        (merge {:include_docs true
                                :startkey [slug {}]
                                :endkey [slug nil]
                                :descending true}
                               couchdb-options))))]
    (if (not-empty states)
      states
      nil)))

(def document-update-conflict
  {:type ::document-update-conflict
   :message (str "This document map doesn't contain the most "
                 "recent :previous-id.")})

(def document-already-exists-conflict
  {:type ::document-already-exists-conflict
   :message "There is an existing document with the provided slug."})

(def document-already-deleted-error
  {:type ::document-already-deleted
   :message "This document has already been deleted."})

(def document-missing-required-keys-error
  {:type ::document-missing-required-keys
   :message (str "This document is missing required keys. "
                 "The keys :slug, :language, :feed, :title "
                 "are required.")})

(defn append-to-document
  "Accepts a map describing a document state to be stored in the given
   database. The map must contain the following
   keys: :action (i.e. :create, :update, :delete),
   :previous-id, :slug, :language and :feed-name. The :previous-id
   value is the CouchDB document ID for the document describing the
   previous state and is only required only for :update and :delete
   actions. If the :previous-id doesn't refer to the last state of the
   feed, a :vix.db/feed-update-conflict exception is thrown.
   Optionally saves an attachment when the document map contains
   an :attachment map with a mime type under
   :type and the Base64 encoded data under :data. Returns the newly
   created document state appended to the existing states. The
   optional fourth argument, couchdb-options, is a map with options
   to be passed to the final get-document call that returns the
   states for the document.

   Currently returns a copy of the attachment for every state of the
   document, which has an obvious downside in terms of bandwidth usage
   (particularly for larger images). This trade-off might change in a
   future version of the API, but for now the consideration is that
   documents with attachments are generally not updated so much for
   this to become a problem. The upside is consistency and immutability
   in the output of this function. The good news is that the optional
   fourth argument, couchdb-options, allows for passing a limit
   (e.g. {:limit 1}). This means that you can restrict the number of
   returned states, which is useful for appends to really big documents.

   Optionally, the document map may contain :start-time and
   :end-time keys mapping to date strings in yyyy-MM-dd HH:mm format.
   These keys and their values are removed and used to create
   :start-date-rfc3339 and :end-date-rfc3339.

   Also see the append-to-feed docstring."
  
  [database
   timezone
   {:keys [action slug previous-id start-time end-time] :as document}
   & [couchdb-options]]
  (let [action (keyword action) ;; fix action for json requests
        previous-state (first (get-document database slug {:limit 1}))
        datestamp (util/now-rfc3339)]
    (cond
     (not (some #{action} [:create :update :delete]))
     (throw+ invalid-action)
     (and (= action :create)
          (some #{(:action previous-state)} [:create :update]))
     (throw+ document-already-exists-conflict)
     (and (not (= action :create))
          (not (= (:_id previous-state) previous-id)))
     (throw+ document-update-conflict)
     (and (= action :delete) (= (:action previous-state) :delete))
     (throw+ document-already-deleted-error)
     (not (subset? #{:slug :language :feed :title} (set (keys document))))
     (throw+ document-missing-required-keys-error)
     :default
     (do
       (clutch/bulk-update
        database
        (filter #(not (nil? %))
                ;; new state
                [(merge (dissoc document
                                :_id
                                :_rev
                                :attachment)
                        {:type "document"
                         :current-state true
                         :datestamp datestamp}
                        (if (= action :create)
                          {:created datestamp
                           :published datestamp}
                          {:created (:created previous-state)
                           :published (:published previous-state)})
                        (when start-time
                          {:start-time-rfc3339
                           (util/editor-datetime-to-rfc3339 start-time
                                                            timezone)})
                        (when end-time
                          {:end-time-rfc3339
                           (util/editor-datetime-to-rfc3339 end-time
                                                            timezone)})
                        (when (not
                               (and (nil? (:data (:attachment document)))
                                    (nil? (:type (:attachment document)))))
                          {:_attachments
                           {:original
                            {:content_type (:type (:attachment document))
                             :data (:data (:attachment document))}}}))
                 ;; update previous state
                 (when previous-state
                   (assoc previous-state :current-state false))]))
       (get-document database slug (or couchdb-options {}))))))

(defn get-available-languages [database]
  "Returns a sequence of available languages directly from the database."
  (map :key (get-view database "views" :languages {:group true})))

(defn get-languages [feeds]
  "Returns a set of languages from the provided feeds"
  (set (map :language feeds)))

(defn get-searchable-feeds [feeds]
  "Returns a map with language strings as keys and the feeds for those
   languages as values."
  (let [languages (get-languages feeds)]
    (zipmap languages
            (map (fn [language]
                   (vec ; has to be a vector because of lucene/create-filter
                    (map :name
                         (filter (fn [feed]
                                   (and (= (:language feed) language)
                                        (true? (:searchable feed))))
                                 feeds))))
                 languages))))