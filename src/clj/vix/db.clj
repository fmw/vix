;; src/vix/db.clj functionality that provides database interaction
;; Copyright 2011-2012, Vixu.com, F.M. (Filip) de Waard <fmw@vixu.com>.
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
  (:use [slingshot.slingshot :only [try+ throw+]])
  (:require [clojure.java.io :as io]
            [com.ashafa.clutch :as clutch]
            [vix.util :as util])
  (:import [org.apache.commons.codec.binary Base64]))

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
  (clutch/save-view database design-doc [:javascript views]))

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
   (catch java.io.IOException _
     (create-views database design-doc views)
     (get-view database
               design-doc
               view-key
               query-params-map
               post-data-map))))

(defn- clean-feed
  "Internal fn to extract value from a CouchDB result and
   remove the internal :current-state value."
  [clutch-result]
  (update-in (dissoc (:value clutch-result) :current-state)
             [:action]
             keyword))

(defn list-feeds
  "Returns a sequence of the most recent states of the feeds in the
   given database, optionally restricting them to the provided
   language. Omits deleted feeds."
  [database & [language]]
  (map clean-feed
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
  (map clean-feed
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
  (map clean-feed
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
  (let [previous-state (first (get-feed database language name 1))
        datestamp (util/now-rfc3339)]
    (cond
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
                                      (dissoc feed :_rev :_id))
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
    (map :value (get-view database
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
  (let [feed [language feed-name]
        docs (map
              :value
              (get-view database
                        "views"
                        :by_feed
                        (merge {:endkey [feed nil]
                                :startkey [feed (or startkey "2999")]
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
   id-or-document and database."
  [database id-or-doc attachment-key]
  (when-let [a (clutch/get-attachment database id-or-doc attachment-key)]
    (let [byte-array-os (java.io.ByteArrayOutputStream.)]
      (io/copy a byte-array-os)
      (Base64/encodeBase64String (.toByteArray byte-array-os)))))

(defn get-document
  "Retrieves the document identified by slug (the URI) from the given
   database, optionally adding an attachment if the
   include-attachment? argument evaluates to true. In that case, the
   attachment identified by the :original key is added to the returned
   map under the :attachment key (pointing to a map, with :type
   and :data keys for respectively the content type and a Base64
   encoded string representation of the attachment)."
  [database slug & [include-attachment?]]
  (let [doc (:value (first (get-view database
                                     "views"
                                     :by_slug
                                     {:include_docs true
                                      :key slug})))]
    (if (and include-attachment? (:original (:_attachments doc)))
      (assoc doc
        :attachment
        {:type (get-in doc [:_attachments :original :content_type])
         :data (get-attachment-as-base64-string database doc :original)})
      doc)))

(defn get-unique-slug
  "Checks given database if desired-slug is available. If so,
   it is returned. Otherwise a prefix is appended (e.g. -2) and the
   process is repeated until a unique slug is found. "
  [database desired-slug]
  (loop [slug desired-slug]
    (let [document (get-document database slug)]
      (if document
        (recur (util/increment-slug slug))
        slug))))

(defn create-document
  "Creates document in the given database, with provided language and
   feed-name, assuming the given timezone for converting :start-time
   and :end-time in the provided document map to UTC. Apart from
   :start-time, :end-time and :slug, the document map is expected to
   have values for :draft, :content and :title. Other keys are also
   supported. Optionally saves an attachment when the document map
   contains an :attachment map with a mime type under
   :type and the Base64 encoded data under :data. Returns the newly
   created document."
  [database
   language
   feed-name
   timezone
   {:keys [slug start-time end-time] :as document}]
  (let [unique-slug (get-unique-slug database slug)
        doc (clutch/put-document
             database
             (merge (dissoc document :attachment)
                    {:type "document"
                     :feed feed-name
                     :language language
                     :slug unique-slug
                     :published (util/now-rfc3339)}
                    (when start-time
                      {:start-time-rfc3339
                       (util/editor-datetime-to-rfc3339 start-time
                                                        timezone)})
                    (when end-time
                      {:end-time-rfc3339
                       (util/editor-datetime-to-rfc3339 end-time
                                                        timezone)})))]
    (if-not (and (nil? (:data (:attachment document)))
                 (nil? (:type (:attachment document))))
      (do
        (clutch/put-attachment database
                               doc
                               (Base64/decodeBase64
                                (:data (:attachment document)))
                               :filename :original
                               :mime-type (:type (:attachment document)))
        ;; return newly fetched doc from db (including attachment)
        (get-document database unique-slug true))
      ;; when there is no attachment we don't need to refetch
      doc)))

(defn update-document
  "Updates document with provided slug in given database,
   using the values from new-doc and given timezone to convert the
   editor datetime to UTC. The :start-time and :end-type keys are used
   for event documents. Generates RFC3339 timestamps for :updated, as
   well as :start-time-rfc3339 and :end-time-rfc3339 (using the values
   from :start-time and :end-time). Updates the following document
   keys: :title, :subtitle, :content, :description, draft, :start-time,
   :end-time, :icon, :related-pages and :related-images. Returns the
   updated document."
  [database timezone slug new-doc]
  (if-let [document (get-document database slug)]
    (let [doc (clutch/put-document
               database
               (merge (dissoc document :attachment)
                      {:updated (util/now-rfc3339)
                       :title (:title new-doc)
                       :subtitle (:subtitle new-doc)
                       :content (:content new-doc)
                       :description (:description new-doc)
                       :draft (:draft new-doc)
                       :start-time (:start-time new-doc)
                       :end-time (:end-time new-doc)
                       :icon (:icon new-doc)
                       :related-pages (:related-pages new-doc)
                       :related-images (:related-images new-doc)}
                      (when (:start-time new-doc)
                        {:start-time-rfc3339 (util/editor-datetime-to-rfc3339
                                              (:start-time new-doc)
                                              timezone)})
                      (when (:end-time new-doc)
                        {:end-time-rfc3339 (util/editor-datetime-to-rfc3339
                                            (:end-time new-doc)
                                            timezone)})))]
      
      (if-not (and (nil? (:data (:attachment new-doc)))
                   (nil? (:type (:attachment new-doc))))
        (do
          (clutch/put-attachment database
                                 doc
                                 (Base64/decodeBase64
                                  (:data (:attachment new-doc)))
                                 :filename :original
                                 :mime-type (:type (:attachment new-doc)))
          ;; return newly fetched doc from db (including attachment)
          (get-document database slug true))
        ;; when there is no attachment we don't need to refetch
        doc))))

(defn delete-document
  "Deletes the document with the given slug from the provided database.
   Returns a map with the :ok key mapped to true and an :id and :rev
   if successful or nil if the document couldn't be found or in case
   of a 409 conflict."
  [database slug]
  (when-let [document (get-document database slug)]
    (try+
     (clutch/delete-document database document)
     ;; could possibly raise an kind of error to differentiate between
     ;; not found and 409, but not important for now.
     (catch java.io.IOException _ ; for 409 conflicts
       nil))))

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