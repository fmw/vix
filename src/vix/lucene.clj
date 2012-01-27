;; src/vix/lucene.clj: search functionality for Vix documents.
;;
;; Copyright 2011-2012, F.M. (Filip) de Waard <fmw@vix.io>.
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

(ns vix.lucene
  (:use [clojure.contrib.json :only [read-json json-str]]
        [clojure.contrib.duck-streams :only (read-lines)]
        [clojure.string :only (trim blank?)]
        [vix.db :only (datetime-string-to-long)])
  (:require [couchdb [client :as couchdb]]
            [net.cgrand.enlive-html :as html]
            [clj-time.coerce :as time-coerce]
            [clojure.contrib.math :as math])
  (:import [org.apache.lucene.document
            Document Field Field$Store Field$Index NumericField]
           [org.apache.lucene.analysis.standard StandardAnalyzer]
           [org.apache.lucene.store Directory NIOFSDirectory RAMDirectory]
           [org.apache.lucene.search
            IndexSearcher QueryWrapperFilter NumericRangeQuery
            TermQuery BooleanQuery BooleanClause$Occur ScoreDoc]
           [org.apache.lucene.queryParser QueryParser]
           [org.apache.lucene.index IndexWriter IndexWriterConfig
            IndexWriterConfig$OpenMode IndexReader Term]
           [org.apache.lucene.util Version]
           [java.io File]))

(def index-path "/var/lucene/vix")

(defn #^StandardAnalyzer create-analyzer []
  "Creates a StandardAnalyzer that tokenizes fulltext fields."
  (StandardAnalyzer. (. Version LUCENE_35)))

(defn create-directory [path]
  "Create a directory with either :RAM or a directory as the path argument."
  (if (= path :RAM)
    (RAMDirectory.)
    (NIOFSDirectory. (File. path))))

(def directory (create-directory index-path))
(def analyzer (create-analyzer))

(defn #^IndexReader create-index-reader [#^Directory directory]
  "Create IndexReader for the specified directory."
  (. IndexReader open directory))

(defn #^Field create-field
  "Creates a new Lucene Field object.
  The Field will be stored if a :stored keyword is passed. The index
  value has two options: :analyzed or :indexed-not-analyzed
  (if neither of these options are passed the field isn't indexed)."
  ([field-name value & options]
     (let [store? (if (some #{:stored} options)
                    (Field$Store/YES)
                    (Field$Store/NO))
           index? (cond
                   (some #{:analyzed} options)
                   (Field$Index/ANALYZED)
                   (some #{:indexed-not-analyzed} options)
                   (Field$Index/NOT_ANALYZED)
                   :default
                   (Field$Index/NO))]
       (Field. field-name (str value) store? index?))))

(defn #^NumericField create-numeric-field
  "Creates a Lucene NumericField object set to the provided value or
   -1 if this value isn't either a Double, Float, Integer or Long
   object.

   Options are
   :stored (to make the value retrievable from the index), and,
   :indexed (to make the field searchable or filterable)."
  [name value & options]
  (let [field (NumericField. name
                             (if (some #{:stored} options)
                               Field$Store/YES
                               Field$Store/NO)
                             (keyword? (some #{:indexed} options)))]
    (cond
     (= (class value) java.lang.Double) (.setDoubleValue field value)
     (= (class value) java.lang.Float) (.setFloatValue field value)
     (= (class value) java.lang.Integer) (.setIntValue field value)
     (= (class value) java.lang.Long) (.setLongValue field value)
     :default (.setIntValue field -1))))

(defn #^String distill-plaintext
  "Distills the text values from an HTML string.
   The value of each node is returned on a new line, followed by the
   values of the title attributes of all img nodes"
  [html]
  (if (some #{\< \>} html)
    (let [resource (html/html-resource (java.io.StringReader. html))]
      (apply str
             (interpose
              "\n"
              (concat
               (map html/text
                    (html/select
                     resource
                     [:body :* (html/text-pred #(not (empty? %)))]))
               (map #(:title (:attrs %)) (html/select resource [:img]))))))
    html))

(defn #^Document create-document [vix-doc]
  "Creates a Lucene Document object representing the Vix document that
   is passed as a map (e.g. directly from the database).."
  (let [{:keys [feed
                slug
                title
                draft
                language
                published
                updated
                content]}
        vix-doc]
    (doto #^Document (Document.)
          ;; create a fulltext field with all the values to search on
          ;; mashed together in a single value. This field is not stored.
          (.add (create-field "fulltext"
                              (apply str
                                     (interpose "\n"
                                                [title
                                                 (distill-plaintext
                                                  content)]))
                              :analyzed))
          
          (.add (create-field "feed" feed :indexed-not-analyzed :stored))
          (.add (create-field "slug" slug :indexed-not-analyzed :stored ))

          (.add (create-field "title" title :analyzed :stored))

          (.add (create-field "draft"
                              (if draft
                                "1"
                                "0")
                              :indexed-not-analyzed
                              :stored))
          
          (.add (create-field "language"
                              language
                              :indexed-not-analyzed
                              :stored))
          (.add (create-numeric-field "published"
                                      (datetime-string-to-long published)
                                      :stored
                                      :indexed))
          (.add (create-numeric-field "updated"
                                      (datetime-string-to-long updated)
                                      :stored
                                      :indexed)))))

(defn #^IndexWriter create-index-writer [analyzer directory mode]
  "Creates an IndexWriter with the provided analyzer and directory.
   The mode has three options: :create, :append or :create-or-append."
  (let [config (IndexWriterConfig. (Version/LUCENE_35) analyzer)]

    (doto config
      (.setRAMBufferSizeMB 49)
      (.setOpenMode (cond
                     (= mode :create)
                     (IndexWriterConfig$OpenMode/CREATE)
                     (= mode :append)
                     (IndexWriterConfig$OpenMode/APPEND)
                     (= mode :create-or-append)
                     (IndexWriterConfig$OpenMode/CREATE_OR_APPEND))))
    
    (IndexWriter. directory config)))

(defn #^IndexWriter close-index-writer [writer]
  "Closes the provided IndexWriter and writes the changes to disk (or RAM)."
  (doto writer
    (.close)))

(defmulti add-documents-to-index!
  "Supports adding documents to either a provided IndexWriter or
   through an IndexWriter created for the provided documents specifically."
  (fn [x documents] [(class x) (class documents)]))

(defmethod add-documents-to-index! [Directory Object] [directory documents]
  "Adds the documents to the index for the provided directory."
  (let [writer (create-index-writer (create-analyzer)
                                    directory
                                    :create-or-append)]
    (add-documents-to-index! writer documents)
    (close-index-writer writer)))

(defmethod add-documents-to-index! [IndexWriter Object] [writer documents]
  "Adds the documents to the provided IndexWriter, but doesn't close it
   (i.e. the IndexWriter needs to be closed externally to write the
   changes to disk or memory)."
  (doseq [document documents]
    (.addDocument writer (create-document document))))

(defn delete-document-from-index! [directory slug]
  "Deletes the document with the provided slug from the provided Lucene
   Directory object."
  (doto (create-index-writer (create-analyzer) directory :append)
    (.deleteDocuments (Term. "slug" slug))
    (.close)))

(defn update-document-in-index! [directory slug document]
  "Deletes the existing document with the provided slug from the
   provided Lucene Directory object and creates a new document using
   the provided document map."
  (let [analyzer (create-analyzer)]
    (doto (create-index-writer analyzer directory :append)
      (.updateDocument (Term. "slug" slug)
                       (create-document document)
                       analyzer)
      (.close))))

(defn #^NumericRangeQuery create-date-range-query
  [field-name start-date-rfc3339 end-date-rfc3339]
  "Creates a NumericRangeQuery for field-name using start and end date
   string arguments."
  (let [min (datetime-string-to-long start-date-rfc3339)
        max (datetime-string-to-long end-date-rfc3339)]
    (when (and (= (class min) java.lang.Long)
               (= (class max) java.lang.Long)
               (>= max min))
      (NumericRangeQuery/newLongRange field-name min max true true))))

(defn #^QueryWrapperFilter create-filter [filters]
  "Creates a Lucene BooleanQuery wrapped in a QueryWrapperFilter with one
   or more filter queries (if zero this function returns nil).

   The following filters are possible:
   :published-between {:min rfc3339-date-string :max rfc-3339-date-string}
   :updated-between {:min rfc3339-date-string :max rfc-3339-date-string}
   :language String (literal)
   :feed String (literal)
   :slug String (literal)
   :draft Boolean

   See the unit tests for examples."
  (let [{:keys [published-between
                updated-between
                language
                feed
                slug
                draft]} filters
                bq (BooleanQuery.)]

    (when (and (string? (:min published-between))
               (string? (:max published-between)))
      (.add bq
            (create-date-range-query "published"
                                     (:min published-between)
                                     (:max published-between))
            BooleanClause$Occur/MUST))    
    
    (when (and (string? (:min updated-between))
               (string? (:max updated-between)))
      (.add bq
            (create-date-range-query "updated"
                                     (:min updated-between)
                                     (:max updated-between))
            BooleanClause$Occur/MUST))

    (when (string? language)
      (.add bq
            (TermQuery. (Term. "language" language))
            BooleanClause$Occur/MUST))

    (when (string? feed)
      (.add bq
            (TermQuery. (Term. "feed" feed))
            BooleanClause$Occur/MUST))

    (when (string? slug)
      (.add bq
            (TermQuery. (Term. "slug" slug))
            BooleanClause$Occur/MUST))

    (when-not (nil? draft)
      (.add bq
            (TermQuery. (Term. "draft" (if draft "1" "0")))
            BooleanClause$Occur/MUST))
    
    (when (pos? (alength (.getClauses bq)))
      (QueryWrapperFilter. bq))))

(defn #^Document get-doc [reader doc-id]
  "Reads the document with the provided doc-id from the index."
  (.document reader doc-id))

(defn get-docs [reader score-docs]
  "Returns a sequence of the individual documents for a Lucene ScoreDoc[]
   array."
  (map #(get-doc reader (.doc %)) score-docs))

(defn document-to-map [document]
  "Converts a Lucene document to a Vix document map."
  {:slug (.get document "slug")
   :title (.get document "title")
   :feed (.get document "feed")
   :language (.get document "language")
   :draft (if (= (.get document "draft") "1")
            true
            false)
   :published (let [pub-long (read-string (.get document "published"))]
                (if (and (number? pub-long) (pos? pub-long))
                  (time-coerce/from-long pub-long)
                  nil))
   :updated (let [pub-long (read-string (.get document "updated"))]
              (if (and (number? pub-long) (pos? pub-long))
                (time-coerce/from-long pub-long)
                nil))})

(defn search
  "Runs a query (with or without a filter) and returns the result.
   Also able to paginate when after-doc-id and after-score are provided.
   See the unit tests for examples. The return value is a map with
   :total-hits and :docs."
  ([query filter limit reader analyzer]
     (search query filter limit nil nil reader analyzer))
  ([query filter limit after-doc-id after-score reader analyzer]
     (let [searcher (IndexSearcher. reader)
           parser (QueryParser. (Version/LUCENE_35) "fulltext" analyzer)
           q (.parse parser query)
           top-docs (if (nil? after-doc-id)
                      (if (nil? filter)
                        (.search searcher q limit)
                        (.search searcher q filter limit))
                      (if (nil? filter)
                        (.searchAfter searcher
                                      (ScoreDoc. after-doc-id after-score)
                                      q
                                      limit)
                        (.searchAfter searcher
                                      (ScoreDoc. after-doc-id after-score)
                                      q
                                      filter
                                      limit)))]

       (. searcher close)
       
       {:total-hits (.totalHits top-docs)
        :docs (map #(assoc (document-to-map (second %))
                      :index {:doc-id (.doc (first %))
                              :score (.score (first %))})
                   (partition 2
                              (interleave
                               (.scoreDocs top-docs)
                               (get-docs reader (.scoreDocs top-docs)))))})))

(defn search-jump-to-page
  "This function performs a search and drops results from the unwanted
   pages until the desired page is found and returned. This function
   is meant for leaping ahead, since the regular search function
   already supports displaying results for pages when there is a known
   document id and score for the last document of the previous page.

   Warning: while fine on small indexes, performance on larger result
   sets can be problematic."
  [query filter per-page target-page reader analyzer]
  (if (= target-page 1)
    (search query filter per-page reader analyzer)
    (let [result (search query
                         filter
                         (* per-page target-page)
                         reader
                         analyzer)]
      (when (<= target-page (math/ceil (/ (:total-hits result) per-page)))
        (assoc result
          :docs
          (drop (* per-page (- target-page 1)) (:docs result)))))))