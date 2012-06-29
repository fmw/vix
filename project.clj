(defproject vix "1.0.0-SNAPSHOT"
  :description "Vix is a document repository and management system."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/data.json "0.1.2"]
                 [org.mindrot/jbcrypt "0.3m"]
                 [compojure "1.0.1"]
                 [ring/ring-core "1.0.2"]
                 [enlive "1.0.0-SNAPSHOT"]
                 [org.jsoup/jsoup "1.6.1"]
                 [clj-time "0.3.7"]
                 [clj-http "0.1.3"]
                 [org.clojars.pieterbreed/clojure-couchdb "0.4.7"]
                 [commons-codec/commons-codec "1.5"]
                 [org.apache.lucene/lucene-core "3.5.0"]
                 [org.apache.lucene/lucene-queries "3.5.0"]
                 [com.draines/postal "1.7.1"]
                 [ring/ring-servlet "1.0.1"]]
  :plugins [[lein-ring "0.7.1"]]
  :ring {:handler vix.routes/app})