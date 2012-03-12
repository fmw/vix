(defproject vix "1.0.0-SNAPSHOT"
  :description "Vix is a document repository and management system."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/data.json "0.1.0"]
                 [org.mindrot/jbcrypt "0.3m"]
                 [compojure "1.0.1"]
                 [ring/ring-core "0.3.8"]
                 [enlive "1.0.0-SNAPSHOT"]
                 [org.jsoup/jsoup "1.6.1"]
                 [clj-time "0.3.7"]
                 [clj-http "0.1.3"]
                 [org.clojars.pieterbreed/clojure-couchdb "0.4.7"]
                 [commons-codec/commons-codec "1.5"]
                 [org.apache.lucene/lucene-core "3.5.0"]
                 [org.apache.lucene/lucene-queries "3.5.0"]]
  :dev-dependencies [[lein-ring "0.4.5"]
                     [swank-clojure "1.3.2"]]
  :ring {:handler vix.routes/app})