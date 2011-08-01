(defproject vix "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/data.json "0.1.0"]
                 [org.mindrot/jbcrypt "0.3m"]
                 [compojure "0.6.3"]
                 [ring/ring-core "0.3.8"]
                 [enlive "1.0.0-SNAPSHOT"]
                 [clj-time "0.3.0"]
                 [clj-http "0.1.3"]
                 [org.clojars.pieterbreed/clojure-couchdb "0.4.7"]]
  :dev-dependencies [[lein-ring "0.4.0"]
                     [org.clojars.autre/lein-vimclojure "1.0.0"]
                     [swank-clojure "1.2.1"]]
  :ring {:handler vix.routes/app})