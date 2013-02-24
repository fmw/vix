(defproject vix "1.0.0-SNAPSHOT"
  :description "Vix is a document repository and management system."
  :dependencies [[org.clojure/clojure "1.5.0-beta1"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/data.json "0.1.2"]
                 [org.clojure/algo.generic "0.1.0"]
                 [org.clojure/clojurescript "0.0-1586"]
                 [org.clojure/google-closure-library "0.0-2029-2"]
                 [org.clojure/google-closure-library-third-party
                  "0.0-2029-2"]
                 [domina "1.0.1"]
                 [slingshot "0.10.3"]
                 [org.mindrot/jbcrypt "0.3m"]
                 [compojure "1.1.0"]
                 [ring/ring-core "1.1.8"]
                 [ring/ring-servlet "1.1.8"]
                 [enlive "1.0.1"]
                 [clj-time "0.4.3"]
                 [clj-http "0.4.3"]
                 [com.ashafa/clutch "0.4.0-SNAPSHOT"]
                 [com.draines/postal "1.8.0"]
                 [org.jsoup/jsoup "1.6.1"]
                 [commons-codec/commons-codec "1.5"]
                 [org.apache.lucene/lucene-core "3.5.0"]
                 [org.apache.lucene/lucene-queries "3.5.0"]]
  :plugins [[lein-ring "0.8.3"]
            [lein-cljsbuild "0.3.0"]]
  :ring {:handler vix.routes/app}
  :min-lein-version "2.0.0"
  :hooks [leiningen.cljsbuild]
  :source-paths ["src" "src/clj"]
  :test-paths ["test/clj"]
  :cljsbuild
  {:builds
   [{:source-paths ["src/cljs"],
     :id "dev",
     :compiler
     {:pretty-print true,
      :output-to "resources/public/js/main-debug.js"},
     :jar true}
    {:source-paths ["test/cljs"],
     :id "test",
     :compiler
     {:pretty-print true,
      :output-to "resources/private/js/unit-test.js",
      :optimizations :whitespace}}
    {:source-paths ["src/cljs"],
     :id "prod",
     :compiler
     {:pretty-print false,
      :output-to "resources/public/js/main.js",
      :optimizations :advanced}}],
   :crossovers [vix.crossover],
   :test-commands
   {"unit"
    ["phantomjs"
     "phantom/unit-test.js"
     "resources/private/html/unit-test.html"]},
   :crossover-jar true,
   :repl-listen-port 9000,
   :repl-launch-commands
   {"firefox-naked"
    ["firefox"
     "resources/private/html/naked.html"
     :stdout
     ".repl-firefox-naked-out"
     :stderr
     ".repl-firefox-naked-err"],
    "phantom"
    ["phantomjs"
     "phantom/repl.js"
     :stdout
     ".repl-phantom-out"
     :stderr
     ".repl-phantom-err"],
    "phantom-naked"
    ["phantomjs"
     "phantom/repl.js"
     "resources/private/html/naked.html"
     :stdout
     ".repl-phantom-naked-out"
     :stderr
     ".repl-phantom-naked-err"],
    "firefox"
    ["firefox" :stdout ".repl-firefox-out" :stderr ".repl-firefox-err"]}})