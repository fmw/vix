(defproject vix "1.0.0-SNAPSHOT"
  :description "Vix is a document repository and management system."
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/data.json "0.1.2"]
                 [org.clojure/algo.generic "0.1.0"]
                 [slingshot "0.10.3"]
                 [org.mindrot/jbcrypt "0.3m"]
                 [compojure "1.1.0"]
                 [ring/ring-core "1.1.1"]
                 [ring/ring-servlet "1.1.0"]
                 [enlive "1.0.1"]
                 [clj-time "0.4.3"]
                 [clj-http "0.4.3"]
                 [com.ashafa/clutch "0.4.0-SNAPSHOT"]
                 [com.draines/postal "1.8.0"]
                 [org.jsoup/jsoup "1.6.1"]
                 [commons-codec/commons-codec "1.5"]
                 [org.apache.lucene/lucene-core "3.5.0"]
                 [org.apache.lucene/lucene-queries "3.5.0"]]
  :plugins [[lein-ring "0.7.1"]
            [lein-cljsbuild "0.2.5"]]
  :ring {:handler vix.routes/app}
  :min-lein-version "2.0.0"
  :hooks [leiningen.cljsbuild]
  :source-paths ["src" "src/clj"]
  :test-paths ["test/clj"]
  :cljsbuild {
              :repl-listen-port 9000
              :repl-launch-commands
              ;; Launch command for connecting the page of choice to
              ;; the REPL. Only works if the page at URL automatically
              ;; connects to the REPL, like
              ;; http://localhost:3000/repl-demo does.
              ;; $ lein trampoline cljsbuild repl-launch firefox <URL>
              {"firefox" ["firefox"
                          :stdout ".repl-firefox-out"
                          :stderr ".repl-firefox-err"]
               ;; Launch command for interacting with your
               ;; ClojureScript at a REPL, without browsing to the app
               ;; (a static HTML file is used).
               ;; $ lein trampoline cljsbuild repl-launch firefox-naked
               "firefox-naked" ["firefox"
                                "resources/private/html/naked.html"
                                :stdout ".repl-firefox-naked-out"
                                :stderr ".repl-firefox-naked-err"]
               ;; This is similar to "firefox" except it uses
               ;;     PhantomJS. $ lein trampoline cljsbuild
               ;;     repl-launch phantom <URL>
               "phantom" ["phantomjs"
                          "phantom/repl.js"
                          :stdout ".repl-phantom-out"
                          :stderr ".repl-phantom-err"]
               ;; This is similar to "firefox-naked" except it uses
               ;;     PhantomJS.
               ;; $ lein trampoline cljsbuild repl-launch phantom-naked
               "phantom-naked" ["phantomjs"
                                "phantom/repl.js"
                                "resources/private/html/naked.html"
                                :stdout ".repl-phantom-naked-out"
                                :stderr ".repl-phantom-naked-err"]}
              :test-commands
              ;; Test command for running the unit tests in
              ;;     "test-cljs" (see below). $ lein cljsbuild test
              {"unit" ["phantomjs"
                       "phantom/unit-test.js"
                       "resources/private/html/unit-test.html"]}
              :crossovers [vix.crossover]
              :crossover-jar true
              :builds {
                       ;; This build has the lowest level of
                       ;; optimizations, so it is useful when
                       ;; debugging the app.
                       :dev
                       {:source-path "src/cljs"
                        :jar true
                        :notify-command ["notify-send"
                                         "-i"
                                         "~/Pictures/clojure-glyph.svg"
                                         "lein-cljsbuild"]
                        :compiler {:output-to
                                   "resources/public/js/main-debug.js"
                                   :libs
                                   ["resources/public/js/soy/"]
                                   :optimizations
                                   :whitespace
                                   :pretty-print
                                   true}}
                       ;; This build has the highest level of
                       ;; optimizations, so it is efficient when
                       ;; running the app in production.
                       :prod
                       {:source-path "src/cljs"
                        :compiler {:output-to "resources/public/js/main.js"
                                   :libs ["resources/public/js/soy/"]
                                   :optimizations :advanced
                                   :pretty-print false}}
                       ;; This build is for the ClojureScript unit
                       ;; tests that will be run via PhantomJS. See
                       ;; the phantom/unit-test.js file for details on
                       ;; how it's run.
                       :test
                       {:source-path "test/cljs"
                        :compiler {:output-to
                                   "resources/private/js/unit-test.js"
                                   :libs
                                   ["resources/public/js/soy/"]
                                   :optimizations
                                   :whitespace
                                   :pretty-print
                                   true}}}})