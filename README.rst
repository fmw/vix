============================================
Vix: Document Repository & Management System
============================================

Vix is a document repository and management system, which is similar to a
`Content Management System`_, but uses the more abstract 'document' definition
for storing content. A document in Vix can be a lot more than a simple
webpage: you can store anything that can be described as a document, e.g. a
webpage, weblog article, media file (video, image, PDF, et cetera) or a
traditional document in an office setting. This early prototype version can be
used to manage content-driven websites, but the main priority for
the near future is to add webshop functionality and provide a stable, high
performance solution for selling products on the internet.

Vix is a free software project and made available under the `Apache License,
version 2.0`_. It is developed by a commercial entity, `Net Collective`_,
which will be providing a hosted version of the application and other
professional services. The motivation for licensing Vix as free software is
that users should not be made dependent on a single entity and deserve
complete freedom in using the software (including but not limited to the right
to make changes to the source code or to hire third party developers to do so
for them). If you want to support this project, you can do so by becoming a
customer of Net Collective.

Technology
==========

Vix is written in `Clojure`_, a Lisp implementation that runs on the
Java Virtual Machine (JVM). This means that Vix embraces the existing
Java infrastructure and (legacy) Java libraries used by enterprise
customers, while using a language that allows for rapid
development. Vix uses `Enlive`_ templating and the `Compojure`_
framework. The client-side code is written in `ClojureScript`_ using
the `Google Closure`_ tools. Vix exposes all functionality through a
RESTful API, allowing for easy integration with external applications.

Installation
============

Vix is still in the initial stages of development, not
feature-complete and not recommended for general production use (we're
currently in the process of rolling it out on a small number of
production sites for testing). That being said, there is a working
prototype that you can try if you feel experimental. This installation
procedure is targeted at developers. If you are a non-technical user
Net Collective is always happy to set up a free beta account for you
in return for your feedback (use the contact information below to get
in touch with the author).

Start by installing the dependencies: `Leiningen`_, `ClojureScript`_
and `CouchDB`_. The compiler for the Soy templates is a separate
download that you can find here: `download Soy`_. For production use
you also need a web server like `Lighttpd`_ or `Apache`_ and an
application server like `Apache Tomcat`_, but you don't need any of
that if you just want to test the application. You will need a working
Java environment, however. ClojureScript works best with the Sun Java
distribution (the package name on Ubuntu is "sun-java6-jdk"), so
GNU/Linux users are encouraged to install that as most distributions
provide the OpenJDK by default.

After satisfying these dependencies you can check out this repository
and fetch the other dependencies using Leiningen::

    mkdir ~/clj ; choose a different directory at your leisure
    cd ~/clj
    git clone git://github.com/fmw/vix.git
    lein deps

At this point you need to create a CouchDB database. By default the
application looks for a database called "vix", but you can change that
in src/vix/db.clj. After you have created a database you can add an
admin user like this::

    cd ~/clj/vix ; or wherever you stored the project directory
    lein repl
    REPL started; server listening on localhost:35140.
    user=> (load "vix/auth")
    nil
    user=> (in-ns 'vix.auth)
    #<Namespace vix.auth>
    vix.auth=> (add-user "http://localhost:5984/" "vix" "my-username" "my-password" {:* [:GET :PUT :DELETE :POST]})
   {:_rev "1-971bd05654d83183728c9d9ff08543b5",
   :_id "64e54e12dbed10a67e49af009d020776",
   :type "user",
   :username "my-username",
   :password "$2a$12$qrni2.vyScJEGc0ZfRXadeKw9Imp8SWvrHzatoF0cCPh.O8cNGIfMwC",
   :permissions {:* [:GET :PUT :DELETE :POST]}}

Note that after "lein repl" only the lines starting with "user=>" and
"vix.auth=>" need to be entered; the other lines are REPL output.

Now compile the client-side templates using the SoyToJsSrcCompiler.jar
utility you downloaded earlier::

    java -jar SoyToJsSrcCompiler.jar --shouldProvideRequireSoyNamespaces \
        --shouldGenerateJsdoc \
        --outputPathFormat resources/public/js/soy/{INPUT_FILE_NAME_NO_EXT}.soy.js \
        soy/editor.soy \
        soy/feed.soy

Now start the repl in the clojurescript/script directory and compile
the client-side code (remember to change the path name from
"/home/fmw/clj/vix" to wherever you stored the vix directory)::

    cd ~/clj/clojurescript
    script/repl
    user=> (use 'cljs.closure) (defn b [] (build "/home/fmw/clj/vix/cljs/src" {:pretty-print true :output-to "/home/fmw/clj/vix/resources/public/js/vix/vix.js" :output-dir "/home/fmw/clj/vix/resources/public/js/out" :libs ["/home/fmw/clj/vix/resources/public/js/soy/"]}))
    user=> (b)

And finally you can start the development server using the
leiningen-ring plugin::

    lein ring server

Use the browser that was launched by this process to navigate to
/admin to see the administration backend.

In the near future we will be providing a .war file that you can
easily deploy on your existing Java infrastructure. For now, you can
compile it thusly::

    lein ring uberwar

Questions and feedback
======================

We eagerly solicit your questions and feedback, because user feedback
is essential when it comes to deciding out what to improve and what
functionality to prioritize. Please don't hesitate to contact Net
Collective. In fact, you can get in touch with the main developer
directly. You can reach F.M. de Waard (Filip) by email at fmw@vix.io.


.. _`Content Management System`: http://en.wikipedia.org/wiki/Content_management_system
.. _`Apache License, version 2.0`: http://www.apache.org/licenses/LICENSE-2.0.html
.. _`Net Collective`: http://netcollective.nl
.. _`Clojure`: http://clojure.org/
.. _`Enlive`: https://github.com/cgrand/enlive
.. _`Compojure`: https://github.com/weavejester/compojure
.. _`ClojureScript`: https://github.com/clojure/clojurescript
.. _`Google Closure`: http://code.google.com/closure/
.. _`Leiningen`: https://github.com/technomancy/leiningen
.. _`download Soy`: http://closure-templates.googlecode.com/files/closure-templates-for-javascript-latest.zip
.. _`ClojureScript quickstart instructions`: https://github.com/clojure/clojurescript/wiki/Quick-Start
.. _`Apache`: http://httpd.apache.org/
.. _`Apache Tomcat`: http://tomcat.apache.org/
.. _`Lighttpd`: http://www.lighttpd.net/
.. _`CouchDB`: http://couchdb.apache.org/
