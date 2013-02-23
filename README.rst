================================
Vix: Website-management software
================================

Vix is a `website-management application`_ (or `Content Management
System`_). E-commerce functionality is to be added later this year.
Vix is used in production to power websites that can be easily managed
by non-technical users. The project is backed by `Vixu.com`_, which is
a commercial entity that provides a Software-as-a-Service version of
the software with managed hosting infrastructure and professional
support. Vixu.com has a strong `commitment to open source`_ and makes
the source code of the application perpetually available under the
free `Apache License, version 2.0`_.

Technology
==========

Vix is written in `Clojure`_, a Lisp implementation that runs on the
Java Virtual Machine (JVM). This means that Vix embraces existing Java
infrastructure and (legacy) Java libraries used by enterprise
customers, while using a language that allows for rapid development.
Vix uses `Enlive`_ templating and the `Compojure`_ framework. The
client-side code is written in `ClojureScript`_ using the `Google
Closure`_ tools. Vix exposes all functionality through a RESTful API,
allowing for easy integration with external applications.

Installation
============

This installation procedure is targeted at developers and describes
how to install the source version. In the future we will provide a
pre-compiled package as well. If you are a non-technical user
`Vixu.com`_ is happy to set up a free demo account for you in return
for your feedback (use the contact information below to get in touch).

Vix requires a working Java environment with `Leiningen`_ and
`CouchDB`_ installed. The other dependencies are installed through
Leiningen. The installation instructions below will also install
`Git`_ and `curl`_. For production use you also need an application
server like `Apache Tomcat`_, but for testing and development you can
launch a simple server using the Ring plugin for Leiningen.

This is the sequence of commands used to install Vix on a clean Ubuntu
12.04 LTS installation (if you already installed some of the
dependencies you can skip those commands). They are meant to be
entered in the order listed. If you're using a different operating
system and need help installing the application you can contact the
author for support (see below for contact information). Note that the
Microsoft Windows operating system is not officially supported, but
Windows users might have some success using Cygwin.

Install Java::

    sudo apt-get install openjdk-6-jdk

OpenJDK is the default Java version for Ubuntu, but it is also
possible to install the Oracle Java distribution.

Install Curl::

    sudo apt-get install curl

Install CouchDB and create the database::

    sudo apt-get install couchdb
    sudo /etc/init.d/couchdb start
    curl -X PUT http://127.0.0.1:5984/vix

Install Leiningen (you can also use another directory instead of
/usr/local/bin, like ~/bin in your home directory, as long as it is
added to the executable path)::

    wget https://raw.github.com/technomancy/leiningen/stable/bin/lein
    sudo mv lein /usr/local/bin/
    sudo chmod +x /usr/local/bin/lein 

Install Git::

    sudo apt-get install git

Install Vix::

    mkdir ~/clj
    cd ~/clj
    git clone git://github.com/fmw/vix.git
    cd vix/
    lein deps

If you chose a different database name earlier you should change the
src/clj/vix/config.clj script to reflect this.

Compile the client-side ClojureScript code using the `lein-cljsbuild`_
plugin::

    lein cljsbuild once

Start the REPL::

    lein repl

Execute the add-user function in order to add a user with full access
privileges::

    REPL started; server listening on localhost:35140.
    user=> (load "vix/auth")
    nil
    user=> (in-ns 'vix.auth)
    #<Namespace vix.auth>
    vix.auth=> (add-user "vix" "my-username" "my-password" {:* [:GET :PUT :DELETE :POST]})
    {:_rev "1-971bd05654d83183728c9d9ff08543b5",
    :_id "64e54e12dbed10a67e49af009d020776",
    :type "user",
    :username "my-username",
    :password "$2a$12$qrni2.vyScJEGc0ZfRXadeKw9Imp8SWvrHzatoF0cCPh.O8cNGIfMwC",
    :permissions {:* [:GET :PUT :DELETE :POST]}}

Only the lines starting with "user=>" and "vix.auth=>" need to be
entered (omitting those prefixes), because the other lines are REPL
output.

Create a directory to store Apache Lucene indexes (you can change the
path by editing the lucene-index-path value in src/clj/vix/config.clj)::

    sudo mkdir /var/lucene
    sudo mkdir /var/lucene/vix
    sudo chown yourusername:yourusergroup /var/lucene/vix

Start the server::

    cd ~/clj/vix/
    lein ring server

Now you can open the admin backend at http://localhost:3000/admin
(assuming everything was installed successfully).

In the near future we will provide a .war file that you can easily
deploy on your existing Java infrastructure. For now, you can compile
it thusly::

    lein ring uberwar

Questions and feedback
======================

Please feel free to contact Filip de Waard directly at fmw@vixu.com.
`Vixu.com`_ offers a hosted version of the software that includes
professional support, but we try to answer any questions and feedback
from members of the general public as well. We don't charge for
community support. Your feedback benefits all users and it is always
nice to hear from people that are using the software, so please get in
touch!

.. _`website-management application`: http:/www.vixu.com/
.. _`Content Management System`: http://en.wikipedia.org/wiki/Content_management_system
.. _`Vixu.com`: http:/www.vixu.com/
.. _`Apache License, version 2.0`: http://www.apache.org/licenses/LICENSE-2.0.html
.. _`commitment to open source`: http://www.vixu.com/en/open-source-website-management-software.html
.. _`Clojure`: http://clojure.org/
.. _`Enlive`: https://github.com/cgrand/enlive
.. _`Compojure`: https://github.com/weavejester/compojure
.. _`ClojureScript`: https://github.com/clojure/clojurescript
.. _`Google Closure`: http://code.google.com/closure/
.. _`Leiningen`: https://github.com/technomancy/leiningen
.. _`Git`: http://git-scm.com/
.. _`curl`: http://curl.haxx.se/
.. _`ClojureScript quickstart instructions`: https://github.com/clojure/clojurescript/wiki/Quick-Start
.. _`Apache Tomcat`: http://tomcat.apache.org/
.. _`CouchDB`: http://couchdb.apache.org/
.. _`lein-cljsbuild`: https://github.com/emezeske/lein-cljsbuild
