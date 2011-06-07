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

Vix is written in `Clojure`_, a Lisp implementation that runs on the Java
Virtual Machine (JVM). This means that Vix embraces the existing Java
infrastructure and (legacy) Java libraries used by enterprise customers, while
using a language that allows for rapid development. Vix uses `Enlive`_ as a
templating solution and the `Compojure`_ framework. On the front-end, we're
using the `jQuery`_, `underscore.js` and `Backbone.js`_ Javascript frameworks.
Vix exposes all functionality through a RESTful API, allowing for easy
integration with external applications.

Installation
============

Right now, Vix is still in the prototyping stage. If you want to try it out
you can install `Leiningen`_, check out this Git repository and run:

    lein ring server

In the near future we will be providing a .war file that you can easily
deploy on your existing Java infrastructure. For now, you can compile it
yourself thusly:

    lein uberwar

.. _`Content Management System`: http://en.wikipedia.org/wiki/Content_management_system
.. _`Apache License, version 2.0`: http://www.apache.org/licenses/LICENSE-2.0.html
.. _`Net Collective`: http://netcollective.nl
.. _`Clojure`: http://clojure.org/
.. _`Enlive`: https://github.com/cgrand/enlive
.. _`Compojure`: https://github.com/weavejester/compojure
.. _`jQuery`: http://jquery.com/
.. _`underscore.js`: http://documentcloud.github.com/underscore/
.. _`Backbone.js`: http://documentcloud.github.com/backbone/
.. _`Leiningen`: https://github.com/technomancy/leiningen
