(uiop:define-package #:reblocks-websocket-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:reblocks-websocket)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:reblocks-websocket-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:reblocks-websocket-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "reblocks-websocket-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "reblocks-websocket - Reblocks extension adding a bidirectional communication via Websocket."
                    :ignore-words ("JSON"
                                   "JS"
                                   "HTTP"
                                   "TODO"
                                   "Unlicense"
                                   "REPL"
                                   "GIT"))
  (reblocks-websocket system)
  "
[![](https://github-actions.40ants.com/40ants/reblocks-websocket/matrix.svg?only=ci.run-tests)](https://github.com/40ants/reblocks-websocket/actions)

![Quicklisp](http://quickdocs.org/badge/reblocks-websocket.svg)

This module allows you to push some information from backend to frontend
and. In this case, updates of widgets's state on the client are
initiated by server. For example, you can have some sort of long running
process on the server and need to show it's status to the user.
"
  (@installation section)
  (@usage section)
  (@api section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
This library depends on Reblocks (Weblocks fork) and a websocket-driver.
If you will use Woo server then probably you'll need this fork of the websocket-driver
([make-woo-work-from-separate-threads](https://github.com/svetlyak40wt/websocket-driver/tree/make-woo-work-from-separate-threads) branch).
However, may be the recent version of websocket-driver will work just find, I don't know.

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :reblocks-websocket)
```

""")


(defsection @usage (:title "Usage"
                    :ignore-words ("ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"))
  "
Define you widget and inherit it from the
REBLOCKS-WEBSOCKET:WEBSOCKET-WIDGET:

```lisp          
(reblocks:defwidget counter-box (reblocks-websocket:websocket-widget)
  ((counter :initform 0
            :accessor counter)))
```

Define a code which will start some sort of background activity. In this
example we are doing it right when widget was created in the beginning
of the user session, but of cause, you can do it as a reaction on an
action.

```lisp
(defmethod initialize-instance ((instance counter-box) &rest restargs)
  (declare (ignorable restargs))
  (call-next-method)

  (reblocks-websocket:in-thread (\"Update counter\")
    (sleep 3)
    ;; Updating counter
    (incf (counter instance))
    (reblocks:update instance)))
```

That is it. Define a render method as usual and use the widget on the
page. Counter will be updated automatically. This works like a magic,
a framework makes all dirty work under the hood.
")


(defautodoc @api (:system "reblocks-websocket"))
