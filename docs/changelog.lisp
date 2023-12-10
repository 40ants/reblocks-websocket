(uiop:define-package #:reblocks-websocket-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:reblocks-websocket-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.5.0 2022-12-10
         "* Support new Reblocks version where REMOVED argument was added to REBLOCKS/WIDGET:UPDATE generic-function.")
  (0.4.0 2019-08-02
         "* Library was fixed to work with latest `Woo`.")
  
  (0.3.0 2018-05-18
         "* Added `send-command` function, which can be used to call custom
            functions on the frontend.

            For example, you can update some progress bar's text with such code.

            Add this as a dependency for your widget:

            ```lisp
            (weblocks-parenscript:make-dependency*
              `(setf (@ window command-handlers update-progress)
                     (lambda (params)
                       (let ((new-text (@ params new-text)))
                          
                         ;; Updating the text of the progress-bar
                         (chain (j-query \".loading-progress\")
                                (html new-text))))))
            ```

            After that, you can call this handler from the server-side:
  
            ```lisp
            (weblocks.websocket:send-command 'update-progress
                                             :new-text \"Processed 100500 items\")
            ```
")
  (0.2.0 2018-05-05
         "* Fixed to work under the Woo server (but others aren't supported for
            now), because this version of weblocks-websocket depends on a hack
            from the
            https://github.com/svetlyak40wt/websocket-driver/tree/make-woo-work-from-separate-threads
            branch, made in the
            https://github.com/svetlyak40wt/websocket-driver/commit/f9955b1fd99bac6cb744b72c734c7d845922ff6a commit.")
  (0.1.1 2017-10-07
         "* Now macro `in-thread` removes header `X-Requested-With` from
            captured request object. This requires `weblocks >= 0.14.4`.")
  (0.1.0 2017-09-20
         "* Initial version."))
