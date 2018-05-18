===========
 ChangeLog
===========

0.3.0
=====

* Added ``send-command`` function, which can be used to call custom
  functions on the frontend.

  For example, you can update some progress bar's text with such code.

  Add this as a dependency for your widget:

  .. code:: common-lisp

     (weblocks-parenscript:make-dependency*
       `(setf (@ window command-handlers update-progress)
              (lambda (params)
                (let ((new-text (@ params new-text)))
                
                  ;; Updating the text of the progress-bar
                  (chain (j-query ".loading-progress")
                         (html new-text))))))

  After that, you can call this handler from the server-side:
  
  .. code:: common-lisp
            
     (weblocks.websocket:send-command 'update-progress
                                      :new-text "Processed 100500 items")
0.2.0
=====

* Fixed to work under the Woo server (but others aren't supported for
  now), because this version of weblocks-websocket depends on a hack
  from the
  https://github.com/svetlyak40wt/websocket-driver/tree/make-woo-work-from-separate-threads
  branch, made in the
  https://github.com/svetlyak40wt/websocket-driver/commit/f9955b1fd99bac6cb744b72c734c7d845922ff6a commit.

0.1.1 (unreleased)
==================

* Now macro ``in-thread`` removes header ``X-Requested-With`` from
  captured request object. This requires ``weblocks >= 0.14.4``.

0.1.0 (unreleased)
==================

* Number features here.
* Like that.
* Add new versions to the top.
* Specify dates as ``2017-04-19``.
* Read `KeepAChangelog.com <http://keepachangelog.com/>`_ for futher
  explanations.
