(uiop:define-package #:reblocks-websocket-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:reblocks-websocket-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))
