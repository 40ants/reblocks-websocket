(defpackage #:reblocks-websocket-test
  (:use #:cl
        #:reblocks-websocket
        #:prove
        #:hamcrest.matchers))
(in-package #:reblocks-websocket-test)


(plan 1)

(subtest "Replace this test with real staff."
  (assert-that (foo 1 2 3)
               (contains 1 2 3)))

(finalize)
