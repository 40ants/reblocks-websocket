(in-package :cl-user)
(defpackage weblocks-websocket-test
  (:use :cl
        :weblocks-websocket
        :prove
        :hamcrest.matchers))
(in-package :weblocks-websocket-test)


(plan 1)

(subtest "Replace this test with real staff."
  (assert-that (foo 1 2 3)
               (contains 1 2 3)))

(finalize)
