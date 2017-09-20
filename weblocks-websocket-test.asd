#|
  This file is a part of weblocks-websocket project.
|#

(in-package :cl-user)
(defpackage weblocks-websocket-test-asd
  (:use :cl :asdf))
(in-package :weblocks-websocket-test-asd)

(defsystem weblocks-websocket-test
  :author ""
  :license ""
  :depends-on (:weblocks-websocket
               :prove
               :hamcrest-prove)
  :components ((:module "t"
                :components
                ((:test-file "weblocks-websocket"))))
  :description "Test system for weblocks-websocket"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
