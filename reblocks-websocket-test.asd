(defsystem "reblocks-websocket-test"
  :author ""
  :license ""
  :depends-on (:reblocks-websocket
               :prove
               :hamcrest-prove)
  :components ((:module "t"
                :components
                ((:test-file "reblocks-websocket"))))
  :description "Test system for reblocks-websocket"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
