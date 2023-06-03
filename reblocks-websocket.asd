#-asdf3.1 (error "reblocks-websocket requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "reblocks-websocket"
  :description "Reblocks extension adding a bidirectional communication via Websocket."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-websocket/"
  :source-control (:git "https://github.com/40ants/reblocks-websocket")
  :bug-tracker "https://github.com/40ants/reblocks-websocket/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("reblocks-websocket/websocket")
  :in-order-to ((test-op (test-op "reblocks-websocket-tests"))))
