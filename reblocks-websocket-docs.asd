(defsystem "reblocks-websocket-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-websocket/"
  :class :package-inferred-system
  :description "Provides documentation for reblocks-websocket."
  :source-control (:git "https://github.com/40ants/reblocks-websocket")
  :bug-tracker "https://github.com/40ants/reblocks-websocket/issues"
  :pathname "docs"
  :depends-on ("reblocks-websocket"
               "reblocks-websocket-docs/index"))
