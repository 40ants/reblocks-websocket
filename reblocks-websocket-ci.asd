(defsystem "reblocks-websocket-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-websocket/"
  :class :package-inferred-system
  :description "Provides CI settings for reblocks-websocket."
  :source-control (:git "https://github.com/40ants/reblocks-websocket")
  :bug-tracker "https://github.com/40ants/reblocks-websocket/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "reblocks-websocket-ci/ci"))
