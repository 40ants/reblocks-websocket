(defsystem "reblocks-websocket-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-websocket/"
  :class :package-inferred-system
  :description "Provides tests for reblocks-websocket."
  :source-control (:git "https://github.com/40ants/reblocks-websocket")
  :bug-tracker "https://github.com/40ants/reblocks-websocket/issues"
  :pathname "t"
  :depends-on ("reblocks-websocket-tests/core")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
