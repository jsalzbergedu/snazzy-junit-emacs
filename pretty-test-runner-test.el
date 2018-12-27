(setq mock (pretty-test-runner-view
            :progress-progress-bar (pretty-test-runner-progress-bar :progress 100)
            :progress-progress-percentage (pretty-test-runner-progress-percentage :progress 100)
            :completed-progress-bar (pretty-test-runner-progress-bar :progress 100)
            :completed-progress-percentage (pretty-test-runner-progress-percentage :progress 100)
            :test-suites (list (pretty-test-runner-test-suite
                                :test-count 1
                                :error-count 0
                                :failure-count 0
                                :tests (list
                                        (pretty-test-runner-test
                                         :name "testXyz"))))))
