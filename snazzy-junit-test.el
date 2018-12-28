(setq mock (szj-view
            :progress-progress-bar (szj-progress-bar :progress 100)
            :progress-progress-percentage (szj-progress-percentage :progress 100)
            :completed-progress-bar (szj-progress-bar :progress 100)
            :completed-progress-percentage (szj-progress-percentage :progress 100)
            :test-suites (list (szj-test-suite
                                :test-count 1
                                :error-count 0
                                :failure-count 0
                                :tests (list
                                        (szj-test
                                         :name "testXyz"))))))
