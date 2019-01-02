(defvar mock '())

(setq mock (szj-view
            :progress-progress-indicator (szj-progress-indicator :progress 100)
            :completed-viewable t
            :completed-progress-indicator (szj-progress-indicator :progress 100)
            :test-suites-viewable t
            :test-suites (list (szj-test-suite
                                :inner-list
                                (szj-list
                                 :list-title
                                 (szj-test-suite-title :test-count 1
                                                       :error-count 0
                                                       :failure-count 0)
                                 :items (list (szj-test :name "testXyz")))))))
