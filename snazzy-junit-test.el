(defvar mock '())

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
(setq mock (szj-test-suite :test-count 1
                           :error-count 0
                           :failure-count 0
                           :tests (list
                                   (szj-test
                                    :name "testXyz"))))

(setq mock
      (szj-test-suite :inner-list (szj-list 
                                   :list-title (szj-test-suite-title :test-count 1
                                                                     :error-count 0
                                                                     :failure-count 0)
                                   :items (list (szj-test :name "testXyz")))))
;; (type-of (car (-filter (lambda (x) (equal (format "%s" (szj-element-id x))
;;                                      "g25"))
;;                        testtest)))

(setf mock (szj-list :list-title (szj-label :text "xyz")
                     :items (list (szj-test :name "asdf"))))

(setq mock (szj-progress-bar :progress 50))

(setq mock (szj-expander))
