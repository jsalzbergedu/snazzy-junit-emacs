;;; snazzy-junit.el --- Run and display tests in a visually appealing format -*- lexical-binding: t -*-

;; Copyright © 2018 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/snazzy-junit-emacs
;; Version: 0.1.0
;; Keywords: pretty test runner junit java
;; Prefix: szj

;; This file is not a part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(defface szj-status-title-face
  '((t :inherit default :weight bold))
  "Face for displaying the status title.")

(defface szj-status-success-data-face
  '((t :inherit default :weight normal :foreground "#16a085"))
  "Face for displaying status data for successful tests")

(defface szj-status-success-data-face-bold
  '((t :inherit default :weight bold :foreground "#16a085"))
  "Face for displaying delimeters for successful tests.")

(defface szj-status-label-face
  '((t :inherit default :underline t))
  "Face for displaying the labels for test data.")

(defface szj-error-face
  '((t :inherit default :foreground "#c0392b" :weight bold))
  "Face for indicating an error.")

(defface szj-failure-face
  '((t :inherit default :foreground "#d35400" :weight bold))
  "Face for indicating a failure.")

(defface szj-success-face
  '((t :inherit default :foreground "#16a085" :weight bold))
  "Face for indicating success.")

;; View
;; Begin definition of the test result document

(cl-defgeneric szj-redraw-widget (widget)
  "Redraw the WIDGET.")

(cl-defgeneric szj-render-widget (widget)
  "Render the WIDGET.")

(cl-defgeneric szj-insert-widget (widget)
  "Insert the WIDGET in to the buffer at the point.
Move the point forward one.
If the widget contains other widgets, this method must be overloaded.")

(cl-defgeneric szj-delete-widget (widget)
  "Delete the WIDGET from the buffer.")

(defclass szj-widget ()
  ((widget-position :initarg :widget-position
             :initform 0
             :type integer
             :custom integer
             :accessor szj-widget-position
             :documentation "The position of the widget"))
  "Common properties of a widget.")

(cl-defmethod szj-insert-widget ((widget szj-widget))
  (let ((inhibit-point-motion-hooks t))
      (setf (szj-widget-position widget) (point))
      (insert (propertize " " 'display
                          (list (szj-render-widget widget))))
      (forward-char)))

(cl-defmethod szj-delete-widget ((widget szj-widget))
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (with-silent-modifications
        (goto-char (szj-widget-position widget))
        (delete-region (point) (+ 1 (point)))))))

(cl-defmethod szj-redraw-widget ((widget szj-widget))
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (with-silent-modifications
        (szj-delete-widget widget)
        (goto-char (szj-widget-position widget))
        (szj-insert-widget widget)))))

(defclass szj-title (szj-widget)
  ((text :initarg :text
         :initform ""
         :type string
         :custom string
         :reader szj-text
         :documentation "The title text."))
  "A title in the pretty test runner document.")

(cl-defmethod szj-render-widget ((widget szj-title))
  (let ((s (szj-text widget)))
    (put-text-property 0 (length s)
                       'face 'szj-status-title-face
                       s)
    s))

(defclass szj-label (szj-widget)
  ((text :initarg :text
         :initform ""
         :type string
         :custom string
         :reader szj-text
         :documentation "The label text."))
  "A label in the pretty test runner document.")

(cl-defmethod szj-render-widget ((widget szj-label))
  (let ((s (szj-text widget)))
    (put-text-property 0 (length s)
                       'face 'szj-status-label-face
                       s)
    (concat s ":")))

(defclass szj-progress-bar (szj-widget)
  ((progress :initarg :progress
             :initform 0
             :type integer
             :custom integer
             :accessor szj-progress
             :documentation "The progress of the test runner. 
Should be less than 100."))
  "A progress bar in the pretty test runner document.")

(cl-defmethod szj-render-widget ((widget szj-progress-bar))
  (let* ((size (round (/ (szj-progress widget) 10)))
         (fmtstr (concat "%-" (format "%d" 10) "s"))
         (hashes (cl-loop repeat size concat "#"))
         (s (format fmtstr hashes))
         (beg "[")
         (end "]"))
    (put-text-property 0 (length s)
                       'face 'szj-status-success-data-face
                       s)
    (put-text-property 0 1 'face
                       'szj-status-success-data-face-bold
                       beg)
    (put-text-property 0 1 'face
                       'szj-status-success-data-face-bold
                       end)
    (concat beg s end)))


(defclass szj-progress-percentage (szj-widget)
  ((progress :initarg :progress
             :initform 0
             :type integer
             :custom integer
             :accessor szj-progress
             :documentation "The progress of the test runner."))
  "A progress percentage in the pretty test runner document.")

(cl-defmethod szj-render-widget ((widget szj-progress-percentage))
  (let* ((s (concat (format "%d" (szj-progress widget)) "%"))
         (beg "(")
         (end ")"))
    (put-text-property 0 (length s)
                       'face 'szj-status-success-data-face
                       s)
    (put-text-property 0 1 'face
                       'szj-status-success-data-face-bold
                       beg)
    (put-text-property 0 1 'face
                       'szj-status-success-data-face-bold
                       end)
    (concat beg s end)))

(defclass szj-test-suite (szj-widget)
  ((name :initarg :name
         :initform "<default name>"
         :type string
         :custom string
         :reader szj-name
         :documentation "The name of the test suite.")
   (tests-label :initarg :tests-label
                :initform (szj-label :text "tests")
                :type szj-label
                :custom szj-label
                :reader szj-tests-label
                :allocation :class
                :documentation "The label: tests:.")
   (test-count :initarg :test-count
               :initform 0
               :type integer
               :custom integer
               :accessor szj-test-count
               :documentation "The number of tests that the test suite has.")
   (errors-label :initarg :errors-label
                 :initform (szj-label :text "errors")
                 :type szj-label
                 :custom szj-label
                 :reader szj-errors-label
                 :allocation :class
                 :documentation "The label: errors:.")
   (error-count :initarg :error-count
                :initform 0
                :type integer
                :custom integer
                :accessor szj-error-count
                :documentation "The number of errors that the test suite has.")
   (failures-label :initarg :failures-label
                   :initform (szj-label :text "failures")
                   :type szj-label
                   :custom szj-label
                   :reader szj-failure-label
                   :allocation :class
                   :documentation "The label: failures:.")
   (failure-count :initarg :failure-count
                  :initform 0
                  :type integer
                  :custom integer
                  :accessor szj-failure-count
                  :documentation "The number of failures that the test suite has.")
   (tests :initarg :tests
          :initform '()
          :type list
          :custom list
          :accessor szj-tests
          :documentation "A list of tests that the test suite has"))
  "A test suite in the pretty test runner document")

(cl-defmethod szj-render-widget ((widget szj-test-suite))
  (concat (let ((name (format "- Test Suite %s" (szj-name widget))))
            "- "
            (put-text-property 0 (length name) 'face
                               'szj-success-face
                               name)
            name)
          ", "
          (szj-render-widget (szj-tests-label widget))
          " "
          (let* ((tests (szj-test-count widget))
                 (tests (format "%d" tests)))
            (put-text-property 0 (length tests) 'face
                               'szj-success-face
                               tests)
            tests)
          ", "
          (szj-render-widget (szj-errors-label widget))
          " "
          (let* ((errors (szj-error-count widget))
                 (errors (format "%d" errors)))
            (put-text-property 0 (length errors) 'face
                               'szj-error-face
                               errors)
            errors)
          ", "
          (szj-render-widget (szj-failure-label widget))
          " "
          (let* ((failures (szj-failure-count widget))
                 (failures (format "%d" failures)))
            (put-text-property 0 (length failures) 'face
                               'szj-failure-face
                               failures)
            failures)
          "\n"
          (let* ((tests (szj-tests widget))
                 (tests (-map 'szj-render-widget tests))
                 (tests (-map (lambda (x) (concat "  " x)) tests)))
            (-reduce-from #'concat "" tests))))

(defclass szj-test (szj-widget)
  ((success-label :initarg :success-label
                  :initform (szj-label :text "success")
                  :type szj-label
                  :custom szj-label
                  :reader szj-success-label
                  :allocation :class
                  :documentation "The label: success:")
   (name :initarg :name
         :type string
         :custom string
         :reader szj-name
         :documentation "The name of the test"))
  "A test in a test suite")

(cl-defmethod szj-render-widget ((widget szj-test))
  (concat "- "
          (szj-render-widget (szj-success-label widget))
          " "
          (let ((s (szj-name widget)))
            (put-text-property 0 (length s) 'face
                               'szj-success-face
                               s)
            s)
          "\n"))

(defclass szj-view (szj-widget)
  ((running-title :initarg :running-title
                  :initform (szj-title :text "Pretty Test Runner -- Running Test(s)")
                  :type szj-title
                  :custom szj-title
                  :reader szj-running-title
                  :allocation :class
                  :documentation "The first line in the document")
   (progress-label :initarg :progress-label
                   :initform (szj-label :text "progress")
                   :type szj-label
                   :custom szj-label
                   :reader szj-progress-label
                   :allocation :class
                   :documentation "The label: progress:.")
   (progress-progress-bar :initarg :progress-progress-bar
                          :initform (szj-progress-bar)
                          :type szj-progress-bar
                          :custom szj-progress-bar
                          :accessor szj-progress-progress-bar
                          :documentation "The progress bar for the execution progress.")
   (progress-progress-percentage :initarg :progress-progress-percentage
                                 :initform (szj-progress-percentage)
                                 :type szj-progress-percentage
                                 :custom szj-progress-percentage
                                 :accessor szj-progress-progress-percentage
                                 :documentation "The percentage for the execution progress.")
   (completed-title :initarg :completed-title
                    :initform (szj-title :text "Pretty Test Runner -- Finished Running Test(s)")
                    :type szj-title
                    :custom szj-title
                    :accessor szj-completed-title
                    :allocation :class
                    :documentation "The title displayed when the test runner is finished running tests.")
   (completed-label :initarg :completed-label
                    :initform (szj-label :text "status")
                    :type szj-label
                    :custom szj-label
                    :reader szj-completed-label
                    :allocation :class
                    :documentation "The label: status:")
   (completed-progress-bar :initarg :completed-progress-bar
                           :initform nil
                           :type (or null szj-progress-bar)
                           :custom (or null szj-progress-bar)
                           :accessor szj-completed-progress-bar
                           :documentation "The progress bar after the tests have completed.")
   (completed-progress-percentage :initarg :completed-progress-percentage
                                  :initform nil
                                  :type (or null szj-progress-percentage)
                                  :custom (or null szj-progress-percentage)
                                  :accessor szj-completed-progress-percentage
                                  :documentation "The percentage after the tests have been completed.")
   (test-suites-label :initarg :test-suites-label
                      :initform (szj-label :text "Test Suites")
                      :type szj-label
                      :custom szj-label
                      :accessor szj-test-suites-label
                      :allocation :class
                      :documentation "The label for the test suites")
   (test-suites :initarg :test-suites
                :initform nil
                :type (or null list)
                :custom (or null list)
                :accessor szj-test-suites
                :documentation "The test suites."))
  "The model of the document.")


(cl-defmethod szj-render-widget ((widget szj-view))
  (concat (szj-render-widget
           (szj-running-title widget))
          "\n"
          (szj-render-widget
           (szj-progress-label widget))
          " "
          (szj-render-widget
           (szj-progress-progress-bar widget))
          " "
          (szj-render-widget
           (szj-progress-progress-percentage widget))
          "\n"
          (when-let ((completed-title (szj-completed-title widget))
                     (completed-label (szj-completed-label widget))
                     (progress-bar (szj-completed-progress-bar widget))
                     (percentage (szj-completed-progress-percentage widget)))
            (concat "\n"
                    (szj-render-widget completed-title)
                    "\n"
                    (szj-render-widget completed-label)
                    "   "
                    (szj-render-widget progress-bar)
                    " "
                    (szj-render-widget percentage)
                    "\n"))
          (when-let ((test-suites-label (szj-test-suites-label widget))
                     (test-suites (szj-test-suites widget)))
            (concat "\n"
                    (szj-render-widget test-suites-label)
                    "\n"
                    (apply #'concat (-map #'szj-render-widget test-suites))))))

;; End definition of the test result document

;; Eventually the different types of loading bars:
;; super-high-res (maybe) [image]
;; high res: |██████████|
;; low res: [##########]

(defun szj--highlight-to-end-at-point (face)
  "Highlight the current point to the end of the line with FACE."
  (put-text-property (point) (+ 1 (line-end-position)) 'face
                     face))
;; (put-text-property (point) (+ 1 (line-end-position)) 'face
;;                    'magit-diff-added-highlight)

(provide 'snazzy-junit)
;;; snazzy-junit.el ends here
