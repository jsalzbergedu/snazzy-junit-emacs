;;; pretty-test-runner.el --- Run and display tests in a visually appealing format -*- lexical-binding: t -*-

;; Copyright © 2018 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/pretty-test-runner-emacs
;; Version: 0.1.0
;; Keywords: pretty test runner junit java

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
;; (defface statusbar )

(defface pretty-test-runner--status-title-face
  '((t :inherit default :weight bold))
  "Face for displaying the status title.")

(defface pretty-test-runner--status-success-data-face
  '((t :inherit default :weight normal :foreground "#16a085"))
  "Face for displaying status data for successful tests")

(defface pretty-test-runner--status-success-data-face-bold
  '((t :inherit default :weight bold :foreground "#16a085"))
  "Face for displaying delimeters for successful tests.")

(defface pretty-test-runner--status-label-face
  '((t :inherit default :underline t))
  "Face for displaying the labels for test data.")

(defface pretty-test-runner--error-face
  '((t :inherit default :foreground "#c0392b" :weight bold))
  "Face for indicating an error.")

(defface pretty-test-runner--failure-face
  '((t :inherit default :foreground "#d35400" :weight bold))
  "Face for indicating a failure.")

(defface pretty-test-runner--success-face
  '((t :inherit default :foreground "#16a085" :weight bold))
  "Face for indicating success.")

;; View
;; Begin definition of the test result document

(cl-defgeneric pretty-test-runner-redraw-widget (widget)
  "Redraw the WIDGET.")

(cl-defgeneric pretty-test-runner-render-widget (widget)
  "Render the WIDGET.")

(cl-defgeneric pretty-test-runner-insert-widget (widget)
  "Insert the WIDGET in to the buffer at the point.
Move the point forward one.
If the widget contains other widgets, this method must be overloaded.")

(cl-defgeneric pretty-test-runner-delete-widget (widget)
  "Delete the WIDGET from the buffer.")

(defclass pretty-test-runner-widget ()
  ((widget-position :initarg :widget-position
             :initform 0
             :type integer
             :custom integer
             :accessor widget-position
             :documentation "The position of the widget"))
  "Common properties of a widget.")

(cl-defmethod pretty-test-runner-insert-widget ((widget pretty-test-runner-widget))
  (let ((inhibit-point-motion-hooks t))
      (setf (widget-position widget) (point))
      (insert (propertize " " 'display
                          (list (pretty-test-runner-render-widget widget))))
      (forward-char)))


(cl-defmethod pretty-test-runner-delete-widget ((widget pretty-test-runner-widget))
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (with-silent-modifications
        (goto-char (widget-position widget))
        (delete-region (point) (+ 1 (point)))))))

(cl-defmethod pretty-test-runner-redraw-widget ((widget pretty-test-runner-widget))
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (with-silent-modifications
        (pretty-test-runner-delete-widget widget)
        (goto-char (widget-position widget))
        (pretty-test-runner-insert-widget widget)))))

;; (cl-defmethod redraw-widget :before ((widget pretty-test-runner-widget))
;;   "Delete the redrawable portion of the widget so that it can be redrawn."
;;   (with-current-buffer (associated-buffer widget)
;;     (delete-region (redrawable-start widget) (redrawable-end widget)))
;;   (redraw-widget widget))

(defclass pretty-test-runner-title (pretty-test-runner-widget)
  ((text :initarg :text
         :initform ""
         :type string
         :custom string
         :reader text
         :documentation "The title text."))
  "A title in the pretty test runner document.")

(cl-defmethod pretty-test-runner-render-widget ((widget pretty-test-runner-title))
  (let ((s (text widget)))
    (put-text-property 0 (length s)
                       'face 'pretty-test-runner--status-title-face
                       s)
    s))

(defclass pretty-test-runner-label (pretty-test-runner-widget)
  ((text :initarg :text
         :initform ""
         :type string
         :custom string
         :reader text
         :documentation "The label text."))
  "A label in the pretty test runner document.")

(cl-defmethod pretty-test-runner-render-widget ((widget pretty-test-runner-label))
  (let ((s (text widget)))
    (put-text-property 0 (length s)
                       'face 'pretty-test-runner--status-label-face
                       s)
    (concat s ":")))

(defclass pretty-test-runner-progress-bar (pretty-test-runner-widget)
  ((progress :initarg :progress
             :initform 0
             :type integer
             :custom integer
             :accessor progress
             :documentation "The progress of the test runner. Should be less than 100."))
  "A progress bar in the pretty test runner document.")

(cl-defmethod pretty-test-runner-render-widget ((widget pretty-test-runner-progress-bar))
  (let* ((size (round (/ (progress widget) 10)))
         (fmtstr (concat "%-" (format "%d" 10) "s"))
         (hashes (cl-loop repeat size concat "#"))
         (s (format fmtstr hashes))
         (beg "[")
         (end "]"))
    (put-text-property 0 (length s)
                       'face 'pretty-test-runner--status-success-data-face
                       s)
    (put-text-property 0 1 'face
                       'pretty-test-runner--status-success-data-face-bold
                       beg)
    (put-text-property 0 1 'face
                       'pretty-test-runner--status-success-data-face-bold
                       end)
    (concat beg s end)))


(defclass pretty-test-runner-progress-percentage (pretty-test-runner-widget)
  ((progress :initarg :progress
             :initform 0
             :type integer
             :custom integer
             :accessor progress
             :documentation "The progress of the test runner."))
  "A progress percentage in the pretty test runner document.")

(cl-defmethod pretty-test-runner-render-widget ((widget pretty-test-runner-progress-percentage))
  (let* ((s (concat (format "%d" (progress widget)) "%"))
         (beg "(")
         (end ")"))
    (put-text-property 0 (length s)
                       'face 'pretty-test-runner--status-success-data-face
                       s)
    (put-text-property 0 1 'face
                       'pretty-test-runner--status-success-data-face-bold
                       beg)
    (put-text-property 0 1 'face
                       'pretty-test-runner--status-success-data-face-bold
                       end)
    (concat beg s end)))

(defclass pretty-test-runner-test-suite (pretty-test-runner-widget)
  ((name :initarg :name
         :initform "<default name>"
         :type string
         :custom string
         :reader name
         :documentation "The name of the test suite.")
   (tests-label :initarg :tests-label
                :initform (pretty-test-runner-label :text "tests")
                :type pretty-test-runner-label
                :custom pretty-test-runner-label
                :reader tests-label
                :documentation "The label: tests:.")
   (test-count :initarg :test-count
               :initform 0
               :type integer
               :custom integer
               :accessor test-count
               :documentation "The number of tests that the test suite has.")
   (errors-label :initarg :errors-label
                 :initform (pretty-test-runner-label :text "errors")
                 :type pretty-test-runner-label
                 :custom pretty-test-runner-label
                 :reader errors-label
                 :documentation "The label: errors:.")
   (error-count :initarg :error-count
                :initform 0
                :type integer
                :custom integer
                :accessor error-count
                :documentation "The number of errors that the test suite has.")
   (failures-label :initarg :failures-label
                   :initform (pretty-test-runner-label :text "failures")
                   :type pretty-test-runner-label
                   :custom pretty-test-runner-label
                   :reader failures-label
                   :documentation "The label: failures:.")
   (failure-count :initarg :failure-count
                  :initform 0
                  :type integer
                  :custom integer
                  :accessor failure-count
                  :documentation "The number of failures that the test suite has.")
   (tests :initarg :tests
          :initform '()
          :type list
          :custom list
          :accessor tests
          :documentation "A list of tests that the test suite has"))
  "A test suite in the pretty test runner document")

(cl-defmethod pretty-test-runner-render-widget ((widget pretty-test-runner-test-suite))
  (concat (let ((name (format "- Test Suite %s" (name widget))))
            "- "
            (put-text-property 0 (length name) 'face
                               'pretty-test-runner--success-face
                               name)
            name)
          ", "
          (pretty-test-runner-render-widget (tests-label widget))
          " "
          (let* ((tests (test-count widget))
                 (tests (format "%d" tests)))
            (put-text-property 0 (length tests) 'face
                               'pretty-test-runner--success-face
                               tests)
            tests)
          ", "
          (pretty-test-runner-render-widget (errors-label widget))
          " "
          (let* ((errors (error-count widget))
                 (errors (format "%d" errors)))
            (put-text-property 0 (length errors) 'face
                               'pretty-test-runner--error-face
                               errors)
            errors)
          ", "
          (pretty-test-runner-render-widget (failures-label widget))
          " "
          (let* ((failures (failure-count widget))
                 (failures (format "%d" failures)))
            (put-text-property 0 (length failures) 'face
                               'pretty-test-runner--failure-face
                               failures)
            failures)
          "\n"
          (let* ((tests (tests widget))
                 (tests (-map 'pretty-test-runner-render-widget tests))
                 (tests (-map (lambda (x) (concat "  " x)) tests)))
            (-reduce-from #'concat "" tests))))

(defconst pretty-test-runner-success-label-const
  (pretty-test-runner-label :text "success"))

(defclass pretty-test-runner-test (pretty-test-runner-widget)
  ((success-label :initarg :success-label
                  :initform (pretty-test-runner-label :text "success")
                  :type pretty-test-runner-label
                  :custom pretty-test-runner-label
                  :reader success-label
                  :documentation "The label: success:")
   (name :initarg :name
         :type string
         :custom string
         :reader name
         :documentation "The name of the test"))
  "A test in a test suite")

(cl-defmethod pretty-test-runner-render-widget ((widget pretty-test-runner-test))
  (concat "- "
          (pretty-test-runner-render-widget (success-label widget))
          " "
          (let ((s (name widget)))
            (put-text-property 0 (length s) 'face
                               'pretty-test-runner--success-face
                               s)
            s)
          "\n"))

(defclass pretty-test-runner-view (pretty-test-runner-widget)
  ((running-title :initarg :running-title
                  :initform (pretty-test-runner-title :text "Pretty Test Runner -- Running Test(s)")
                  :type pretty-test-runner-title
                  :custom pretty-test-runner-title
                  :reader running-title
                  :documentation "The first line in the document")
   (progress-label :initarg :progress-label
                   :initform (pretty-test-runner-label :text "progress")
                   :type pretty-test-runner-label
                   :custom pretty-test-runner-label
                   :reader progress-label
                   :documentation "The label: progress:.")
   (progress-progress-bar :initarg :progress-progress-bar
                          :initform (pretty-test-runner-progress-bar)
                          :type pretty-test-runner-progress-bar
                          :custom pretty-test-runner-progress-bar
                          :accessor progress-progress-bar
                          :documentation "The progress bar for the execution progress.")
   (progress-progress-percentage :initarg :progress-progress-percentage
                                 :initform (pretty-test-runner-progress-percentage)
                                 :type pretty-test-runner-progress-percentage
                                 :custom pretty-test-runner-progress-percentage
                                 :accessor progress-progress-percentage
                                 :documentation "The percentage for the execution progress.")
   (completed-title :initarg :completed-title
                    :initform (pretty-test-runner-title :text "Pretty Test Runner -- Finished Running Test(s)")
                    :type pretty-test-runner-title
                    :custom null pretty-test-runner-title
                    :accessor completed-title
                    :documentation "The title displayed when the test runner is finished running tests.")
   (completed-label :initarg :completed-label
                    :initform (pretty-test-runner-label :text "status")
                    :type pretty-test-runner-label
                    :custom pretty-test-runner-label
                    :reader completed-label
                    :documentation "The label: status:")
   (completed-progress-bar :initarg :completed-progress-bar
                           :initform nil
                           :type (or null pretty-test-runner-progress-bar)
                           :custom (or null pretty-test-runner-progress-bar)
                           :accessor completed-progress-bar
                           :documentation "The progress bar after the tests have completed.")
   (completed-progress-percentage :initarg :completed-progress-percentage
                                  :initform nil
                                  :type (or null pretty-test-runner-progress-percentage)
                                  :custom (or null pretty-test-runner-progress-percentage)
                                  :accessor completed-progress-percentage
                                  :documentation "The percentage after the tests have been completed.")
   (test-suites-label :initarg :test-suites-label
                      :initform (pretty-test-runner-label :text "Test Suites")
                      :type pretty-test-runner-label
                      :custom pretty-test-runner-label
                      :accessor test-suites-label
                      :documentation "The label for the test suites")
   (test-suites :initarg :test-suites
                :initform nil
                :type (or null list)
                :custom (or null list)
                :accessor test-suites
                :documentation "The test suites."))
  "The model of the document.")


(cl-defmethod pretty-test-runner-render-widget ((widget pretty-test-runner-view))
  (concat (pretty-test-runner-render-widget
           (running-title widget))
          "\n"
          (pretty-test-runner-render-widget
           (progress-label widget))
          " "
          (pretty-test-runner-render-widget
           (progress-progress-bar widget))
          " "
          (pretty-test-runner-render-widget
           (progress-progress-percentage widget))
          "\n"
          (when-let ((completed-title (completed-title widget))
                     (completed-label (completed-label widget))
                     (progress-bar (completed-progress-bar widget))
                     (percentage (completed-progress-percentage widget)))
            (concat "\n"
                    (pretty-test-runner-render-widget completed-title)
                    "\n"
                    (pretty-test-runner-render-widget completed-label)
                    "   "
                    (pretty-test-runner-render-widget progress-bar)
                    " "
                    (pretty-test-runner-render-widget percentage)
                    "\n"))
          (when-let ((test-suites-label (test-suites-label widget))
                     (test-suites (test-suites widget)))
            (concat "\n"
                    (pretty-test-runner-render-widget test-suites-label)
                    "\n"
                    (apply #'concat (-map #'pretty-test-runner-render-widget test-suites))))))

;; End definition of the test result document

;; Eventually the different types of loading bars:
;; super-high-res (maybe) [image]
;; high res: |██████████|
;; low res: [##########]

(defun pretty-test-runner--highlight-to-end-at-point (face)
  "Highlight the current point to the end of the line with FACE."
  (put-text-property (point) (+ 1 (line-end-position)) 'face
                     face))
;; (put-text-property (point) (+ 1 (line-end-position)) 'face
;;                    'magit-diff-added-highlight)


;;; pretty-test-runner.el ends here
