;; snazzy-junit.el --- Run and display tests in a visually appealing format -*- lexical-binding: t -*-

;; Copyright © 2018 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/snazzy-junit-emacs
;; Version: 0.1.0
;; Keywords: snazzy junit unit testing java
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

;; Quick implementation of the observer pattern
(cl-defgeneric szj-notify (subject)
  "Notify the subject's observers.")

(cl-defgeneric szj-add-listener (subject listener)
  "Add a LISTENER to the SUBJECT.")

(cl-defgeneric szj-format-with-id (widget str)
  "Format the STR with the WIDGET id.")

(defclass szj-subject ()
  ((listeners :initarg :listeners
              :initform nil
              :type list
              :custom list
              :accessor szj-listeners
              :documentation "The objects that depend on this widget's state."))
  "An object that notifies listeners when szj-notify is called on it.")

(cl-defmethod szj-notify ((subject szj-subject))
  (cl-loop for observer in (szj-listeners subject)
           do (funcall observer)))

(cl-defmethod szj-add-listener ((subject szj-subject) listener)
  (push listener (szj-listeners subject)))

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

(cl-defgeneric szj-point-at-element (widget)
  "Determine whether the point is currently at WIDGET.")

(cl-defgeneric szj-element-ids (widget)
  "Get all the element ids that the WIDGET is responsible for.")

(defclass szj-element ()
  ((element-id :initform (gensym)
               :type symbol
               :custom symbol
               :accessor szj-element-id))
  "An element of the test result document.")

;; ;; TODO delete after debugging
(defvar testtest nil)
(cl-defmethod initialize-instance :after ((object szj-element) &rest slots)
  (push object testtest))

(cl-defmethod szj-point-at-element ((element szj-element))
  (when-let ((id-element (szj-element-id element))
             (id-text (get-text-property (point) 'szj-element-id)))
    (eq id-element id-text)))

(defclass szj-widget (szj-subject szj-element)
  ((widget-position :initarg :widget-position
                    :initform 0
                    :type integer
                    :custom integer
                    :accessor szj-widget-position
                    :documentation "The position of the widget")
   (children :initarg :children
             :initform nil
             :type list
             :custom list
             :accessor szj-children
             :documentation "The children of the widget, to be rendered."))
  "A widget.")


(cl-defmethod szj-element-ids ((widget szj-widget))
  (list (szj-element-id widget)))

(cl-defmethod szj-insert-widget ((widget szj-widget))
  (let ((inhibit-point-motion-hooks t))
      (setf (szj-widget-position widget) (point))
      (with-silent-modifications
        (insert (propertize (szj-render-widget widget) 'szj-element-id
                            (szj-element-id widget))))))

(cl-defmethod szj-insert-widget ((widget string))
  (let ((inhibit-point-motion-hooks t))
    (with-silent-modifications
      (insert widget))))

(cl-defmethod szj-delete-widget ((widget szj-widget))
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
          (point 0))
      (with-silent-modifications
        (goto-char (szj-widget-position widget))
        (setq point (point))
        (while (szj-point-at-element widget)
          (delete-region point (+ 1 point)))))))

(cl-defmethod szj-redraw-widget ((widget szj-widget))
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (with-silent-modifications
        (szj-delete-widget widget)
        (goto-char (szj-widget-position widget))
        (szj-insert-widget widget)))))

(defclass szj-plaintext (szj-widget)
  ((text :initarg :text
         :type string
         :custom string
         :reader szj-text
         :documentation "The text"))
  "A piece of text in the test result document.")

(cl-defmethod szj-render-widget ((widget szj-plaintext))
  (szj-text widget))

(defclass szj-title (szj-widget)
  ((text :initarg :text
         :initform ""
         :type string
         :custom string
         :reader szj-text
         :documentation "The title text."))
  "A title in the test result document.")

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
  "A label in the test result document.")

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
  "A progress bar in the test result document.")

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
  "A progress percentage in the test result document.")

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

(defclass szj-progress-indicator (szj-widget)
  ((progress :initarg :progress
             :initform 0
             :type integer
             :custom integer
             :accessor szj-progress
             :documentation "The progress of the test runner.")
   (bar :initarg :progress-bar
        :initform (szj-progress-bar)
        :type szj-progress-bar
        :custom szj-progress-bar
        :accessor szj-bar
        :documentation "The progress bar of the indicator.")
   (percentage :initarg :progress-percentage
               :initform (szj-progress-percentage)
               :type szj-progress-percentage
               :custom szj-progress-percentage
               :accessor szj-percentage
               :documentation "The percentage of the indicator"))
  "A progress bar and percentage in the test result document.")

(cl-defmethod szj-render-widget ((widget szj-progress-indicator))
  (let ((bar (szj-bar widget))
        (percentage (szj-percentage widget))
        (progress (szj-progress widget)))
    (setf (szj-progress bar) progress)
    (setf (szj-progress percentage) progress)
    (concat (szj-render-widget bar) " " (szj-render-widget percentage))))

(cl-defgeneric szj-container-allchildren (container)
  "Get all the children, visible or invisible, of the container.
May contain widgets and containers.
Does not contain string children of the container.")

(cl-defgeneric szj-container-children (container)
  "Get the (viewable) children of the container.
May contain strings, widgets, and containers.")

(cl-defgeneric szj-set-container-position (container position)
  "Set the position of the CONTAINER to POSITION.")

(defclass szj-container (szj-element szj-subject)
  ((container-position :initform 0
                       :initarg :container-position
                       :type integer
                       :custom integer
                       :accessor szj-container-position
                       :documentation "The starting position."))
  "A container of widgets.")

(cl-defmethod szj-element-ids ((widget szj-container))
  (apply #'append
         (cons (list (szj-element-id widget))
               (-map #'szj-element-ids (szj-container-allchildren widget)))))

(cl-defmethod szj-delete-widget ((widget szj-container))
  (save-excursion
    (goto-char (szj-container-position widget))
    (let* ((inhibit-point-motion-hooks t)
           (to-remove (szj-element-ids widget))
           (point (point)))
      (while (< point (point-max))
        (let ((none-left t))
          (cl-loop for sym in to-remove
                   do (when (eq (get-text-property point 'szj-element-id) sym)
                        (delete-region point (+ 1 point))
                        (setq none-left nil)))
          (when none-left
            (forward-char)
            (setq point (+ 1 point))))))))

(cl-defgeneric szj-stamp-container-child (widget child)
  "Stamp the container child. 
In the case of a widget, do nothing. In the case of a string, 
add the element id if there isn't one already.")

(cl-defmethod szj-stamp-container-child ((widget szj-container)
                                         (child szj-widget))
  child)

(cl-defmethod szj-stamp-container-child ((widget szj-container) (child string))
  (if (get-text-property 0 'szj-element-id child)
      child
    (propertize child 'szj-element-id (szj-element-id widget))))

(cl-defmethod szj-stamp-container-child ((widget szj-container)
                                         (child szj-container))
  child)

(cl-defmethod szj-container-children :around ((container szj-container))
  (let ((ret (cl-call-next-method container)))
    (cl-loop for child in ret
             collect (szj-stamp-container-child container child))))

(cl-defmethod szj-insert-widget ((widget szj-container))
  (let ((inhibit-point-motion-hooks t))
    (save-excursion
      (setf (szj-container-position widget) (point))
      (cl-loop for w in (szj-container-children widget)
               do (szj-insert-widget w)))))

(cl-defmethod szj-redraw-widget ((widget szj-container))
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (goto-char (szj-container-position widget))
      (szj-delete-widget widget)
      (szj-insert-widget widget))))

(defclass szj-expander (szj-widget)
  ((folded :initform nil
           :initarg :folded
           :type boolean
           :custom boolean
           :accessor szj-folded
           :documentation "Whether the expander has been folded"))
  "A expander in the test runner document.")

(cl-defmethod szj-render-widget ((widget szj-expander))
  (let* ((f (lambda ()
             (interactive)
             (setf (szj-folded widget) (not (szj-folded widget)))
             (szj-notify widget)))
         (map (make-sparse-keymap)))
    (define-key map [return] f)
    (define-key map [mouse-1] f)
    (propertize (if (szj-folded widget) "+" "-")
                'keymap map)))

(defclass szj-list (szj-container)
  ((list-expander :initform (szj-expander)
                  :type szj-expander
                  :custom szj-expander
                  :reader szj-list-expander
                  :documentation "The expander of the list.")
   (list-title :initarg :list-title
               :type szj-widget
               :custom szj-widget
               :accessor szj-list-title
               :documentation "The title of the list")
   (items :initarg :items
          :initform nil
          :type list
          :custom list
          :accessor szj-items
          :documentation "The items of the list."))
  "A list of items in the test result document.")

(cl-defmethod initialize-instance :after ((object szj-list) &rest slots)
  (let ((list-expander (szj-list-expander object)))
    (szj-add-listener list-expander (lambda ()
                                      (szj-redraw-widget object)))))

(cl-defmethod szj-container-allchildren ((container szj-list))
  (append (list (szj-list-expander container)
                (szj-list-title container))
          (szj-items container)))

(cl-defmethod szj-container-children ((container szj-list))
  (append (list (szj-list-expander container)
                " "
                (szj-list-title container)
                "\n")
          (unless (szj-folded (szj-list-expander container))
            (let* ((items (szj-items container))
                   ret)
              (cl-loop for item in items
                       do
                       (push "  " ret)
                       (push item ret)
                       (push "\n" ret))
              (setq ret (nreverse ret))
              ret))))

(defclass szj-test-suite-title (szj-widget)
  ((name :initarg :name
         :initform "<default name>"
         :type string
         :custom string
         :reader szj-name
         :documentation "The name of the test suite.")
   (tests-label :initform (szj-label :text "tests")
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
               :documentation "The number of tests the test suite has.")
   (errors-label :initform (szj-label :text "errors")
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
                :documentation "The number of errors the test suite has.")
   (failures-label :initform (szj-label :text "failures")
                   :type szj-label
                   :custom szj-label
                   :reader szj-failures-label
                   :allocation :class
                   :documentation "The label: failures:.")
   (failure-count :initarg :failure-count
                  :initform 0
                  :type integer
                  :custom integer
                  :reader szj-failure-count
                  :documentation "The number of failures the test suite has."))
  "The title of a test suite in the test result document.")

(cl-defmethod szj-render-widget ((widget szj-test-suite-title))
  (concat (let ((name (format "Test Suite %s" (szj-name widget))))
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
          (szj-render-widget (szj-failures-label widget))
          " "
          (let* ((failures (szj-failure-count widget))
                 (failures (format "%d" failures)))
            (put-text-property 0 (length failures) 'face
                               'szj-failure-face
                               failures)
            failures)))

(defclass szj-test-suite (szj-container)
  ((inner-list :initarg :inner-list
               :type szj-list
               :custom szj-list
               :accessor szj-inner-list
               :documentation "The iner list of the test suite."))
  "A test suite in the test result document.")

(cl-defmethod szj-container-allchildren ((container szj-test-suite))
  (list (szj-inner-list container)))

(cl-defmethod szj-container-children ((container szj-test-suite))
  (list (szj-inner-list container)))

;; (cl-defmethod szj-set-container-position ((container szj-test-suite) position)
;;   (let ((point (point))) 
;;     (setf (szj-container-position container) point)
;;     (szj-set-container-position (szj-inner-list container) point)))

;;   ((name :initarg :name
;;         :initform "<default name>"
;;         :type string
;;         :custom string
;;         :reader szj-name
;;         :documentation "The name of the test suite.")
;;   (tests-label :initform (szj-label :text "tests")
  ;;               :type szj-label
  ;;               :custom szj-label
  ;;               :reader szj-tests-label
  ;;               :allocation :class
  ;;               :documentation "The label: tests:.")
  ;;  (test-count :initarg :test-count
  ;;              :initform 0
  ;;              :type integer
  ;;              :custom integer
  ;;              :accessor szj-test-count
  ;;              :documentation "The number of tests that the test suite has.")
  ;;  (errors-label :initform (szj-label :text "errors")
  ;;                :type szj-label
  ;;                :custom szj-label
  ;;                :reader szj-errors-label
  ;;                :allocation :class
  ;;                :documentation "The label: errors:.")
  ;;  (error-count :initarg :error-count
  ;;               :initform 0
  ;;               :type integer
  ;;               :custom integer
  ;;               :accessor szj-error-count
  ;;               :documentation "The number of errors that the test suite has.")
  ;;  (failures-label :initform (szj-label :text "failures")
  ;;                  :type szj-label
  ;;                  :custom szj-label
  ;;                  :reader szj-failure-label
  ;;                  :allocation :class
  ;;                  :documentation "The label: failures:.")
  ;;  (failure-count :initarg :failure-count
  ;;                 :initform 0
  ;;                 :type integer
  ;;                 :custom integer
  ;;                 :accessor szj-failure-count
  ;;                 :documentation "The number of failures that the test suite has.")
  ;;  (tests :initarg :tests
  ;;         :initform '()
  ;;         :type list
  ;;         :custom list
  ;;         :accessor szj-tests
  ;;         :documentation "A list of tests that the test suite has"))
  ;; "A test suite in the test result document")


;; (cl-defmethod szj-container-children ((container szj-test-suite))
;;   (list (let ((name (format "- Test Suite %s" (szj-name widget))))
;;           (put-text-property 0 (length name) 'face
;;                              'szj-success-face
;;                              name)
;;           name)
;;         ", "
;;         (szj-tests-label container)
;;         " "
;;         (let* ((tests (szj-test-count container))
;;                (tests (format "%d" tests)))
;;           (put-text-property 0 (length tests) 'face
;;                              'szj-success-face
;;                              tests)
;;           tests)
;;         ", "
;;         (szj-errors-label container)
;;         " "
;;         (let* ((errors (szj-error-count container))
;;                (errors (format "%d" errors)))
;;           (put-text-property 0 (length errors) 'face
;;                              'szj-error-face
;;                              errors)
;;           errors)
;;         ", "
;;         (let* ((failures (szj-failure-count container))
;;                (failures (format "%d" failures)))
;;           (put-text-property 0 (length failures) 'face
;;                              'szj-failure-face
;;                              failures)
;;           failures)))

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
  ((test-expander :initform (szj-expander)
                  :type szj-expander
                  :custom szj-expander
                  :reader szj-test-expander
                  :documentation "The expander")
   (success-label :initform (szj-label :text "success")
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

(cl-defmethod initialize-instance :after ((object szj-test) &rest slots)
  (let ((test-expander (szj-test-expander object)))
    (szj-add-listener test-expander (lambda ()
                                      (szj-redraw-widget object)))))

(cl-defmethod szj-render-widget ((widget szj-test))
  (concat (szj-render-widget (szj-test-expander widget))
          " "
          (szj-render-widget (szj-success-label widget))
          " "
          (let ((s (szj-name widget)))
            (put-text-property 0 (length s) 'face
                               'szj-success-face
                               s)
            s)
          "\n"))

;; (defclass szj-view (szj-container)
;;   ((running-title :initform (szj-title :text "Snazzy Junit Runner -- Running Test(s)")
;;                   :type szj-title
;;                   :custom szj-title
;;                   :reader szj-running-title
;;                   :allocation :class
;;                   :documentation "The first line in the document")
;;    (progress-label :initform (szj-label :text "progress")
;;                    :type szj-label
;;                    :custom szj-label
;;                    :reader szj-progress-label
;;                    :allocation :class
;;                    :documentation "The label: progress:.")
;;    (progress-progress-indicator :initarg :progress-progress-indicator
;;                                 :initform (szj-progress-indicator)
;;                                 :type szj-progress-indicator
;;                                 :custom szj-progress-indicator
;;                                 :accessor szj-progress-progress-indicator
;;                                 :documentation "The indicator for the execution progress")
;;    (completed-title :initform (szj-title :text "Snazzy Junit Runner -- Finished Running Test(s)")
;;                     :type szj-title
;;                     :custom szj-title
;;                     :accessor szj-completed-title
;;                     :allocation :class
;;                     :documentation "The title displayed when the test runner is finished running tests.")
;;    (completed-label :initform (szj-label :text "status")
;;                     :type szj-label
;;                     :custom szj-label
;;                     :reader szj-completed-label
;;                     :allocation :class
;;                     :documentation "The label: status:")
;;    (completed-progress-indicator :initarg :completed-progress-indicator
;;                                  :initform nil
;;                                  :type (or null szj-progress-indicator)
;;                                  :custom (or null szj-progress-indicator)
;;                                  :accessor szj-completed-progress-indicator
;;                                  :documentation "The progress indicator after the tests have been completed.")
;;    (test-suites-label :initform (szj-label :text "Test Suites")
;;                       :type szj-label
;;                       :custom szj-label
;;                       :accessor szj-test-suites-label
;;                       :allocation :class
;;                       :documentation "The label for the test suites")
;;    (test-suites :initarg :test-suites
;;                 :initform nil
;;                 :type (or null list)
;;                 :custom (or null list)
;;                 :accessor szj-test-suites
;;                 :documentation "The test suites."))
;;   "The model of the document.")


;; (cl-defmethod szj-render-widget ((widget szj-view))
;;   (concat (szj-render-widget
;;            (szj-running-title widget))
;;           "\n"
;;           (szj-render-widget
;;            (szj-progress-label widget))
;;           " "
;;           (szj-render-widget
;;            (szj-progress-progress-bar widget))
;;           " "
;;           (szj-render-widget
;;            (szj-progress-progress-percentage widget))
;;           "\n"
;;           (when-let ((completed-title (szj-completed-title widget))
;;                      (completed-label (szj-completed-label widget))
;;                      (progress-bar (szj-completed-progress-bar widget))
;;                      (percentage (szj-completed-progress-percentage widget)))
;;             (concat "\n"
;;                     (szj-render-widget completed-title)
;;                     "\n"
;;                     (szj-render-widget completed-label)
;;                     "   "
;;                     (szj-render-widget progress-bar)
;;                     " "
;;                     (szj-render-widget percentage)
;;                     "\n"))
;;           (when-let ((test-suites-label (szj-test-suites-label widget))
;;                      (test-suites (szj-test-suites widget)))
;;             (concat "\n"
;;                     (szj-render-widget test-suites-label)
;;                     "\n"
;;                     (apply #'concat (-map #'szj-render-widget test-suites))))))

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
