;;; failure.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name
;;;
;;; Parse failure handling and error reporting with line/column info.
;;; Provides both a structure for failure data and a condition for
;;; signaling parse errors with restarts.

(in-package #:iparse/failure)

;;;; Failure Structure

(defstruct (parse-failure (:constructor %make-failure))
  "Represents a parse failure with location and reason information."
  (index 0 :type fixnum :read-only t)
  (reasons nil :type list :read-only t)
  (line nil :type (or null fixnum))
  (column nil :type (or null fixnum))
  (text-line nil :type (or null string))
  (input nil :type (or null string) :read-only t))


;;;; Condition Classes

(define-condition iparse-error (error)
  ((failure :initarg :failure
            :reader iparse-error-failure
            :documentation "The parse-failure structure with details."))
  (:report (lambda (c stream)
             (format stream "~A" (format-failure (iparse-error-failure c)))))
  (:documentation "Condition signaled when parsing fails.
Use restarts to handle: USE-VALUE or CONTINUE."))

(defun make-failure (index reasons)
  "Create a parse failure at INDEX with REASONS.
REASONS is a list of reason plists like (:tag :string :expecting \"foo\")."
  (%make-failure :index index :reasons reasons))


;;;; Failure Merging
;;;;
;;;; During parsing, we track the furthest failure. If two failures
;;;; occur at the same index, we merge their reasons.

(defun merge-failures (f1 f2)
  "Merge two failures, keeping the one at the furthest index.
If at the same index, merge their reasons."
  (cond
    ((null f1) f2)
    ((null f2) f1)
    ((> (parse-failure-index f1) (parse-failure-index f2)) f1)
    ((< (parse-failure-index f1) (parse-failure-index f2)) f2)
    ;; Same index: merge reasons
    (t (%make-failure
        :index (parse-failure-index f1)
        :reasons (remove-duplicates
                  (append (parse-failure-reasons f1)
                          (parse-failure-reasons f2))
                  :test #'equal)))))


;;;; Line/Column Calculation

(defun index->line-column (index text)
  "Convert character INDEX to (LINE . COLUMN) in TEXT.
Both LINE and COLUMN are 1-based."
  (let ((line 1)
        (col 1))
    (loop for i from 0 below (min index (length text))
          for char = (char text i)
          do (if (char= char #\Newline)
                 (setf line (1+ line) col 1)
                 (incf col)))
    (cons line col)))

(defun get-line-text (line-number text)
  "Extract the LINE-NUMBER-th line (1-based) from TEXT."
  (with-input-from-string (stream text)
    (loop repeat (1- line-number)
          do (read-line stream nil ""))
    (read-line stream nil "")))


;;;; Failure Augmentation

(defun augment-failure (failure text)
  "Add line/column/text-line information to FAILURE."
  (let* ((pos (index->line-column (parse-failure-index failure) text))
         (line (car pos))
         (col (cdr pos))
         (text-line (get-line-text line text)))
    (%make-failure
     :index (parse-failure-index failure)
     :reasons (parse-failure-reasons failure)
     :line line
     :column col
     :text-line text-line)))


;;;; Failure Formatting

(defun format-reason (reason)
  "Format a single failure reason as a string."
  (let ((tag (getf reason :tag))
        (expecting (getf reason :expecting)))
    (case tag
      (:string (format nil "expected ~S" expecting))
      (:string-ci (format nil "expected ~S (case-insensitive)" expecting))
      (:regexp (format nil "expected match for ~A" expecting))
      (:char (format nil "expected character ~A" expecting))
      (:nt (format nil "expected ~A" expecting))
      (:negative-lookahead
       (let ((not-val (getf expecting :not)))
         (format nil "did not expect ~A" not-val)))
      (:end-of-input (format nil "expected end of input"))
      (otherwise (format nil "expected ~A" (or expecting tag))))))

(defun format-reasons (reasons)
  "Format all failure reasons as a readable string."
  (if (null reasons)
      "unexpected input"
      (format nil "~{~A~^~%, or ~}"
              (mapcar #'format-reason reasons))))

(defun make-caret-line (column)
  "Create a line with a caret pointing at COLUMN."
  (concatenate 'string
               (make-string (1- column) :initial-element #\Space)
               "^"))

(defun format-failure (failure)
  "Format FAILURE as a human-readable error message."
  (if (parse-failure-line failure)
      (format nil "Parse error at line ~D, column ~D:~%~A~%~A~%~A"
              (parse-failure-line failure)
              (parse-failure-column failure)
              (parse-failure-text-line failure)
              (make-caret-line (parse-failure-column failure))
              (format-reasons (parse-failure-reasons failure)))
      (format nil "Parse error at index ~D:~%~A"
              (parse-failure-index failure)
              (format-reasons (parse-failure-reasons failure)))))

(defmethod print-object ((f parse-failure) stream)
  (if (and *print-escape* (not *print-readably*))
      (print-unreadable-object (f stream :type t)
        (format stream "at ~D (~{~A~^, ~})"
                (parse-failure-index f)
                (mapcar #'format-reason (parse-failure-reasons f))))
      (format stream "~A" (format-failure f))))


;;;; Signaling with Restarts

(defun signal-parse-error (failure)
  "Signal an iparse-error condition with restarts.

Available restarts:
  USE-VALUE value - Return value as the parse result
  CONTINUE        - Return NIL and continue

Example:
  (handler-bind ((iparse-error
                  (lambda (c) (invoke-restart 'use-value :fallback))))
    (parse p input))"
  (restart-case
      (error 'iparse-error :failure failure)
    (use-value (value)
      :report "Specify a value to use as the parse result."
      :interactive (lambda ()
                     (format t "Enter a value: ")
                     (list (read)))
      (values value t))
    (continue ()
      :report "Return NIL and continue."
      (values nil nil))))
