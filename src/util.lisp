;;; util.lisp
;;;
;;; SPDX-License-Identifier: EPL-1.0
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; This is a Common Lisp port of instaparse by Mark Engelberg.
;;; Original: https://github.com/Engelberg/instaparse
;;; Licensed under the Eclipse Public License 1.0.
;;;
;;; Utility functions and data structures for iparse.

(in-package #:iparse/util)

;;;; Segment - O(1) Substring Wrapper
;;;;
;;;; In modern implementations, subseq on strings creates a copy (O(n)).
;;;; Segment provides O(1) substring by storing offset and length.

(defstruct (segment (:constructor %make-segment))
  "A view into a string with O(1) substring operation."
  (string "" :type string :read-only t)
  (offset 0 :type fixnum :read-only t)
  (length 0 :type fixnum :read-only t))

(defun make-segment (string &optional (offset 0) (length (- (length string) offset)))
  "Create a segment viewing STRING from OFFSET for LENGTH characters."
  (declare (type string string)
           (type fixnum offset length))
  (assert (<= 0 offset))
  (assert (<= 0 length))
  (assert (<= (+ offset length) (length string)))
  (%make-segment :string string :offset offset :length length))

(declaim (inline segment-char))
(defun segment-char (segment index)
  "Return the character at INDEX within the segment."
  (declare (type segment segment)
           (type fixnum index))
  (char (segment-string segment) (+ (segment-offset segment) index)))

(defun segment-subseq (segment start &optional end)
  "Return a new segment viewing a subsequence. O(1) operation."
  (declare (type segment segment)
           (type fixnum start))
  (let* ((end (or end (segment-length segment)))
         (new-length (- end start)))
    (declare (type fixnum end new-length))
    (assert (<= 0 start))
    (assert (<= start end))
    (assert (<= end (segment-length segment)))
    (%make-segment :string (segment-string segment)
                   :offset (+ (segment-offset segment) start)
                   :length new-length)))

(defun segment-to-string (segment)
  "Convert segment to an actual string. O(n) operation."
  (declare (type segment segment))
  (subseq (segment-string segment)
          (segment-offset segment)
          (+ (segment-offset segment) (segment-length segment))))

(defun segment-remaining (segment index)
  "Return segment from INDEX to end."
  (declare (type segment segment)
           (type fixnum index))
  (segment-subseq segment index))

(defmethod print-object ((seg segment) stream)
  (print-unreadable-object (seg stream :type t)
    (format stream "~S [~D:~D]"
            (if (< (segment-length seg) 40)
                (segment-to-string seg)
                (concatenate 'string
                             (subseq (segment-to-string seg) 0 37)
                             "..."))
            (segment-offset seg)
            (+ (segment-offset seg) (segment-length seg)))))


;;;; Adjustable Vector Helper

(defun make-adjustable-vector (&optional (initial-size 16))
  "Create an adjustable vector with fill pointer, suitable for use as a stack."
  (make-array initial-size
              :adjustable t
              :fill-pointer 0))


;;;; Metaobject - Wrapper for Values with Metadata
;;;;
;;;; Clojure uses with-meta/meta for attaching arbitrary data to objects.
;;;; We simulate this with a simple wrapper struct.

(defstruct (metaobject (:constructor %make-metaobject))
  "A wrapper that attaches metadata to any value."
  (value nil :read-only t)
  (metadata nil :type list :read-only t))

(defun with-meta (value metadata)
  "Return VALUE wrapped with METADATA (a plist).
If VALUE is already a metaobject, replaces its metadata."
  (if (metaobject-p value)
      (%make-metaobject :value (metaobject-value value)
                        :metadata metadata)
      (%make-metaobject :value value
                        :metadata metadata)))

(defun get-meta (value)
  "Return the metadata plist attached to VALUE, or NIL if none."
  (if (metaobject-p value)
      (metaobject-metadata value)
      nil))

(defun unwrap-meta (value)
  "Return the underlying value, stripping any metaobject wrapper."
  (if (metaobject-p value)
      (metaobject-value value)
      value))


;;;; Keyword Utilities

(defun make-keyword (name)
  "Intern NAME as a keyword."
  (intern (string-upcase (string name)) :keyword))
