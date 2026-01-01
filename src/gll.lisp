;;; gll.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name
;;;
;;; GLL (Generalized LL) Parsing Algorithm
;;;
;;; This implements a trampoline-based GLL parser that handles
;;; ambiguous and left-recursive grammars. Key features:
;;; - Stack-based execution to avoid deep recursion
;;; - Memoization via nodes keyed by (index, parser) pairs
;;; - Listener-based result propagation
;;; - Generation counter for deduplication
;;; - Negative listener queue for ordered choice/negative lookahead

(in-package #:iparse/gll)

;;;; Success Result

(defstruct (success (:constructor make-success (result index)))
  "A successful parse result with the parsed value and continuation index."
  (result nil :read-only t)
  (index 0 :type fixnum :read-only t))


;;;; Node - Memoization Point
;;;;
;;;; Each unique (index, parser) pair gets a node that caches results
;;;; and tracks listeners waiting for results.

(defstruct (node (:constructor %make-node))
  "Memoization node for a (index, parser) pair."
  (listeners (make-adjustable-vector 8) :type vector)
  (full-listeners (make-adjustable-vector 4) :type vector)
  (results (make-hash-table :test 'equal :size 4) :type hash-table)
  (full-results (make-hash-table :test 'equal :size 4) :type hash-table))

(defun make-node ()
  (%make-node))


;;;; Trampoline - Execution Context

(defclass trampoline ()
  ((grammar :initarg :grammar
            :reader tramp-grammar
            :type hash-table
            :documentation "Grammar: keyword -> parser map.")
   (text :initarg :text
         :reader tramp-text
         :type string
         :documentation "Original input text.")
   (segment :initarg :segment
            :reader tramp-segment
            :type segment
            :documentation "Segment wrapper for efficient substring.")
   ;; Mutable execution state
   (%stack :initform (make-adjustable-vector 32)
           :accessor tramp-%stack
           :type vector
           :documentation "Current work stack of thunks.")
   (%next-stack :initform (make-adjustable-vector 32)
                :accessor tramp-%next-stack
                :type vector
                :documentation "Deferred work for next generation.")
   (%generation :initform 0
                :accessor tramp-%generation
                :type fixnum
                :documentation "Current generation counter.")
   (%nodes :initform (make-hash-table :test 'equal :size 64)
           :accessor tramp-%nodes
           :type hash-table
           :documentation "Memoization: (index . parser) -> node.")
   (%success :initform nil
             :accessor tramp-%success
             :documentation "Most recent successful parse.")
   (%failure :initform nil
             :accessor tramp-%failure
             :type (or null parse-failure)
             :documentation "Furthest failure encountered.")
   (%negative-listeners :initform nil
                        :accessor tramp-%negative-listeners
                        :type list
                        :documentation "Alist: index -> list of thunks.")
   (%msg-cache :initform (make-hash-table :test 'equal :size 128)
               :accessor tramp-%msg-cache
               :type hash-table
               :documentation "Deduplication cache: (listener . index) -> count."))
  (:documentation "Execution context for GLL parsing."))

(defun make-trampoline (grammar text)
  "Create a new trampoline for parsing TEXT with GRAMMAR."
  (make-instance 'trampoline
                 :grammar grammar
                 :text text
                 :segment (make-segment text)))


;;;; Stack Operations

(declaim (inline push-stack))
(defun push-stack (tramp thunk)
  "Push a thunk onto the execution stack."
  (vector-push-extend thunk (tramp-%stack tramp)))

(defun push-message (tramp listener result)
  "Schedule a message to LISTENER about RESULT.
Uses generation counter for deduplication."
  (let* ((cache (tramp-%msg-cache tramp))
         (index (success-index result))
         (key (cons listener index))
         (count (gethash key cache 0))
         (thunk (lambda () (funcall listener result))))
    (if (> count (tramp-%generation tramp))
        ;; Defer to next generation
        (vector-push-extend thunk (tramp-%next-stack tramp))
        ;; Add to current stack
        (vector-push-extend thunk (tramp-%stack tramp)))
    (setf (gethash key cache) (1+ count))))


;;;; Negative Listeners (for ordered choice and negative lookahead)

(defun push-negative-listener (tramp index thunk)
  "Register a negative listener at INDEX.
Negative listeners execute only when all other work is exhausted."
  (let ((existing (assoc index (tramp-%negative-listeners tramp))))
    (if existing
        (push thunk (cdr existing))
        (push (cons index (list thunk))
              (tramp-%negative-listeners tramp)))))

(defun pop-highest-negative-listener (tramp)
  "Pop and return the negative listener at the highest index.
Returns (INDEX . THUNK) or NIL."
  (when (tramp-%negative-listeners tramp)
    ;; Sort by index descending and pop first
    (setf (tramp-%negative-listeners tramp)
          (sort (tramp-%negative-listeners tramp) #'> :key #'car))
    (let* ((entry (first (tramp-%negative-listeners tramp)))
           (index (car entry))
           (thunks (cdr entry))
           (thunk (pop (cdr (first (tramp-%negative-listeners tramp))))))
      ;; Remove entry if no more thunks
      (when (null (cdr (first (tramp-%negative-listeners tramp))))
        (pop (tramp-%negative-listeners tramp)))
      (cons index thunk))))


;;;; Node Management

(defun node-key (index parser)
  "Create a node key from INDEX and PARSER."
  (cons index parser))

(defun get-node (tramp node-key)
  "Get or create the node for NODE-KEY."
  (or (gethash node-key (tramp-%nodes tramp))
      (setf (gethash node-key (tramp-%nodes tramp)) (make-node))))

(defun listener-exists-p (tramp node-key)
  "Return T if any listeners exist for NODE-KEY."
  (let ((node (gethash node-key (tramp-%nodes tramp))))
    (and node (plusp (length (node-listeners node))))))

(defun full-listener-exists-p (tramp node-key)
  "Return T if any full-listeners exist for NODE-KEY."
  (let ((node (gethash node-key (tramp-%nodes tramp))))
    (and node (plusp (length (node-full-listeners node))))))

(defun result-exists-p (tramp node-key)
  "Return T if any results exist for NODE-KEY."
  (let ((node (gethash node-key (tramp-%nodes tramp))))
    (and node (plusp (hash-table-count (node-results node))))))


;;;; Result Propagation

(defun total-success-p (tramp result)
  "Return T if RESULT consumed all input."
  (= (success-index result) (length (tramp-text tramp))))

(defun push-result (tramp node-key result)
  "Push a parse result to the node, notifying listeners."
  (let* ((node (get-node tramp node-key))
         (parser (cdr node-key))
         (start-index (car node-key))
         ;; Apply reduction if specified
         (result (apply-parser-reduction parser result start-index))
         ;; Check if result reaches end of input
         (total-p (total-success-p tramp result))
         (results-table (if total-p
                            (node-full-results node)
                            (node-results node))))
    ;; Only process if result is new
    (unless (gethash result results-table)
      (setf (gethash result results-table) t)
      ;; Notify regular listeners
      (loop for listener across (node-listeners node)
            do (push-message tramp listener result))
      ;; Notify full listeners only for total results
      (when total-p
        (loop for listener across (node-full-listeners node)
              do (push-message tramp listener result))))))

(defun apply-parser-reduction (parser result start-index)
  "Apply parser's reduction to RESULT if specified."
  (let* ((raw-result (success-result result))
         (end-index (success-index result))
         (reduction (parser-reduction parser))
         (hidden-p (parser-hide parser)))
    (cond
      (hidden-p
       (make-success nil end-index))
      ((null reduction)
       result)
      ((eq reduction :raw)
       ;; Raw: pass through for flattening
       result)
      ((reduction-p reduction)
       (make-success
        (with-meta
            (apply-reduction reduction raw-result)
            (list :start-index start-index :end-index end-index))
        end-index))
      (t result))))


;;;; Listener Registration

(defun push-listener (tramp node-key listener)
  "Register LISTENER to receive results for NODE-KEY.
Initiates parsing if this is the first listener."
  (let ((first-listener-p (not (listener-exists-p tramp node-key)))
        (node (get-node tramp node-key)))
    (vector-push-extend listener (node-listeners node))
    ;; Notify of existing results
    (maphash (lambda (result _)
               (declare (ignore _))
               (push-message tramp listener result))
             (node-results node))
    (maphash (lambda (result _)
               (declare (ignore _))
               (push-message tramp listener result))
             (node-full-results node))
    ;; If first listener, initiate parse
    (when first-listener-p
      (let ((index (car node-key))
            (parser (cdr node-key)))
        (push-stack tramp (lambda () (parse-at parser index tramp)))))))

(defun push-full-listener (tramp node-key listener)
  "Register LISTENER to receive only full (total) results for NODE-KEY."
  (let ((first-listener-p (not (full-listener-exists-p tramp node-key)))
        (node (get-node tramp node-key)))
    (vector-push-extend listener (node-full-listeners node))
    ;; Notify of existing full results
    (maphash (lambda (result _)
               (declare (ignore _))
               (push-message tramp listener result))
             (node-full-results node))
    ;; If first full listener, initiate full parse
    (when first-listener-p
      (let ((index (car node-key))
            (parser (cdr node-key)))
        (push-stack tramp (lambda () (full-parse-at parser index tramp)))))))


;;;; Failure Handling

(defun fail (tramp node-key index reason-plist)
  "Record a parse failure at INDEX."
  (let ((new-failure (make-failure index (list reason-plist))))
    (setf (tramp-%failure tramp)
          (merge-failures (tramp-%failure tramp) new-failure))))

(defun succeed (tramp node-key result index)
  "Record a successful parse result."
  (push-result tramp node-key (make-success result index)))


;;;; Parser Dispatch

(defgeneric parse-at (parser index tramp)
  (:documentation "Parse PARSER at INDEX in TRAMP context."))

(defgeneric full-parse-at (parser index tramp)
  (:documentation "Parse PARSER at INDEX, requiring consumption of all remaining input."))


;;;; Listener Factories

(defun make-node-listener (node-key tramp)
  "Create a listener that forwards results to NODE-KEY."
  (lambda (result)
    (push-result tramp node-key result)))

(defun make-look-listener (node-key tramp)
  "Create a listener for positive lookahead (don't consume input)."
  (lambda (result)
    (declare (ignore result))
    (succeed tramp node-key nil (car node-key))))

(defun make-cat-listener (results-so-far remaining-parsers node-key tramp)
  "Create a listener for concatenation."
  (lambda (result)
    (let* ((parsed-result (success-result result))
           (continue-index (success-index result))
           (new-results (conj-flat results-so-far parsed-result)))
      (if remaining-parsers
          ;; More parsers to go
          (push-listener tramp
                         (node-key continue-index (first remaining-parsers))
                         (make-cat-listener new-results
                                            (rest remaining-parsers)
                                            node-key
                                            tramp))
          ;; All done
          (succeed tramp node-key new-results continue-index)))))

(defun make-cat-full-listener (results-so-far remaining-parsers node-key tramp)
  "Create a listener for concatenation requiring full parse."
  (lambda (result)
    (let* ((parsed-result (success-result result))
           (continue-index (success-index result))
           (new-results (conj-flat results-so-far parsed-result)))
      (cond
        ;; Last parser: use full listener
        ((and remaining-parsers (null (rest remaining-parsers)))
         (push-full-listener tramp
                             (node-key continue-index (first remaining-parsers))
                             (make-cat-full-listener new-results
                                                     (rest remaining-parsers)
                                                     node-key
                                                     tramp)))
        ;; More parsers: use regular listener
        (remaining-parsers
         (push-listener tramp
                        (node-key continue-index (first remaining-parsers))
                        (make-cat-full-listener new-results
                                                (rest remaining-parsers)
                                                node-key
                                                tramp)))
        ;; All done
        (t
         (succeed tramp node-key new-results continue-index))))))

(defun make-plus-listener (results-so-far parser prev-index node-key tramp)
  "Create a listener for one-or-more repetition."
  (lambda (result)
    (let* ((parsed-result (success-result result))
           (continue-index (success-index result)))
      (if (= continue-index prev-index)
          ;; Parser didn't consume input - stop iteration
          nil
          ;; Parser consumed input
          (let ((new-results (conj-flat results-so-far parsed-result)))
            ;; Try for more AND report current results
            (push-listener tramp
                           (node-key continue-index parser)
                           (make-plus-listener new-results
                                               parser
                                               continue-index
                                               node-key
                                               tramp))
            (succeed tramp node-key new-results continue-index))))))

(defun make-plus-full-listener (results-so-far parser prev-index node-key tramp)
  "Create a listener for one-or-more requiring full parse."
  (lambda (result)
    (let* ((parsed-result (success-result result))
           (continue-index (success-index result)))
      (if (= continue-index prev-index)
          nil
          (let ((new-results (conj-flat results-so-far parsed-result)))
            (if (= continue-index (length (tramp-text tramp)))
                ;; Reached end - report
                (succeed tramp node-key new-results continue-index)
                ;; Not at end - keep trying
                (push-listener tramp
                               (node-key continue-index parser)
                               (make-plus-full-listener new-results
                                                        parser
                                                        continue-index
                                                        node-key
                                                        tramp))))))))

(defun make-rep-listener (results-so-far count parser min max prev-index node-key tramp)
  "Create a listener for bounded repetition."
  (lambda (result)
    (let* ((parsed-result (success-result result))
           (continue-index (success-index result))
           (new-results (conj-flat results-so-far parsed-result))
           (new-count (1+ count)))
      ;; Only continue if parser consumed input (prevent infinite loop)
      (when (/= continue-index prev-index)
        ;; Report if within bounds
        (when (<= min new-count max)
          (succeed tramp node-key new-results continue-index))
        ;; Keep trying if under max
        (when (< new-count max)
          (push-listener tramp
                         (node-key continue-index parser)
                         (make-rep-listener new-results
                                            new-count
                                            parser
                                            min max
                                            continue-index
                                            node-key
                                            tramp)))))))

(defun make-top-listener (tramp)
  "Create the top-level listener that captures final success."
  (lambda (result)
    (setf (tramp-%success tramp) result)))


;;;; Parser Method Implementations

;;; String Parser
(defmethod parse-at ((parser string-parser) index tramp)
  (let* ((s (parser-string parser))
         (len (length s))
         (text (tramp-text tramp))
         (text-len (length text))
         (node-key (node-key index parser)))
    (if (and (<= (+ index len) text-len)
             (string= s text :start2 index :end2 (+ index len)))
        (succeed tramp node-key s (+ index len))
        (fail tramp node-key index (list :tag :string :expecting s)))))

(defmethod full-parse-at ((parser string-parser) index tramp)
  (parse-at parser index tramp))

;;; Case-insensitive String Parser
(defmethod parse-at ((parser string-ci-parser) index tramp)
  (let* ((s (parser-string parser))
         (len (length s))
         (text (tramp-text tramp))
         (text-len (length text))
         (node-key (node-key index parser)))
    (if (and (<= (+ index len) text-len)
             (string-equal s text :start2 index :end2 (+ index len)))
        (succeed tramp node-key (subseq text index (+ index len)) (+ index len))
        (fail tramp node-key index (list :tag :string-ci :expecting s)))))

(defmethod full-parse-at ((parser string-ci-parser) index tramp)
  (parse-at parser index tramp))

;;; Regexp Parser
(defmethod parse-at ((parser regexp-parser) index tramp)
  (let* ((scanner (parser-scanner parser))
         (text (tramp-text tramp))
         (node-key (node-key index parser)))
    (multiple-value-bind (start end)
        (cl-ppcre:scan scanner text :start index)
      (if (and start (= start index))
          (succeed tramp node-key (subseq text start end) end)
          (fail tramp node-key index
                (list :tag :regexp :expecting (parser-pattern parser)))))))

(defmethod full-parse-at ((parser regexp-parser) index tramp)
  (parse-at parser index tramp))

;;; Character Range Parser
(defmethod parse-at ((parser char-range-parser) index tramp)
  (let* ((text (tramp-text tramp))
         (node-key (node-key index parser)))
    (if (< index (length text))
        (let* ((char (char text index))
               (code (char-code char)))
          (if (<= (parser-lo parser) code (parser-hi parser))
              (succeed tramp node-key (string char) (1+ index))
              (fail tramp node-key index
                    (list :tag :char
                          :expecting (format nil "[~A-~A]"
                                             (code-char (parser-lo parser))
                                             (code-char (parser-hi parser)))))))
        (fail tramp node-key index (list :tag :char :expecting "character")))))

(defmethod full-parse-at ((parser char-range-parser) index tramp)
  (parse-at parser index tramp))

;;; Epsilon Parser
(defmethod parse-at ((parser epsilon-parser) index tramp)
  (succeed tramp (node-key index parser) nil index))

(defmethod full-parse-at ((parser epsilon-parser) index tramp)
  (parse-at parser index tramp))

;;; Non-terminal Parser
(defmethod parse-at ((parser nt-parser) index tramp)
  (let* ((keyword (parser-keyword parser))
         (target-parser (gethash keyword (tramp-grammar tramp)))
         (node-key (node-key index parser)))
    (if target-parser
        (push-listener tramp
                       (node-key index target-parser)
                       (make-node-listener node-key tramp))
        (error "Unknown non-terminal: ~A" keyword))))

(defmethod full-parse-at ((parser nt-parser) index tramp)
  (let* ((keyword (parser-keyword parser))
         (target-parser (gethash keyword (tramp-grammar tramp)))
         (node-key (node-key index parser)))
    (if target-parser
        (push-full-listener tramp
                            (node-key index target-parser)
                            (make-node-listener node-key tramp))
        (error "Unknown non-terminal: ~A" keyword))))

;;; Alternation Parser
(defmethod parse-at ((parser alt-parser) index tramp)
  (let ((node-key (node-key index parser)))
    (dolist (alt-parser (parser-parsers parser))
      (push-listener tramp
                     (node-key index alt-parser)
                     (make-node-listener node-key tramp)))))

(defmethod full-parse-at ((parser alt-parser) index tramp)
  (let ((node-key (node-key index parser)))
    (dolist (alt-parser (parser-parsers parser))
      (push-full-listener tramp
                          (node-key index alt-parser)
                          (make-node-listener node-key tramp)))))

;;; Ordered Choice Parser
(defmethod parse-at ((parser ord-parser) index tramp)
  (let* ((node-key (node-key index parser))
         (parser1 (parser-parser1 parser))
         (parser2 (parser-parser2 parser)))
    ;; Try parser1
    (push-listener tramp
                   (node-key index parser1)
                   (make-node-listener node-key tramp))
    ;; Register parser2 as negative listener (only if parser1 produces no results)
    (push-negative-listener tramp index
                            (lambda ()
                              (unless (result-exists-p tramp (node-key index parser1))
                                (push-listener tramp
                                               (node-key index parser2)
                                               (make-node-listener node-key tramp)))))))

(defmethod full-parse-at ((parser ord-parser) index tramp)
  (let* ((node-key (node-key index parser))
         (parser1 (parser-parser1 parser))
         (parser2 (parser-parser2 parser)))
    (push-full-listener tramp
                        (node-key index parser1)
                        (make-node-listener node-key tramp))
    (push-negative-listener tramp index
                            (lambda ()
                              (unless (result-exists-p tramp (node-key index parser1))
                                (push-full-listener tramp
                                                    (node-key index parser2)
                                                    (make-node-listener node-key tramp)))))))

;;; Concatenation Parser
(defmethod parse-at ((parser cat-parser) index tramp)
  (let* ((parsers (parser-parsers parser))
         (node-key (node-key index parser)))
    (when parsers
      (push-listener tramp
                     (node-key index (first parsers))
                     (make-cat-listener *afs-empty*
                                        (rest parsers)
                                        node-key
                                        tramp)))))

(defmethod full-parse-at ((parser cat-parser) index tramp)
  (let* ((parsers (parser-parsers parser))
         (node-key (node-key index parser)))
    (when parsers
      (if (null (rest parsers))
          ;; Single parser: use full listener directly
          (push-full-listener tramp
                              (node-key index (first parsers))
                              (make-cat-full-listener *afs-empty*
                                                      nil
                                                      node-key
                                                      tramp))
          ;; Multiple parsers
          (push-listener tramp
                         (node-key index (first parsers))
                         (make-cat-full-listener *afs-empty*
                                                 (rest parsers)
                                                 node-key
                                                 tramp))))))

;;; Optional Parser
(defmethod parse-at ((parser opt-parser) index tramp)
  (let* ((inner (parser-parser parser))
         (node-key (node-key index parser)))
    ;; Always succeeds with empty result
    (succeed tramp node-key nil index)
    ;; Also try matching
    (push-listener tramp
                   (node-key index inner)
                   (make-node-listener node-key tramp))))

(defmethod full-parse-at ((parser opt-parser) index tramp)
  (let* ((inner (parser-parser parser))
         (node-key (node-key index parser)))
    ;; Only succeed with empty if at end
    (when (= index (length (tramp-text tramp)))
      (succeed tramp node-key nil index))
    (push-full-listener tramp
                        (node-key index inner)
                        (make-node-listener node-key tramp))))

;;; Plus Parser (one or more)
(defmethod parse-at ((parser plus-parser) index tramp)
  (let* ((inner (parser-parser parser))
         (node-key (node-key index parser)))
    (push-listener tramp
                   (node-key index inner)
                   (make-plus-listener *afs-empty* inner index node-key tramp))))

(defmethod full-parse-at ((parser plus-parser) index tramp)
  (let* ((inner (parser-parser parser))
         (node-key (node-key index parser)))
    (push-listener tramp
                   (node-key index inner)
                   (make-plus-full-listener *afs-empty* inner index node-key tramp))))

;;; Star Parser (zero or more)
(defmethod parse-at ((parser star-parser) index tramp)
  (let* ((inner (parser-parser parser))
         (node-key (node-key index parser)))
    ;; Always succeeds with empty
    (succeed tramp node-key nil index)
    ;; Also try one-or-more
    (push-listener tramp
                   (node-key index inner)
                   (make-plus-listener *afs-empty* inner index node-key tramp))))

(defmethod full-parse-at ((parser star-parser) index tramp)
  (let* ((inner (parser-parser parser))
         (node-key (node-key index parser)))
    ;; Succeed with empty only if at end
    (when (= index (length (tramp-text tramp)))
      (succeed tramp node-key nil index))
    (push-listener tramp
                   (node-key index inner)
                   (make-plus-full-listener *afs-empty* inner index node-key tramp))))

;;; Rep Parser (bounded repetition)
(defmethod parse-at ((parser rep-parser) index tramp)
  (let* ((inner (parser-parser parser))
         (min (parser-min parser))
         (max (parser-max parser))
         (node-key (node-key index parser)))
    ;; If min is 0, we can succeed immediately with empty
    (when (zerop min)
      (succeed tramp node-key nil index))
    ;; Try matching
    (when (plusp max)
      (push-listener tramp
                     (node-key index inner)
                     (make-rep-listener *afs-empty* 0 inner min max index node-key tramp)))))

(defmethod full-parse-at ((parser rep-parser) index tramp)
  ;; For simplicity, delegate to regular parse and filter
  (parse-at parser index tramp))

;;; Lookahead Parser
(defmethod parse-at ((parser look-parser) index tramp)
  (let* ((inner (parser-parser parser))
         (node-key (node-key index parser)))
    (push-listener tramp
                   (node-key index inner)
                   (make-look-listener node-key tramp))))

(defmethod full-parse-at ((parser look-parser) index tramp)
  (parse-at parser index tramp))

;;; Negative Lookahead Parser
(defmethod parse-at ((parser neg-parser) index tramp)
  (let* ((inner (parser-parser parser))
         (node-key (node-key index parser))
         (inner-key (node-key index inner)))
    ;; If inner already has results, fail immediately
    (if (result-exists-p tramp inner-key)
        (fail tramp node-key index
              (list :tag :negative-lookahead
                    :expecting (list :not (format nil "~A" inner))))
        (progn
          ;; Register listener that fails if inner succeeds
          (push-listener tramp inner-key
                         (lambda (result)
                           (declare (ignore result))
                           (fail tramp node-key index
                                 (list :tag :negative-lookahead
                                       :expecting (list :not (format nil "~A" inner))))))
          ;; Register negative listener that succeeds if inner never produces results
          (push-negative-listener tramp index
                                  (lambda ()
                                    (unless (result-exists-p tramp inner-key)
                                      (succeed tramp node-key nil index))))))))

(defmethod full-parse-at ((parser neg-parser) index tramp)
  (parse-at parser index tramp))


;;;; Main Execution Loop

(defun run-trampoline (tramp &optional found-result-p)
  "Execute the trampoline until exhausted or success found.
Returns a lazy list of successful results."
  (let ((stack (tramp-%stack tramp)))
    (cond
      ;; Success found - return it and prepare for more
      ((tramp-%success tramp)
       (let ((result (tramp-%success tramp)))
         (setf (tramp-%success tramp) nil)
         (cons result
               (lambda () (run-trampoline tramp t)))))

      ;; Main stack has work
      ((plusp (fill-pointer stack))
       (let ((thunk (vector-pop stack)))
         (funcall thunk))
       (run-trampoline tramp found-result-p))

      ;; Process negative listeners
      ((tramp-%negative-listeners tramp)
       (let ((entry (pop-highest-negative-listener tramp)))
         (when entry
           (funcall (cdr entry))))
       (run-trampoline tramp found-result-p))

      ;; Swap stacks for next generation
      (found-result-p
       (rotatef (tramp-%stack tramp) (tramp-%next-stack tramp))
       (setf (fill-pointer (tramp-%next-stack tramp)) 0)
       (incf (tramp-%generation tramp))
       (run-trampoline tramp nil))

      ;; Exhausted
      (t nil))))


;;;; Public Interface

(defun parse-grammar (grammar start-rule text)
  "Parse TEXT using GRAMMAR starting from START-RULE.
Returns the first successful parse result, or NIL with failure info."
  (let* ((tramp (make-trampoline grammar text))
         (start-parser (gethash start-rule grammar)))
    (unless start-parser
      (error "Start rule ~A not found in grammar" start-rule))

    ;; Register top listener for full parse
    (push-full-listener tramp
                        (node-key 0 start-parser)
                        (make-top-listener tramp))

    ;; Run and get results
    (let ((results (run-trampoline tramp)))
      (if results
          (values (success-result (car results)) t)
          (values (augment-failure (tramp-%failure tramp) text) nil)))))

(defun parse-grammar-full (grammar start-rule text)
  "Parse TEXT using GRAMMAR, returning all possible parses as a lazy list."
  (let* ((tramp (make-trampoline grammar text))
         (start-parser (gethash start-rule grammar)))
    (unless start-parser
      (error "Start rule ~A not found in grammar" start-rule))

    (push-full-listener tramp
                        (node-key 0 start-parser)
                        (make-top-listener tramp))

    (run-trampoline tramp)))
