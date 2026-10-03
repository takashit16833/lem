(defpackage :lem-ncurses/kitty-keyboard
  (:use :cl
        :lem)
  (:export :parse-csi-sequence
           :enable
           :disable
           :enabled-p))
(in-package :lem-ncurses/kitty-keyboard)

(defparameter +known-modifier-bits+ #xff)

(defparameter +push-disambiguate-sequence+
  (format nil "~C[>1u" #\Esc))

(defparameter +pop-keyboard-mode-sequence+
  (format nil "~C[<u" #\Esc))

(defvar *keyboard-mode-pushed-p* nil)

(defun enabled-p ()
  *keyboard-mode-pushed-p*)

(defun split-on (string delimiter)
  (let ((result '())
        (start 0))
    (loop
      (let ((pos (position delimiter string :start start)))
        (if pos
            (progn
              (push (subseq string start pos) result)
              (setf start (1+ pos)))
            (progn
              (push (subseq string start) result)
              (return (nreverse result))))))))

(defun decimal-integer (string)
  (when (and (plusp (length string))
             (every #'digit-char-p string))
    (parse-integer string)))

(defun unicode-scalar-p (code)
  (and (integerp code)
       (<= 0 code #x10ffff)
       (not (<= #xd800 code #xdfff))))

(defun pua-code-p (code)
  (<= #xe000 code #xf8ff))

(defun functional-u-sym (code)
  (cond
    ((= code 27) "Escape")
    ((= code 13) "Return")
    ((= code 9) "Tab")
    ((= code 127) "Backspace")
    ((= code 57363) "ContextMenu")
    ((<= 57376 code 57387)
     (format nil "F~D" (+ 13 (- code 57376))))
    ((<= 57399 code 57408)
     (format nil "~D" (- code 57399)))
    ((= code 57409) ".")
    ((= code 57410) "/")
    ((= code 57411) "*")
    ((= code 57412) "-")
    ((= code 57413) "+")
    ((= code 57414) "Return")
    ((= code 57415) "=")
    ((= code 57416) ",")
    ((= code 57417) "Left")
    ((= code 57418) "Right")
    ((= code 57419) "Up")
    ((= code 57420) "Down")
    ((= code 57421) "PageUp")
    ((= code 57422) "PageDown")
    ((= code 57423) "Home")
    ((= code 57424) "End")
    ((= code 57425) "Insert")
    ((= code 57426) "Delete")
    (t nil)))

(defun tilde-sym (number)
  (cdr (assoc number
              '((2 . "Insert")
                (3 . "Delete")
                (5 . "PageUp")
                (6 . "PageDown")
                (7 . "Home")
                (8 . "End")
                (11 . "F1")
                (12 . "F2")
                (13 . "F3")
                (14 . "F4")
                (15 . "F5")
                (17 . "F6")
                (18 . "F7")
                (19 . "F8")
                (20 . "F9")
                (21 . "F10")
                (23 . "F11")
                (24 . "F12")
                (29 . "ContextMenu")))))

(defun letter-final-sym (final)
  (cdr (assoc final
              '((#\A . "Up")
                (#\B . "Down")
                (#\C . "Right")
                (#\D . "Left")
                (#\F . "End")
                (#\H . "Home")
                (#\P . "F1")
                (#\Q . "F2")
                (#\S . "F4")))))

(defun modifier-args (encoded)
  (unless (and (integerp encoded) (plusp encoded))
    (return-from modifier-args (values nil :unsupported)))
  (let* ((bits (1- encoded))
         (unknown (logand bits (lognot +known-modifier-bits+))))
    (unless (zerop unknown)
      (return-from modifier-args (values nil :unsupported)))
    (values
     (list :shift (logbitp 0 bits)
           :meta (or (logbitp 1 bits)
                     (logbitp 5 bits))
           :ctrl (logbitp 2 bits)
           :super (logbitp 3 bits)
           :hyper (logbitp 4 bits))
     :ok)))

(defun parse-modifier-field (field)
  (if (or (null field) (string= field ""))
      (values 1 1 :ok)
      (let ((parts (split-on field #\:)))
        (when (> (length parts) 2)
          (return-from parse-modifier-field
            (values nil nil :unsupported)))
        (let ((modifier (if (string= (first parts) "")
                            1
                            (decimal-integer (first parts))))
              (event-type (if (and (second parts)
                                   (not (string= (second parts) "")))
                              (decimal-integer (second parts))
                              1)))
          (unless (and modifier event-type)
            (return-from parse-modifier-field
              (values nil nil :malformed)))
          (values modifier event-type :ok)))))

(defun make-key-with-modifiers (sym encoded-modifier)
  (multiple-value-bind (args status)
      (modifier-args encoded-modifier)
    (unless (eq status :ok)
      (return-from make-key-with-modifiers (values nil status)))
    (values (apply #'make-key
                   (append args (list :sym sym)))
            :key)))

(defun codepoint-sym (code)
  (or (functional-u-sym code)
      (cond
        ((= code 32) "Space")
        ((pua-code-p code) nil)
        ((unicode-scalar-p code)
         (let ((char (code-char code)))
           (and char (string char))))
        (t nil))))

(defun parse-u-sequence (body)
  (let ((fields (split-on body #\;)))
    (when (> (length fields) 2)
      (return-from parse-u-sequence (values nil :unsupported)))
    (let ((key-field (first fields))
          (modifier-field (second fields)))
      (when (or (null key-field)
                (position #\: key-field))
        (return-from parse-u-sequence (values nil :unsupported)))
      (let ((code (decimal-integer key-field)))
        (unless code
          (return-from parse-u-sequence (values nil :malformed)))
        (multiple-value-bind (modifier event-type modifier-status)
            (parse-modifier-field modifier-field)
          (unless (eq modifier-status :ok)
            (return-from parse-u-sequence
              (values nil modifier-status)))
          (unless (member event-type '(1 2))
            (return-from parse-u-sequence (values nil :unsupported)))
          (let ((sym (codepoint-sym code)))
            (unless sym
              (return-from parse-u-sequence
                (values nil :unsupported)))
            (make-key-with-modifiers sym modifier)))))))

(defun parse-letter-sequence (body final)
  (let ((sym (letter-final-sym final)))
    (unless sym
      (return-from parse-letter-sequence (values nil :unsupported)))
    (if (string= body "")
        (make-key-with-modifiers sym 1)
        (let ((fields (split-on body #\;)))
          (unless (and (<= (length fields) 2)
                       (string= (first fields) "1"))
            (return-from parse-letter-sequence
              (values nil :unsupported)))
          (let ((modifier (if (second fields)
                              (decimal-integer (second fields))
                              1)))
            (unless modifier
              (return-from parse-letter-sequence
                (values nil :malformed)))
            (make-key-with-modifiers sym modifier))))))

(defun parse-tilde-sequence (body)
  (let ((fields (split-on body #\;)))
    (unless (<= (length fields) 2)
      (return-from parse-tilde-sequence (values nil :unsupported)))
    (let* ((number (decimal-integer (first fields)))
           (sym (and number (tilde-sym number)))
           (modifier (if (second fields)
                         (decimal-integer (second fields))
                         1)))
      (unless (and number modifier)
        (return-from parse-tilde-sequence (values nil :malformed)))
      (unless sym
        (return-from parse-tilde-sequence (values nil :unsupported)))
      (make-key-with-modifiers sym modifier))))

(defun parse-csi-sequence (sequence)
  "Parse SEQUENCE, the bytes after ESC [, as one complete CSI event.

Return two values: a Lem key or NIL and a status keyword."
  (when (zerop (length sequence))
    (return-from parse-csi-sequence (values nil :malformed)))
  (let* ((final (char sequence (1- (length sequence))))
         (body (subseq sequence 0 (1- (length sequence)))))
    (when (or (and (plusp (length body))
                   (member (char body 0) '(#\? #\> #\=)))
              (char= final #\c)
              (char= final #\R))
      (return-from parse-csi-sequence (values nil :response)))
    (cond
      ((char= final #\u)
       (parse-u-sequence body))
      ((char= final #\~)
       (parse-tilde-sequence body))
      ((and (char= final #\Z) (string= body ""))
       (make-key-with-modifiers "Tab" 2))
      ((find final "ABCDEFHPQS" :test #'char=)
       (parse-letter-sequence body final))
      (t
       (values nil :unsupported)))))

(defun enable ()
  "Push KKP flag 1 when the user enabled it for this ncurses session."
  (setf *keyboard-mode-pushed-p* nil)
  (when (and (variable-value
              'lem-ncurses/config:enable-kitty-keyboard-protocol
              :global)
             (lem-ncurses/term:raw-terminal-output-available-p))
    (setf *keyboard-mode-pushed-p* t)
    (lem-ncurses/term:write-terminal-sequence
     +push-disambiguate-sequence+)
    t))

(defun disable ()
  "Pop the keyboard mode pushed by ENABLE, exactly once."
  (when *keyboard-mode-pushed-p*
    (unwind-protect
         (lem-ncurses/term:write-terminal-sequence
          +pop-keyboard-mode-sequence+)
      (setf *keyboard-mode-pushed-p* nil))))
