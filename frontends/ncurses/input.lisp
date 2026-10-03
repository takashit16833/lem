(defpackage :lem-ncurses/input
  (:use :cl
        :lem
        :lem-ncurses/key)
  (:export :get-event))
(in-package :lem-ncurses/input)

;; for input
;;  (we don't use stdscr for input because it calls wrefresh implicitly
;;   and causes the display confliction by two threads)
(defvar *padwin* nil)
(defvar *pending-codes* nil)

(defun ensure-padwin ()
  (unless *padwin*
    (setf *padwin* (charms/ll:newpad 1 1))
    (charms/ll:keypad *padwin* 1)
    (charms/ll:wtimeout *padwin* -1))
  *padwin*)

(defun getch ()
  (if *pending-codes*
      (pop *pending-codes*)
      (progn
        (ensure-padwin)
        (charms/ll:wgetch *padwin*))))

(defun prepend-pending-codes (codes)
  (setf *pending-codes*
        (append codes *pending-codes*)))

(defmacro with-getch-input-timeout ((time) &body body)
  `(progn
     (charms/ll:wtimeout *padwin* ,time)
     (unwind-protect (progn ,@body)
       (charms/ll:wtimeout *padwin* -1))))

(defun utf8-bytes (c)
  (cond
    ((<= c #x7f) 1)
    ((<= #xc2 c #xdf) 2)
    ((<= #xe0 c #xef) 3)
    ((<= #xf0 c #xf4) 4)
    (t 1)))

(defun get-key (code)
  (let* ((char (let ((nbytes (utf8-bytes code)))
                 (if (= nbytes 1)
                     (code-char code)
                     (let ((vec (make-array nbytes :element-type '(unsigned-byte 8))))
                       (setf (aref vec 0) code)
                       (with-getch-input-timeout (100)
                         (loop :for i :from 1 :below nbytes
                               :do (setf (aref vec i) (getch))))
                       (handler-case (schar (babel:octets-to-string vec) 0)
                         (babel-encodings:invalid-utf8-continuation-byte ()
                           (code-char code)))))))
         (key (char-to-key char)))
    key))

(defun csi-final-byte-p (code)
  (and (integerp code)
       (<= #x40 code #x7e)))

(defun byte-code-p (code)
  (and (integerp code)
       (<= 0 code #xff)))

(defun collect-csi-sequence (first-code)
  "Collect one CSI sequence after ESC [.

FIRST-CODE is the first code after the left bracket. Return three values:
the sequence string (without ESC [), a status keyword and an optional
interrupting ncurses code."
  (let ((codes (list first-code))
        (count 1))
    (loop
      (when (csi-final-byte-p (car (last codes)))
        (return (values (coerce (mapcar #'code-char codes) 'string)
                        :complete
                        nil)))
      (when (> count 128)
        (loop
          (let ((code (getch)))
            (cond
              ((= code -1)
               (return-from collect-csi-sequence
                 (values nil :discard nil)))
              ((not (byte-code-p code))
               (return-from collect-csi-sequence
                 (values nil :discard code)))
              ((csi-final-byte-p code)
               (return-from collect-csi-sequence
                 (values nil :discard nil)))))))
      (let ((code (getch)))
        (cond
          ((= code -1)
           (return (values codes :incomplete nil)))
          ((not (byte-code-p code))
           (return (values codes :interrupted code)))
          (t
           (setf codes (nconc codes (list code)))
           (incf count)))))))

(defun replay-incomplete-csi (codes &optional interrupt-code)
  (let ((replay (cons (char-code #\[)
                      (if (stringp codes)
                          (map 'list #'char-code codes)
                          (copy-list codes)))))
    (when interrupt-code
      (setf replay (nconc replay (list interrupt-code))))
    (prepend-pending-codes replay)))

(defun kitty-abort-key-p (key)
  (and (key-p key)
       (key-ctrl key)
       (not (key-meta key))
       (not (key-super key))
       (not (key-hyper key))
       (not (key-shift key))
       (string= (key-sym key) "]")))

(defun parse-csi-event (first-code)
  (multiple-value-bind (sequence status interrupt-code)
      (collect-csi-sequence first-code)
    (case status
      (:complete
       (lem-ncurses/kitty-keyboard:parse-csi-sequence sequence))
      (:incomplete
       (replay-incomplete-csi sequence)
       (values (get-key-from-name "escape") :key))
      (:interrupted
       (replay-incomplete-csi sequence interrupt-code)
       (values (get-key-from-name "escape") :key))
      (:discard
       (when interrupt-code
         (prepend-pending-codes (list interrupt-code)))
       (values nil :unsupported))
      (otherwise
       (values nil :unsupported)))))

(let ((resize-code (get-code "[resize]"))
      (abort-code (get-code "C-]"))
      (escape-code (get-code "escape")))
  (defun get-event ()
    (tagbody :start
      (return-from get-event
        (let ((code (getch)))
          (cond ((= code -1) (go :start))
                ((= code resize-code) :resize)
                ((= code abort-code) :abort)
                ((= code escape-code)
                 (let ((code (with-getch-input-timeout
                                 ((variable-value 'lem-ncurses/config:escape-delay))
                               (getch))))
                   (cond ((= code -1)
                          (get-key-from-name "escape"))
                         ((= code #.(char-code #\[))
                          (with-getch-input-timeout (100)
                            (let ((first-code (getch)))
                              (cond
                                ((= first-code -1)
                                 (get-key-from-name "escape"))
                                ((= first-code #.(char-code #\<))
                                 ;;sgr(1006)
                                 (uiop:symbol-call :lem-mouse-sgr1006
                                                   :parse-mouse-event
                                                   #'getch))
                                ((byte-code-p first-code)
                                 (multiple-value-bind (event status)
                                     (parse-csi-event first-code)
                                   (cond
                                     ((eq status :key)
                                      (if (kitty-abort-key-p event)
                                          :abort
                                          event))
                                     (t
                                      (go :start)))))
                                (t
                                 (prepend-pending-codes
                                  (list (char-code #\[) first-code))
                                 (get-key-from-name "escape"))))))
                         (t
                          (let ((key (get-key code)))
                            (make-key :meta t
                                      :sym (key-sym key)
                                      :ctrl (key-ctrl key)))))))
                (t
                 (get-key code))))))))
