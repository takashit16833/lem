(defpackage :lem-tests/ncurses-kitty-keyboard
  (:use :cl :rove :lem))
(in-package :lem-tests/ncurses-kitty-keyboard)

(defun parse-key (sequence)
  (lem-ncurses/kitty-keyboard:parse-csi-sequence sequence))

(defun key-matches-p (key &key ctrl meta super hyper shift sym)
  (and key
       (match-key key
                  :ctrl ctrl
                  :meta meta
                  :super super
                  :hyper hyper
                  :shift shift
                  :sym sym)))

(deftest parse-super-key
  (multiple-value-bind (key status) (parse-key "115;9u")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "s"))))

(deftest parse-shift-super-key
  (multiple-value-bind (key status) (parse-key "122;10u")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :shift t :sym "z"))))

(deftest parse-control-key
  (multiple-value-bind (key status) (parse-key "99;5u")
    (ok (eq status :key))
    (ok (key-matches-p key :ctrl t :sym "c"))))

(deftest parse-control-right-bracket
  (multiple-value-bind (key status) (parse-key "93;5u")
    (ok (eq status :key))
    (ok (key-matches-p key :ctrl t :sym "]"))))

(deftest parse-special-u-keys
  (multiple-value-bind (key status) (parse-key "27u")
    (ok (eq status :key))
    (ok (key-matches-p key :sym "Escape")))
  (multiple-value-bind (key status) (parse-key "13;9u")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "Return")))
  (multiple-value-bind (key status) (parse-key "9;9u")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "Tab")))
  (multiple-value-bind (key status) (parse-key "127;9u")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "Backspace"))))

(deftest parse-default-modifier
  (multiple-value-bind (key status) (parse-key "115u")
    (ok (eq status :key))
    (ok (key-matches-p key :sym "s")))
  (multiple-value-bind (key status) (parse-key "115;1u")
    (ok (eq status :key))
    (ok (key-matches-p key :sym "s"))))

(deftest parse-functional-keys
  (multiple-value-bind (key status) (parse-key "1;9A")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "Up")))
  (multiple-value-bind (key status) (parse-key "3;9~")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "Delete")))
  (multiple-value-bind (key status) (parse-key "13;9~")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "F3")))
  (multiple-value-bind (key status) (parse-key "Z")
    (ok (eq status :key))
    (ok (key-matches-p key :shift t :sym "Tab"))))

(deftest parse-ncurses-extended-key-names
  ;; ncurses can consume KKP legacy functional-key sequences before the
  ;; CSI collector sees them. Recover the original modifier field from
  ;; the extended terminfo key name returned by keyname().
  (multiple-value-bind (key status)
      (lem-ncurses/kitty-keyboard:parse-ncurses-key-name "kLFT4")
    (ok (eq status :key))
    (ok (key-matches-p key :meta t :shift t :sym "Left")))
  (multiple-value-bind (key status)
      (lem-ncurses/kitty-keyboard:parse-ncurses-key-name "kRIT4")
    (ok (eq status :key))
    (ok (key-matches-p key :meta t :shift t :sym "Right")))
  (multiple-value-bind (key status)
      (lem-ncurses/kitty-keyboard:parse-ncurses-key-name "kLFT5")
    (ok (eq status :key))
    (ok (key-matches-p key :ctrl t :sym "Left")))
  (multiple-value-bind (key status)
      (lem-ncurses/kitty-keyboard:parse-ncurses-key-name "kUP9")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "Up")))
  (multiple-value-bind (key status)
      (lem-ncurses/kitty-keyboard:parse-ncurses-key-name "KEY_LEFT")
    (ng key)
    (ok (eq status :unsupported))))

(deftest ncurses-generated-code-prefers-kkp-key-name
  ;; 545 is historically hard-coded as C-Left in Lem.  If ncurses assigns
  ;; the same integer to kLFT4, KKP must recover M-Shift-Left from keyname
  ;; before the historical integer table is consulted.
  (let ((lem-ncurses/kitty-keyboard::*keyboard-mode-pushed-p* t))
    (let ((key (lem-ncurses/input::kkp-ncurses-key 545 "kLFT4")))
      (ok (key-matches-p key :meta t :shift t :sym "Left"))))
  ;; KKP disabled keeps the existing ncurses path unchanged.
  (let ((lem-ncurses/kitty-keyboard::*keyboard-mode-pushed-p* nil))
    (ng (lem-ncurses/input::kkp-ncurses-key 545 "kLFT4"))))

(deftest unicode-does-not-use-ncurses-keycodes
  (multiple-value-bind (key status) (parse-key "259;9u")
    (ok (eq status :key))
    (ok (key-super key))
    (ok (string= (key-sym key) (string (code-char 259))))
    (ng (string= (key-sym key) "Up"))))

(deftest parse-f13-and-keypad
  (multiple-value-bind (key status) (parse-key "57376u")
    (ok (eq status :key))
    (ok (key-matches-p key :sym "F13")))
  (multiple-value-bind (key status) (parse-key "57414;9u")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "Return")))
  (multiple-value-bind (key status) (parse-key "57419;9u")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "Up"))))

(deftest alt-and-meta-fold-into-lem-meta
  (multiple-value-bind (key status) (parse-key "97;3u")
    (ok (eq status :key))
    (ok (key-matches-p key :meta t :sym "a")))
  (multiple-value-bind (key status) (parse-key "97;33u")
    (ok (eq status :key))
    (ok (key-matches-p key :meta t :sym "a"))))

(deftest lock-bits-do-not-change-shortcut
  (multiple-value-bind (key status) (parse-key "115;73u")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "s"))))

(deftest release-and-advanced-fields-are-not-executed
  (multiple-value-bind (key status) (parse-key "115;9:3u")
    (ng key)
    (ok (eq status :unsupported)))
  (multiple-value-bind (key status) (parse-key "115:83;9u")
    (ng key)
    (ok (eq status :unsupported)))
  (multiple-value-bind (key status) (parse-key "115;9;115u")
    (ng key)
    (ok (eq status :unsupported))))

(deftest terminal-responses-are-not-keys
  (dolist (sequence '("?0u" "?1;2c" "1;2R"))
    (multiple-value-bind (key status) (parse-key sequence)
      (ng key)
      (ok (eq status :response)))))

(deftest unsupported-functional-key-is-not-inserted
  ;; MUTE_VOLUME is a valid KKP functional key but Lem has no matching
  ;; named key yet. It must be consumed safely instead of becoming text.
  (multiple-value-bind (key status) (parse-key "57440u")
    (ng key)
    (ok (eq status :unsupported))))

(deftest malformed-input-is-rejected
  (dolist (sequence '("u" "115;0u" "-1u" "1114112u" "55296u"))
    (multiple-value-bind (key status) (parse-key sequence)
      (ng key)
      (ok (member status '(:malformed :unsupported))))))

(deftest repeat-is-treated-as-another-key-press
  (multiple-value-bind (key status) (parse-key "115;9:2u")
    (ok (eq status :key))
    (ok (key-matches-p key :super t :sym "s"))))


(deftest core-control-key-compatibility
  ;; The first KKP implementation intentionally preserves Lem core's
  ;; historical C-i/C-m/C-[ conversions.
  (multiple-value-bind (key status) (parse-key "105;5u")
    (ok (eq status :key))
    (ok (key-matches-p key :sym "Tab")))
  (multiple-value-bind (key status) (parse-key "109;5u")
    (ok (eq status :key))
    (ok (key-matches-p key :sym "Return")))
  (multiple-value-bind (key status) (parse-key "91;5u")
    (ok (eq status :key))
    (ok (key-matches-p key :sym "Escape"))))

(deftest parse-control-space
  (multiple-value-bind (key status) (parse-key "32;5u")
    (ok (eq status :key))
    (ok (key-matches-p key :ctrl t :sym "Space"))))

(defun make-code-reader (codes)
  (let ((codes (copy-list codes)))
    (lambda ()
      (if codes
          (pop codes)
          -1))))

(deftest collect-complete-csi-sequence
  (let ((reader (make-code-reader
                 (map 'list #'char-code "15;9u"))))
    (multiple-value-bind (sequence status interrupt)
        (lem-ncurses/input::collect-csi-sequence
         (char-code #\1)
         reader)
      (ok (eq status :complete))
      (ok (string= sequence "115;9u"))
      (ng interrupt))))

(deftest collect-partial-csi-sequence
  (let ((reader (make-code-reader
                 (map 'list #'char-code "15;"))))
    (multiple-value-bind (sequence status interrupt)
        (lem-ncurses/input::collect-csi-sequence
         (char-code #\1)
         reader)
      (declare (ignore sequence))
      (ok (eq status :incomplete))
      (ng interrupt))))

(deftest collect-csi-interrupted-by-ncurses-event
  (let ((reader (make-code-reader
                 (list (char-code #\1)
                       (char-code #\5)
                       410))))
    (multiple-value-bind (sequence status interrupt)
        (lem-ncurses/input::collect-csi-sequence
         (char-code #\1)
         reader)
      (declare (ignore sequence))
      (ok (eq status :interrupted))
      (ok (= interrupt 410)))))

(deftest nested-input-timeout-restores-state
  (let ((lem-ncurses/input::*padwin* nil)
        (lem-ncurses/input::*getch-timeout* -1))
    (lem-ncurses/input::with-getch-input-timeout (100)
      (ok (= lem-ncurses/input::*getch-timeout* 100))
      (lem-ncurses/input::with-getch-input-timeout (20)
        (ok (= lem-ncurses/input::*getch-timeout* 20)))
      (ok (= lem-ncurses/input::*getch-timeout* 100)))
    (ok (= lem-ncurses/input::*getch-timeout* -1))))


(deftest kitty-keyboard-push-pop-is-balanced
  (let ((stream (make-string-output-stream))
        (lem-ncurses/term::*tty-name* nil)
        (lem-ncurses/kitty-keyboard::*keyboard-mode-pushed-p* nil))
    (let ((lem-ncurses/term::*terminal-output-stream* stream))
      (lem/common/var:with-global-variable-value
          (lem-ncurses/config:enable-kitty-keyboard-protocol t)
        (lem-ncurses/kitty-keyboard:enable)
        ;; A second enable must not push a second stack entry.
        (lem-ncurses/kitty-keyboard:enable)
        (lem-ncurses/kitty-keyboard:disable)
        ;; A second disable must not pop the caller's stack entry.
        (lem-ncurses/kitty-keyboard:disable))
      (ok
       (string=
        (get-output-stream-string stream)
        (format nil "~C[>1u~C[<u" #\Esc #\Esc))))))

(deftest user-setting-can-disable-early-default
  (let ((stream (make-string-output-stream))
        (lem-ncurses/term::*tty-name* nil)
        (lem-ncurses/kitty-keyboard::*keyboard-mode-pushed-p* nil)
        (variable 'lem-ncurses/config:enable-kitty-keyboard-protocol))
    (let ((lem-ncurses/term::*terminal-output-stream* stream)
          (saved (variable-value
                  'lem-ncurses/config:enable-kitty-keyboard-protocol
                  :global)))
      (unwind-protect
           (progn
             (setf (variable-value variable :global) t)
             (lem-ncurses/kitty-keyboard:enable)
             (setf (variable-value variable :global) nil)
             (lem-ncurses/kitty-keyboard::sync-enabled-state)
             (ng (lem-ncurses/kitty-keyboard:enabled-p))
             (ok
              (string=
               (get-output-stream-string stream)
               (format nil "~C[>1u~C[<u" #\Esc #\Esc))))
        (setf (variable-value variable :global) saved)))))
