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

(deftest unknown-pua-is-not-inserted
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
