(defpackage :lem-ncurses/config
  (:use :cl
        :lem)
  (:export :escape-delay
           :enable-kitty-keyboard-protocol))
(in-package :lem-ncurses/config)

;; escape key delay setting
(define-editor-variable escape-delay 100)


;; Kitty Keyboard Protocol
(define-editor-variable enable-kitty-keyboard-protocol nil)
