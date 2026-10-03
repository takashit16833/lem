(defsystem "lem-ncurses-tests"
  :depends-on ("lem-ncurses"
               "rove")
  :pathname "tests/ncurses"
  :components ((:file "kitty-keyboard"))
  :perform (test-op (o c)
             (declare (ignore o))
             (symbol-call :rove :run c)))
