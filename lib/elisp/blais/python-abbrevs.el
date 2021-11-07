;; -*- lisp-interaction -*-
;;; Author: Martin Blais <blais@furius.ca>
;;; My own abbreviations for Python mode.

(defun pyabbrevs-function-header ()
  (interactive)
  (let ((ccol))
    (setq ccol (current-column))
    (insert "#-\n#")
    (backward-char 3) (repeat-last-character 1)
    (forward-char 2)
    (or (looking-at "$") (insert "\n"))
  ))

(defvar python-big-separator-list
  '("EXTERNAL DECLARATIONS"
    "LOCAL DECLARATIONS"
    "PUBLIC DECLARATIONS"
    "MAIN"
    "TESTS")
  "Legal strings for big Python separators.")

(defun pyabbrevs-insert-big-separator ()
  (let ((title (completing-read "Separator Title:"
				(mapcar 'list python-big-separator-list))))
    (insert "#=") (repeat-last-character t)
    (insert "\n# " title "\n")
    (insert "#=") (repeat-last-character t)
    (insert "\n" )))

(defun pyabbrevs-raise-exit ()
  (interactive)
  (indent-according-to-mode)
  (insert "raise SystemExit")
  ;;(insert "raise SystemExit(\"Error: \")")
  (backward-char 2))

;; Note: this list comes from the Python documentation under the Built-in
;; Exceptions section.
(defvar python-exception-names
  '("Exception"
    "SystemExit"
    "StopIteration"
    "StandardError"
    "KeyboardInterrupt"
    "ImportError"
    "EnvironmentError"
    "IOError"
    "OSError"
    "WindowsError"
    "EOFError"
    "RuntimeError"
    "NotImplementedError"
    "NameError"
    "UnboundLocalError"
    "AttributeError"
    "SyntaxError"
    "IndentationError"
    "TabError"
    "TypeError"
    "AssertionError"
    "LookupError"
    "IndexError"
    "KeyError"
    "ArithmeticError"
    "OverflowError"
    "ZeroDivisionError"
    "FloatingPointError"
    "ValueError"
    "UnicodeError"
    "UnicodeEncodeError"
    "UnicodeDecodeError"
    "UnicodeTranslateError"
    "ReferenceError"
    "SystemError"
    "MemoryError"
    "Warning"
    "UserWarning"
    "DeprecationWarning"
    "PendingDeprecationWarning"
    "SyntaxWarning"
    "OverflowWarning"
    "RuntimeWarning"
    "FutureWarning")
  "Name of the Python builtin exceptions.")

(defun pyabbrevs-try-except (rbeg rend)
  (interactive "r")
  (let ((excname (completing-read "Exception:"
				  (mapcar 'list python-exception-names)))
	(col))
    (indent-according-to-mode)
    (setq col (current-column))
    (insert "try:\n\n")
    (move-to-column col t)
    (insert (format "except %s, e:\n" excname))
    (pyabbrevs-raise-exit)
    (forward-line -2)
    (indent-according-to-mode)
  ))

(defun pyabbrevs-ioerror ()
  (interactive)
  (let ((cursor))
    (indent-according-to-mode)
    (backward-char py-indent-offset)
    (insert "except IOError, e:\n")
    (indent-according-to-mode)
    (insert "raise SystemExit(\"Error: ")
    (setq cursor (point))
    (insert " (%s)\" % str(e))")
    (goto-char cursor)
  ))

(defun pyabbrevs-add-option ()
  (interactive)
  (let ((cursor))
    (open-line 1)
    (indent-according-to-mode)
    ;; (insert "parser.add_option('-")
    (insert "parser.add_argument('-")
    (setq cursor (point))
    (insert "', '--', action='store_true',\n")
    (indent-according-to-mode)
    (insert "help=\"\")")
    (goto-char cursor)
  ))

;;;-----------------------------------------------------------------------------

(defvar pyabbrevs-log-config
  "logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')")

(defvar pyabbrevs-main
"import argparse
import logging

def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    #parser.add_argument('filenames', nargs='+', help='Filenames')
    args = parser.parse_args()

    # ...


if __name__ == '__main__':
    main()
")


(defvar pyabbrevs-test
  "
import unittest

class TestXXX(unittest.TestCase):

    def test_simple(self):
        \"\"

if __name__ == '__main__':
    unittest.main()

")

(defvar pyabbrevs-test-long
  "
import unittest

class Test(unittest.TestCase):

    def test_simple( self ):
        ''

def suite():
    suite = unittest.TestSuite()
    suite.addTest(TestFields('test_simple'))
    return suite

if __name__ == '__main__':
    unittest.main()
")

(defvar pyabbrevs-doctest
  "
if __name__ == '__main__':
    import doctest; doctest.testmod()
")



;;;-----------------------------------------------------------------------------

(defvar pyabbrevs-header
  "#!/usr/bin/env python
\"\"\"
\"\"\"

")

(defvar pyabbrevs-script
  (concat pyabbrevs-header pyabbrevs-main))



;;;-----------------------------------------------------------------------------

(defvar pyabbrevs-checker
  "import os, pychecker.checker
os.environ['PYCHECKER'] = '-q'
")

;;;-----------------------------------------------------------------------------

(defun pyabbrevs-insert-lines (lines &optional markre)
  "Insert the given lines and return to the marker character if present."
  (let ((begin (point))
	(col (current-column)))
    (dolist (x lines)
      (indent-to-column col)
      (insert x)
      (insert "\n"))
    (if markre
	(progn
	  (re-search-backward markre begin t)
	  (goto-char (match-end 0))))))

;;;-----------------------------------------------------------------------------

(defun pyabbrevs-debug-print ()
  (pyabbrevs-insert-lines
   '("## FIXME: remove"
     "import sys"
     "from pprint import pprint, pformat"
     "print >> sys.stderr, pformat()"
     "sys.stderr.flush()")
   "format("))

;;;-----------------------------------------------------------------------------

(defun pyabbrevs-insert-method ()
  (pyabbrevs-insert-lines
   '("def (self):"
     "    \"\"\""
     ""
     "    \"\"\"") "def "))

(defun pyabbrevs-insert-docstring ()
  (pyabbrevs-insert-lines
   '("\"\"\""
     ""
     "\"\"\""))
  (forward-line -2)
  (indent-for-tab-command))

(defun pyabbrevs-insert-conn ()
  (pyabbrevs-insert-lines
   '("conn, cursor = dbpool().connection_ro(1)"
     "try:"
     ""
     "finally:"
     "conn.release()")))

(defun pyabbrevs-trace ()
  (let* ((upword
	  (save-excursion
	    (previous-line)
	    (thing-at-point 'word)))
	 (expr (completing-read "Expression: " nil nil nil upword)))
    (indent-according-to-mode)
    (insert (format "trace(\"%s\", %s)" expr expr))
    (backward-char 1)
    ))

(defun pyabbrevs-insert-constructor ()
  (pyabbrevs-insert-lines
   '("def __init__(self):"
     ".__init__(self)") "def"))


;;;-----------------------------------------------------------------------------

(define-abbrev-table
  'python-mode-abbrev-table
  (list
   '("nf" "" pyabbrevs-function-header 0)
   '("sep" "" pyabbrevs-insert-big-separator 0)
   '("ex" "" pyabbrevs-raise-exit 0)
   '("x" "" pyabbrevs-raise-exit 0)
   '("exit" "" pyabbrevs-raise-exit 0)
   '("t" "" pyabbrevs-trace 0)
   '("tr" "" pyabbrevs-trace 0)
   '("try" "" pyabbrevs-try-except 0)
   '("ioerr" "" pyabbrevs-ioerror 0)
   '("raise" "" pyabbrevs-raise-exit 0)
   `("test" ,pyabbrevs-test nil 0)
   `("testsuite" ,pyabbrevs-test-long nil 0)
   `("doctest" ,pyabbrevs-doctest nil 0)
   `("head" ,pyabbrevs-header nil 0)
   `("header" ,pyabbrevs-header nil 0)
   `("main" ,pyabbrevs-main nil 0)
   `("log" ,pyabbrevs-log-config nil 0)
   `("basicConfig" ,pyabbrevs-log-config nil 0)
   `("all" ,pyabbrevs-script nil 0)
   '("opt" "" pyabbrevs-add-option 0)
   '("pp" "from pprint import pprint, pformat" nil 0)
   '("numpy" "import numpy as np" nil 0)
   '("mpl" "import matplotlib.pyplot as plt" nil 0)
   ;; '("gpl" "" pyabbrevs-insert-gpl 0)
   '("per" "print >> sys.stderr, " nil 0)
   '("prs" "import sys; print >> sys.stderr, " nil 0)
   '("er" ">> sys.stderr, " nil 0)
   '("with" "from __future__ import with_statement" nil 0)
   '("pa" "from os.path import *" nil 0)
   `("im" "#  imports" ,(lambda () (backward-word) (backward-char)) 0)
   `("chk" ,pyabbrevs-checker nil 0)
   '("coding" "-*- coding: iso-8859-1 -*-" nil 0)
   '("meth" "" pyabbrevs-insert-method 0)
   '("init" "" pyabbrevs-insert-constructor 0)
   '("def" "" pyabbrevs-insert-method 0)
   '("ds" "" pyabbrevs-insert-docstring 0)
   '("conn" "" pyabbrevs-insert-conn 0)
   '("rest" "" insert-rest 0)
   '("blog" "bli" insert-blog 0)
   '("ni" "raise NotImplementedError ## FIXME: TODO" nil 0)
   '("te" "self.assertEqual" nil 0)
   '("teq" "self.assertEqual" nil 0)
   '("tne" "self.assertNotEqual" nil 0)
   '("ta" "self.assert_" nil 0)
   '("tas" "self.assert_" nil 0)
   '("tt" "self.assertTrue" nil 0)
   '("tf" "self.assertFalse" nil 0)
   '("tr" "with self.assertRaises():" nil 0)
   '("traises" "with self.assertRaises():" nil 0)
   ))

(define-abbrev-table
  'text-mode-abbrev-table
  (list
   '("rest" "" insert-rest 0)
   '("blog" "" insert-blog 0)
   ))

;; (defun python-abbrev-print ()
;;   "Help me change old habits."
;;   (insert "print()") (backward-char 1) t)
;; (put 'python-abbrev-print 'no-self-insert t)
;; (define-abbrev python-mode-abbrev-table "print" "" 'python-abbrev-print)

(provide 'python-abbrevs)
