;; -*- mode: lisp-interaction; fill-column: 80 -*-
;; Setup for Jupyter notebooks (via emacs-jupyter)
;;
;; Author: Martin Blais <blais@furius.ca>
;;
;; Support for Jupyter notebooks best works with emacs-juypter + code-cells +
;; percent mode for Jupytext notebooks. I'm adding a few bindings below for
;; invoking code-cells functions (don't bother using their mode).
;;
;; Install it like this:
;;   (require 'jupyter-setup)
;;   (jupyter-define-keybindings)
;; No keybindings are installed by default.

(require 'jupyter)


;;------------------------------------------------------------------------------
;; Connecting to kernels.

(defun jupyter-get-latest-connection-file ()
  (caar (cl-sort
         (directory-files-and-attributes "~/.local/share/jupyter/runtime"
                                         t ".*\.json$")
         #'(lambda (x y) (time-less-p y x))
         :key #'(lambda (x) (nth 6 x)))))

;; Note: there's some useful improvements on this living at:
;; https://sqrtminusone.xyz/posts/2021-05-01-org-python/
(defun jupyter-connect-repl-latest ()
  "Automatically find the latest kernel that was started and connect to it."
  (interactive)
  (let* ((latest-connection-file (jupyter-get-latest-connection-file))
         (file-dir (directory-file-name (file-name-directory (buffer-file-name))))
         (py-chdir (format "import os.path; os.chdir(\"%s\")" file-dir)))
    (message "Connecting via %s" latest-connection-file)
    (jupyter-connect-repl latest-connection-file nil t nil t)
    (message "Setting kernel CWD to %s" py-chdir)
    (jupyter-eval-string py-chdir)))


;;------------------------------------------------------------------------------
;; Various utility functions. See keybindings.

(defun jupyter-chdir-to-buffer ()
  (interactive)
  (let* ((file-dir (directory-file-name (file-name-directory (buffer-file-name))))
         (py-chdir (format "import os.path; os.chdir(\"%s\")" file-dir)))
    (message "Setting kernel CWD to %s" py-chdir)
    (jupyter-eval-string py-chdir)))

(defun jupyter-print-help (beg end)
  (interactive "r")
  (let ((py-render (format "help(%s)" (buffer-substring beg end))))
    (jupyter-eval-string py-render)
    ))

(defun jupyter-render-and-view-dataframe (beg end)
  (interactive "r")
  (let ((py-render (format "%s.to_csv('/tmp/dataframe.csv')"
                           (buffer-substring beg end))))
    (jupyter-eval-string py-render)
    (call-process "tmux" nil nil nil "new-window" "vd -f csv '/tmp/dataframe.csv'")
    ;; Note: Should probably delete the temp file.
    ))

(defun jupyter-print-dataframe (beg end)
  (interactive "r")
  (jupyter-eval-string (format "print((%s.to_string()))" (buffer-substring beg end))))

(defun jupyter-print-shape (beg end)
  (interactive "r")
  (jupyter-eval-string (format "print((%s.shape))" (buffer-substring beg end))))

(defun jupyter-reload-thing-at-point ()
  (interactive)
  (let ((module (thing-at-point 'symbol)))
    (message "Reloading module: %s" module)
    (jupyter-eval-string (format "import importlib; importlib.reload(%s)"
                                 module))))

(defun jupyter-eval-thing-at-point ()
  (interactive)
  (let ((expr (thing-at-point 'symbol)))
    (jupyter-eval-string expr)))


;;------------------------------------------------------------------------------
;; Window configuration.

(defun jupyter-window-configuration ()
  "Reset the window configuration to something reasonable for working."
  (interactive)
  (let ((win1 (window-buffer (nth 0 (window-list-stable))))
        (win2 (window-buffer (nth 1 (window-list-stable)))))

    (delete-other-windows)
    (split-window nil nil 'right)
    (split-window nil nil 'right)
    (split-window nil nil 'right)

    (select-window (nth 3 (window-list-stable)))
    ;(split-window nil nil 'below)

    (select-window (nth 2 (window-list-stable)))
    (split-window nil nil 'below)

    (balance-windows)
    (set-window-buffer (nth 0 (window-list-stable)) win1)
    (set-window-buffer (nth 1 (window-list-stable)) win2)

    (dolist (winbuf '((2 "result")
                      (3 "error")
                      (4 "output")
                      ;;(5 "traceback")
                      ;;"display"
                      ;;"repl"
                      ))
      (set-window-buffer (nth (car winbuf) (window-list-stable))
                         (jupyter-get-buffer-create (cadr winbuf))))
    ))


;;------------------------------------------------------------------------------
;; Merging output buffers together.
;; Note: This is tricky.

;; Never display evaluation results in the minibuffer. This is set to 10 by
;; default.
(setq jupyter-eval-short-result-max-lines -1)


;; Redirect some buffers to others, to minimize teh number of buffers.
;;
;; Note: Merging the result isn't obvious, since it clears the output before
;; printing itself. Merging the display into the output should be fine though.
(defvar jupyter-buffer-name-translations
  '(("display" "output")
    ("traceback" "error"))
  "An association list of (FROM, TO) pairs of buffer name
  translations for Jupyter display buffers.")

(defun jupyter-translate-buffer-name (args)
  "Translate buffer names to others, in order to join some of their outputs."
  (let* ((orig-name (car args))
         (pair (assoc orig-name jupyter-buffer-name-translations 'string=))
         (new-name (if pair (cadr pair) orig-name)))
    (cons new-name (cdr args))))

(advice-add 'jupyter-get-buffer-create :filter-args #'jupyter-translate-buffer-name)


;; Override for the function that displays to the traceback buffer, so that it
;; does not reset the buffer, in order to be able to merge the error and
;; traceback buffers to a single output.
(defun jupyter-override-reset (orig &rest args)
  "Override the reset flag of certain buffer types."
  (flet ((jupyter--reset-display-buffer-p (arg) nil))
    (apply orig args)))
(advice-add 'jupyter-display-traceback :around #'jupyter-override-reset)


(defun jupyter-reset-display-buffer (name)
  "Reset one of the Jupyter display buffers, explicitly, by name."
  (let ((buffer (jupyter-get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

;; Clear all the display buffers before evaluating anything.
(defun reset-jupyter-display-buffers (orig &rest args)
  (dolist (name '("output" "result" "error" "traceback" "display"))
    ;; (unless (assoc name jupyter-buffer-name-translations)
    (jupyter-reset-display-buffer name)))
(advice-add 'jupyter-eval-string :before #'reset-jupyter-display-buffers)
;(advice-add 'jupyter-eval-line-or-region :before #'reset-jupyter-display-buffers)


;;------------------------------------------------------------------------------
;; Support for code cells movement.

(require 'code-cells)

(defun code-cells-eval-and-forward ()
  "Move cursor to the next cell boundary."
  (interactive)
  (call-interactively 'code-cells-eval)
  (code-cells-forward-cell 1))

(defun code-cells-insert-new ()
  "Insert a new cell boundary."
  (interactive)
  (insert "# %%\n"))


;;------------------------------------------------------------------------------
;; Keybindings.

(defun jupyter-define-keybindings ()
  "Create key bindings and abbrevs for emacs-jupyter."
  (define-key* python-mode-map [(control c)(@)] 'jupyter-connect-repl-latest)
  (define-key* python-mode-map [(control c)(\!)] 'jupyter-repl-restart-kernel)
  (define-key* python-mode-map [(control c)(control p)] 'code-cells-backward-cell)
  (define-key* python-mode-map [(control c)(control n)] 'code-cells-forward-cell)
  (define-key* python-mode-map [(control meta m)] 'code-cells-eval)
  (define-key* python-mode-map [(shift meta m)] 'code-cells-eval-and-forward)
  (define-key* python-mode-map [(control c)(n)] 'code-cells-insert-new)
  (define-key* python-mode-map [(control c)(n)] 'code-cells-insert-new)
  (define-key* python-mode-map [(control c)(\&)] 'jupyter-render-and-view-dataframe)
  (define-key* python-mode-map [(control c)(h)] 'jupyter-print-help)
  (define-key* python-mode-map [(control c)(p)] 'jupyter-print-dataframe)
  (define-key* python-mode-map [(control c)(y)] 'jupyter-print-shape)
  (define-key* python-mode-map [(control c)(r)] 'jupyter-reload-thing-at-point)
  (define-key* python-mode-map [(control c)(e)] 'jupyter-eval-thing-at-point)
  (define-key* python-mode-map [(control c)(?0)] 'jupyter-window-configuration)

  ;; Define an abbrev for cells.
  (define-abbrev python-mode-abbrev-table "ce" "# %%")
  )


(provide 'jupyter-setup)
