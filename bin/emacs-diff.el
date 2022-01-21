;; -*- mode: lisp-interaction; fill-column: 80 -*-
;; emacs-diff and emacs-merge setup in elisp.
;;
;; Quick help commands:
;; https://www.gnu.org/software/emacs/manual/html_node/ediff/Quick-Help-Commands.html
;;
;; Notes:
;; - If you exit without saving, the local file is kept as is.
;;
;; A = mine/local
;; B = yours/other
;; C = output
;; Ancestor = base/ancestor
;;
;; Copyright (C) 2016-2022 Martin Blais. All Rights Reserved.
;; Martin Blais <blais@furius.ca>
;;
;; FIXME: TODO
;; - I want a function to copy the ancestor to C, bound to 'r'.
;; - Can it render diffs in the non-current region?
;; - I'd like a full three-way horizontal window split, with the result at the botom.
;; - Render the output colors slightly differently (more red?).
;; - Is there a mode where the left/right spaces will be equal?
;; - Set it up to work with vc-diff
;; - Build a recursive mode for diffing hierarchies of files... ediff-directories is *awful*
;; - Configure ediff-combination-pattern so that if I save I can use xxdiff -U

(require 'ediff)

;; Org-mode buffers must not be folded, it makes them impossible to diff.
(setq org-startup-folded nil)

;; Make default side-by-side.
(setq ediff-split-window-function 'split-window-horizontally)

;; Don't ask to follow links on symbolic link to Git-controlled source files.
(setq vc-follow-symlinks nil)

;; Ignore whitespace.
;; TODO(blais): Make this optional.
;; (setq ediff-diff-options "-w")

;; Disallow edits (Note: this should be a cmdline option; it does not allow merging).
;(setq ediff-make-buffers-readonly-at-startup t)

;; Prevent a separate control frame.
(if t
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; TODO(blais): Finish implementing this setup for four-way fully horizontal diff.
  ;; Setup three horizontal windows.
  (progn
    (defun ediff-setup-windows-plain-merge (buf-A buf-B buf-C control-buffer)
      ;; skip dedicated and unsplittable frames
      (ediff-destroy-control-frame control-buffer)
      (let ((window-min-height 1)
            merge-window-share merge-window-lines
            wind-A wind-B wind-C)

        ;; Create control window.
        (ediff-with-current-buffer control-buffer
          (setq merge-window-share ediff-merge-window-share))
        (delete-other-windows)
        (set-window-dedicated-p (selected-window) nil)
        (split-window-vertically)
        (ediff-select-lowest-window)
        (ediff-setup-control-buffer control-buffer)

        ;; Go to the upper window and split it betw A, B, and C.
        (other-window 1)
        (setq merge-window-lines
              (max 2 (round (* (window-height) merge-window-share))))
        (switch-to-buffer buf-A)
        (setq wind-A (selected-window))

        ;; XEmacs used to have a lot of trouble with display
        ;; It did't set things right unless we tell it to sit still
        ;; 19.12 seems ok.
        ;;(if (featurep 'xemacs) (sit-for 0))

        ;;(split-window-vertically (max 2 (- (window-height) merge-window-lines)))
        (split-window-horizontally)
        (if (eq (selected-window) wind-A)
            (other-window 1))
        (setq wind-C (selected-window))
        (switch-to-buffer buf-C)

        (select-window wind-A)
        (split-window-horizontally)
        (balance-windows)

        (if (eq (selected-window) wind-A)
            (other-window 1))
        (switch-to-buffer buf-B)
        (setq wind-B (selected-window))

        (ediff-with-current-buffer control-buffer
          (setq ediff-window-A wind-A
                ediff-window-B wind-B
                ediff-window-C wind-C))

        (ediff-select-lowest-window)
        (ediff-setup-control-buffer control-buffer)
        ))
    ))


;; Position at first difference on startup.
(add-hook 'ediff-startup-hook 'ediff-next-difference)

;; Auto-cleanup historical buffers when exiting ediff.
(defun ediff-revision-janitor ()
  "Automatically kill buffers which have a revision number to them."
  (dolist (c (list (cons ediff-buffer-A 'A)
                   (cons ediff-buffer-B 'B)
                   (if ediff-merge-job
                       (cons ediff-ancestor-buffer 'Ancestor)
                     (cons ediff-buffer-C 'C)) ))
    (let ((buf (car c))
          (tag (cdr c)))
      (when (and buf
                 (buffer-file-name buf)
                 (string-match ".*\\.~[0-9]+~$" (buffer-file-name buf)))
        (ediff-dispose-of-variant-according-to-user buf tag nil nil)))
    ))
(add-hook 'ediff-cleanup-hook 'ediff-revision-janitor)

(defun ediff-save-window-configuration ()
  "Save the window configuration in a register, and upon exit, restore it."
  (window-configuration-to-register ?@))

(defadvice ediff-buffers (before save-window-configuration activate)
  (ediff-save-window-configuration))

(defadvice ediff-files (before save-window-configuration activate)
  (ediff-save-window-configuration))

(defadvice ediff-merge (before save-window-configuration activate)
  (ediff-save-window-configuration))

;;------------------------------------------------------------------------------
;; Disable confirmations.

;; TODO(blais): Build a macro to turn all of those into one-liners.

(defadvice save-buffers-kill-emacs (around no-y-or-n activate)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))

(defadvice ediff-copy-A-to-C (around auto-confirm compile activate)
  "Don't require confirmation when copying."
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))
(defadvice ediff-copy-B-to-C (around auto-confirm compile activate)
  "Don't require confirmation when copying."
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))

(defadvice ediff-combine-diffs (around auto-confirm compile activate)
  "Don't require confirmation when copying."
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))

;;------------------------------------------------------------------------------
;; Fixup the colors so that they are reasonable.

(setq font-lock-global-modes nil)
;(setq ediff-use-faces t)
;(setq ediff-highlight-all-diffs t)

(dolist (font '(default))
  (set-face-attribute font nil :background "#bebebe")
  (set-face-attribute font nil :foreground "#000000"))

(custom-set-faces

 ;; Default / background, a neutral light gray.
 '(default                     ((((class color)) (:background "#bebebe" :foreground "#000000"))))

 ;; Current diff hunks highlighted.
 '(ediff-current-diff-A        ((((class color)) (:background "#cdc9a5" :foreground "#000000"))))
 '(ediff-current-diff-B        ((((class color)) (:background "#cdc9a5" :foreground "#000000"))))
 '(ediff-current-diff-Ancestor ((((class color)) (:background "#cdc9a5" :foreground "#000000"))))
 '(ediff-current-diff-C        ((((class color)) (:background "#eedda5" :foreground "#000000" :weight bold))))

 ;; Per-character highlight.
 '(ediff-fine-diff-A           ((((class color)) (:background "#eeeecc" :foreground "#000000"))))
 '(ediff-fine-diff-B           ((((class color)) (:background "#eeeecc" :foreground "#000000"))))
 '(ediff-fine-diff-Ancestor    ((((class color)) (:background "#eeeecc" :foreground "#000000"))))
 '(ediff-fine-diff-C           ((((class color)) (:background "#ffffee" :foreground "#000000"))))

 ;; Ediff supports different colors for alternating hunks; this is wholly
 ;; unnecessary beacuse there's always *some* default space between hunks, by
 ;; definition. Non-current diff hunks are makde distinct, but kept back/darker
 ;; a bit.
 '(ediff-even-diff-A           ((((class color)) (:background "#aba783" :foreground "#000000"))))
 '(ediff-even-diff-B           ((((class color)) (:background "#aba783" :foreground "#000000"))))
 '(ediff-even-diff-C           ((((class color)) (:background "#aba783" :foreground "#000000"))))
 '(ediff-even-diff-Ancestor    ((((class color)) (:background "#aba783" :foreground "#000000"))))
 '(ediff-odd-diff-A            ((((class color)) (:background "#aba783" :foreground "#000000"))))
 '(ediff-odd-diff-B            ((((class color)) (:background "#aba783" :foreground "#000000"))))
 '(ediff-odd-diff-C            ((((class color)) (:background "#aba783" :foreground "#000000"))))
 '(ediff-odd-diff-Ancestor     ((((class color)) (:background "#aba783" :foreground "#000000"))))

 )


;;------------------------------------------------------------------------------
;; Add the comparison function from kill-ring.

(defun get-last-few-selections ()
  "Return a list of the last two selections.
If the current selection is active, this returns the current
selection and the last killed one. With a prefix, returns the
last three."
  (let* ((idx (if mark-active -1 0))
         (selections (list (current-kill (+ idx 1) t)
                           (if (= idx 0)
                               (current-kill idx t)
                             (buffer-substring (region-beginning) (region-end))))))
    (if current-prefix-arg
        (cons (current-kill (+ idx 2) t) selections)
      selections)))

(defun ediff-compare-kill-ring ()
  "A version of ediff-buffers that compares the last two kills in
the kill-ring."
  (interactive)

  (let* ((selections (get-last-few-selections))
         (nselections (length selections))
         (buffer-A (get-buffer-create "*ediff-buffer-A*"))
         (buffer-B (get-buffer-create "*ediff-buffer-B*"))
         (buffer-C (when (> nselections 2) (get-buffer-create "*ediff-buffer-C*")))
         (buffers (filter 'identity (list buffer-A buffer-B buffer-C))))

    (with-current-buffer buffer-A
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (insert (car selections)) )

    (with-current-buffer buffer-B
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (insert (cadr selections)) )

    (when (> nselections 2)
      (with-current-buffer buffer-C
        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max))
        (insert (caddr selections))))

    ;; Do it, here, now.
    (apply 'ediff-buffers buffers)

    ;; Register to clean up the temp buffers automatically.
    ;; (make-variable-buffer-local 'ediff-quit-hook)
    ;; (add-hook 'ediff-quit-hook (lambda () (mapcar 'kill-buffer buffers)))
    ))

;;------------------------------------------------------------------------------
;; Setup to work with vc-dir
;; TODO(blais): Remove this? Figure this out?
;; (Setting up ediff to work with vc-dir is a huge PIA.
;; Why isn't this simple? Pissed off, not enough time to set it up.)

;; (require 'vc-ediff)
;;
;; (defun ediff-current-buffer-revision ()
;;   "Run Ediff to diff current buffer's file against VC depot.
;; Uses `vc.el' or `rcs.el' depending on `ediff-version-control-package'."
;;   (interactive)
;;   (let ((file (or (buffer-file-name)
;; 		  (error "Current buffer is not visiting a file"))))
;;     (if (and (buffer-modified-p)
;; 	     (y-or-n-p (message "Buffer %s is modified. Save buffer? "
;; 				(buffer-name))))
;; 	(save-buffer (current-buffer)))
;;     (ediff-load-version-control)
;;     (funcall
;;      (intern (format "ediff-%S-internal" ediff-version-control-package))
;;      "" "" nil)))

;;  (defun user-ediff-vc-diff (historic &optional not-urgent)
;;    "A binding for invoking ediff directly from vc-dir-mode."
;;    (interactive (list current-prefix-arg t))
;;    (if historic
;;        (call-interactively 'vc-version-diff)
;;      (when buffer-file-name (vc-buffer-sync not-urgent))
;;      (let* ((vc-fileset (vc-deduce-fileset t))
;;  	   (file (caadr vc-fileset)))
;;
;;      (ediff-load-version-control)
;;
;;      (message (prin1-to-string file))
;;
;;      (let ((file (format "%s/conf/etc/emacsrc" user-projects-root)))
;;        (funcall
;;         (intern (format "ediff-%S-internal" ediff-version-control-package))
;;         "" "" file))
;;      ""
;;      )))
;;

;; (define-key* vc-dir-mode-map [(meta ?=)] 'vc-ediff)


(defun ediff-initialize-standalone ()
  "Minimal initialization required when starting in standalone mode."

  (menu-bar-mode 0)
  (when (fboundp 'tool-bar-mode)
    (unless (eq system-type 'darwin)
      (tool-bar-mode nil)))
  (setq make-backup-files nil)

  (require 'ido)
  (custom-set-variables
   '(frame-background-mode (quote dark)))

  ;; Note: We cannot use flet since 24.3, so resort to this dirty trick...
  (defadvice y-or-n-p (around ediff-easy-exit activate)
    (if (eq (string-match "Quit.*Ediff" (ad-get-arg 0)) nil) ad-do-it t))

  (defadvice ediff-quit (after really-exit-emacs-after-diff activate)
    "Make quitting ediff quit Emacs."
    (kill-emacs)))

(defun ediff-guess-from-filenames (left right)
  "Guess whether to invoke ediff on directories or files."
  (if (file-directory-p left)
      (ediff-directories left right nil)
    (ediff-files left right)))


(ediff-initialize-standalone)
;;(ediff-guess-from-filenames "$MINE" "$YOURS")
