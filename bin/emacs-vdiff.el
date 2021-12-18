;; -*- mode: lisp-interaction; fill-column: 80 -*-
;; emacs-vdiff setup.

;; Make 'q' just quit emacs as well as vdiff.



(require 'package)
(package-initialize)

(require 'vdiff)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

(defun run-vdiff-files (f1 f2)
  (vdiff-files f1 f2)
  (vdiff-refine-all-hunks)
  (vdiff-hydra/body)
  )


;; ;; Whether to lock scrolling by default when starting vdiff
;; (setq vdiff-lock-scrolling t)
;;
;; ;; diff program/algorithm to use. Allows choice of diff or git diff along with
;; ;; the various algorithms provided by these commands. See
;; ;; `vdiff-diff-algorithms' for the associated command line arguments.
;; (setq vdiff-diff-algorithm 'diff)
;;
;; ;; diff3 command to use. Specify as a list where the car is the command to use
;; ;; and the remaining elements are the arguments to the command.
;; (setq vdiff-diff3-command '("diff3"))
;;
;; ;; Don't use folding in vdiff buffers if non-nil.
;; (setq vdiff-disable-folding nil)
;;
;; ;; Unchanged lines to leave unfolded around a fold
;; (setq vdiff-fold-padding 6)
;;
;; ;; Minimum number of lines to fold
;; (setq vdiff-min-fold-size 4)
;;
;; ;; If non-nil, allow closing new folds around point after updates.
;; (setq vdiff-may-close-fold-on-point t)
;;
;; ;; Function that returns the string printed for a closed fold. The arguments
;; ;; passed are the number of lines folded, the text on the first line, and the
;; ;; width of the buffer.
;; (setq vdiff-fold-string-function 'vdiff-fold-string-default)
;;
;; ;; Default syntax table class code to use for identifying "words" in
;; ;; `vdiff-refine-this-change'. Some useful options are
;; ;;
;; ;; "w"   (default) words
;; ;; "w_"  symbols (words plus symbol constituents)
;; ;;
;; ;; For more information see
;; ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
;; (setq vdiff-default-refinement-syntax-code "w")
;;
;; If non-nil, automatically refine all hunks.
(setq vdiff-auto-refine t)

;; ;; How to represent subtractions (i.e., deleted lines). The
;; ;; default is full which means add the same number of (fake) lines
;; ;; as those that were removed. The choice single means add only one
;; ;; fake line. The choice fringe means don't add lines but do
;; ;; indicate the subtraction location in the fringe.
;; (setq vdiff-subtraction-style 'full)
;;
;; Character to use for filling subtraction lines. See also
;; `vdiff-subtraction-style'.
(setq vdiff-subtraction-fill-char ?x)


(custom-set-faces


 ;; Default / background, a neutral light gray.
 '(default ((((class color)) (:background "#bebebe" :foreground "#000000"))))

 ;;   "Face for additions")
 ;;'(vdiff-addition-face ((((class color)) (:background "#d7ffd7" :foreground "#000000"))))

 ;;   "Face for changes")
 '(vdiff-change-face ((((class color)) (:background "#cdc9a5" :foreground "#000000"))))

 ;;   "Face for word changes within a change hunk")
 '(vdiff-refine-changed ((((class color)) (:background "#eeffbb" :foreground "#000000"))))

 ;;   "Face for closed folds")
 ;; '(vdiff-closed-fold-face ((((class color)) (:background "#ff0000" :foreground "#000000"))))

 ;;   "Face for open folds")
 ;; '(vdiff-open-fold-face ((((class color)) (:background "#ff0000" :foreground "#000000"))))

 ;;   "Face for subtractions")
 '(vdiff-subtraction-face ((((class color)) (:background "#a3a3a3" :foreground "#888888"))))

 ;;   "Face for subtraction fringe indicators")
;;; '(vdiff-subtraction-fringe-face ((((class color)) (:background "#ff0000" :foreground "#000000"))))

 ;;   "Face for word changes within an addition hunk")
; '(vdiff-refine-added ((((class color)) (:background "#ff0000" :foreground "#000000"))))

 ;;   "Face for selecting hunk targets.")
 ;; '(vdiff-target-face ((((class color)) (:background "#ff0000" :foreground "#000000"))))

 )


;; StringToken colorList[] = {

;;    { "Same", COLOR_SAME,
;;      " Identical text " },
;;    { "SameBlank", COLOR_SAME_BLANK,
;;      " Identical text (blank side, for filler lines when ignore-blank-lines "
;;      "is enabled) " },

;;    { "Insert", COLOR_INSERT,
;;      " Insert text (side with text) " },
;;    { "InsertBlank", COLOR_INSERT_BLANK,
;;      " Insert text (blank side) " },

;;    { "DiffAll", COLOR_DIFF_ALL,
;;      " Different in all files " },
;;    { "DiffAllSup", COLOR_DIFF_ALL_SUP,
;;      " Different in all files (shadowed) " },
;;    { "DiffAllOnly", COLOR_DIFF_ALL_ONLY,
;;      " Different in all files (only text on lines) " },
;;    { "DiffAllNonly", COLOR_DIFF_ALL_NONLY,
;;      " Different in all files (blank side) " },

;;    { "DiffOne", COLOR_DIFF_ONE,
;;      " (diff3 only) Different in one file " },
;;    { "DiffOneSup", COLOR_DIFF_ONE_SUP,
;;      " (diff3 only) Different in one file (shadowed) " },
;;    { "DiffOneOnly", COLOR_DIFF_ONE_ONLY,
;;      " (diff3 only) Different in one file (only text on lines) " },
;;    { "DiffOneNonly", COLOR_DIFF_ONE_NONLY,
;;      " (diff3 only) Different in one file (blank side)" },
;;    { "DiffTwo", COLOR_DIFF_TWO,
;;      " (diff3 only) Common text in two files only " },
;;    { "DiffTwoSup", COLOR_DIFF_TWO_SUP,
;;      " (diff3 only) Common text in two files only (shadowed) " },
;;    { "DiffTwoOnly", COLOR_DIFF_TWO_ONLY,
;;      " (diff3 only) Common text in two files only (only text on lines) " },
;;    { "DiffTwoNonly", COLOR_DIFF_TWO_NONLY,
;;      " (diff3 only) Common text in two files only (blank side) " },

;;    { "Delete", COLOR_DELETE,
;;      " (diff3 only) Delete text (side with text) " },
;;    { "DeleteBlank", COLOR_DELETE_BLANK,
;;      " (diff3 only) Delete text (blank side) " },

;;    { "DiffDel", COLOR_DIFFDEL,
;;      " (diff3 only) Different and delete text " },
;;    { "DiffDelSup", COLOR_DIFFDEL_SUP,
;;      " (diff3 only) Different and delete text (shadowed) " },
;;    { "DiffDelOnly", COLOR_DIFFDEL_ONLY,
;;      " (diff3 only) Different and delete text (only text on lines) " },
;;    { "DiffDelNonly", COLOR_DIFFDEL_NONLY,
;;      " (diff3 only) Different and delete text (blank side) " },
;;    { "DiffDelBlank", COLOR_DIFFDEL_BLANK,
;;      " (diff3 only) Different and delete text (empty side) " },

;;    { "Selected", COLOR_SELECTED,
;;      " Selected text " },
;;    { "SelectedSup", COLOR_SELECTED_SUP,
;;      " Selected text (shadowed) " },

;;    { "IgnoreDisplay", COLOR_IGNORE_DISPLAY,
;;      " Ignored for display purposes " },
;;    { "IgnoreDisplaySup", COLOR_IGNORE_DISPLAY_SUP,
;;      " Ignored for display purposes (shadowed) " },
;;    { "IgnoreDisplayOnly", COLOR_IGNORE_DISPLAY_ONLY,
;;      " Ignored for display purposes (only text on lines) " },
;;    { "IgnoreDisplayNonly", COLOR_IGNORE_DISPLAY_NONLY,
;;      " Ignored for display purposes (blank side) " },

;;    { "Deleted", COLOR_DELETED,
;;      " Deleted text " },
;;    { "DeletedSup", COLOR_DELETED_SUP,
;;      " Deleted text (shadowed) " },
;;    { "Ignored", COLOR_IGNORED,
;;      " Ignore text " },

;;    { "Directories", COLOR_DIRECTORIES,
;;      " (dir.diffs only) Directories in directory diffs " },

;;    { "MergedUndecided", COLOR_MERGED_UNDECIDED,
;;      " Merged view undecided text " },
;;    { "MergedDecided1", COLOR_MERGED_DECIDED_1,
;;      " Merged view decided text, file 1 " },
;;    { "MergedDecided1Sup", COLOR_MERGED_DECIDED_1_SUP,
;;      " Merged view decided text, file 1 (shadowed) " },
;;    { "MergedDecided2", COLOR_MERGED_DECIDED_2,
;;      " Merged view decided text, file 2 " },
;;    { "MergedDecided2Sup", COLOR_MERGED_DECIDED_2_SUP,
;;      " Merged view decided text, file 2 (shadowed) " },
;;    { "MergedDecided3", COLOR_MERGED_DECIDED_3,
;;      " Merged view decided text, file 3 " },
;;    { "MergedDecided3Sup", COLOR_MERGED_DECIDED_3_SUP,
;;      " Merged view decided text, file 3 (shadowed) " },
;;    { "MergedDecidedNeither", COLOR_MERGED_DECIDED_NEITHER,
;;      " Merged view decided text, neither files " },

;;    { "Background", COLOR_BACKGROUND,
;;      " Global background color " },
;;    { "Cursor", COLOR_CURSOR,
;;      " Line cursor color " },
;;    { "VerticalLine", COLOR_VERTICAL_LINE,
;;      " Vertical line color " },
;;    { "TextSelection", COLOR_TEXT_SELECTION,
;;      " Color of text region selection " }
;; };


   ;; if ( qApp != 0 ) { // protect setNamedColor() in case we have no display.
   ;;    setFbColors( COLOR_SAME                   , "#bebebe", "#000000" );
   ;;    setFbColors( COLOR_SAME_BLANK             , "#b3b3b3", "#000000" );
   ;;    setFbColors( COLOR_DIFF_ONE               , "#eee8aa", "#000000" );
   ;;    setFbColors( COLOR_DIFF_ONE_SUP           , "#cdc9a5", "#000000" );
   ;;    setFbColors( COLOR_DIFF_ONE_ONLY          , "#eee8aa", "#000000" );
   ;;    setFbColors( COLOR_DIFF_ONE_NONLY         , "#cdc9a5", "#000000" );
   ;;    setFbColors( COLOR_DIFF_TWO               , "#b2dfee", "#000000" );
   ;;    setFbColors( COLOR_DIFF_TWO_SUP           , "#9ac0cd", "#000000" );
   ;;    setFbColors( COLOR_DIFF_TWO_ONLY          , "#b2dfee",  "#000000" );
   ;;    setFbColors( COLOR_DIFF_TWO_NONLY         , "#9ac0cd", "#000000" );

   ;;    setFbColors( COLOR_DELETE                 , "#b2dfee", "#000000" );
   ;;    setFbColors( COLOR_DELETE_BLANK           , "#a3a3a3", "#000000" );

   ;;    setFbColors( COLOR_INSERT                 , "#b4eeb4", "#000000" );
   ;;    setFbColors( COLOR_INSERT_BLANK           , "#a3a3a3", "#000000" );

   ;;    setFbColors( COLOR_DIFF_ALL               , "#eee8aa", "#000000" );
   ;;    setFbColors( COLOR_DIFF_ALL_SUP           , "#cdc9a5", "#000000" );
   ;;    setFbColors( COLOR_DIFF_ALL_ONLY          , "#eee8aa", "#000000" );
   ;;    setFbColors( COLOR_DIFF_ALL_NONLY         , "#cdc9a5", "#000000" );

   ;;    setFbColors( COLOR_DIFFDEL                , "#eee8aa", "#000000" );
   ;;    setFbColors( COLOR_DIFFDEL_SUP            , "#cdc9a5", "#000000" );
   ;;    setFbColors( COLOR_DIFFDEL_ONLY           , "#eee8aa", "#000000" );
   ;;    setFbColors( COLOR_DIFFDEL_NONLY          , "#cdc9a5", "#000000" );
   ;;    setFbColors( COLOR_DIFFDEL_BLANK          , "#a3a3a3", "#000000" );

   ;;    setFbColors( COLOR_SELECTED               , "#dda0dd", "#000000" );
   ;;    setFbColors( COLOR_SELECTED_SUP           , "#d8bfd8", "#000000" );

   ;;    setFbColors( COLOR_IGNORE_DISPLAY         , "#d9d9d9", "#000000" );
   ;;    setFbColors( COLOR_IGNORE_DISPLAY_SUP     , "#cccccc", "#000000" );
   ;;    setFbColors( COLOR_IGNORE_DISPLAY_ONLY    , "#cccccc", "#000000" );
   ;;    setFbColors( COLOR_IGNORE_DISPLAY_NONLY   , "#cccccc", "#000000" );

   ;;    setFbColors( COLOR_DELETED                , "#778899", "#000000" );
   ;;    setFbColors( COLOR_DELETED_SUP            , "#708090", "#000000" );

   ;;    setFbColors( COLOR_IGNORED                , "#b3b3b3", "#4d4d4d" );

   ;;    setFbColors( COLOR_DIRECTORIES            , "#48d1cc", "#000000" );

   ;;    setFbColors( COLOR_MERGED_UNDECIDED       , "#cdc9a5", "#000000" );
   ;;    setFbColors( COLOR_MERGED_DECIDED_1       , "#999999", "#000000" );
   ;;    setFbColors( COLOR_MERGED_DECIDED_1_SUP   , "#b3b3b3", "#000000" );
   ;;    setFbColors( COLOR_MERGED_DECIDED_2       , "#999999", "#000000" );
   ;;    setFbColors( COLOR_MERGED_DECIDED_2_SUP   , "#b3b3b3", "#000000" );
   ;;    setFbColors( COLOR_MERGED_DECIDED_3       , "#999999", "#000000" );
   ;;    setFbColors( COLOR_MERGED_DECIDED_3_SUP   , "#b3b3b3", "#000000" );
   ;;    setFbColors( COLOR_MERGED_DECIDED_NEITHER , "#b3b3b3", "#000000" );
   ;; }

   ;; _backColors[ COLOR_BACKGROUND ] =      QColor( 0x40, 0x61, 0x6a );
   ;; _backColors[ COLOR_CURSOR ] =          QColor( 0xff, 0xff, 0xff ); // white
   ;; _backColors[ COLOR_VERTICAL_LINE ] =   QColor( 0xff, 0x00, 0x00 ); // red
   ;; _backColors[ COLOR_TEXT_SELECTION ] =  QColor( 0xff, 0x00, 0x00 ); // red
