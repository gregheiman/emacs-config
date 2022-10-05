;;; functions.el --- Auxillary functions for configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Gregory Heiman
;; Author: Greg Heiman <gregheiman02@gmail.com>
;; Created: 21 Apr 2021
;; Keywords: Configuration
;; URL: https://github.com/Ushrio/Emacs-Config
;; This file is not part of GNU Emacs.
;; This file is free software. Distributed under the MIT license.

;;; Commentary:
;; Functions, Advice, and Custom Minor Modes
;; Uses Outline-Minor-Mode
;; C-c @ C-c or zo (Evil mode) - Hide entry
;; C-c @ C-e or zc (Evil mode) - Show entry

;;; Code:
(defun efs/display-startup-time ()
  "Log startup time and garbage collections to echo area"
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))

(defun gh/full-auto-save ()
  "Auto save all buffers when autosave fires"
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))

(defun gh/custom-theme-faces ()
  "Set custom faces for the current theme"
  (custom-set-faces '(font-lock-variable-name-face ((t (:foreground nil :inherit default)))))
  (custom-set-faces '(font-lock-warning-face ((t (:background nil :inherit warning)))))
  (font-lock-add-keywords nil
                          `(("[-+]?\\b[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\b" . font-lock-warning-face) ;; Numbers
                            ("\\(?:\\.\\|->\\)\\_<\\([_a-zA-Z]*[a-zA-Z0-9_]+\\)\\([\t]*\\)(" 1 font-lock-function-name-face) ;; Member functions
                            ("\\_<\\([_a-z]*[a-z0-9_]+\\)\\([\t]*\\)(" 1 font-lock-function-name-face) ;; Functions
                            ("\\_<\\([_A-Z]*[a-z0-9_]+\\)\\([\t]*\\)(" 1 font-lock-type-face) ;; Constructors
                            ("\\(?:\\.\\|->\\)\\~\\_<\\([_a-zA-Z]*[a-zA-Z0-9_]+\\)\\([\t]*\\)(" 1 font-lock-type-face)))) ;; Destructors

(defun gh/c-mode-configuration ()
  "Set C style configuration"
  (setq c-basic-offset 4) ;; Set 4 space tabs
  (c-set-offset 'substatement-open 0)
  (eldoc-add-command 'c-electric-paren))

(defun gh/java-mode-configuration ()
  "Set Java style configuration."
  (setq c-basic-offset 4) ;; Set 4 space tabs
  (c-set-offset 'case-label '+)) ;; Properly indent case statments

(defun gh/elisp-mode-configuration ()
  "Set Elisp style configuration"
  (setq-local tab-width 2)) ;; 2 space tabs

(defun gh/get-java-package-from-dir (dir)
  "Retrieve the java package name using the files directory path."
  (string-match "\\(.*\\)/java/\\(.*\\)/\\(.*\\)" dir)
  (string-replace "/" "." (match-string 2 dir)))

(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  "Show the information of git diff on modeline."
  (setq ad-return-value
	(concat (propertize ad-return-value 'face '(:inherit mode-line))
		" ["
		(let ((plus-minus (vc-git--run-command-string
				   file "diff" "--numstat" "--")))
		  (if (and plus-minus
		           (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
		      (concat
		       (propertize (format "+%s" (match-string 1 plus-minus)) 'face '(:inherit success))
		       (propertize (format " -%s" (match-string 2 plus-minus)) 'face '(:inherit font-lock-warning-face)))
		    (propertize "✔" 'face '(:inherit success))))
		"]")))

(when (executable-find "ctags")
  (defun create-tags-ctags (dir-name)
    "Create tags file using Ctags."
    (interactive "DDirectory: ")
    (shell-command
     (format "ctags -a -e -R %s" (directory-file-name dir-name)))))

(defun create-tags-etags (dir-name)
  "Create tags file usig Etags."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*\" | etags -" dir-name)))

(defun gh/git-prompt-branch-name ()
  "Get current git branch name"
  (let ((args '("symbolic-ref" "HEAD" "--short")))
    (with-temp-buffer
      (apply #'process-file "git" nil (list t nil) nil args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun gh/eshell-prompt ()
  "Set custom eshell prompt"
  (concat
   (propertize (or (ignore-errors (format "(%s) " (gh/git-prompt-branch-name))) ""))
   (propertize (concat (if (string= (eshell/pwd) (getenv "HOME")) "~" (eshell/basename (eshell/pwd))) " λ "))))

(defun gh/open-ansi-term-in-split ()
  "Open up an ansi-term window in a new horizontal split"
  (interactive)
  (split-window-below)
  (other-window 1)
  (shrink-window 10)
  (ansi-term "/bin/bash"))

(defun gh/org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  "Modifies org-export to place exported files in a different directory"
  (unless pub-dir
    (setq pub-dir (expand-file-name "~/org/org-exported-files"))
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))

(advice-add 'org-export-output-file-name :around #'gh/org-export-output-file-name-modified)

(defvar org-electric-pairs '((?$ . ?$)) "Electric pairs for org-mode.")

(defun gh/org-add-electric-pairs ()
  "Add pairs to electric pair mode for org mode"
  (interactive)
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs)
  (setq-local electric-pair-inhibit-predicate ;; Stop electric pair mode from pairing < and >
              `(lambda (c)
                 (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))


(defun gh/set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(provide 'functions)

;;; functions.el ends here
