;; Functions, Advice, and Custom Minor Modes
;; Uses Outline-Minor-Mode
;; C-c @ C-c Hide entry
;; C-c @ C-e Show entry

;;; Emacs functions
  (defun efs/display-startup-time ()
    "Log startup time and garbage collections to echo area"
    (message "Emacs loaded in %s with %d garbage collections."
      (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
      gcs-done)
  )

  (defun full-auto-save ()
    "Auto save all buffers when autosave fires"
    (interactive)
    (save-excursion
        (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (buffer-file-name) (buffer-modified-p))
            (basic-save-buffer))))
  )

;;; Custom Theme Faces
  (defun gh/custom-theme-faces ()
    "Set custom faces for the current theme"
    (custom-set-faces '(font-lock-variable-name-face ((t (:foreground nil :inherit default)))))
    (font-lock-add-keywords nil
    `(("[-+]?\\b[0-9]*\\.?[0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\b" . font-lock-warning-face) ;; Numbers
      ("\\(?:\\.\\|->\\)\\_<\\([_a-zA-Z]*[a-zA-Z0-9_]+\\)\\([\t]*\\)(" 1 font-lock-function-name-face))) ;; Member functions
  )

;;; C Mode Configuration
 (defun gh/c-mode-configuration ()
    "Set C style configuration"
    (setq c-basic-offset 4) ;; Set 4 space tabs
    (c-set-offset 'substatement-open 0)
    (setq c-set-style "k&r") ;; The God style
  )

;;; Java Mode Configuration
  (defun gh/java-mode-configuration ()
    "Set Java style configuration."
    (c-set-offset 'case-label '+) ;; Properly indent case statments
  )

;;; Elisp Mode Configuration
  (defun gh/elisp-mode-configuration ()
    "Set Elisp style configuration"
    (setq-local tab-width 2) ;; 2 space tabs
  )

;;; Modeline
  (defun simple-mode-line-render (left right) ;; Allows items to be right aligned on modeline
    "Return a string of `window-width' length containing LEFT, and RIGHT
    aligned respectively."
    (let* ((available-width (- (window-width) (length left) 2)))
      (format (format " %%s %%%ds " available-width) left right))
  )

  (defun vc-branch () ;; Cut out the Git from vc-mode
    "Retrieve just the git branch name for the current file"
    (let ((backend (vc-backend (buffer-file-name))))
        (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))
  )

  (defun flycheck-indicator () ;; Results in errors|warnings
    "Display just the errors and warning from the Flycheck indicator"
    (if (equal (string-match "\\(FlyC:\\)\\([0-9]+\\)|\\([0-9]+\\)" (flycheck-mode-line-status-text)) nil)
		    (propertize "✔" 'face '(:inherit success))
      (concat
         (propertize (format "%s" (match-string 2 (flycheck-mode-line-status-text))) 'face '(:inherit error))
         (propertize (format "%s" "|") 'face '(:inherit mode-line))
         (propertize (format "%s" (match-string 3 (flycheck-mode-line-status-text))) 'face '(:inherit flycheck-error-list-warning))
      ))
  )

  ;; Add the Git diff summary to the end of vc-mode output
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
		"]"))
  )

;;; Tags
  (when (executable-find "ctags")
    (defun create-tags-ctags (dir-name)
        "Create tags file using Ctags."
        (interactive "DDirectory: ")
        (shell-command
         (format "ctags -a -e -R --kinds-all=* %s" (directory-file-name dir-name))
         ))
  )

  (defun create-tags-etags (dir-name)
    "Create tags file usig Etags."
    (interactive "DDirectory: ")
    (eshell-command
        (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name))
  )

  ;;;  Jonas.Jarnestrom<at>ki.ericsson.se A smarter
  ;;;  find-tag that automagically reruns etags when it cant find a
  ;;;  requested item and then makes a new try to locate it.
  ;;;  Fri Mar 15 09:52:14 2002
  (defadvice find-tag (around refresh-etags activate)
    "Rerun etags and reload tags if tag not found and redo find-tag.
    If buffer is modified, ask about save before running etags."
    (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
    ad-do-it
        (error (and (buffer-modified-p)
            (not (ding))
            (y-or-n-p "Buffer is modified, save it? ")
            (save-buffer))
            (er-refresh-etags extension)
            ad-do-it))))
    (defun er-refresh-etags (&optional extension)
    "Run etags on all peer files in current dir and reload them silently."
    (interactive)
    (shell-command (format "etags *.%s" (or extension "el")))
    (let ((tags-revert-without-query t))  ; don't query, revert silently
        (visit-tags-table default-directory nil))
  )

;;; Eshell
 (defun git-prompt-branch-name ()
    "Get current git branch name"
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position)))))
  )

  (defun eshell-prompt ()
      (concat
        (propertize (or (ignore-errors (format "(%s) " (git-prompt-branch-name))) ""))
        (propertize (concat (if (string= (eshell/pwd) (getenv "HOME")) "~" (eshell/basename (eshell/pwd))) " λ "))
        )
  )

;;; Terminal and ANSI-Term
  (defun gh/open-ansi-term-in-split ()
    "Open up an ansi-term window in a new horizontal split"
    (interactive)
    (split-window-below)
    (other-window 1)
    (shrink-window 10)
    (ansi-term "/bin/bash")
  )

;;; Org Mode
  (defun gh/org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    "Modifies org-export to place exported files in a different directory"
    (unless pub-dir
      (setq pub-dir (expand-file-name "~/Org/Org-Exported-Files"))
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil)
  )
  (advice-add 'org-export-output-file-name :around #'gh/org-export-output-file-name-modified)

  (defvar org-electric-pairs '((?$ . ?$)) "Electric pairs for org-mode.")
  (defun gh/org-add-electric-pairs ()
    "Add pairs to electric pair mode for org mode"
    (interactive)
    (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)
    (setq-local electric-pair-inhibit-predicate ;; Stop electric pair mode from pairing < and >
                   `(lambda (c)
                      (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))
  )

;;; Hydra
    (defhydra hydra-org-roam (:exit t :color pink :hint nil)
        "
        ^Node^             ^Goto^           ^Capture^          ^Misc
        ^^^^^^^^-----------------------------------------------------------------
        _f_: Find          _d_: Date        _c_: Today         _s_: Sync DB
        _i_: Insert        _t_: Today       _u_: Tomorrow      _g_: Graph
        _r_: Random        _y_: Yesterday
                          _T_: Tomorrow
        "
        ("f" org-roam-node-find)
        ("i" org-roam-node-insert)
        ("r" org-roam-node-random)
        ("s" org-roam-db-sync)
        ("d" org-roam-dailies-goto-date)
        ("c" org-roam-dailies-capture-today)
        ("u" org-roam-dailies-capture-tomorrow)
        ("t" org-roam-dailies-goto-today)
        ("y" org-roam-dailies-goto-yesterday)
        ("T" org-roam-dailies-goto-tomorrow)
        ("g" org-roam-graph)
        ("q" nil "quit" :color blue))

   (defhydra hydra-projectile (:color pink :columns 5 :exit t)
    "Projectile"
    ("f"   projectile-find-file                "Find File")
    ("r"   projectile-recentf                  "Recent Files")
    ("z"   projectile-cache-current-file       "Cache Current File")
    ("x"   projectile-remove-known-project     "Remove Known Project")

    ("d"   projectile-find-dir                 "Find Directory")
    ("b"   projectile-switch-to-buffer         "Switch to Buffer")
    ("c"   projectile-invalidate-cache         "Clear Cache")
    ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")

    ("o"   projectile-multi-occur              "Multi Occur")
    ("t"   projectile-regenerate-tags          "Regenerate Tags")
    ("p"   projectile-switch-project           "Switch Project")
    ("k"   projectile-kill-buffers             "Kill Buffers")

    ("P"   projectile-test-project             "Test Project")
    ("K"   projectile-install-project          "Install Project")
    ("L"   projectile-package-project          "Package Project")
    ("C"   projectile-compile-project          "Compile Project")
    ("U"   projectile-run-project              "Run Project")
    ("q"   nil "Quit" :color blue))

  (defhydra hydra-lsp (:exit t :hint nil :color pink)
    "
   Buffer^^               Server^^                   Symbol
  -------------------------------------------------------------------------------------
   [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
   [_x_] format region    [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
   [_a_] code actions     [_M-s_] describe session   [_R_] references   [_s_] signature
   "
    ("d" lsp-find-declaration)
    ("D" lsp-find-definitions)
    ("R" lsp-find-references)
    ("i" lsp-find-implementation)
    ("t" lsp-find-type-definition)
    ("s" lsp-signature-help)
    ("o" lsp-describe-thing-at-point)
    ("r" lsp-rename)

    ("f" lsp-format-buffer)
    ("x" lsp-code-region)
    ("a" lsp-execute-code-action)

    ("M-s" lsp-describe-session)
    ("M-r" lsp-restart-workspace)
    ("S" lsp-shutdown-workspace)
    ("q" nil "quit" :color blue))

   (defhydra hydra-project (:color pink :columns 5 :exit t)
    "Project.el"
    ("f"   project-find-file                   "Find File")
    ("g"   project-find-regexp                 "Project Find Regexp")
    ("r"   project-query-replace-regexp        "Project Query Replace Regexp")
    ("F"   project-or-external-find-file       "Project or External Find File")
    ("G"   project-or-external-find-regexp     "Project or External Find Regexp")

    ("d"   project-find-dir                    "Find Directory")
    ("b"   project-switch-to-buffer            "Switch to Buffer")
    ("v"   project-vc-dir                      "Project VC Dir")
    ("p"   project-switch-project              "Switch Project")
    ("D"   project-dired                       "Project Direc")
    ("k"   project-kill-buffers                "Kill Buffers")

    ("s"   project-shell                       "Project Shell")
    ("!"   project-shell-command               "Project Shell Command")
    ("&"   project-async-shell-command         "Project Async Shell Command")
    ("e"   project-eshell                      "Project Eshell")

    ("c"   project-compile-project             "Compile Project")
    ("x"   project-execute-exdented-command    "Project Execute Extended Command")
    ("q"   nil "Quit" :color blue))

;;; Dired
  (defun gh/dired-preview-file ()
    "Preview file in Dired mode"
    (interactive)
    (dired-find-file-other-window)
    (local-set-key (kbd "q") 'View-quit)
  )
