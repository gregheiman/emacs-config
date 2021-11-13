;; Functions, Advice, and Custom Minor Modes
;; Uses Outline-Minor-Mode
;; C-c @ C-c Hide entry
;; C-c @ C-e Show entry

;;; Emacs functions
  (defun efs/display-startup-time () ;; Log startup time
        (message "Emacs loaded in %s with %d garbage collections."
                (format "%.2f seconds"
                        (float-time
                        (time-subtract after-init-time before-init-time)))
                gcs-done))

  (defun full-auto-save () ;; Auto save all buffers when autosave fires
    (interactive)
    (save-excursion
        (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (buffer-file-name) (buffer-modified-p))
            (basic-save-buffer)))))

;;; DAP Mode
  (define-minor-mode +dap-running-session-mode
    "A mode for adding keybindings to running DAP sessions"
    nil
    nil
    (make-sparse-keymap)
    (evil-normalize-keymaps) ;; if you use evil, this is necessary to update the keymaps
    ;; The following code adds to the dap-terminated-hook
    ;; so that this minor mode will be deactivated when the debugger finishes
    (when +dap-running-session-mode
      (let ((session-at-creation (dap--cur-active-session-or-die)))
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (+dap-running-session-mode -1)))))))

;;; C Mode Configuration
 (defun c-mode-configuration ()
    "Set C style configuration"
    (setq c-basic-offset 4) ;; Set 4 space tabs
    (c-set-offset 'substatement-open 0) 
    (setq c-set-style "k&r") ;; The God style
  )

;;; Java Mode Configuration
  (defun java-mode-configuration ()
    "Set Java style configuration."
    (c-set-offset 'case-label '+) ;; Properly indent case statments
  )

;;; Elisp Mode Configuration
  (defun elisp-mode-configuration ()
    "Set elisp style configuration"
    (setq tab-width 2) ;; 2 space tabs
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
         (propertize (format "%s" "|") 'face '(:inherit font-lock-modeline-face))
         (propertize (format "%s" (match-string 3 (flycheck-mode-line-status-text))) 'face '(:inherit flycheck-error-list-warning))
      ))
  )

  ;; Add the Git diff summary to the end of vc-mode output
  (defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  "Show the information of git diff on modeline."
  (setq ad-return-value
	(concat (propertize ad-return-value 'face '(:inherit font-lock-modeline-face))
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

;;; Impatient Mode
  (defun gh/markdown-html-filter (buffer) ;; Use this filter with impatient mode with M-x imp-set-user-filter RET markdown-html RET
    "Impatient mode filter to show markdown correctly."
    (princ (with-current-buffer buffer
    (format
     "<!DOCTYPE html>
      <html>
        <title>Impatient Markdown Preview</title>
        <xmp theme=\"spacelab\">
          %s
        </xmp>
        <script type=\"text/javascript\" src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script>
      </html>"
     (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer))
  )

  (defun gh/impatient-markdown-preview () ;; Preview current markdown buffer with impatient mode
    "Preview Markdown in realtime with impatient mode."
    (interactive)
    (unless (process-status "httpd")
        (httpd-start))
    (impatient-mode)
    (imp-set-user-filter 'gh/markdown-html-filter)
    (imp-visit-buffer)
  )

  (defun gh/impatient-html-preview () ;; Preview current HTML buffer with impatient mode
    "Preview HTML files in realtime with impatient mode."
    (interactive)
    (unless (process-status "httpd")
        (httpd-start))
    (impatient-mode)
    (imp-visit-buffer)
  )

;;; Eshell
 (defun git-prompt-branch-name ()
    "Get current git branch name"
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position))))))

  (defun eshell-prompt ()
      (concat
        (propertize (or (ignore-errors (format "(%s) " (git-prompt-branch-name))) ""))
        (propertize (concat (if (string= (eshell/pwd) (getenv "HOME")) "~" (eshell/basename (eshell/pwd))) " λ "))
        ))

;;; Terminal and ANSI-Term
  (defun gh/open-ansi-term-in-split ()
    "Open up an ansi-term window in a new horizontal split"
    (interactive)
    (split-window-below)
    (other-window 1)
    (ansi-term "/bin/bash")
  )

;;; Org Mode
  (defun gh/org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    "Modifies org-export to place exported files in a different directory"
    (unless pub-dir
      (setq pub-dir (expand-file-name "~/Org/Org-Exported-Files"))
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil))
  (advice-add 'org-export-output-file-name :around #'gh/org-export-output-file-name-modified)

  (defvar org-electric-pairs '((?$ . ?$)) "Electric pairs for org-mode.")
  (defun gh/org-add-electric-pairs ()
    (interactive)
    (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs)
    (setq-local electric-pair-inhibit-predicate ;; Stop electric pair mode from pairing < and >
                   `(lambda (c)
                      (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))
    )

;;; Mu4e
  (defun my-mu4e-set-account ()
        "Set the account for composing a message.
         This function is taken from: 
           https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
        (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
          (string-match "/\\(.*?\\)/" maildir)
          (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                   (mapconcat #'(lambda (var) (car var))
                  my-mu4e-account-alist "/"))
                 (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                 nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
          (if account-vars
        (mapc #'(lambda (var)
            (set (car var) (cadr var)))
              account-vars)
            (error "No email account found"))))


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
    (interactive)
    (dired-find-file-other-window)
    (local-set-key (kbd "q") 'View-quit))
