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
  (defun markdown-html-filter (buffer) ;; Use this filter with impatient mode with M-x imp-set-user-filter RET markdown-html RET
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

  (defun impatient-markdown-preview () ;; Preview current markdown buffer with impatient mode
    "Preview Markdown in realtime with impatient mode."
    (interactive)
    (unless (process-status "httpd")
        (httpd-start))
    (impatient-mode)
    (imp-set-user-filter 'markdown-html-filter)
    (imp-visit-buffer)
  )

  (defun impatient-html-preview () ;; Preview current HTML buffer with impatient mode
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

;;; Org Mode
  (defun org-mode-setup ()
    "Startup configuration when using Org mode."
    (org-indent-mode)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (turn-on-flyspell)
    (turn-on-font-lock)
    )

  (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    "Modifies org-export to place exported files in a different directory"
    (unless pub-dir
      (setq pub-dir (expand-file-name "~/Org/Org-Exported-Files"))
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil))
  (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

;;; Minibuffer

;;; Mu4e
(defun gh/my-render-html-message ()
  "Use EWW redering engine to render mail in mu4e"
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

