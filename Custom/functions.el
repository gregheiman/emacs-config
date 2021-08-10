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

;;; C Mode Configuration
 (defun c-mode-configuration ()
    "Set C style configuration"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    (setq c-set-style "k&r")
  )

;;; Java Mode Configuration
  (defun java-mode-configuration ()
    (c-set-offset 'case-label '+)
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

;;; Tags Configuration
  (when (executable-find "ctags")
    (defun create-tags-ctags (dir-name)
        "Create tags file using Ctags."
        (interactive "DDirectory: ")
        (shell-command
        (format "ctags -f TAGS -e -R %s" (directory-file-name dir-name)))
        )
  )

  (defun create-tags-etags (dir-name)
    "Create tags file usig Etags."
    (interactive "DDirectory: ")
    (eshell-command 
        (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name))
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

;;; CLI Mode
  (defun cli-mode ()
    "Set the compile buffer to be editable for interative CLIs."
    (interactive)
    (if (not (member 'cli-mode-start-bait compilation-start-hook))
        (progn
          (add-hook 'compilation-start-hook #'cli-mode-start-bait)
          (add-hook 'compilation-finish-functions #'cli-mode-finish-bait))
      (remove-hook 'compilation-start-hook #'cli-mode-start-bait)
      (remove-hook 'compilation-finish-functions #'cli-mode-finish-bait)))
  
  (defun cli-mode-start-bait (process)
    "This is the function that should be added to `compilation-start-mode-hook' for cli mode."
    (pop-to-buffer (get-buffer "*compilation*"))
    (comint-mode)
    (setq inhibit-read-only t)
    (goto-char (point-max)))
  
  (defun cli-mode-finish-bait (process report)
    (pop-to-buffer (get-buffer "*compilation*"))
    (compilation-mode))
