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
    ;; write a function to do the spacing
    (defun simple-mode-line-render (left right)
    "Return a string of `window-width' length containing LEFT, and RIGHT
    aligned respectively."
    (let* ((available-width (- (window-width) (length left) 2)))
        (format (format " %%s %%%ds " available-width) left right)))

    (defun vc-branch () ;; Cut out the Git from vc-mode
        (let ((backend (vc-backend (buffer-file-name))))
          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))
        )

  (defun flycheck-indicator () ;; Results in errors|warnings
    (if (equal (string-match "\\(FlyC:\\)\\([0-9]+\\)|\\([0-9]+\\)" (flycheck-mode-line-status-text)) nil)
		    (propertize "✔" 'face '(:inherit success))
      (concat
         (propertize (format "%s" (match-string 2 (flycheck-mode-line-status-text))) 'face '(:inherit error))
         (propertize (format "%s" "|") 'face '(:inherit font-lock-modeline-face))
         (propertize (format "%s" (match-string 3 (flycheck-mode-line-status-text))) 'face '(:inherit flycheck-error-list-warning))
      ))
    )

;;; Tags Configuration
    ;; Function to build ctags tags file
    (when (executable-find "ctags")
      (defun create-tags-ctags (dir-name)
          "Create tags file."
          (interactive "DDirectory: ")
          (shell-command
          (format "ctags -f TAGS -e -R %s" (directory-file-name dir-name)))
          )
    )
    ;; Function to built etags tags file
    (defun create-tags-etags (dir-name)
        "Create tags file."
        (interactive "DDirectory: ")
        (eshell-command 
         (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

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
           (current-buffer)))

  (defun impatient-markdown-preview () ;; Preview current markdown buffer with impatient mode
    "Preview Markdown in realtime with impatient mode."
    (interactive)
    (unless (process-status "httpd")
        (httpd-start))
    (impatient-mode)
    (imp-set-user-filter 'markdown-html-filter)
    (imp-visit-buffer))

  (defun impatient-html-preview () ;; Preview current HTML buffer with impatient mode
    "Preview HTML files in realtime with impatient mode."
    (interactive)
    (unless (process-status "httpd")
        (httpd-start))
    (impatient-mode)
    (imp-visit-buffer))

;;; Eshell
  (defun eshell-prompt ()
      (concat
        (propertize (concat (if (string= (eshell/pwd) (getenv "HOME")) "~" (eshell/basename (eshell/pwd))) " λ "))
        (propertize (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) ""))
  ))


