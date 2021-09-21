;;; Get the original file-name-handler-alist for resetting the file-name-handler-alist
  (defvar file-name-handler-alist-original file-name-handler-alist)

;; Performace improvments to increase start time
  (setq gc-cons-threshold (* 1024 1024 1024)) ;; 1 GB
  (setq gc-cons-percentage 0.8)
  (setq-default file-name-handler-alist nil)
  (setq-default inhibit-compacting-font-caches)

;;; Set gui options before the gui even loads to save time
  (add-hook 'emacs-startup-hook 'toggle-frame-maximized) ;; Start Emacs maximized
  (tool-bar-mode 0) ;; Hide the tool bar
  (scroll-bar-mode 0) ;; Hide the scroll bar
  (menu-bar-mode 0) ;; Hide the menu bar
  (setq inhibit-startup-screen t) ;; Hide startup screen
 

;;; Reset all changes to gc and file-name-handler-alist after startup   
  (run-with-idle-timer
   5 nil
   (lambda ()
     (setq file-name-handler-alist file-name-handler-alist-original)
     (setq gc-cons-threshold 2000000) ;; 2 MB
     (setq gc-cons-percentage 0.5)
     (message "gc-cons-threshold and file-name-handler-alist restored")))      
