(setq gc-cons-threshold 8000000) ;; Set GC threshold to 8 MB

;; Set gui options before the gui even loads to save time
(add-hook 'emacs-startup-hook 'toggle-frame-maximized) ;; Start Emacs maximized
(tool-bar-mode 0) ;; Hide the tool bar
(scroll-bar-mode 0) ;; Hide the scroll bar
(menu-bar-mode 0) ;; Hide the menu bar
(setq inhibit-startup-screen t) ;; Hide startup screen

