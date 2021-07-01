(require 'org) 
(org-babel-load-file ;; Load up my init.org file which contains my Emacs configuration
 (expand-file-name "init.org"
                   user-emacs-directory))

;;; Emacs Custom Set Variables (Don't Touch Emacs Will Handle Them)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" default))
 '(fci-rule-color "#3E4451")
 '(package-selected-packages
   '(evil-commentary evil-surround magit with-editor which-key use-package undo-tree transient projectile lsp-java gruvbox-theme flycheck evil-collection eglot doom-modeline dashboard counsel company atom-one-dark-theme atom-dark-theme))
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]]))
    
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
