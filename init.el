(setq vc-follow-symlinks t) ;; Don't prompt to follow symlinks

(require 'org) 
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

