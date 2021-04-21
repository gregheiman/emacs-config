(require 'package) ;; Add MELPA package repo
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
             ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents ;; Auto download package archive repository manifest if not present
  (package-refresh-contents))

(unless (package-installed-p 'use-package) ;; Download use-package if not present
  (package-install 'use-package))

(eval-when-compile ;; Use use-package to manage packages
  (require 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Always download packages that are marked under use-package if they aren't installed

(use-package evil ;; Vim keybindings
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map "L" 'evil-window-right)
  (define-key evil-normal-state-map "K" 'evil-window-up)
  (define-key evil-normal-state-map "J" 'evil-window-down)
  (define-key evil-normal-state-map "H" 'evil-window-left))
(use-package evil-collection ;; Extend default evil mode keybindings to more modes
  :after evil
  :custom (evil-collection-company-use-tng nil)
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))
(use-package gruvbox-theme ;; Color theme
  :init
  (load-theme 'gruvbox t))
(use-package company ;; Text auto completion framework
  :bind (("TAB" . company-indent-or-complete-common)
         :map company-active-map
         ("<tab>" . company-select-next-or-abort)
         ("TAB" . company-select-next-or-abort)
         ("S-TAB" . compnay-select-previous-or-abort)
         ("<backtab>" . company-select-previous-or-abort))
  :hook (after-init . global-company-mode))
(use-package eglot ;; Language server mode
  :hook ((c++-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (java-mode . eglot-ensure)))
(use-package ivy ;; Auto completion for everything else
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :hook (after-init . ivy-mode))
(use-package counsel ;; Extend ivy completion to more Emacs functions
  :hook (after-init . counsel-mode))
(use-package which-key ;; Show possible keybindings when you pause a keycord
  :hook (after-init . which-key-mode))
(use-package flycheck ;; Improved linting and checking
  :config
  (setq flycheck-display-error-function #'flycheck-display-error-messages) ;; Show error messages in echo area
  :hook (prog-mode . flycheck-mode))
(use-package doom-modeline ;; Improved modeline
  :init
  (setq doom-modeline-height 23)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-percent-position nil)
  (setq all-the-icons-scale-factor 1.1)
  (setq doom-modeline-major-mode-icon nil)
  (set-face-attribute 'mode-line nil :family "Iosevka" :height 100)
  (set-face-attribute 'mode-line-inactive nil :family "Iosevka" :height 100)
  :config
  (doom-modeline-mode 1))
(use-package projectile ;; Project management
  :init
  (when (file-directory-p "~/Documents/Code") ;; Projectile will search this path for projects
    (setq projectile-project-search-path '("~/Documents/Code")))
  (setq projectile-switch-project-action #'projectile-dired) ;; Auto open dired when opening project
  :config
  (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))
  (use-package magit) ;; Git managment within Emacs
(use-package dashboard ;; Improved start screen
  :init
  (setq dashboard-items '((recents  . 5)(projects . 5)(bookmarks . 5)))
  :config
  (dashboard-setup-startup-hook))

;; Emacs Options
(set-face-attribute 'default nil :font "Iosevka-12" ) ;; Set font options
(set-frame-font "Iosevka-12" nil t)
(tool-bar-mode 0) ;; Hide the tool bar
(scroll-bar-mode 0) ;; Hide the scroll bar
(recentf-mode 1) ;; Keep a list of recently opened files
(global-hl-line-mode) ;; Highlight the current line
(delete-selection-mode t) ;; Whatever is highlighted will be replaced iwth whatever is typed or pasted
(global-display-line-numbers-mode 1) ;; Line numbers
(electric-pair-mode 1) ;; Auto pair delimeters
(show-paren-mode t) ;; Highlight matching delimeter pair
(setq show-paren-style 'parenthesis)
(setq indent-tabs-mode nil) ;; Use spaces for tabs instead of tab characters
(setq tab-width 4) ;; Set the tab width to 4 characters
(setq electric-indent-inhibit t) ;; Make return key indent to current indent level
(setq backward-delete-char-untabify-method 'hungry) ;; Have Emacs backspace the entire tab at a time
(setq c-basic-offset 4) ;; Set the width of tab in C/C++/Obj. C

;; Emacs Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (eglot projectile dashboard doom-modeline counsel which-key use-package magit ivy gruvbox-theme flycheck evil-collection company async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
