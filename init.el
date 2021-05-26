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
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>bn") 'next-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bp") 'previous-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bl") 'list-buffers)
  (evil-define-key 'normal 'global (kbd "<leader>bg") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "L") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "K") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "J") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "H") 'evil-window-left))
(use-package evil-collection ;; Extend default evil mode keybindings to more modes
  :after evil
  :custom (evil-collection-company-use-tng nil) ;; Don't autocomplete like vim
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
  :hook (c++-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (java-mode . eglot-ensure)
	 (python-mode . eglot-ensure)
	 (tex-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) . ("clangd")))
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1))) ;; turn off flymake mode
  (defconst my-eglot-eclipse-jdt-home ;; Set up Eglot to work with eclipse.jdt.ls
     "/home/greg/Programs/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_1.6.100.v20201223-0822.jar"
     "Point to eclipse jdt jar.")
   (defun my-eglot-eclipse-jdt-contact (interactive)
     "Contact with the jdt server input INTERACTIVE."
     (let ((cp (getenv "CLASSPATH")))
       (setenv "CLASSPATH" (concat cp ":" my-eglot-eclipse-jdt-home))
       (unwind-protect (eglot--eclipse-jdt-contact nil)
	 (setenv "CLASSPATH" cp))))
   (setcdr (assq 'java-mode eglot-server-programs) #'my-eglot-eclipse-jdt-contact)) 
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
  :hook (prog-mode . global-flycheck-mode))
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
(delete-selection-mode t) ;; Whatever is highlighted will be replaced with whatever is typed or pasted
(global-display-line-numbers-mode 1) ;; Line numbers
(electric-pair-mode 1) ;; Auto pair delimeters
(show-paren-mode t) ;; Highlight matching delimeter pair
(auto-save-visited-mode) ;; Auto save files without the #filename#
(defun full-auto-save () ;; Auto save all buffers when autosave fires
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)
(setq path-to-ctags "/usr/bin/ctags-universal")
(defun create-tags (dir-name) ;; Run m-x create-tags to create tags
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
)
(setq show-paren-style 'parenthesis)
(setq indent-tabs-mode nil) ;; Use spaces for tabs instead of tab characters
(setq tab-width 4) ;; Set the tab width to 4 characters
(setq electric-indent-inhibit t) ;; Make return key indent to current indent level
(setq backward-delete-char-untabify-method 'hungry) ;; Have Emacs backspace the entire tab at a time
(setq c-default-style "k&r"
      c-basic-offset 4) ;; Set the width of tab in C/C++/Obj. C

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
