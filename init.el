(require 'package) ;; Add MELPA package repo
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
             ("elpa" . "https://elpa.gnu.org/packages/")
             ("melpa-stable" . "https://stable.melpa.org/packages/")))
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
  (evil-set-undo-system 'undo-tree)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>bl") 'list-buffers)
  (evil-define-key 'normal 'global (kbd "<leader>bg") 'switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "]q") 'flycheck-next-error)
  (evil-define-key 'normal 'global (kbd "[q") 'flycheck-previous-error)
  (evil-define-key 'normal 'global (kbd "]Q") 'flycheck-last-error)
  (evil-define-key 'normal 'global (kbd "[Q") 'flycheck-first-error)
  (evil-define-key 'normal 'global (kbd "gc") 'comment-dwim)
  (evil-define-key 'normal 'global (kbd "L") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "K") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "J") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "H") 'evil-window-left)
    (eval-after-load 'evil-ex
        '(evil-ex-define-cmd "fin[d]" 'projectile-find-file))
    (eval-after-load 'evil-ex
        '(evil-ex-define-cmd "gre[p]" 'projectile-grep)))
(use-package evil-collection ;; Extend default evil mode keybindings to more modes
  :after evil
  :custom (evil-collection-company-use-tng nil) ;; Don't autocomplete like vim
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))
(use-package undo-tree ;; Undo tree to enable redoing with Evil
  :hook (after-init . global-undo-tree-mode))
(use-package atom-one-dark-theme ;; Color theme
  :init
  (load-theme 'atom-one-dark t))
(use-package company ;; Text auto completion framework
  :bind (("TAB" . company-indent-or-complete-common)
         :map company-active-map
         ("<tab>" . company-select-next-or-abort)
         ("TAB" . company-select-next-or-abort)
         ("S-TAB" . compnay-select-previous-or-abort)
         ("<backtab>" . company-select-previous-or-abort))
  :hook (after-init . global-company-mode))
(use-package lsp-mode ;; Enable LSP support in Emacs
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
  (java-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-headerline-breadcrumb-enable nil) ;; Remove top header line
  (setq lsp-signature-auto-activate nil) ;; Stop signature definitions popping up
  (setq lsp-enable-snippet nil) ;; Disable snippets (Snippets require YASnippet)
  :commands
  lsp lsp-deferred)
(use-package lsp-java) ;; Support for the Eclipse.jdt.ls LSP
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
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; Stop flycheck from treating init.el as package file
  :hook (prog-mode . global-flycheck-mode))
 (use-package doom-modeline ;; Improved modeline
   :init
   (setq doom-modeline-height 23)
   (setq doom-modeline-buffer-file-name-style 'file-name)
   (setq doom-modeline-percent-position nil)
   (setq doom-modeline-major-mode-icon nil)
   (setq all-the-icons-scale-factor 1.0)
   (set-face-attribute 'mode-line nil :family "Iosevka" :height 100)
  (set-face-attribute 'mode-line-inactive nil :family "Iosevka" :height 100)
  :config
 (with-eval-after-load 'evil ;; Define custom evil state icon for modeline
    (doom-modeline-def-segment evil-state-seg
      "Display current Evil State."
      (propertize (format " <%s>" (upcase (substring (symbol-name evil-state) 0 1)))
                  'face '(:weight bold))))
  (doom-modeline-def-modeline 'main
    '(bar evil-state-seg matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker lsp input-method buffer-encoding major-mode process vcs " "))
  :hook (
  (window-setup . doom-modeline-mode)))
(use-package projectile ;; Project management
  :init
  (when (file-directory-p "~/Documents/Code") ;; Projectile will search this path for projects
    (setq projectile-project-search-path '("~/Documents/Code")))
  (setq projectile-switch-project-action #'projectile-dired) ;; Auto open dired when opening project
  :config
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-files "*.class") ;; Ignore class files in projectile
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))
;;(use-package magit) ;; Git managment within Emacs (Very slow on Windows)
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
(add-hook 'emacs-startup-hook 'toggle-frame-maximized) ;; Start Emacs maximized
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
(setq path-to-ctags "c:/Users/heimangreg/Universal-Ctags/ctags.exe")
(defun create-tags (dir-name) ;; Run m-x create-tags to create tags
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
)
(setq-default show-paren-style 'parenthesis)
(setq-default indent-tabs-mode nil) ;; Use spaces for tabs instead of tab characters
(setq tab-width 4) ;; Set the tab width to 4 characters
(setq electric-indent-inhibit t) ;; Make return key indent to current indent level
(setq backward-delete-char-untabify-method 'hungry) ;; Have Emacs backspace the entire tab at a time
(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))
;; (add-hook 'java-mode-hook (lambda() ;; Setup custom java indent
;; 			    (setq c-default-style "java")
;; 			    (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
;; 			    (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0)
;; 			    (c-offsets-alist . ((inline-open . 0)
;; 				(topmost-intro-cont    . +)
;; 				(statement-block-intro . +)
;; 				(knr-argdecl-intro     . 5)
;; 				(substatement-open     . +)
;; 				(substatement-label    . +)
;; 				(label                 . +)
;; 				(statement-case-open   . +)
;; 				(statement-cont        . ++)
;; 				(arglist-intro  . c-lineup-arglist-intro-after-paren)
;; 				(arglist-close  . c-lineup-arglist)
;; 				(access-label   . 0)
;; 				(inher-cont     . ++)
;; 				(func-decl-cont . ++)))))
(add-hook 'c++-mode-hook (lambda() ;; Setup custom C/C++ indent
				       (setq c-default-style "k&r")
				       (setq c-basic-offset 4)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")) ;; Write backups to ~/.emacs.d/backup/
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      5 ; how many of the newest versions to keep
      kept-old-versions      2) ; and how many of the old
(add-hook 'dired-mode-hook (lambda()
			     (auto-revert-mode 1) ;; Automatically update Dired
			     (setq auto-revert-verbose nil))) ;; Be quiet about updating Dired

;; Emacs Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts

