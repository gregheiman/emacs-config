(require 'package) ;; Add MELPA package repo
(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
    ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize) ;; Initialize package.el
(unless package-archive-contents ;; Auto download package archive repository manifest if not present
  (package-refresh-contents))

(unless (package-installed-p 'use-package) ;; Download use-package if not present
    (package-install 'use-package))

(eval-when-compile ;; Use use-package to manage packages
    (require 'use-package))

(require 'use-package)
    (setq use-package-always-ensure t) ;; Always download packages that are marked under use-package if they aren't installed
    (setq use-package-always-defer t) ;; Always defer package loading. If absolutely nessacary use :demand to override

(use-package evil ;; Vim keybindings
   :demand
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
   (evil-define-key 'normal 'global (kbd "L") 'evil-window-right)
   (evil-define-key 'normal 'global (kbd "K") 'evil-window-up)
   (evil-define-key 'normal 'global (kbd "J") 'evil-window-down)
   (evil-define-key 'normal 'global (kbd "H") 'evil-window-left)
   (evil-define-key 'normal 'global (kbd "gc") 'comment-dwim)
   (eval-after-load 'evil-ex
       '(evil-ex-define-cmd "find" 'projectile-find-file))
   (eval-after-load 'evil-ex
       '(evil-ex-define-cmd "browse-old" 'recentf-open-files))
   (eval-after-load 'swiper ;; Use swiper for / search if loaded
   (evil-define-key 'normal 'global (kbd "/") 'swiper)))

(use-package evil-collection ;; Extend default evil mode keybindings to more modes
   :after evil
   :demand
   :config
   (evil-collection-init)
   :custom ((evil-collection-company-use-tng nil)) ;; Don't autocomplete like vim
   :custom ((evil-collection-setup-minibuffer t)))

(use-package evil-surround ;; Port of vim-surround to emacs
   :after evil
   :demand
   :config
   (global-evil-surround-mode 1))

(use-package undo-tree ;; Undo tree to enable redoing with Evil
   :after evil
   :demand
   :config
   (global-undo-tree-mode 1))

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
    :hook ((lsp-mode . lsp-enable-which-key-integration))
    :config
    (setq-default lsp-keymap-prefix "C-c l")
    (setq-default lsp-headerline-breadcrumb-enable nil) ;; Remove top header line
    (setq-default lsp-signature-auto-activate nil) ;; Stop signature definitions popping up
    (setq-default lsp-enable-snippet nil) ;; Disable snippets (Snippets require YASnippet)
    (setq-default lsp-enable-symbol-highlighting nil) ;; Disable highlighting of symbols
    :commands (lsp lsp-deferred))

(use-package lsp-java ;; Support for the Eclipse.jdt.ls language server
    :hook ((java-mode . lsp-deferred))
    :config
    (setq-default lsp-enable-dap-auto-configure nil))

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
    :config
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
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
    :config
    (setq doom-modeline-height 23)
    (setq doom-modeline-buffer-file-name-style 'file-name)
    (setq doom-modeline-percent-position nil)
    (setq doom-modeline-major-mode-icon nil)
    (setq all-the-icons-scale-factor 1.0)
    (set-face-attribute 'mode-line nil :family "Iosevka" :height 100)
    (set-face-attribute 'mode-line-inactive nil :family "Iosevka" :height 100)
    (with-eval-after-load 'evil ;; Define custom evil state icon for modeline
        (doom-modeline-def-segment evil-state-seg
        "Display current Evil State."
        (propertize (format " <%s>" (upcase (substring (symbol-name evil-state) 0 1)))
                    'face '(:weight bold))))
    (doom-modeline-def-modeline 'main
        '(bar evil-state-seg matches buffer-info remote-host buffer-position parrot selection-info)
        '(misc-info minor-modes checker lsp input-method buffer-encoding major-mode process vcs " "))
    :hook ((window-setup . doom-modeline-mode)))

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

(use-package magit ;; Git managment within Emacs (Very slow on Windows)
    :commands (magit))

(use-package dashboard ;; Improved start screen
    :demand
    :config
    (setq dashboard-items '((recents  . 5)(projects . 5)(bookmarks . 5)))
    (setq dashboard-set-footer nil)
    (dashboard-setup-startup-hook))

(use-package org
    :config
    (add-hook 'after-save-hook (lambda ()(if (y-or-n-p "Reload?")(load-file user-init-file))) nil t) ;; Offer to reload org-mode file after save
    (add-hook 'after-save-hook (lambda ()(if (y-or-n-p "Tangle?")(org-babel-tangle))) nil t)) ;; Offer to tangle org-mode file after save

(set-face-attribute 'default nil :font "Iosevka-12" ) ;; Set font options
(set-frame-font "Iosevka-12" nil t)

(tool-bar-mode 0) ;; Hide the tool bar
(scroll-bar-mode 0) ;; Hide the scroll bar
(menu-bar-mode 0) ;; Hide the menu bar

(add-hook 'emacs-startup-hook 'toggle-frame-maximized) ;; Start Emacs maximized
(recentf-mode 1) ;; Keep a list of recently opened files
(global-hl-line-mode) ;; Highlight the current line
(delete-selection-mode t) ;; Whatever is highlighted will be replaced with whatever is typed or pasted
(global-display-line-numbers-mode 1) ;; Line numbers
(electric-pair-mode 1) ;; Auto pair delimeters
(show-paren-mode t) ;; Highlight matching delimeter pair
(set-default 'truncate-lines t) ;; Disable wrapping of lines
(setq-default show-paren-style 'parenthesis)
(setq-default indent-tabs-mode nil) ;; Use spaces for tabs instead of tab characters
(setq tab-width 4) ;; Set the tab width to 4 characters
(setq electric-indent-inhibit t) ;; Make return key indent to current indent level
(setq backward-delete-char-untabify-method 'hungry) ;; Have Emacs backspace the entire tab at a time
(setq-default buffer-file-coding-system 'utf-8-unix) ;; Automatically use unix line endings and utf-8
(setq vc-follow-symlinks t) ;; Don't prompt to follow symlinks
(setq gc-cons-threshold 10000000) ;; Set GC threshold to 10 MB
(setq read-process-output-max (* 1024 1024)) ;; 1MB

(auto-save-visited-mode) ;; Auto save files without the #filename#
(setq-default buffer-file-coding-system 'utf-8-unix) ;; Automatically use unix line endings and utf-8
(defun full-auto-save () ;; Auto save all buffers when autosave fires
(interactive)
(save-excursion
    (dolist (buf (buffer-list))
    (set-buffer buf)
    (if (and (buffer-file-name) (buffer-modified-p))
        (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")) ;; Write backups to ~/.emacs.d/backup/
    backup-by-copying      t  ; Don't de-link hard links
    version-control        t  ; Use version numbers on backups
    delete-old-versions    t  ; Automatically delete excess backups:
    kept-new-versions      5 ; how many of the newest versions to keep
    kept-old-versions      2) ; and how many of the old

(add-hook 'dired-mode-hook (lambda()
                            (auto-revert-mode 1) ;; Automatically update Dired
                            (setq auto-revert-verbose nil))) ;; Be quiet about updating Dired

(grep-apply-setting 'grep-template "rg --no-heading -H -uu -g <F> <R> <D>")

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts
