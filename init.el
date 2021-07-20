;; Uses Outline-Minor-Mode
;; C-c @ C-c Hide entry
;; C-c @ C-e Show entry

;;; Package Configuration
    (require 'package) ;; Add package repos
    (setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
    (package-initialize) ;; Initialize package.el

    (unless package-archive-contents ;; Auto download package archive repository manifest if not present
      (package-refresh-contents))

;;; Use-Package Configuration
    (unless (package-installed-p 'use-package) ;; Download use-package if not present
      (package-install 'use-package))

    (eval-when-compile ;; Use use-package to manage packages
      (require 'use-package))

    (require 'use-package)
      (setq use-package-always-ensure t) ;; Always download packages that are marked under use-package if they aren't installed
      (setq use-package-always-defer t) ;; Always defer package loading. If absolutely nessacary use :demand to override

;;; Evil Mode
    (use-package evil ;; Vim keybindings
      :hook ((after-init . evil-mode))
      :init
      (setq evil-want-keybinding nil) ;; Needed to use evil-collection
      :config
      (evil-set-undo-system 'undo-tree)
      (evil-set-leader 'normal (kbd "\\"))
      (evil-define-key 'normal 'global (kbd "<leader>bl") 'list-buffers)
      (evil-define-key 'normal 'global (kbd "<leader>bg") 'switch-to-buffer)
      (evil-define-key 'normal 'global (kbd "]q") 'flycheck-next-error)
      (evil-define-key 'normal 'global (kbd "[q") 'flycheck-previous-error)
      (evil-define-key 'normal 'global (kbd "]Q") 'flycheck-last-error)
      (evil-define-key 'normal 'global (kbd "[Q") 'flycheck-first-error)
      (evil-define-key 'normal 'global (kbd "gc") 'comment-dwim)
      (evil-define-key 'normal 'global (kbd "/") 'consult-line)
      (eval-after-load 'evil-ex
          '(evil-ex-define-cmd "find" 'projectile-find-file))
      (eval-after-load 'evil-ex
          '(evil-ex-define-cmd "browse-old" 'recentf-open-files)))

    (use-package evil-collection ;; Extend default evil mode keybindings to more modes
      :demand
      :after evil
      :config
      (evil-collection-init)
      :custom ((evil-collection-company-use-tng nil)) ;; Don't autocomplete like vim
    )

    (use-package evil-surround ;; Port of vim-surround to emacs
      :demand
      :after evil
      :config
      (global-evil-surround-mode 1))

    (use-package undo-tree ;; Undo tree to enable redoing with Evil
      :demand
      :after evil
      :config
      (global-undo-tree-mode t))

;;; Theme
    (use-package doom-themes ;; Color theme
      :config
      (doom-themes-org-config) ;; Corrects some of org-mode's fontification issues
      :init
      (load-theme 'doom-sourcerer t))


;;; Company Mode
    (use-package company ;; Text auto completion framework
      :bind (("TAB" . company-indent-or-complete-common)
      :map company-active-map
        ("<tab>" . company-select-next-or-abort)
        ("TAB" . company-select-next-or-abort)
        ("S-TAB" . compnay-select-previous-or-abort)
        ("<backtab>" . company-select-previous-or-abort))
      :config
      (setq company-format-margin-function 'company-text-icons-margin)
      (setq company-text-icons-add-background t)
      :hook (prog-mode . global-company-mode))

;;; LSP Mode
  (use-package lsp-mode ;; Enable LSP support in Emacs
    :hook ((lsp-mode . lsp-enable-which-key-integration))
    :config
        (setq-default lsp-keymap-prefix "C-c l")
        (setq-default lsp-headerline-breadcrumb-enable nil) ;; Remove top header line
        (setq-default lsp-signature-auto-activate nil) ;; Stop signature definitions popping up
        (setq-default lsp-enable-snippet nil) ;; Disable snippets (Snippets require YASnippet)
        (setq-default lsp-enable-symbol-highlighting nil) ;; Disable highlighting of symbols
        (setq-default lsp-semantic-tokens-enable nil) ;; Not everything needs to be a color
    :bind-keymap ("C-c l" . lsp-command-map)
    :commands (lsp))

  (use-package lsp-java ;; Support for the Eclipse.jdt.ls language server
    :disabled 
    :hook ((java-mode . lsp))
    :config
    (setq-default lsp-enable-dap-auto-configure nil))

;;; Vertico, Orderless, Marginalia, and Consult (Completion)
  (use-package vertico
    :hook ((after-init . vertico-mode))
    :config
      (setq vertico-cycle t) ;; Optionally enable cycling for `vertico-next' and `vertico-previous'
  ) 

 (use-package orderless
    :after vertico
    :config
      (setq completion-styles '(orderless)
            completion-category-defaults nil
            completion-category-overrides '((file (styles partial-completion))))
 )

 (use-package marginalia
   :after vertico
   :hook ((vertico-mode . marginalia-mode))
  )

  (use-package consult
  :after vertico
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines))
       :config
	 (autoload 'projectile-project-root "projectile")
         (setq consult-project-root-function #'projectile-project-root)
  )

;;; Which-Key Mode
    (use-package which-key ;; Show possible keybindings when you pause a keycord
    :defer 5
    :hook ((after-init . which-key-mode))
    :commands (which-key))

;;; Flycheck Mode
    (use-package flycheck ;; Improved linting and checking
      :config
        (setq flycheck-display-error-function #'flycheck-display-error-messages) ;; Show error messages in echo area
        (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; Stop flycheck from treating init.el as package file
      :hook (prog-mode . global-flycheck-mode))

;;; Projectile Mode
  (use-package projectile ;; Project management
    :hook ((prog-mode . projectile-mode))
    :config
        (when (file-directory-p "~/Documents/Code") ;; Projectile will search this path for projects
            (setq projectile-project-search-path '("~/Documents/Code")))
        (setq projectile-switch-project-action #'projectile-dired) ;; Auto open dired when opening project
        ;; (add-hook 'prog-mode-hook (lambda ()
        ;;                             (add-hook 'after-save-hook 'regenerate-tags-if-exists)))
    :bind-keymap ("C-c p" . projectile-command-map))

  (defun regenerate-tags-if-exists ()
    "Regenerate tags if the tags file already exists"
    (if (equal (file-exists-p (concat (projectile-project-root) "TAGS")) t)
        (projectile-regenerate-tags) ;; Create after-save hook to regen tags
    )
  )

;;; Magit Mode
  (use-package magit ;; Git managment within Emacs (Very slow on Windows)
    :commands (magit))

;;; Org Mode Et Al.
  (use-package org ;; Powerful plain text note taking and more
    :hook ((org-mode . org-indent-mode)))

  (use-package org-roam ;; Add powerful non-hierarchical note taking tools to org
    :after org
    :custom
    (org-roam-v2-ack t))

;;; Flyspell Mode
    (use-package flyspell
      :config
        (when (executable-find "ispell")
            (add-hook 'org-mode-hook 'turn-on-flyspell)))

;;; Outline-Minor-Mode
    (use-package outline
      :config
      (setq outline-blank-line t) ;; Have a blank line before a heading
      :hook (
             (emacs-lisp-mode . outline-minor-mode)
             (outline-minor-mode . outline-hide-body)))

;;; Emacs Configuration
 (use-package emacs
  :hook ((emacs-startup . efs/display-startup-time)
         (auto-save . full-auto-save)
         (c-mode . c-mode-configuration))
  :config
    ;; Font configuration
    (set-face-attribute 'default nil :font "Iosevka-12" ) ;; Set font options
    (set-frame-font "Iosevka-12" nil t)

    ;; Add to the interface
    (global-hl-line-mode) ;; Highlight the current line
    (global-display-line-numbers-mode 1) ;; Line numbers
    (column-number-mode t) ;; Show column numbers in modeline
    (show-paren-mode t) ;; Highlight matching delimeter pair
    (setq-default show-paren-style 'parenthesis)

    ;; Bring Emacs into the 21st century
    (recentf-mode 1) ;; Keep a list of recently opened files
    (delete-selection-mode t) ;; Whatever is highlighted will be replaced with whatever is typed or pasted
    (electric-pair-mode 1) ;; Auto pair delimeters
    (auto-save-visited-mode) ;; Auto save files without the #filename#

    ;; Indent configuration 
    (setq-default indent-tabs-mode nil) ;; Use spaces for tabs instead of tab characters
    (setq tab-width 4) ;; Set the tab width to 4 characters
    (setq electric-indent-inhibit t) ;; Make return key indent to current indent level
    (setq backward-delete-char-untabify-method 'hungry) ;; Have Emacs backspace the entire tab at a time

    ;; Personal preference
    (set-default 'truncate-lines t) ;; Disable wrapping of lines
    (setq read-process-output-max (* 1024 1024)) ;; 1MB
    (setq vc-follow-symlinks t) ;; Don't prompt to follow symlinks

    ;; Set default line endings and encoding
    (setq-default buffer-file-coding-system 'utf-8-unix) 
    (set-default-coding-systems 'utf-8-unix) ;; Automatically use unix line endings and utf-8

    ;; Auto revert files
    (global-auto-revert-mode 1) ;; Auto update when files change on disk
    (setq auto-revert-verbose nil) ;; Be quite about updating files when they're changed on disk

    ;; Scrolling configuration
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    (setq scroll-step 1) ;; keyboard scroll one line at a time

    ;; Backup file configuration
    (setq backup-directory-alist '(("." . "~/.emacs.d/backup")) ;; Write backups to ~/.emacs.d/backup/
        backup-by-copying      t  ; Don't de-link hard links
        version-control        t  ; Use version numbers on backups
        delete-old-versions    t  ; Automatically delete excess backups:
        kept-new-versions      5 ; how many of the newest versions to keep
        kept-old-versions      2) ; and how many of the old
  )

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

;;; C Configuration
 (defun c-mode-configuration ()
    "Set C style configuration"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    (setq c-set-style "k&r")
  )

;;; Modeline Configuration
    (setq-default mode-line-format
        (list
            " " ;; Extra space at front
            '((:eval (propertize (format "<%s> |" (upcase (substring (symbol-name evil-state) 0 1)))
                                    'face '(:weight bold)))) ;; Evil mode
            " %m |" ;; Mode
            " %b |" ;; Buffer name
            '(:eval (propertize (format " %s/%s:%s |" "%l" (line-number-at-pos (point-max)) "%c"))) ;; Current line/total lines:column number
            '((:eval (propertize (format "%s |"(flycheck-mode-line-status-text))))) ;; Flycheck indicator
            '(vc-mode vc-mode) ;; Git branch indicator
            " | %+" ;; Show if file is modified or read-only
            ))

;;; Dired Configuration
    (add-hook 'dired-mode-hook (lambda()
                                (auto-revert-mode 1) ;; Automatically update Dired
                                (setq auto-revert-verbose nil))) ;; Be quiet about updating Dired

;;; Grep Configuration
    (when (executable-find "rg")
        (setq grep-use-null-device nil) ;; Don't append NUL to end on windows (Not nessacary for rg)
        (setq-default grep-template "rg -n -H --no-heading -g <F> -e <R> .") ;; For lgrep
        (setq-default grep-find-template "rg -n -H --no-heading -e <R> -g .") ;; For rgrep
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

    ;;;  Jonas.Jarnestrom<at>ki.ericsson.se A smarter               
    ;;;  find-tag that automagically reruns etags when it cant find a               
    ;;;  requested item and then makes a new try to locate it.                      
    ;;;  Fri Mar 15 09:52:14 2002    
    (defadvice find-tag (around refresh-etags activate)
    "Rerun etags and reload tags if tag not found and redo find-tag.              
    If buffer is modified, ask about save before running etags."
    (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
    ad-do-it
        (error (and (buffer-modified-p)
            (not (ding))
            (y-or-n-p "Buffer is modified, save it? ")
            (save-buffer))
            (er-refresh-etags extension)
            ad-do-it))))
    (defun er-refresh-etags (&optional extension)
    "Run etags on all peer files in current dir and reload them silently."
    (interactive)
    (shell-command (format "etags *.%s" (or extension "el")))
    (let ((tags-revert-without-query t))  ; don't query, revert silently          
    (visit-tags-table default-directory nil)))

    ;; Don't ask before rereading the TAGS files if they have changed
    (setq tags-revert-without-query t)

;;; Markdown Configuration
    (when (executable-find "pandoc")
        ;; Set pandoc as the program that gets called when
        ;; you issue a markdown command
        (custom-set-variables
        '(markdown-command "pandoc"))
    )

;;; Emacs Keybindings
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts

;;; Custom-Set-Variables (Set by Emacs)
    
    
    
    (custom-set-variables
    ;; custom-set-variables was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    '(custom-safe-themes
    '("b7e460a67bcb6cac0a6aadfdc99bdf8bbfca1393da535d4e8945df0648fa95fb" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "6b1abd26f3e38be1823bd151a96117b288062c6cde5253823539c6926c3bb178" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "f7fed1aadf1967523c120c4c82ea48442a51ac65074ba544a5aefc5af490893b" "8621edcbfcf57e760b44950bb1787a444e03992cb5a32d0d9aec212ea1cd5234" "22a514f7051c7eac7f07112a217772f704531b136f00e2ccfaa2e2a456558d39" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" default))
    '(markdown-command "pandoc")
    '(package-selected-packages
    '(org-roam marginalia consult orderless vertico doom-themes company-ctags which-key use-package undo-tree projectile magit gruvbox-theme flycheck evil-surround evil-commentary evil-collection company atom-one-dark-theme)))




    (custom-set-faces
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    )
