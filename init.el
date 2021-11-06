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

;;; Native Comp
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (progn
      (setq-default native-comp-async-report-warnings-errors nil)
      (setq-default comp-deferred-compilation t)
      (setq-default native-comp-speed 2)
      (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
      (setq package-native-compile t)
      ))

;;; Load Custom Files
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/Custom")) ;; The directory that my custom files are kept in
  (load "functions") ;; Load functions

;;; Evil Mode
  (use-package evil ;; Vim keybindings
    :hook ((after-init . evil-mode))
    :init
      (setq evil-want-keybinding nil) ;; Needed to use evil-collection
    :bind (
      :map evil-normal-state-map
        ("<leader>bl" . list-buffers)
        ("<leader>bd" . kill-this-buffer)
        ("<leader>bg" . switch-to-buffer)
        ("]q" . compilation-next-error)
        ("[q" . compilation-previous-error)
        ("]Q" . compilation-first-error)
        ("[Q" . compilation-last-error)
        ("]b" . next-buffer)
        ("[b" . previous-buffer)
        ("]B" . last-buffer)
        ("[B" . first-buffer)
        ("gc" . comment-dwim)
        ("<leader>f" . lgrep)
      ) 
    :config
      (setq evil-insert-state-message nil)
      (evil-set-undo-system 'undo-tree)
      (evil-set-leader 'normal (kbd "\\"))
      (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
      (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)
      (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
      (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

      (eval-after-load 'evil-ex
          '(evil-ex-define-cmd "find" 'find-file))
      (eval-after-load 'evil-ex
          '(evil-ex-define-cmd "browse-old" 'recentf-open-files))
      (eval-after-load 'evil-ex
          '(evil-ex-define-cmd "Files" 'project-find-file))
      (eval-after-load 'evil-ex
          '(evil-ex-define-cmd "Term" 'gh/open-ansi-term-in-split))
  )

    (use-package evil-collection ;; Extend default evil mode keybindings to more modes
      :after evil
      :init
        (evil-collection-init)
      :custom
        (evil-collection-company-use-tng nil) ;; Don't autocomplete like vim
    )

    (use-package evil-surround ;; Port of vim-surround to emacs
      :after evil
      :config
        (global-evil-surround-mode 1))

    (use-package undo-tree ;; Undo tree to enable redoing with Evil
      :after evil
      :config
        (global-undo-tree-mode t))

;;; Theme
  (use-package modus-themes ;; High contrast themes 
    :init
    (setq modus-themes-paren-match '(bold underline)
          modus-themes-bold-constructs t
          modus-themes-subtle-line-numbers t
          modus-themes-mode-line '(borderless))
    (load-theme 'modus-operandi t)
  )

  (use-package doom-themes ;; A set of modern beautiful themes
    :init
    :config
      (setq doom-themes-enable-bold t)
      (setq doom-themes-enable-italic nil)
      (doom-themes-org-config)
  )

;;; In-Buffer Text Completion 
  (use-package corfu ;; In buffer text completion
    :hook ((prog-mode . corfu-mode)
           (shell-mode . corfu-mode)
           (eshell-mode . corfu-mode)
           (org-mode . corfu-mode))
    :custom
      (corfu-cycle t)
      (corfu-auto t)
      (corfu-commit-predicate nil)
      (corfu-quit-at-boundary t)
      (corfu-quit-no-match t)
      (corfu-echo-documentation t)
    :bind (
      :map corfu-map
            ("TAB" . corfu-next)
            ([tab] . corfu-next)
            ("S-TAB" . corfu-previous)
            ([backtab] . corfu-previous)
    )
  )

;;; LSP and DAP Mode
  (use-package lsp-mode ;; LSP support in Emacs
    :hook (lsp-mode . lsp-enable-which-key-integration)
          ((c-mode c++-mode objc-mode) . (lambda () (when (executable-find "clangd") (lsp))))
    :config
      (setq-default lsp-completion-provider :none)
      (setq-default lsp-keymap-prefix "C-c l")
      (setq-default lsp-headerline-breadcrumb-enable nil) ;; Remove top header line
      (setq-default lsp-signature-auto-activate nil) ;; Stop signature definitions popping up
      (setq-default lsp-enable-symbol-highlighting nil) ;; Disable highlighting of symbols
      (setq-default lsp-semantic-tokens-enable nil) ;; Not everything needs to be a color
      :commands (lsp lsp-deferred)
  )

  (use-package lsp-java ;; Support for the Eclipse.jdt.ls language server
    :after lsp-mode
    :hook (java-mode . lsp)
  )

  (use-package lsp-pyright ;; Support for the Pyright language server
    :after lsp-mode
    :hook (python-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp)))
  )

  (use-package dap-mode ;; DAP support for Emacs
    :after lsp-mode
    :hook ((dap-session-created . +dap-running-session-mode)
           (dap-stopped . +dap-running-session-mode))
    :config
      (add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                              (when (dap--session-running session)
                                                (+dap-running-session-mode 1))))
  )

;;; Minibuffer Completion etc. 
  (use-package minibuffer
    :ensure nil
    :hook (minibuffer-setup-hook . cursor-intangible-mode)
    :config
      ;; Make cursor intangible in minibuffer
      (setq minibuffer-prompt-properties 
        '(read-only t cursor-intangible t face minibuffer-prompt))
  )

  (use-package icomplete ;; Builtin minibuffer completion system
    :ensure nil
    :config
      (setq icomplete-hide-common-prefix nil)
      (setq icomplete-show-matches-on-no-input t)
    :bind (
           :map minibuffer-local-completion-map
             ("SPC" . 'self-insert-command)
           :map icomplete-minibuffer-map
             ("<return>" . icomplete-force-complete-and-exit)
             ("<down>" . icomplete-forward-completions)
             ("C-n" . icomplete-forward-completions)
             ("<up>" . icomplete-backward-completions)
             ("C-p" . icomplete-backward-completions)
    )
  )

  (use-package vertico ;; Fast, lightweight, and improved minibuffer completion system
    :hook (after-init . vertico-mode)
    :config
      (setq vertico-cycle t)
  )

  (use-package marginalia ;; Documentation in the minibuffer
    :init
      (marginalia-mode)
  )

  (use-package orderless ;; Allow for space delimeted searching
    :init
       (setq completion-styles '(substring initials flex partial-completion orderless))
       (setq completion-category-overrides
             '((file (styles . (partial-completion orderless))))
       )
    :config
      (setq orderless-smart-case t) 
  )

;;; Which-Key Mode
  (use-package which-key ;; Show possible keybindings when you pause a keycord
    :defer 5
    :hook ((after-init . which-key-mode))
    :commands (which-key))

;;; Hydra
  (use-package hydra
    :bind (("C-c o r" . hydra-org-roam/body)
           ("C-c p" . hydra-project/body)
           ("C-c l" . hydra-lsp/body))
  )

;;; Flycheck Mode
  (use-package flycheck ;; Improved linting and checking
    :bind-keymap ("C-c f" . flycheck-command-map)
    :config
      (setq flycheck-display-error-function #'flycheck-display-error-messages) ;; Show error messages in echo area
      (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; Stop flycheck from treating init.el as package file
    :hook (prog-mode . global-flycheck-mode))

;;; Expand Region
  (use-package expand-region ;; Semantically select regions of text easily
    :bind
      ("C-=" . er/expand-region)
  )

;;; Project Management
  (use-package projectile ;; Project management
    :disabled
    :config
        (when (file-directory-p "~/Documents/Code") ;; Projectile will search this path for projects
            (setq projectile-project-search-path '("~/Documents/Code")))
        (setq projectile-switch-project-action #'projectile-dired) ;; Auto open dired when opening project
        (if (and (executable-find "rg") (eq projectile-indexing-method 'native)) 
            (setq projectile-git-command "rg --files | rg")) ;; Only works if indexing method is native (Default on Windows)
  )

  (use-package project ;; Built-in project managment package
    :ensure nil
    :config
      (when (file-directory-p "~/Documents/Code")
        (eval-after-load 'project (project-remember-projects-under "~/Documents/Code" t))
      )
  )

;;; Version Control
  (use-package magit ;; Git managment within Emacs (Very slow on Windows)
    :bind-keymap ("C-c m" . magit-mode-map)
    :commands (magit)
  )

;;; Impatient Mode
  (use-package impatient-mode ;; Live preview HTML and Markdown files
    :bind (("C-c i m" . gh/impatient-markdown-preview)
           ("C-c i h" . gh/impatient-html-preview))
    )

;;; Org Mode Et Al.
  ;; Prefix all org modes with C-c o (So for org-agenda C-c o a)
  (use-package org ;; Powerful plain text note taking and more
    :hook ((org-mode . visual-line-mode)
           (org-mode . prettify-symbols-mode)
           (org-mode . flyspell-mode)
           (org-mode . font-lock-mode)
           (org-mode . org-cdlatex-mode)
           (after-save . org-latex-preview))
    :bind-keymap ("C-c o o" . org-mode-map)
    :config
      (setq org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-cycle-separator-lines 2
        org-startup-with-latex-preview t
        org-startup-indented t)
      (if (executable-find "latexmk") ;; Set the command for org -> latex -> pdf
        (setq-default org-latex-pdf-process '("latexmk -output-directory=%o -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f")))
  )

  (use-package org-agenda ;; Powerful TODO list and agenda tracking
    :ensure nil
    :after org
    :bind-keymap ("C-c o a" . org-agenda-mode-map)
    :config
      (setq org-agenda-files (directory-files-recursively (expand-file-name "~/Org/Org-Agenda") "\\.org$"))) ;; Add all .org files in folder to org agenda list
      (setq org-log-done 'time) ;; Auto mark time when TODO item is marked done

  (use-package org-roam ;; Add powerful non-hierarchical note taking tools to org
    :init
      (setq org-roam-v2-ack t)
    :config
      (setq org-roam-directory (file-truename "~/Org/Org-Roam"))
      (setq org-roam-completion-everywhere t)
      (org-roam-db-autosync-enable)
      (setq org-roam-capture-templates
       '(
         ("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+author: Greg Heiman\n#+date: %U\n#+filetags:\n")
           :unnarrowed t)
         )
      )
  )

  (use-package ox-hugo ;; Publish Org mode documents to static websites using Hugo
    :after ox
  )

;;; LaTeX
  (use-package auctex ;; Powerful LaTeX tool suite

  )

  (use-package cdlatex ;; Fast insertion of environment templates and math in LaTeX

  )

;;; Flyspell Mode
  (use-package flyspell
    :hook ((markdown-mode . 'turn-on-flyspell)
           (LaTeX-mode . 'turn-on-flyspell)) 
    :config
      (if (executable-find "aspell") ;; Use aspell if available
        (setq ispell-program-name "aspell"))
  )
    

;;; Outline-Minor-Mode
    (use-package outline ;; Minor mode that allows for folding code
      :config
        (setq outline-blank-line t) ;; Have a blank line before a heading
      :hook (
             (emacs-lisp-mode . outline-minor-mode)
             (outline-minor-mode . outline-hide-body)))

;;; Eshell Mode
  (use-package eshell
    :config
      (setq-default eshell-prompt-function 'eshell-prompt)
      (setq-default eshell-highlight-prompt nil))

;;; Gnus
  (use-package gnus ;; Builtin Emacs news reader that can be configured for email
    :ensure nil
    :config
      (setq gnus-select-method '(nnnil nil))
      (setq gnus-secondary-select-methods
      '((nnimap "gregheiman02@gmail.com"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnir-search-engine imap)
                (nnmail-expiry-target "nnimap+gregheiman02@gmail.com:[Gmail]/Trash")
                (nnmail-expiry-wait 'immediate))
        (nnimap "w459e964@wichita.edu"
                (nnimap-address "outlook.office365.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnir-search-engine imap)
                (nnmail-expiry-target "nnimap+w459e964@wichita.edu:Trash")
                (nnmail-expiry-wait 'immediate))))
      (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]") ;; Make gnus NOT ignore [Gmail] folders
      (setq gnus-posting-styles ;; Correct way to set up responding to emails with the correct email
            '((".*" ;; Matches all groups of messages
               (address "Greg Heiman <gregheiman02@gmail.com>")
               ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 gregheiman02@gmail.com"))
              ("w459e964@wichita.edu" ;; Matches Gnus group called w459e964@wichita.edu
               (address "Greg Heiman <w459e964@wichita.edu")
               (organization "Wichita State University")
               ("X-Message-SMTP-Method" "smtp smtp.office365.com 587 w459e964@wichita.edu"))
              )
      )
  )

;;; Emacs Configuration
 (use-package emacs
   :hook ((emacs-startup . efs/display-startup-time)
          (auto-save . full-auto-save))
   :config
     ;; Set information about ourselves
     (setq user-mail-address "gregheiman02@gmail.com"
           user-full-name "Greg Heiman")
     ;; Font configuration
     (set-face-attribute 'default nil :font "Iosevka-12" ) ;; Set font options
     (set-frame-font "Iosevka-12" nil t)

     ;; Add to the interface
     (global-hl-line-mode) ;; Highlight the current line
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
     (defalias 'yes-or-no-p 'y-or-n-p) ;; Mad efficiency gains
     (setq custom-file (make-temp-file "emacs-custom-")) ;; Closest thing to disabling custom
     (setq compilation-scroll-output t) ;; Auto scroll to bottom of compilation buffer
 
     ;; Set default line endings and encoding
     (setq-default buffer-file-coding-system 'utf-8-unix) 
     (set-default-coding-systems 'utf-8-unix) ;; Automatically use unix line endings and utf-8
 
     ;; Auto revert files
     (global-auto-revert-mode 1) ;; Auto update when files change on disk
     (setq auto-revert-verbose nil) ;; Be quite about updating files when they're changed on disk

     ;; Auto follow vc symlinks and auto-revert vc changes
     (setq vc-follow-symlinks t) ;; Don't prompt to follow symlinks
     (setq auto-revert-check-vc-info t) ;; Auto revert vc

     ;; Don't show commands that aren't valid with current modes (Only in Emacs > 28)
     (if (not (version< emacs-version "28")) 
         (setq read-extended-command-predicate #'command-completion-default-include-p))

     ;; Set the bell to flash the modeline rather than audio or standard visual bell
     (setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

     ;; Scrolling configuration
     (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
     (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
     (setq scroll-step 1) ;; keyboard scroll one line at a time
     (setq mouse-wheel-scroll-amount '(5)) ;; mouse wheel scroll 5 lines at a time
 
     ;; Backup file configuration
     (setq backup-directory-alist '(("." . "~/.emacs.d/backup")) ;; Write backups to ~/.emacs.d/backup/
         backup-by-copying      t  ; Don't de-link hard links
         version-control        t  ; Use version numbers on backups
         delete-old-versions    t  ; Automatically delete excess backups:
         kept-new-versions      5 ; how many of the newest versions to keep
         kept-old-versions      2) ; and how many of the old
  )

;;; Line Numbers
  (use-package display-line-numbers
    ;; Display line numbers for some modes
    :hook ((text-mode . display-line-numbers-mode)
           (prog-mode . display-line-numbers-mode)
           (conf-mode . display-line-numbers-mode)
           (org-mode . display-line-numbers-mode)))

;;; Modeline Configuration
  (use-package modeline
    :ensure nil
    :init
      (setq-default mode-line-format
          '((:eval (simple-mode-line-render
                      ;; left
                      (format-mode-line
                       (list
                        " "
                        '(:eval (propertize (format "<%s> " (upcase (substring (symbol-name evil-state) 0 1))))) ;; Evil mode
                        '(:eval (propertize (format "%s " (vc-branch))))
                        '(:eval (propertize (format "%s" "%b")))
                        '(:eval (propertize (format " %s " "[%*]")))
                        ))
                      ;; right
                       (format-mode-line
                        (list
                         '(:eval (propertize (format "[%s] " (flycheck-indicator))))
                         '(:eval (propertize (format "%s" (upcase (symbol-name buffer-file-coding-system)))))
                         '(:eval (propertize (format " %s " "%m")))
                         '(:eval (propertize (format "%s/%s:%s " "%l" (line-number-at-pos (point-max)) "%c")))
                         ))
                      ))))
  )

;;; Dired Configuration
  (use-package dired
    :ensure nil
    :hook (dired-mode . (lambda()
                          (auto-revert-mode 1) ;; Automatically update Dired
                          (setq auto-revert-verbose nil))) ;; Be quiet about updating Dired
  )

;;; Grep Configuration
  (use-package grep
    :config
      (if (executable-find "rg")
          (grep-apply-setting 'grep-template "rg -n -H --no-heading -g <F> -e <R> .") ;; For lgrep
          (setq grep-use-null-device nil) ;; Don't append NUL to end on windows (Not nessacary for rg)
      )
  )

;;; Tags Configuration
  (use-package etags
    :config 
      (setq-default tags-revert-without-query t) ;; Don't ask before rereading the TAGS files if they have changed
      (setq tags-add-tables nil) ;; Don't ask to keep current list of tags tables also
  )

;;; Markdown Configuration
  (use-package markdown-mode
    :config
      (if (executable-find "pandoc")
          ;; Set pandoc as the program that gets called when
          ;; you issue a markdown command
        (setq markdown-command "pandoc"))
  )

;;; Java Configuration
  (use-package java-mode
    :ensure nil
    :hook (java-mode . java-mode-configuration)
  )

;;; C Configuration
  (use-package c-mode
    :ensure nil
    :hook ((c-mode . c-mode-configuration)
           (c++-mode . c-mode-configuration)
           (objc-mode . c-mode-configuration))
  )

;;; Emacs Keybindings
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts

