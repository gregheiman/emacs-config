;;; init.el --- Main configuartion file for Emacs -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Gregory Heiman
;; Author: Greg Heiman <gregheiman02@gmail.com>
;; Created: 21 Apr 2021
;; Keywords: Configuration
;; URL: https://github.com/gregheiman/emacs-config
;; This file is not part of GNU Emacs.
;; This file is free software. Distributed under the MIT license.

;;; Commentary:
;; Configuration file for GNU Emacs.
;; Uses Outline-Minor-Mode
;; C-c @ C-c or zo (Evil mode) - Hide entry
;; C-c @ C-e or zc (Evil mode) - Show entry

;;; Code:
(require 'package) ;; Add package repos
(setq package-archives
    '(("melpa" . "https://melpa.org/packages/")
        ("nongnu-elpa" . "https://elpa.nongnu.org/nongnu/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gun.org/devel/")))
(package-initialize) ;; Initialize package.el
(unless package-archive-contents ;; Auto download package archive repository manifest if not present
    (package-refresh-contents))

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(eval-when-compile
  (require 'exec-path-from-shell))
(when (memq window-system '(mac ns x)) ;; Take PATH vars from shell when Emacs is launched from GUI window on MacOS or Linux
  (exec-path-from-shell-initialize))
(when (daemonp) ;; Take PATH vars from shell when Emacs is launched from daemon
  (exec-path-from-shell-initialize))

(unless (package-installed-p 'use-package) ;; Download use-package if not present
    (package-install 'use-package))
(eval-when-compile ;; Use use-package to manage packages
    (require 'use-package))
(setq use-package-always-ensure t) ;; Always download packages that are marked under use-package if they aren't installed
(setq use-package-always-defer t) ;; Always defer package loading. If absolutely necessary use :demand to override

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    (progn
        (setq-default native-comp-async-report-warnings-errors nil)
        (setq-default comp-deferred-compilation t)
        (setq-default native-comp-speed 2)
        (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
        (setq package-native-compile t)))

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "Custom"))) ;; The directory that my custom files are kept in
(load "functions") ;; Custom functions
(load "skeletons") ;; Custom skeletons
;; Custom hydras
(when (package-installed-p 'hydra) ;; Custom Hydras
  (load "hydras"))

;;; Third-Party
;;;; Evil
(use-package evil ;; Vim keybindings
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil) ;; Needed to use evil-collection
  (setq evil-insert-state-message nil) ;; Remove INSERT message from minibuffer
  :bind (:map evil-normal-state-map
         ("<leader>bl" . ibuffer)
         ("<leader>bd" . kill-buffer-and-window)
         ("<leader>bg" . switch-to-buffer)
         ("]q" . flymake-goto-next-error)
         ("[q" . flymake-goto-prev-error)
         ("]b" . next-buffer)
         ("[b" . previous-buffer)
         ("<leader>f" . lgrep)
         ("K" . eldoc)
         ("gd" . xref-find-definitions)
         ("gr" . xref-find-references)
         ("gf" . project-find-file))
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-set-leader 'normal (kbd "\\"))
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "find" 'find-file))
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "oldfiles" 'recentf))
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "Files" 'project-find-file))
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "Term" 'gh/open-ansi-term-in-split)))

(use-package evil-collection ;; Extend default evil mode keybindings to more modes
  :after evil
  :diminish evil-collection-unimpaired-mode
  :init
  (evil-collection-init)
  :config
  (setq-default evil-collection-company-use-tng nil)) ;; Don't autocomplete like vim

(use-package evil-surround ;; Port of vim-surround to Evil
  :after evil
  :diminish
  :init
  (global-evil-surround-mode 1))

(use-package evil-commentary ;; Port of vim-commentary to Evil
  :after evil
  :diminish
  :init
  (evil-commentary-mode))

(use-package undo-tree ;; Undo tree to enable redoing with Evil
  :after evil
  :diminish
  :init
  (global-undo-tree-mode t)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;;;; Color Theme
(use-package tao-theme
  :init
  (load-theme 'tao-yin t)
  :config
  (custom-set-faces
    '(line-number ((t (:inherit font-lock-comment-face))))
    '(mode-line ((t (:foregroud "#C3C3C3"))))
    '(minibuffer-prompt ((t (:foreground "#C3C3C3" :background unspecified :inherit unspecified))))
    '(error ((t (:foreground unspecified :background unspecified :underline (:style wave :color "#FF0000")))))
    '(success ((t (:foreground "#B6FDB8"))))
    '(warning ((t (:foreground "#FDFBB6" :background unspecified))))
    '(dired-directory ((t (:foreground "#B6D6FD"))))
    '(vertico-current ((t (:background "#676767" :foreground "#FFFFFF"))))
    '(completions-annotations ((t (:foreground "#B6D6FD"))))
    '(completions-common-part ((t (:foreground "#B6D6FD" :inherit unspecified))))
    '(completions-first-difference ((t (:foreground "#FFB5B5" :inherit unspecified))))
    '(orderless-match-face-0 ((t :foreground "#B6D6FD")))
    '(orderless-match-face-1 ((t :foreground "#B6FDB8")))
    '(orderless-match-face-2 ((t :foreground "#FDFBB6")))
    '(orderless-match-face-3 ((t :foreground "#FFB5B5")))
    '(markdown-markup-face ((t :foreground "#C3C3C3")))
    '(markdown-header-delimiter-face ((t :foreground "#C3C3C3")))
    '(markdown-language-keyword-face ((t :foreground "#C3C3C3" :background unspecified )))
    '(eglot-diagnostic-tag-unnecessary-face ((t :foreground unspecified :underline (:style wave :color "#FDFBB6"))))
    '(show-paren-match ((t (:foreground "#B6D6FD" :box (:color "#9E9E9E" :line-width -1)))))
    '(show-paren-mismatch ((t (:foreground "#FFCBB5" :background unspecified :box (:color "#9E9E9E" :line-width -1)))))
    '(whitespace-line ((t (:foreground "#FFB5B5"))))
    '(compilation-error ((t (:foreground unspecified :underline unspecified :inherit error))))
    '(flymake-error ((t (:foreground unspecified :underline unspecified :inherit error))))
    '(flymake-warning ((t (:foreground unspecified :underline unspecified :inherit warning))))
    '(font-lock-preprocessor-face ((t (:foreground "#B6FDB8"))))
    '(font-lock-string-face ((t (:foreground "#B6D6FD"))))
    '(font-lock-number-face ((t (:foreground "#B6D6FD"))))
    '(font-lock-comment-face ((t (:foreground "#676767"))))
    '(font-lock-doc-face ((t (:foreground "#676767"))))))

;;;; In-Buffer Editing
(use-package corfu ;; In buffer text completion
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (term-mode . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-echo-documentation t)
  (corfu-preselect-first nil)
  (corfu-popupinfo-mode (cons 1.0 1.0)))

(use-package eglot ;; Minimal LSP client
  :hook (((c-mode c++-mode c-or-c++-mode objc-mode) . eglot-ensure)
         (python-base-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         ((js-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure)
         (eglot-managed-mode . (lambda ()
                                 (setq eldoc-documentation-functions ;; Show flymake diagnostics first.
                                       (cons #'flymake-eldoc-function
                                             (remove #'flymake-eldoc-function eldoc-documentation-functions)))
                                 (setq eldoc-documentation-function #'eldoc-documentation-compose))))
  :bind (:map evil-normal-state-map
         ("gi" . eglot-find-implementation)
         ("gy" . eglot-find-typeDefinition)
         :map eglot-mode-map
         ("C-c l" . hydra-eglot/body))
  :custom
  ;; (eglot-events-buffer-size 0 "Disable events logging to speed up Eglot")
  (eglot-extend-to-xref nil "Ensure that system headers/libs use the same LSP instance")
  (eglot-autoshutdown t "Auto-shutdown servers aftre the last buffer using it is deleted")
  (eglot-ignored-server-capabilities '(list :documentHighlightProvider) "Ignore certain server capabilities.")
  :config
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments)))

;;;; Minibuffer
(use-package vertico ;; The minibuffer package with it all
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package marginalia ;; Annotate completions with richer information
  :hook (after-init . marginalia-mode))

(use-package orderless ;; Allow for space delimeted searching
   :custom
   (completion-styles '(orderless initials basic))
   (completion-category-overrides '((file (styles basic partial-completion))))
   (completion-ignore-case t))

(use-package which-key ;; Show possible keybindings when you pause a keycord
  :defer 5
  :diminish
  :hook ((after-init . which-key-mode)))

(use-package hydra) ;; Beautiful, practical custom keybind menus

;;;; AI
(use-package aider
  :config
  (setq aider-args '("--model ollama_chat/deepseek-coder-v2:6.7b"))
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

;;;; Extensions for Built-In Modes
(use-package auctex) ;; Provides lots of useful tools for editing latex

(use-package cdlatex) ;; Fast input methods for latex

(use-package org-appear ;; Auto toggle all auto hidden org elements
  :after org
  :config
  (setq org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t))

(use-package org-fragtog ;; Auto toggle org latex previews
  :after org)

(use-package org-roam ;; Add powerful non-hierarchical note taking tools to Org
  :ensure-system-package (gcc clang)
  :init
  (setq org-roam-v2-ack t)
  :bind ("C-c o r" . hydra-org-roam/body)
  :config
  (setq org-roam-directory (file-truename "~/org/org-roam"))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template "${title:*} ${tags:50}")
  (org-roam-db-autosync-enable)
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+author: Greg Heiman\n#+date: %U\n#+filetags: ")
           :unnarrowed t))))

;;;; New Major Modes
(use-package cmake-mode) ;; Major mode for CMake

(use-package dockerfile-mode) ;; Major mode for Dockerfiles

(use-package go-mode) ;; Major mode for Go

(use-package jinja2-mode ;; Major mode for Jinja templates
  :init
  (add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode)))

(use-package markdown-mode ;; Major mode for Markdown
  :config
  (setq markdown-enable-math t)
  (if (executable-find "pandoc") ;; Set pandoc as the program that gets called when you issue a markdown command
      (setq markdown-command "pandoc")))

(use-package rust-mode) ;; Major mode for Rust

(use-package terraform-mode ;; Major mode for Terraform
  :init
  (add-to-list 'auto-mode-alist '("\\.tftpl\\'" . terraform-mode)))

(use-package yaml-mode;; Major mode for YAML
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;;; Utilities
(use-package avy ;; Quickly jump to visible location
  :after evil
  :bind (:map evil-normal-state-map
              ("s" . avy-goto-char-2)
              ("gl" . avy-goto-line)))

(use-package docker) ;; Manage and run Docker containers from Emacs

(use-package magit ;; Git managment within Emacs (Very slow on Windows)
  :bind-keymap ("C-c m" . magit-mode-map))

(use-package diminish) ;; Remove minor modes from the modeline

(use-package use-package-ensure-system-package) ;; Use package extension to ensure that system packages are installed

;;; Built-in
;;;; Core Components
(use-package emacs ;; Emacs configuration section
  :ensure nil
  :diminish isearch-mode
  :hook ((emacs-startup . gh/display-startup-time)
         (auto-save . gh/full-auto-save))
  :config
  ;; Set information about ourselves
  (setq user-mail-address "gregheiman02@gmail.com"
        user-full-name "Greg Heiman")

  ;; Font configuration
  (defvar custom-font "Courier New") ;; Default font likely to be installed on most OS
  ;; If preferred font is installed use it
  (cond ((find-font (font-spec :name "JetBrains Mono"))
     (setq custom-font "JetBrains Mono")))

  ;; Set the default font size for Emacs
  (cond ((string-equal system-type "darwin")
            (set-face-attribute 'default nil :font (concat custom-font " 13")))
        (t (set-face-attribute 'default nil :font (concat custom-font " 11"))))

  ;; Set the default font size for emacsclient
  (when (and (daemonp) (string-equal system-type "gnu/linux"))
    (add-to-list 'default-frame-alist '(font . (concat custom-font " 12"))))
  (when (and (daemonp) (string-equal system-type "darwin"))
    (add-to-list 'default-frame-alist '(font . (concat custom-font " 13"))))

  ;; Add to the interface
  (global-hl-line-mode 1) ;; Highlight the current line
  (show-paren-mode t) ;; Highlight matching delimeter pair
  (setq-default show-paren-style 'mixed)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Start all emacs frames maximized

  ;; Bring Emacs into the 21st century
  (recentf-mode 1) ;; Keep a list of recently opened files
  (delete-selection-mode t) ;; Whatever is highlighted will be replaced with whatever is typed or pasted
  (electric-pair-mode 1) ;; Auto pair delimeters
  (auto-save-visited-mode) ;; Auto save files without the #filename#
  (setq auto-save-default nil) ;; Use auto-save-visited-mode instead
  (setq save-silently t) ;; No messages when saving
  (setq auto-save-no-message t)

  ;; Removes *messages* from the buffer.
  (setq-default message-log-max nil)
  (when (get-buffer "*Messages*")
    (kill-buffer "*Messages*"))

  ;; Indent configuration
  (setq-default indent-tabs-mode nil) ;; Use spaces for tabs instead of tab characters
  (setq-default tab-width 4) ;; Set the default tab width to 4 characters
  (setq-default electric-indent-inhibit t) ;; Turn off on-the-fly re-indentation
  (setq backward-delete-char-untabify-method 'hungry) ;; Have Emacs backspace the entire tab at a time

  ;; Personal preference
  (set-default 'truncate-lines t) ;; Disable wrapping of lines
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (defalias 'yes-or-no-p 'y-or-n-p) ;; Mad efficiency gains
  (setq custom-file (make-temp-file "emacs-custom-")) ;; Closest thing to disabling custom

  ;; Set default line endings and encoding
  (set-default-coding-systems 'utf-8-unix)
  (set-locale-environment "en_US.UTF-8")
  (setq buffer-file-coding-system 'utf-8-unix)
  (setq default-file-name-coding-system 'utf-8-unix)
  (setq default-keyboard-coding-system 'utf-8-unix)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (setq default-sendmail-coding-system 'utf-8-unix)
  (setq default-terminal-coding-system 'utf-8-unix)

  ;; Version control settings
  (setq vc-follow-symlinks t) ;; Don't prompt to follow symlinks

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

  (setq delete-by-moving-to-trash t) ;; Use the system's trashcan

  ;; Scratch configuration
  (setq initial-major-mode 'org-mode) ;; Scratch buffer should be in org mode
  (setq initial-scratch-message "")) ;; Scratch buffer message should be blank

(use-package auto-revert ;; Built-in updating of buffers when file(s) on disk change
  :ensure nil
  :init
  (global-auto-revert-mode 1)
  :custom
  ;; (auto-revert-check-vc-info t "Auto revert version control info.") ;; This is too slow on MacOS
  (auto-revert-avoid-polling t "Avoid polling on systems where file notifications are available.")
  (auto-revert-verbose nil "Don't show message when auto-reverting a buffer."))

(use-package eshell ;; Emacs lisp shell
  :ensure nil
  :config
  (setq-default eshell-prompt-function 'gh/eshell-prompt)
  (setq-default eshell-highlight-prompt nil))

(use-package compilation ;; Built-in compilation mode
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output t "Auto scroll to bottom of compilation buffer"))

(use-package modeline ;; Built-in status line at the bottom of the screen
  :ensure nil
  :init
  (column-number-mode t) ;; Show column numbers in modeline
  (setq mode-line-percent-position nil)
  (setq mode-line-position-column-line-format '(" %l:%c ")))

(use-package dired ;; Built-in file manager
  :ensure nil
  :hook (dired-mode . (lambda () (auto-revert-mode 1) (setq auto-revert-verbose nil)))
  :bind (:map dired-mode-map
              ("C-c d" . hydra-dired/body)))

(use-package gnus ;; Built-in Emacs news reader that can be configured for email
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
                  (nnmail-expiry-wait 'immediate))))
  (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]") ;; Make gnus NOT ignore [Gmail] folders
  (setq gnus-posting-styles ;; Correct way to set up responding to emails with the correct email
        '((".*" ;; Matches all groups of messages
           (address "Greg Heiman <gregheiman02@gmail.com>")
           ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 gregheiman02@gmail.com")))))

(use-package minibuffer ;; Built-in interface used for many utility tasks
  :ensure nil
  :hook (minibuffer-setup-hook . cursor-intangible-mode)
  :config
  ;; Make cursor intangible in minibuffer
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package project ;; Built-in project managment package
  :ensure nil
  :bind ("C-c p" . hydra-project/body)
  :config
  (setq project-vc-extra-root-markers '("package.json" "pyproject.toml" "requirements.txt" ".dir-locals.el" ".project.el"))
  (setq project-vc-ignores '(".venv/" "node_modules/" "__*/" ".*/")))

;;;; Editing
(use-package abbrev ;; Built-in in buffer snippets
  :ensure nil
  :diminish
  :hook ((c-mode . abbrev-mode)
         (c++-mode . abbrev-mode)
         (java-mode . abbrev-mode)
         (org-mode . abbrev-mode))
  :config
  (setq save-abbrevs nil))

(use-package skeleton ;; Built-in skeletons for new files and code structures
  :ensure nil
  :init
  (setq skeleton-end-hook nil)
  (setq skeleton-further-elements '((abbrev-mode nil))))

(use-package flymake ;; On the fly diagnostic checking
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 1.5))

(use-package flyspell ;; Built-in spell checking
  :ensure nil
  :hook ((markdown-mode . gh/flyspell-on-for-buffer-type)
         (org-mode . gh/flyspell-on-for-buffer-type)
         (git-commit-setup . git-commit-turn-on-flyspell)
         (text-mode . gh/flyspell-on-for-buffer-type)
         (LaTeX-mode . gh/flyspell-on-for-buffer-type)
         (latex-mode . gh/flyspell-on-for-buffer-type))
  :config
  (setq flyspell-issue-message-flag nil) ;; Do not print messages for every word
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC")) ;; Skip src block in org mode files
  (if (executable-find "aspell") ;; Use aspell if available
      (setq ispell-program-name "aspell")
      (setq ispell-list-command "--list")))

(use-package outline ;; Built-in minor mode that allows for folding code
  :ensure nil
  :diminish outline-minor-mode
  :hook ((emacs-lisp-mode . outline-minor-mode)
         (outline-minor-mode . outline-hide-body))
  :config
  (setq outline-blank-line t)) ;; Have a blank line before a heading

(use-package eldoc ;; Built-in documentation mode
  :diminish
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer t))

(use-package whitespace ;; Built-in highlighting of various whitespaces in files
  :ensure nil
  :diminish
  :hook ((prog-mode . whitespace-mode))
  :config
  ;; Highlight lines longer than 100 chars. and trailing spaces
  (setq whitespace-style '(face trailing lines-tail))
  (setq whitespace-line-column 100))

(use-package treesit ;; Built-in tree-sitter package
  :ensure nil
  :commands (treesit-ready-p)
  :init
  (setq treesit-language-source-alist
    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
      (c "https://github.com/tree-sitter/tree-sitter-c")
      (cmake "https://github.com/uyha/tree-sitter-cmake")
      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
      (css "https://github.com/tree-sitter/tree-sitter-css")
      (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
      (go "https://github.com/tree-sitter/tree-sitter-go")
      (html "https://github.com/tree-sitter/tree-sitter-html")
      (js "https://github.com/tree-sitter/tree-sitter-javascript")
      (json "https://github.com/tree-sitter/tree-sitter-json")
      (lua "https://github.com/Azganoth/tree-sitter-lua")
      (make "https://github.com/alemuller/tree-sitter-make")
      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
      (python "https://github.com/tree-sitter/tree-sitter-python")
      (php "https://github.com/tree-sitter/tree-sitter-php")
      (rust "https://github.com/tree-sitter/tree-sitter-rust")
      (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
      (sql "https://github.com/m-novikov/tree-sitter-sql")
      (toml "https://github.com/tree-sitter/tree-sitter-toml")
      (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
      (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

;;;; Major Modes
(use-package c-mode ;; Built-in C major mode configuration
  :ensure nil
  :hook ((c-mode . gh/c-mode-configuration)
         (c-mode . (lambda () (add-hook 'after-save-hook (lambda () (hide-ifdefs)) nil t))))
  :init
  (with-eval-after-load "cc-mode"
    (define-abbrev c-mode-abbrev-table "aif" "" 'c-if-statement)
    (define-abbrev c-mode-abbrev-table "aelif" "" 'c-elif-statement)
    (define-abbrev c-mode-abbrev-table "aelse" "" 'c-else-statement)
    (define-abbrev c-mode-abbrev-table "amain" "" 'c-main-function)))

(use-package c++-mode ;; Built-in C++ major mode configuration
  :ensure nil
  :hook ((c++-mode . gh/c-mode-configuration)
         (c++-mode . (lambda () (add-hook 'after-save-hook (lambda () (hide-ifdefs)) nil t))))
  :init
  (with-eval-after-load "cc-mode"
    (define-abbrev c++-mode-abbrev-table "aif" "" 'c-if-statement)
    (define-abbrev c++-mode-abbrev-table "aelif" "" 'c-elif-statement)
    (define-abbrev c++-mode-abbrev-table "aelse" "" 'c-else-statement)
    (define-abbrev c++-mode-abbrev-table "amain" "" 'c-main-function)))

(use-package elisp-mode ;; Built-in Elisp major mode configuration
  :ensure nil
  :hook (elisp-mode . gh/elisp-mode-configuration))

(use-package java-mode ;; Built-in Java major mode configuration
  :ensure nil
  :hook (java-mode . gh/java-mode-configuration)
  :init
  (define-auto-insert '(java-mode . "Java Class Skeleton") 'java-class-skeleton)
  (with-eval-after-load "cc-mode"
    (define-abbrev java-mode-abbrev-table "aif" "" 'java-if-statement)
    (define-abbrev java-mode-abbrev-table "aelif" "" 'java-elif-statement)
    (define-abbrev java-mode-abbrev-table "aelse" "" 'java-else-statement)
    (define-abbrev java-mode-abbrev-table "amain" "" 'java-main-function)))

(use-package js-mode ;; Built-in Javascript major mode configuration
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.[cm]js?\\'" . js-mode)))

(use-package objc-mode ;; Built-in Objective C major mode configuration
  :ensure nil
  :hook (objc-mode . gh/c-mode-configuration))

(use-package org ;; Built-in powerful plain text note taking and more
  :diminish org-indent-mode org-cdlatex-mode visual-line-mode
  :hook ((org-mode . visual-line-mode)
         (org-mode . font-lock-mode)
         (org-mode . org-cdlatex-mode)
         (org-mode . org-fragtog-mode)
         (org-mode . org-appear-mode)
         (org-mode . gh/org-add-electric-pairs))
  :bind (:map org-mode-map
              ("C-j" . org-next-visible-heading)
              ("C-k" . org-previous-visible-heading)
              ("M-j" . org-metadown)
              ("M-k" . org-metaup)
              ("RET" . org-return))
  :init
  (with-eval-after-load "org"
    (define-abbrev org-mode-abbrev-table "asrc" "" 'org-src-block))
  (org-babel-do-load-languages 'org-babel-load-languages ;; Set the langs. to load for org src blocks
                               (append org-babel-load-languages
                                       '((C . t)
                                         (python . t))))
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)) ;; Increase size of latex previews
  (setq org-src-fontify-natively t
        org-hide-emphasis-markers t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-cycle-separator-lines 2
        org-startup-with-latex-preview t
        org-startup-indented t
        org-return-follows-link t)
  (if (executable-find "latexmk") ;; Set the command for org -> latex -> pdf
      (setq-default org-latex-pdf-process '("latexmk -output-directory=%o -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))))

(use-package org-agenda ;; Built-in powerful TODO list and agenda tracking
  :ensure nil
  :bind (:map org-agenda-mode-map
              ("C-c o a" . hydra-org-agenda/body))
  :config
  (setq org-agenda-files (directory-files-recursively (expand-file-name "~/org/org-agenda") "\\.org$")) ;; Add all .org files in folder to org agenda list
  (setq org-log-done 'time)) ;; Auto mark time when TODO item is marked done

(use-package sql-mode ;; Built-in SQL editing and interaction major mode
  :ensure nil
  :hook ((sql-mode . (lambda () (sql-set-product 'postgres)))))

;;;;; Tree-Sitter Modes
(use-package bash-ts-mode ;; Built-in Bash major mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'bash)
  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (with-eval-after-load 'sh-mode-hook
    (setq bash-ts-mode-hook sh-mode-hook)))

(use-package c-ts-mode ;; Built-in C major mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'bsd)
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (setq c-ts-mode-hook c-mode-hook))

(use-package c++-ts-mode ;; Built-in C++ major mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'cpp)
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'bsd)
  (c++-ts-mode-hook c++-mode-hook)
  :init
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode)))

(use-package c-or-c++-ts-mode ;; Build-in C/C++ major mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'c-or-c++)
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'bsd)
  (c-or-c++-ts-mode-hook c-or-c++-mode-hook)
  :init
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

(use-package cmake-ts-mode ;; Built-in CMake major mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'cmake)
  :init
  (add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode)))

(use-package dockerfile-ts-mode ;; Built-in Dockerfile mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'dockerfile)
  :init
  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode)))

(use-package go-ts-mode ;; Built-in Go mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'go)
  :init
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  (setq go-ts-mode-hook go-mode-hook))

(use-package java-ts-mode ;; Built-in Java mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'java)
  :init
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  (setq java-ts-mode-hook java-mode-hook))

(use-package js-ts-mode ;; Built-in JavaScript mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'js)
  :custom
  (js-ts-mode-hook js-mode-hook)
  :init
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode)))

(use-package python-ts-mode ;; Built-in Python Mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'python)
  :init
  (setq python-ts-mode-hook python-mode-hook))

(use-package toml-ts-mode ;; Built-in TOML Mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'toml)
  :custom
  (toml-ts-mode-hook conf-toml-mode-hook)
  :init
  (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode)))

(use-package typescript-ts-mode ;; Built-in TypeScript mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'typescript)
  :hook (typescript-ts-mode . gh/typescript-ts-mode-configuration)
  :init
  (add-to-list 'auto-mode-alist '("\\.mts?\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-ts-mode)))

(use-package tsx-ts-mode ;; Built-in TSX mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'typescript)
  :hook (tsx-ts-mode . gh/typescript-ts-mode-configuration)
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-ts-mode)))

(use-package rust-ts-mode ;; Built-in Rust mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'rust)
  :custom
  (rust-ts-mode-hook rust-mode-hook)
  :init
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode)))

(use-package yaml-ts-mode ;; Built-in YAML mode using tree-sitter
  :ensure nil
  :if (treesit-language-available-p 'yaml)
  :custom
  (yaml-ts-mode-hook yaml-mode-hook)
  :init
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))

;;;; Utilities
(use-package ediff ;; Built-in diff interface
  :ensure nil
  :config
  ;; Default to side-by-side frame placement
  (setq ediff-split-window-function #'split-window-horizontally))

(use-package grep ;; Built-in grep
  :ensure nil
  :config
  (if (executable-find "rg")
      (setq grep-template "rg --color=never -n -H --no-heading -g '<F>' -e <R> ./**")
    (setq grep-use-null-device nil)))

(use-package etags ;; Built-in tagging similar to ctags
  :ensure nil
  :config
  (setq tags-revert-without-query t) ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-add-tables nil)) ;; Don't ask to keep current list of tags tables also)

(use-package ibuffer ;; Built-in buffer management
  :ensure nil
  :bind (:map ibuffer-mode-map
              ("C-c b" . hydra-ibuffer-main/body)))

(use-package hide-ifdef ;; Built-in dim ifdefs if they are disabled
  :ensure nil
  :diminish Ifdef
  :hook ((c-mode . hide-ifdef-mode)
         (c++-mode . hide-ifdef-mode))
  :init
  (setq hide-ifdef-shadow t)
  (setq hide-ifdef-initially t))

(use-package display-line-numbers ;; Built-in line numbers minor mode
  :hook ((text-mode . display-line-numbers-mode)
         (prog-mode . display-line-numbers-mode)
         (conf-mode . display-line-numbers-mode)
         (org-mode . display-line-numbers-mode)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quits

(provide 'init)

;;; init.el ends here
