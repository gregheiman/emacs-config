;;; early-init.el --- Configuration file that gets run before init.el -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Gregory Heiman
;; Author: Greg Heiman <gregheiman02@gmail.com>
;; Created: 21 Apr 2021
;; Keywords: Configuration
;; URL: https://github.com/Ushrio/Emacs-Config
;; This file is not part of GNU Emacs.
;; This file is free software. Distributed under the MIT license.

;;; Commentary:
;; Configuration file for GNU Emacs that is run before init.el.
;; Used for setting performance settings to improve startup speed.
;; Built for general purpose programming and note taking.
;; Uses Outline-Minor-Mode
;; C-c @ C-c or zo (Evil mode) - Hide entry
;; C-c @ C-e or zc (Evil mode) - Show entry

;;; Code
(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold (* 1024 1024 1024)) ;; 1 GB
(setq gc-cons-percentage 0.8)
(setq-default file-name-handler-alist nil)
(setq-default inhibit-compacting-font-caches)

(tool-bar-mode 0) ;; Hide the tool bar
(scroll-bar-mode 0) ;; Hide the scroll bar
(menu-bar-mode 0) ;; Hide the menu bar
(setq inhibit-startup-screen t) ;; Hide startup screen

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq file-name-handler-alist file-name-handler-alist-original)
   (setq gc-cons-threshold 2000000) ;; 2 MB
   (setq gc-cons-percentage 0.5)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

(provide 'early-init)

;;; early-init.el ends here
