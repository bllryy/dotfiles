;; Package management setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ===============================================
;; BASIC EMACS SETTINGS
;; ===============================================

;; Clean up UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

;; Better defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-display-line-numbers-mode)
(column-number-mode)
(global-hl-line-mode)
(show-paren-mode)

;; Save backup files in a dedicated directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;; Better scrolling
(setq scroll-conservatively 100)

;; Font (adjust size as needed)
;;(set-face-attribute 'default nil :font "Monaco-12")

;; ===============================================
;; THEME
;; ===============================================

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;; ===============================================
;; NAVIGATION & COMPLETION
;; ===============================================

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-ibuffer)
         ("C-c g" . counsel-git-grep)))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package which-key
  :diminish
  :config
  (which-key-mode))

;; ===============================================
;; PROJECT MANAGEMENT
;; ===============================================

(use-package projectile
  :diminish
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

;; ===============================================
;; VERSION CONTROL
;; ===============================================

(use-package magit
  :bind ("C-x g" . magit-status))

;; ===============================================
;; TERMINAL
;; ===============================================

(use-package vterm
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-buffer-name-string "vterm %s"))

(use-package multi-vterm
  :bind (("C-c t" . multi-vterm)
         ("C-c T" . multi-vterm-dedicated-toggle)))

;; ===============================================
;; FILE MANAGEMENT
;; ===============================================

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"))

;;(use-package dired-single
;;  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; ===============================================
;; PROGRAMMING LANGUAGES (CTF relevant)
;; ===============================================

;; C/C++
(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "linux"
        c-basic-offset 4)
  :bind (:map c-mode-map
              ("C-c C-c" . compile))
  :hook ((c-mode . (lambda ()
                     (setq compile-command
                           (format "gcc -Wall -Wextra -g -o %s %s"
                                   (file-name-sans-extension (buffer-name))
                                   (buffer-name)))))))

;; Rust syntax highlighting
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save nil)  ; Just syntax highlighting, no auto-format
  :bind (:map rust-mode-map
              ("C-c C-c" . compile))
  :hook (rust-mode . (lambda ()
                       (setq compile-command "cargo build"))))

;; TOML support (for Cargo.toml)
(use-package toml-mode
  :mode "\\.toml\\'")

;; Python (common in CTFs)
(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq python-indent-offset 4))

;; Assembly
(use-package nasm-mode
  :mode "\\.\\(asm\\|s\\|S\\)$")

;; Web languages (for web challenges)
(use-package web-mode
  :mode ("\\.html?\\'" "\\.css\\'" "\\.js\\'" "\\.php\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; SQL
(use-package sql-mode
  :ensure nil
  :mode "\\.sql\\'")



;; Java
(use-package java-mode
  :ensure nil
  :mode "\\.java\\'"
  :config
  (setq c-basic-offset 4)
  :bind (:map java-mode-map
              ("C-c C-c" . compile))
  :hook (java-mode . (lambda ()
                       (setq compile-command
                             (format "javac %s && java %s"
                                     (buffer-name)
                                     (file-name-sans-extension (buffer-name)))))))




;; ===============================================
;; CTF-SPECIFIC TOOLS
;; ===============================================

;; Hex editing
(use-package hexl-mode
  :ensure nil
  :mode "\\.\\(bin\\|exe\\|dll\\)$")

;; Better hex viewer
(use-package nhexl-mode
  :mode "\\.\\(bin\\|exe\\|dll\\|img\\)$")

;; REST client for API testing
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; JSON handling
(use-package json-mode
  :mode "\\.json\\'")

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Markdown for writeups
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (setq markdown-command "multimarkdown"))

;; ===============================================
;; ORG MODE (for CTF notes and writeups)
;; ===============================================

(use-package org
  :config
  (setq org-directory "~/ctf-notes/")
  (setq org-agenda-files '("~/ctf-notes/"))
  (setq org-log-done t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  
  ;; Code block languages for CTFs
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (C . t)
     (sql . t)
     (js . t))))

;; Better org bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; ===============================================
;; UTILITIES
;; ===============================================

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Company completion
(use-package company
  :diminish
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1))

;; Flycheck syntax checking
(use-package flycheck
  :diminish
  :config
  (global-flycheck-mode))

;; ===============================================
;; CTF-SPECIFIC KEYBINDINGS
;; ===============================================

;; Quick access to commonly used functions
(global-set-key (kbd "C-c h") 'hexl-find-file)      ; Open file in hex mode
(global-set-key (kbd "C-c c") 'compile)             ; Quick compile
(global-set-key (kbd "C-c r") 'recompile)           ; Quick recompile
(global-set-key (kbd "C-c n") 'org-capture)         ; Quick notes
(global-set-key (kbd "C-x C-b") 'ibuffer)           ; Better buffer list

;; ===============================================
;; CUSTOM FUNCTIONS FOR CTF WORK
;; ===============================================

(defun ctf-new-challenge (name)
  "Create a new directory structure for a CTF challenge."
  (interactive "sChallenge name: ")
  (let ((dir (expand-file-name name "~/ctf-challenges/")))
    (make-directory dir t)
    (make-directory (concat dir "/files") t)
    (make-directory (concat dir "/scripts") t)
    (find-file (concat dir "/notes.org"))
    (insert (format "#+TITLE: %s\n#+DATE: %s\n\n* Challenge Description\n\n* Solution\n\n* Scripts\n\n* Flag\n" 
                    name (format-time-string "%Y-%m-%d")))))

(defun ctf-encode-base64 (start end)
  "Encode region as base64."
  (interactive "r")
  (shell-command-on-region start end "base64" nil t))

(defun ctf-decode-base64 (start end)
  "Decode region from base64."
  (interactive "r")
  (shell-command-on-region start end "base64 -d" nil t))

(defun ctf-encode-url (start end)
  "URL encode region."
  (interactive "r")
  (shell-command-on-region start end "python3 -c 'import sys, urllib.parse; print(urllib.parse.quote(sys.stdin.read().strip()))'" nil t))

(defun ctf-decode-url (start end)
  "URL decode region."
  (interactive "r")
  (shell-command-on-region start end "python3 -c 'import sys, urllib.parse; print(urllib.parse.unquote(sys.stdin.read().strip()))'" nil t))

;; Bind these functions
(global-set-key (kbd "C-c e b") 'ctf-encode-base64)
(global-set-key (kbd "C-c d b") 'ctf-decode-base64)
(global-set-key (kbd "C-c e u") 'ctf-encode-url)
(global-set-key (kbd "C-c d u") 'ctf-decode-url)
(global-set-key (kbd "C-c C-n") 'ctf-new-challenge)

;; ===============================================
;; STARTUP
;; ===============================================

;; Create CTF directories if they don't exist
(unless (file-exists-p "~/ctf-challenges/")
  (make-directory "~/ctf-challenges/" t))

(unless (file-exists-p "~/ctf-notes/")
  (make-directory "~/ctf-notes/" t))

;; Welcome message
(setq initial-scratch-message 
      ";; Welcome to CTF Emacs!\n;; Press C-c C-n to create a new challenge\n;; Press C-c t for terminal\n;; Press C-x C-f to open files\n\n")

(message "CTF Emacs configuration loaded! Happy hacking!")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
