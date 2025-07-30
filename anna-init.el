;; installed packages
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(rustic flycheck-gradle gradle-mode flycheck-kotlin kotlin-mode flycheck highlight-indent-guides ace-window anzu rainbow-delimiters move-text mwim smartparens aggressive-indent company-quickhelp company magit-lfs magit neotree csproj-mode csharp-mode ws-butler haml-mode nhexl-mode lsp-mode sql-indent web-mode dockerfile-mode yaml-mode atom-one-dark-theme))
 '(require-final-newline t)
 '(safe-local-variable-values '((engine . jinja)))
 '(sp-highlight-pair-overlay nil)
 '(warning-suppress-types '((org-roam))))

;; add melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.config/emacs/use-package")
  (require 'use-package))

(use-package atom-one-dark-theme
  :ensure t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t)

(use-package sql-indent
  :ensure t
  :defer t)

(use-package lsp-mode
  :ensure t
  :defer t)

(use-package nhexl-mode
  :ensure t
  :defer t)

(use-package haml-mode
  :ensure t
  :defer t)

(use-package hledger-mode
  :ensure t
  :defer t
  :mode "\\.journal\\'")

(use-package toc-org
  :ensure t
  :defer t
  :hook (org-mode . toc-org-mode))

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/sync-nc/kb/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package ws-butler
  :ensure t
  :config
  ;; remove trailing whitespace
  (ws-butler-global-mode 1))

(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-global-mode 1))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package csharp-mode
  :ensure t
  :defer t)

(use-package csproj-mode
  :ensure t
  :defer t)

(use-package neotree
  :ensure t)

(use-package magit
  :ensure t)

(use-package magit-lfs
  :requires magit
  :ensure t)

(use-package company
  :ensure t)

(use-package company-quickhelp
  :requires company
  :ensure t)

(use-package aggressive-indent
  :ensure t)

(use-package smartparens
  :ensure t)

(use-package mwim
  :ensure t)

(use-package move-text
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package anzu
  :ensure t)

(use-package ace-window
  :ensure t
  ;; rebind M-o to ace-window
  :bind ("M-o" . 'ace-window))

(use-package highlight-indent-guides
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package kotlin-mode
  :ensure t
  :defer t)

(use-package flycheck-kotlin
  :requires (flycheck kotlin-mode)
  :ensure t
  :defer t)

(use-package gradle-mode
  :ensure t
  :defer t)

(use-package flycheck-gradle
  :requires (flycheck gradle-mode)
  :ensure t
  :defer t)

(use-package rustic
  :ensure t
  :defer t
  :init
  (setq rustic-lsp-server 'rust-analyzer))

(use-package password-store
  :ensure t
  :defer t)

(use-package pass
  :ensure t
  :defer t)

;; set scratch
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")

;; set kill-whole-line to C-c k
(global-set-key (kbd "C-c k") 'kill-whole-line)
;; set neotree to f8
(global-set-key [f8] 'neotree-toggle)

;; load atom one dark theme
(load-theme 'atom-one-dark t)
;; (setq base16-theme-256-color-source "colors")
;; (load-theme 'base16-tomorrow-night t)

;; set font for gui emacs
;; (add-to-list 'default-frame-alist '(font .  "Monospace-12" ))
;; (set-face-attribute 'default t :font  "Monospace-12" )

;; (set-frame-font "Fira Code-12" nil t)

;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; handle indenting not working with haml-mode for some reason
(with-eval-after-load 'haml-mode
  (define-key haml-mode-map (kbd "RET") 'newline-and-indent))

;; use spaces for tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvar custom-tab-width)
(setq custom-tab-width tab-width)
(defvaralias 'custom-tab-width 'tab-width)
(setq rustic-indent-offset tab-width)
(defvaralias 'rustic-indent-offset 'tab-width)
(setq css-indent-offset tab-width)
(defvaralias 'css-indent-offset 'tab-width)
(defvar python-indent-offset)
(setq python-indent-offset 2)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; show matching parens
(show-paren-mode 1)

;; turn on spellcheck and autofill for text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
;; but turn them off for yaml
(add-hook 'yaml-mode-hook 'turn-off-auto-fill)
(add-hook 'yaml-mode-hook 'turn-off-flyspell)

;; use web-mode for web stuff
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.tera\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(setq web-mode-enable-engine-detection t)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; show commit formatting
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

;; unified diffs
(setq diff-switches "-u -w")

;; show column, as well
(setq column-number-mode 2)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; handle indentation with C-a/C-e
(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)

;; move lines with M-up/M-down
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; do open next/prev line with M-n/M-p
(global-set-key (kbd "M-n") 'open-next-line)
(global-set-key (kbd "M-p") 'open-previous-line)
(defvar newline-and-indent t)

;; anzu (match index/count)
(global-anzu-mode 1)

;; indent sql
(add-hook 'sql-mode-hook 'sqlind-minor-mode)

;; rainbow delimiters when editing code
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; highlight indent guides
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'fill)

;; smartparens for code
(require 'smartparens-config) ; load lang configs as well
(add-hook 'prog-mode-hook 'smartparens-mode)

;; aggressively indent code
(global-aggressive-indent-mode 1)

;; no aggressive indent for haml
(add-to-list 'aggressive-indent-excluded-modes 'haml-mode)

;; turn on line numbers but not for term or neotree
(global-display-line-numbers-mode 1)
(add-hook 'neo-after-create-hook (lambda(&rest _) (display-line-numbers-mode -1)))
(add-hook 'term-mode-hook (lambda() (display-line-numbers-mode -1)))

(require 'flycheck)
(require 'rustic)
(require 'smartparens-rust)
;;(setq rustic-lsp-server 'rust-analyzer)
(add-hook 'rustic-mode-hook #'lsp)
(sp-with-modes 'rustic-mode
  (sp-local-pair "'" "'"
                 :unless '(sp-in-comment-p sp-in-string-quotes-p sp-in-rust-lifetime-context)
                 :post-handlers'(:rem sp-escape-quotes-after-insert))
  (sp-local-pair "<" ">"
                 :when '(sp-rust-filter-angle-brackets)
                 :skip-match 'sp-rust-skip-match-angle-bracket))

;; turn on ido mode and separate by newline
(ido-mode 1)
(setq ido-separator "\n")

;; don't put backup files in same dir
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.local/share/emacs/backups/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; put autosaves in temp dir
(setq auto-save-file-name-transforms
      `((".*", temporary-file-directory t)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
