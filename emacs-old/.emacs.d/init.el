;; Disable UI elements and startup screen
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Initialize package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; Install and enable Gruber Darker theme
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

;; Pink UWU theme
;;(use-package pink-bliss-uwu-theme
;;  :config
;;  (load-theme 'pink-bliss-uwu t))

(require 'elcord)
(elcord-mode)

;; Display line numbers and highlight current line
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(## elcord gruber-darker-theme pink-bliss-uwu-theme rust-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Increase default font size
(set-face-attribute 'default nil :height 170)
