(setq vc-follow-symlinks t)

(setq display-buffer-alist
      '(;; no window
	("*Pwndoc Export*"
	 (display-buffer-no-window)
	 (allow-no-window . t)
	 )
	("*Async Shell Command*"
	 (display-buffer-no-window)
	 (allow-no-window . t)
	 )
	("*commands-history*"
	 (display-buffer-no-window)
	 (allow-no-window . t)
	 )
	))

(setq native-comp-async-report-warnings-errors nil)

(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)

(use-package package
  :config
  
  ;; Set priorities of using melpa-stable.
  ;; The higher the number, the higher the priority.
  (setq package-archive-priorities
	'(("melpa-stable" . 2)
	  ("MELPA" . 1)
	  ("gnu" . 0)))

  ;; Add to list melpa stable
  (setq package-archives
	`(("melpa-stable" . "https://stable.melpa.org/packages/")
	  ("MELPA" . "https://melpa.org/packages/")
	  ("gnu" . "https://elpa.gnu.org/packages/")
	  ))
  )

(use-package spacemacs-theme
  :straight t
  :init
  (setq spacemacs-theme-comment-bg nil)
  )

(use-package magit
  :straight t
  :bind (("C-x g" . 'magit-status))
  )

(use-package ivy
  :straight t
  :config
  (ivy-mode 1)
  
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)))

  ;; useful when using ivy-posframe to make sure that long lines are
  ;; still visible.
  (setq ivy-truncate-lines nil)  
  )

(use-package better-shell
  :straight t
  )

(use-package eterm-256color
  :straight t
  :hook (term-mode . eterm-256color-mode)
  )

(use-package vterm
  :straight t
  :bind*(:map vterm-mode-map
	      ("C-x C-k" . vterm-copy-mode)
	      :map vterm-copy-mode-map
	      ("C-x C-k" . vterm-copy-mode))
  :config
  (setq vterm-max-scrollback 100000)
  )

(use-package multi-vterm
  :straight t
  )

(use-package org
  :straight t

  :config
  (require 'org-tempo)  ;; to use <-s for expanding into blkc src
  (require 'org-crypt)
  (require 'epa-file)

  (epa-file-enable)  
  (org-crypt-use-before-save-magic)       
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)

  (setq org-ellipsis " ▼")
  
  (setq org-adapt-indentation t)

  (setq org-todo-keywords  '((sequence "TODO" "DOING REVIEW" "|" "DONE" "ARCHIVED")))

  ;; Setting Colours (faces) for todo states to give clearer view of work 
  (setq org-todo-keyword-faces
	'(("TODO" . "BlueViolet")
	  ("DOING" . "yellow")
	  ("BLOCKED" . "red")
	  ("REVIEW" . "DarkOrange")
	  ("DONE" . "LawnGreen")
	  ("ARCHIVED" .  "RoyalBlue")))

  ;; Hide markup elements in org-mode
  (setq org-hide-emphasis-markers nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (C . t)
     (lisp . t)
     (shell . t)
     (scheme . t)
     (python . t)))

  ;; https://www.reddit.com/r/emacs/comments/4fus4i/tree_structure_using_capture_in_org_mode/
  (setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file org-outline-path-complete-in-steps nil)
  )

(use-package denote
  :straight t
  :config
  (setq denote-directory (expand-file-name "~/notes/denote"))
  
  (setq denote-save-buffers nil)
  (setq denote-known-keywords
	'("emacs" "projects" "programming"
	  "books" "math" "activities" "life" "activities"
	  "writing" "network" "movie" "tv"
	  "security" "system" "tool"
	  )
	)
  
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  (denote-rename-buffer-mode 1)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  )

(set-fontset-font "fontset-default" nil  (font-spec :size 20 :name "Symbola"))

(setq display-time-default-load-average nil)

(display-time)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode 0) ;; Turn off the toolbar

(prefer-coding-system 'utf-8-unix)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-processes nil)

;; Set global value for paragraph width
(setq-default fill-column 70)

;; Stop emacs from losing informations.
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Smooth scroll
(setq scroll-step 3)
(setq line-number-mode t)
(setq inhibit-startup-screen t)
(setq ring-bell-function (quote ignore))

;; add column number in the main bar
(column-number-mode)
(global-visual-line-mode)

(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; split vertically when doing switch-to-buffer-other-window
;; source: https://superuser.com/questions/55466/how-can-i-change-switch-to-buffer-other-window-to-split-vertically-instead-of-ho
(setq split-width-threshold nil)

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes 
		'("\\.zip\\'" ".zip" "unzip")))  

;; Set font to be used
(add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
(set-face-attribute 'default t :font "Liberation Mono-11.5")

(setq resize-mini-windows 'grow-only)

(setq auth-source-save-behavior nil)

(setq grep-command "grep --color=auto -nrH --null")

(setq browse-url-browser-function 'browse-url-chromium)

(setq env/dotemacs-path (expand-file-name "~/dotfiles/emacs/minimal.org"))

(add-hook 'org-mode-hook
	  (lambda ()
            (when (or (equal (buffer-file-name) env/dotemacs-path))
	      (add-hook 'after-save-hook
			(lambda ()
			  (org-babel-tangle)
			  (message "Dotfile tangled"))
			nil
			t			
			)
              )))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(add-hook 'dired-mode-hook 'auto-revert-mode)

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(add-hook 'comint-output-filter-functions #'comint-osc-process-output)

;; nil value means 'do not set tabs, ever!'
(setq-default tab-stop-list nil)
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 2)

(setq org-src-preserve-indentation 't)

(unless (file-exists-p "~/.emacs.d/.auto_saves/")
  (make-directory "~/.emacs.d/.auto_saves/")
  )

(setq make-backup-files nil
      auto-save-default t
      auto-save-timeout 1
      auto-save-interval 300
      auto-save-file-name-transforms '((".*" "~/.emacs.d/.auto_saves/" t))
      create-lockfiles nil)

;; Avoid "file name too long" when creating a copy of a file that is
;; too long by compusing the hash of its full path.
;;
;; Ref: https://emacs.stackexchange.com/questions/48301/spacemacs-and-file-name-too-long-error-on-auto-save
(advice-add 'make-auto-save-file-name :around
            #'my/shorten-auto-save-file-name)
(defun my/shorten-auto-save-file-name (&rest args)
  (let ((buffer-file-name
         (when buffer-file-name (sha1 buffer-file-name))))
    (apply args)))

(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
	 ("\\.hin$"    . c++-mode)
	 ("\\.cin$"    . c++-mode)
	 ("\\.inl$"    . c++-mode)
	 ("\\.rdc$"    . c++-mode)
	 ("\\.h$"      . c++-mode)
	 ("\\.c$"      . c++-mode)
	 ("\\.cc$"     . c++-mode)
	 ("\\.c8$"     . c++-mode)
	 ("\\.txt$"    . indented-text-mode)
	 ("\\.emacs$"  . emacs-lisp-mode)
	 ("\\.gen$"    . gen-mode)
	 ("\\.ms$"     . fundamental-mode)
	 ("\\.m$"      . objc-mode)
	 ("\\.mm$"     . objc-mode)
	 ("\\.asm$"    . asm86-mode)
	 ("\\.inc$"    . asm86-mode)
	 ("\\.ts"      . typescript-ts-mode)
	 ) auto-mode-alist))

(setq auto-mode-alist
      (append
       '((".offlineimaprc"   . conf-mode)
	 (".moc.conf"        . conf-mode)
	 ("github_blog.prf"  . conf-mode)
	 ("knwl_org.prf"     . conf-mode)
	 ("public_org.prf"   . conf-mode)
	 ("i3config"         . conf-mode)
	 (".i3blocks.conf"   . conf-mode)
	 (".compton.conf"    . conf-mode)
	 (".gitconfig"       . conf-mode)
	 ) auto-mode-alist))

(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode latex-mode scheme-mode python-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-debug-face)

(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(DEBUG\\)" 1 'font-lock-debug-face t)
	   ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
	   ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)

(modify-face 'font-lock-fixme-face "Magenta" nil nil t nil t nil nil)
(modify-face 'font-lock-debug-face "DarkOrange" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "DeepSkyBlue" nil nil t nil t nil nil)

(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c m") 'comment-region)
(global-set-key (kbd "C-c n") 'uncomment-region)
(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-c s") #'my/denote--find-note-by-title)

(load-theme 'spacemacs-dark t)

(setq env/terminal-path (replace-regexp-in-string "\n$" "" (shell-command-to-string "which bash")))

(unless (= (length env/terminal-path) 0)
  (multi-vterm)
  (setq display-line-numbers nil)
  )
