(setq env/terminal-path (replace-regexp-in-string "\n$" "" (shell-command-to-string "which bash")))
(setq env/dotemacs-path (expand-file-name "~/dotfiles/emacs/dotemacs.org"))
(setq env/tool-path (expand-file-name "~/dotfiles/emacs/custom/tool.org"))
(setq env/os-packages-path "/home/leo/utils/emacs/packages/")

;; This can be useful when using the same configuration over different
;; computers, as it allows you to differentiate the execution context
;; based on hardware resources such as CPU model.
(setq env/cpu (shell-command-to-string "cat /proc/cpuinfo | grep 'model name' | uniq | awk -F '[:]' '{ print $2 }' | sed -e 's/^[ \t]*//' | tr -d '\n'"))
(setq env/context
      (cond ((string= env/cpu "Intel(R) Core(TM) i7-2600K CPU @ 3.40GHz") 'PERSONAL-DESKTOP)
	    ((string= env/cpu "Intel(R) Core(TM) i5-7200U CPU @ 2.50GHz") 'PERSONAL-LAPTOP)
	    ((string= env/cpu "11th Gen Intel(R) Core(TM) i7-1165G7 @ 2.80GHz") 'WORK-LAPTOP)
	    ('t "undefined" )))

(setq env/work (eq env/context 'WORK-LAPTOP))
(setq env/home (eq env/context 'PERSONAL-DESKTOP))

(setq agenda-home "agenda-life__life.org")
(setq agenda-work "agenda-work__work.org")

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

(dolist (package (directory-files env/os-packages-path))
  (when (and (not (string= package ".")) (not (string= package "..")))
    (add-to-list 'load-path (concat env/os-packages-path package))
    )
  )

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

(defun my/exwm-init ()
  ;; setup composite transparency and background
  ;; (start-process "" nil "/usr/bin/picom" "-b" "--config" "/home/leo/.config/picom/picom.conf")
  ;; (start-process "" nil "/home/leo/utils/wallpaper")
  
  ;; setup italian layout for keyboard
  (start-process "setxkbmap" nil "/usr/bin/setxkbmap" "-layout" "it")
  )

(defun my/dmenu-starter ()
  "Choose application to run"
  (interactive)
  (let* ((programs (split-string (shell-command-to-string "xstarter -P") "\n" t))
	 (program-to-execute (ivy-read "App to launch: " programs)))
    (start-process "" nil program-to-execute)))

  (defun my/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))

(defun my/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Maze Solver"
     (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line)
     (call-interactively #'exwm-input-release-keyboard)
     )
    ("Emacs"
     (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line)
     (call-interactively #'exwm-input-release-keyboard)
     )
    ("Emulator"
     ;; no matter which sub-buffer, always make the window
     ;; float. 'exwm--id' is a buffer local window that contains the X
     ;; id of the window.
     (exwm-floating--set-floating exwm--id)
     )
    ("app"
     ;; (exwm-floating-toggle-floating)
     )    
    )
  )

  (defun my/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))

(defun my/exwm-update-title ()
  (interactive)
  (pcase exwm-class-name      
    ("burp-StartBurp"
     (exwm-workspace-rename-buffer "burp"))   
    ))

(defun my/exwm-float-and-resize ()
  "Handles floating windows in EXWM. When moving from fixed to
floating, establish a simple 1200x900 floating resolution that
works OK in most cases."
  (interactive)
  (exwm-floating-toggle-floating)  
  (when exwm--floating-frame
    (let* ((current-width (frame-pixel-width))
	   (current-height (frame-pixel-height))
	   (desired-width 500)
	   (desired-height 500)
	   (delta-width (- current-width desired-width))
	   (delta-height (- current-height desired-height))	  
	   )
      (exwm-layout-shrink-window delta-height)
      (exwm-layout-shrink-window-horizontally delta-width)	      
      )
    )  
  )

(use-package exwm
  :straight t
  :config

  ;; without setting heigh icon might be too small to recognize
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (setq exwm-systemtray-height 15)

  (require 'exwm-randr)
  (exwm-randr-enable)

  ;; use mouse to select windows
  (setq mouse-autoselect-window t)
  (setq focus-follows-mouse t)  

  ;; Press Win+2 to spawn workspace in second monitor
  (setq exwm-workspace-number 2)

  ;; EXWM hooks
  (add-hook 'exwm-update-class-hook #'my/exwm-update-class)
  (add-hook 'exwm-update-title-hook #'my/exwm-update-title)
  (add-hook 'exwm-manage-finish-hook #'my/configure-window-by-class)  

  ;; NOTE: At some point EXWM was not sharing screen with second
  ;; monitor when I used the laptop, and by changing this list to
  ;; '(1 "HDMI-1" 2 "eDP-1") it started to work again, so now Im computing
  ;; the list based on the context
  ;;
  (setq exwm-randr-workspace-monitor-plist
	(cond
	 ((eq env/context 'PERSONAL-DESKTOP) '(1 "DVI-D-0" 2 "HDMI-0"))
	 ((eq env/context 'PERSONAL-LAPTOP) '(1 "HDMI-1" 2 "eDP-1"))
	 ((eq env/context 'WORK-LAPTOP) '(1 "HDMI-1" 2 "eDP-1"))
	 ))
  
  ;; these keys should always pass through emacs
  (setq exwm-input-prefix-keys
	'(?\C-x ?\C-u 
		?\C-t ?\C-h
		?\C-p ?\C-n
		?\C-g ?\M-x ?\M-`
		?\M-& ?\M-:
		?\C-\M-j  ;; buffer list
		?\C-\     ;; ctrl+space    
		))
  
  (setq exwm-input-simulation-keys
	'(([?\C-b] . [left])
	  ([?\C-f] . [right])
	  ([?\C-p] . [up])
	  ([?\C-n] . [down])
	  ([?\C-a] . [home])
	  ([?\C-e] . [end])
	  ([?\M-v] . [prior])
	  ([?\C-v] . [next])
	  ([?\C-d] . [delete])
	  ([?\C-k] . [S-end delete])))

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; when switching buffer consider also buffers taken from other workspaces.
  (setq exwm-layout-show-all-buffers t)
  (setq exwm-workspace-show-all-buffers 1)

  ;; Keybinds, changing this after EXWM initializes has no effect
  (setq exwm-input-global-keys
	`(
	  ;; TODO: should I add a different keybind here?
	  ;; one that I can reach with one hand if needed...	  
	  ;; reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
	  ([?\s-r] . exwm-reset)

	  ;; delete window
	  ([?\s-k] . delete-window)

	  ;; move between windows
	  ([s-left] . windmove-left)
	  ([s-right] . windmove-right)
	  ([s-up] . windmove-up)
	  ([s-down] . windmove-down)

	  ;; move window to another workspace
	  ([?\s-m] . exwm-workspace-move-window)

	  ;; launch applications via shell command
	  ([?\s-\ ] . (lambda () (interactive) (mg/consult-xstarter)))

          ;; switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-p] . (lambda () (interactive) (exwm-workspace-switch-create 1)))
          ([?\s-n] . (lambda () (interactive) (exwm-workspace-switch-create 2)))	  

	  ;; start process dmenu-like
	  ([?\s-d] . (lambda () (interactive) (my/dmenu-starter)))
	  ([?\s-q] . (lambda () (interactive) (my/tool)))	  

	  ;; utilities
	  ([?\s-e] . (lambda () (interactive) (start-process "" nil (locate-file "chromium" exec-path exec-suffixes 1))))
	  ([?\s-i] . (lambda () (interactive) (start-process "" nil "/usr/bin/setxkbmap" "it")))
	  ([?\s-u] . (lambda () (interactive) (start-process "" nil "/usr/bin/setxkbmap" "us")))
	  ([?\s-^] . (lambda () (interactive) (start-process "" nil "/usr/bin/shutdown" "now")))
	  ([?\s-?] . (lambda () (interactive) (start-process "" nil "/usr/bin/reboot" "")))
	  ([?\s-j] . (lambda () (interactive) (async-shell-command (format "report %s" (buffer-file-name)))))

	  ))

  ;; set alpha values for showing wallpaper 
  ;;
  ;; just remember to use picom or any compositor to make emacs
  ;; transparent, otherwise this will not be enough
  ;; 
  (set-frame-parameter (selected-frame) 'alpha '(95 .95)) ;; current frame
  (add-to-list 'default-frame-alist '(alpha . (95 . 95))) ;; future frames

  ;; maximize initial frame
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (add-hook 'exwm-init-hook #'my/exwm-init)
  )

(use-package desktop-environment
  :straight t
  :after (exwm)
  :config

  ;; no longer using this, imlemented my own version in
  ;; 'take-screenshot'. Maybe I should remove this package and just roll my own code.
  (exwm-input-set-key (kbd "s-s") #'take-screenshot)
  ;; (setq desktop-environment-screenshot-directory "/home/leo/wa/screenshots/")
  ;; (setq desktop-environment-screenshot-command "maim ~/wa/screenshots/$(date +%Y-%m-%d-%h:%m:%s).png -s")
  ;; (setq desktop-environment-screenshot-partial-command "maim ~/wa/screenshots/$(date +%Y-%m-%d-%h:%m:%s).png -s")
  ;; (setq desktop-environment-screenshot-delay-argument nil)

  (exwm-input-set-key (kbd "s-l") #'desktop-environment-lock-screen)
  (setq desktop-environment-screenlock-command "xsecurelock")
  )

(defun take-screenshot ()
  (interactive)
  (let* ((dir (if (and (boundp 'my/work-current-activity) my/work-current-activity)
		  (concat my/work-current-activity-dir "/data/screens")
		"~/wa/screenshots"))
	 (cmd (concat "maim --noopengl " dir "/$(date +%Y-%m-%d-%h:%m:%s).png -s"))
	 )
    (my/tool-execute-cmd cmd)
    )
  )

(use-package time
  :straight t
  :after (exwm)
  :custom
  (display-time-format "[%d/%b %H:%M]")
  (display-time-use-mail-icon nil)
  ;; (display-time-mail-string "📫")
  ;; (display-time-mail-directory nil)
  ;; (display-time-mail-function
  ;;  (lambda ()
  ;;    (-some-p #'integerp (mapcar
  ;; 			    (lambda (maildir)
  ;; 			      (let ((display-time-mail-directory maildir))
  ;; 				(display-time-mail-check-directory)))
  ;; 			    (file-expand-wildcards "~/Maildir/*/INBOX/new")))))
  
  :config
  (display-time-mode)

  ;; only display battery when using a laptop
  (when (or (eq env/context 'PERSONAL-LAPTOP)
	    (eq env/context 'WORK-LAPTOP))
    (display-battery-mode)  
    ))

;; (use-package bluetooth :straight t )

(use-package ibuffer
  :straight t
  :config
  ;; don't ask for confirmation of "dangerous" operations such as
  ;; deleting buffers
  (setq ibuffer-expert t)

  ;; define a group-organized view where buffers are organized into
  ;; groups depending on whether they match a given regex pattern or
  ;; not. This structure is dynamically modified by the function
  ;; 'work/compute-ibuffer-group' using information taken from the
  ;; list of currently active buffers.
  (setq default-ibuffer-saved-filter-groups
	(quote (("default"
		 ("org" (mode . org-mode))
		 ("chromium" (name . "^Chromium"))	       
		 ("vterminal" (name . "^\\*vterminal"))
		 ;;
		 ;; TODO: learn how to recognize when a buffer is an
		 ;; external application handled by EXWM
		 ;; 
		 ("exwm" (mode . exwm-mode))
		 ("erc" (mode . erc-mode))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")))	       
		 ))))

  ;; items for each group are sorted alphabetically using the buffer name
  (setq ibuffer-default-sorting-mode 'alphabetic)

  ;; as soon as you enter or refresh ibuffer, switch to a
  ;; group-organized view using a group configuration computed on the
  ;; fly depending on currently open buffers.
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (when env/home
		(setq ibuffer-saved-filter-groups (home/compute-ibuffer-group))
		)
	      (when env/work
		(setq ibuffer-saved-filter-groups (work/compute-ibuffer-group))
		)	      
	      (ibuffer-switch-to-saved-filter-groups "default"))
	    )
  )

(use-package beacon
  :straight t
  :config
  (setq beacon-size 10)
  (beacon-mode 1))

(use-package popwin
 :ensure t
 :config
 (popwin-mode 1)
 )

(use-package spacemacs-theme
  :straight t
  :init
  (setq spacemacs-theme-comment-bg nil)
  )

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  
  ;; At some point it kept giving me the following error
  ;;
  ;; Error during redisplay: (eval (doom-modeline-segment--time)) signaled (error "Invalid image type ‘svg’") [7 times]
  ;;
  ;; I fixed it by following the tip from
  ;; https://emacs.stackexchange.com/questions/74289/emacs-28-2-error-in-macos-ventura-image-type-invalid-image-type-svg
  (add-to-list 'image-types 'svg)
  )

(use-package gnu-elpa-keyring-update
  :straight t
  )

(use-package pinentry
  :straight t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package projectile
  :straight t
  :config (projectile-mode)
  :bind-keymap
  ;; all projectile-related keybinds start from the same root. 
  ("C-c p" . projectile-command-map)
  :init
  ;; for now I want to keep track of two main types of projects: my
  ;; personal programming project and the work activities.
  (setq projectile-project-search-path
	'("/home/leo/projects/PROGRAMMING/"
	  "/home/leo/projects/YOUTUBE/"
	  )
	)
  ;; the first thing we want to do when switching project is to open
  ;; the dired buffer within the project folder.
  (setq projectile-switch-project-action #'projectile-dired) 
  )

(use-package magit
  :straight t
  :bind (("C-x g" . 'magit-status))
  )

(use-package docker
  :straight t
  :config
  (setq docker-container-default-sort-key '("Status" . t))
  )

(use-package devdocs
  :straight t
  )

(use-package lsp-mode
  :straight t
  :commands lsp
  :custom

  ;; what to use when checking on-save. "check" is default, I prefer clippy
  ;; (lsp-rust-analyzer-cargo-watch-command "check")
  
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)

  ;; inlay hints stuff
  (lsp-inlay-hint-enable t)

  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :custom  
  (lsp-signature-render-documentation nil)  
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil)
  )

(use-package dap-mode
  :straight t
  :config
  (dap-register-debug-template
   "Node Docker Debugging"
   (list :type "node"
	 :program "__ignored"
	 :protocol "inspector"

         :port "9229"
         :name "Node Docker Debugging"
	 :localRoot "/home/leo/projects/PROGRAMMING/backend/"
	 :remoteRoot "/app"
	 :sourceMaps t
	 ))
  )

(add-hook 'js-ts-mode-hook
	  (lambda ()
	    (require 'dap-node)
	    (dap-node-setup)
	    ))

(use-package php-mode
  :straight t
  )

(use-package typescript-ts-mode
  :straight t
  )

(use-package ztree
  :straight t
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-keep-variants nil)
  (setq ztree-diff-additional-options '("-w" "-i"))
  )

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")
	(java "https://github.com/tree-sitter/tree-sitter-java")
	)
      )

(use-package tree-sitter
  :straight t
  :config
  ;; activate tree-sitter on any buffer containing code for which it
  ;; has a parser available
  (global-tree-sitter-mode)
  ;; for some reason it did not recognize typescript-ts-mode within
  ;; the list, so I had to add it myself to make 'tree-sit-hl-mode'
  ;; work.
  (add-to-list 'tree-sitter-major-mode-language-alist
	       '(typescript-ts-mode . typescript)
	       )
  (add-to-list 'tree-sitter-major-mode-language-alist
	       '(c-ts-mode . c)
	       )
  ;; enable syntax highlighting using treesitter AST
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

;; (use-package treesit-auto
;;   :straight t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;; (global-treesit-auto-mode)
;;   )

;; (use-package restclient
;;   :straight t
;;   )

(use-package request
  :straight t
  )

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs (list "~/dotfiles/emacs/snippets"))
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

(use-package ivy-prescient
  :straight t
  :config
  (ivy-prescient-mode 1)
  (setq prescient-filter-method 'regexp)
  )

(use-package ivy-posframe
  :straight t
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (setq ivy-posframe-border-width 2)
  ;; 
  ;; (setq ivy-posframe-parameters
  ;; 	'((left-fringe . 5)
  ;;         (right-fringe . 5)))
  ;; 
  ;; inherit stile from default one, this is useful so that later when
  ;; we apply the spacemacs theme it also gets applied to ivy-posframe
  ;; buffer  
  (put 'ivy-posframe 'face-alias 'default)  
  (ivy-posframe-mode 1)  
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
  (add-hook 'org-mode-hook (lambda () (yas-minor-mode) (yas-reload-all)))
  )

;; Full list of icons:
;; ("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥")

;; (use-package org-modern
;;   :straight t
;;   :config
;;   (with-eval-after-load 'org (global-org-modern-mode))
;;   (setq org-modern-fold-stars
;; 	'(("†" . "†") ("☥" . "☥") ("✚" . "✚") ("✤" . "✤") ("✤" . "✤")))
;;   (setq org-modern-todo-faces
;; 	(quote
;; 	 (
;; 	  ("DONE" :background "gray20" :foreground "white")
;; 	  ("PREPARE" :background "indian red" :foreground "black")	  
;; 	  ("FINISH" :background "orange" :foreground "black")
;; 	  ("PUBLISH" :background "light green" :foreground "black")
;; 	  )))  
;;   )

;;    (use-package org-noter
;;    :straight t
;;    :config
;;    (setq org-noter-always-create-frame nil)
;; )

(use-package org-capture
  :defer t
  :bind (("C-c c" . org-capture))

  :config
  (setq org-capture-templates 
	`(
	  ;; This is used to add new entry to the org calendar
	  ("cl" "Add life calendar" entry
	   (file ,agenda-home)
	   "* %^{Title}\nSCHEDULED: %^t\n%?"
	   :prepend t
	   )
	  ("cw" "Add work calendar" entry
	   (file ,agenda-work)
	   "* %^{Title}\nSCHEDULED: %^t\n%?"
	   :prepend t
	   )	  
	  )
	)
  )

(use-package visual-fill-column
  :straight t
  :config
  (setq visual-fill-column-width 110
	visual-fill-column-center-text t)
  )

(use-package org-present
  :straight t
  :config
  (defun my/org-present-start ()
    ;; Center the presentation and wrap lines
    (visual-fill-column-mode 1)
    (visual-line-mode 1))

  (defun my/org-present-end ()
    ;; Stop centering the document
    (visual-fill-column-mode 0)
    (visual-line-mode 0))

  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
  )

(use-package ox-reveal
  :straight t
  :config
  (setq org-reveal-mathjax t)
  (setq org-reveal-root "https://archive.leonardotamiano.xyz/misc/reveal/")
  )

(use-package htmlize
  :straight t
  )

(use-package emojify
  :straight t
  )

(use-package ox-hugo
  :straight t
  :after ox
  )

(straight-use-package
 '(ox-zola :host github :repo "gicrisf/ox-zola"
   :files (:defaults "*.el" "backend" "stylesheets")
   :includes ox-hugo))

(require 'ox-zola)

(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"  
  :magic ("%PDF" . pdf-view-mode)
  :straight t
  :config
  (pdf-tools-install)

  ;; taken from http://pragmaticemacs.com/emacs/more-pdf-tools-tweaks/
  (setq-default pdf-view-display-size 'fit-width)  
  ;; turn off cua so copy works
  ;; (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  
  (setq pdf-annot-default-markup-annotation-properties nil)

  (setq pdf-annot-default-annotation-properties
	'((t
	   (label . ""))
	  (text
	   (color . "yellow")
	   (icon . "Note"))
	  (highlight
	   (color . "MediumPurple1"))
	  (underline
	   (color . "blue"))
	  (squiggly
	   (color . "orange"))
	  (strike-out
	   (color . "red"))))


  :bind*(:map pdf-view-mode-map
	      ("h" . pdf-annot-add-highlight-markup-annotation)
	      ("t" . pdf-annot-add-text-annotation)
	      ("D" . pdf-annot-delete))
  )

     ;; (use-package pdf-view-restore
     ;;   :after pdf-tools
     ;;   :straight t
     ;;   :config
     ;;   (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

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

(use-package rainbow-mode :straight t )

(use-package tramp-term :straight t )

(use-package expand-region :straight t )

(use-package which-key
  :straight t
  :config
  (which-key-mode)
  )

(use-package keepass-mode
  :straight t
  :config
  )

;; (load-file "/home/leo/dotfiles/emacs/packages/websocket.el")

;; (add-to-list 'load-path "/home/leo/dotfiles/emacs/packages/calfw")
;; (require 'calfw-cal)
;; (require 'calfw-ical)
;; (require 'calfw-org)

(load-file "~/dotfiles/emacs/custom/conversion.el")
(load-file "~/dotfiles/emacs/custom/chromium.el")
(load-file "~/dotfiles/emacs/custom/system.el")
(load-file "~/dotfiles/emacs/custom/tool.el")
(load-file "~/dotfiles/emacs/custom/vterm.el")
(load-file "~/dotfiles/emacs/custom/notes.el")
(load-file "~/dotfiles/emacs/custom/misc.el")
(load-file "~/dotfiles/emacs/custom/mobile.el")

(add-to-list 'load-path "~/projects/PROGRAMMING/youtube.el/src/")
(when (locate-library "youtube")
  (require 'youtube)
  )

(add-to-list 'load-path "~/projects/PROGRAMMING/pwndoc.el/src/")
(when (locate-library "pwndoc")
  (require 'pwndoc)
  (setq pwndoc--curl-options '("-k"))
  )

(add-to-list 'load-path "~/projects/PROGRAMMING/wapt.el/src/")
(when (locate-library "wapt")
  (require 'wapt)
  
  (setq wapt--var-public-directory "~/projects/PROGRAMMING/wapt.el/")
  (setq wapt--var-public-mitmproxy-binary-path "~/.local/bin/mitmdump")
  (setq wapt--defaults
	'((:project . "tmp")
	  (:proxy . "127.0.0.1:7070")
	  (:layout . "minimal")
	  (:ws-port . "3000")
	  ))
  )

(add-to-list 'load-path "~/projects/GIT-PUBLIC/password-manager.el/src/")
(require 'password-manager)

(when env/work
  (setq bitwarden/binpath "/snap/bin/bw")
  )

(when env/home
  (load-file "~/dotfiles/emacs/custom/home.el")
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

(add-hook 'org-mode-hook
	  (lambda ()
            (when (or (equal (buffer-file-name) env/dotemacs-path)
		      (equal (buffer-file-name) env/tool-path))
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

(global-set-key (kbd "C-c a") 'refresh-current-file)
(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c w") 'compress-lines)
(global-set-key (kbd "C-c m") 'comment-region)
(global-set-key (kbd "C-c n") 'uncomment-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c f") #'my/grep)

(define-key c++-mode-map (kbd "C-c C-c") 'compile)

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c d") #'docker)
(global-set-key (kbd "C-x g") #'magit-status)

(global-set-key (kbd "C-c r") #'exwm-randr-refresh)
(global-set-key (kbd "C-c e") #'my/exwm-float-and-resize)

(global-set-key (kbd "C-c s") #'my/denote--find-note-by-title)

(load-theme 'spacemacs-dark t)

(find-file env/dotemacs-path)

(unless (= (length env/terminal-path) 0)
  (multi-vterm)
  (setq display-line-numbers nil)
  )
