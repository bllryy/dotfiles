;; This function splits the window to create space for a little vterm
;; buffer.

(defun my/vterm-window-split ()
  (interactive)
  (split-window-below -15)
  (other-window 1)
  (switch-to-buffer "*vterminal<1>*"))

;; This function copies the output of the last command in vterm.
(setq my/prompts '("\\[leo@archlinux .*\\]\\$ "
		   "root@L:.*# "
		   "leo@kali:.*\\$ " 
		   ))    


;; (defun my/vterm-copy-command ()
;;   (interactive)
;;   (vterm-copy-mode)
;;   (dolist (prompt my/prompts)
;;     (when (re-search-backward prompt nil 't 1)
;;       (setq my-max-point (point))
;;       (re-search-backward prompt nil nil 1)
;;       (setq my-min-point (point))

;;       (kill-new
;;        (let* ((string (buffer-substring my-min-point my-max-point))
;; 	      (len (length string))
;; 	      (current-buffer-name (buffer-name (window-buffer (minibuffer-selected-window)))))

;; 	 (switch-to-buffer "*commands-history*" 't)

;; 	 (insert string)
;; 	 (backward-char len)
;; 	 ;; need to add `* ` at the start of the string
;; 	 (insert "* ")
;; 	 ;; then `#+begin_example` at the start of next line
;; 	 (end-of-line)
;; 	 (insert "\n")
;; 	 (insert "#+begin_example")
;; 	 (insert "\n")
;; 	 ;; then  `#+end_example` at the last line
;; 	 (end-of-buffer)
;; 	 (insert "\n")
;; 	 (insert "#+end_example")
;; 	 (insert "\n")

;; 	 (switch-to-buffer current-buffer-name)

;; 	 string))

;;       (vterm-copy-mode)
;;       (execute-kbd-macro (kbd "<return>"))
;;       (setq kill-ring (cdr kill-ring))
;;       (setq kill-ring-yank-pointer kill-ring)
;;       nil)))

;; I have updated the version and now it's able to recognize the
;; command and put it under the correct org-mode tag.

(defun my/vterm-copy-command ()
  (interactive)
  (vterm-copy-mode)
  (dolist (prompt my/prompts)
    (when (re-search-backward prompt nil 't 1)
      (setq my-max-point (point))
      (re-search-backward prompt nil nil 1)
      (setq my-min-point (point))

      (kill-new
       (let* ((string (buffer-substring my-min-point my-max-point))
	      (len (length string))
	      (current-buffer-name (buffer-name (window-buffer (minibuffer-selected-window)))))

	 (switch-to-buffer "*tmp-cmd*" 't)
	 
	 (insert string)
	 (backward-char len)
	 (insert "*** ")
	 (end-of-line)
	 (insert "\n#+begin_example\n")
	 (end-of-buffer)
	 (insert "\n#+end_example\n")
	 
	 (let* ((cleaned_string (buffer-substring
				 (progn
				   (beginning-of-buffer)
				   (point)
				   )
				 (progn
				   (end-of-buffer)
				   (point)
				   )))				 
		(cmd_name (progn
			    (beginning-of-buffer)
			    (re-search-forward prompt)
			    (thing-at-point 'word' 'no-properties))))
	   (switch-to-buffer "*commands-history*" 't)
	   (org-mode)

	   (if (search-forward (concat "** " cmd_name "\n") nil 't 1)
	       (progn
		 (insert cleaned_string)
		 )
	     (progn
	       (insert (concat "** " cmd_name "\n"))
	       (insert cleaned_string)
	       ))
           
	   (kill-buffer "*tmp-cmd*")
	   (beginning-of-buffer)
	   (switch-to-buffer current-buffer-name)
	   cleaned_string
	   )))
      
      (vterm-copy-mode)
      (execute-kbd-macro (kbd "<return>"))
      (setq kill-ring (cdr kill-ring))
      (setq kill-ring-yank-pointer kill-ring)
      nil)))

;; keybinds
(global-set-key (kbd "C-c l") 'multi-vterm)
(global-set-key (kbd "C-c v") 'my/vterm-window-split)
(global-set-key (kbd "C-c o") 'my/vterm-copy-command)
