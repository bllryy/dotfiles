;; This function reloads the file I'm currently reading. This is done
;; to get the latest version of the file quick when I synch files using
;; Dropbox and such services.

(defun refresh-current-file()
  (interactive)
  (let ((current-buffer-name (buffer-name (window-buffer (minibuffer-selected-window))))
	(current-file-name (buffer-file-name (window-buffer (minibuffer-selected-window))))
	)    
    (kill-buffer-if-not-modified current-buffer-name)
    (find-file current-file-name)))

;; This function is used to compress multiple lines of text into a
;; single line and by putting a single white space as separator
;; between the different lines. Taken fromù
;; 
;; - https://stackoverflow.com/questions/8674912/how-to-collapse-whitespaces-in-a-region

(defun compress-lines (begin end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (replace-string "\n" " ")     
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
	(replace-match " ")))))

(defun colorize-compilation-buffer ()
  (require 'ansi-color)
  (ansi-color-apply-on-region compilation-filter-start (point-max)))


(defun my/cmd-to-string (cmd)
  (interactive)
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string cmd)
   )
  )

(defun my/cmd-to-string-as-root (cmd)
  (interactive)
  (with-temp-buffer
    (cd "/sudo::/")
    (replace-regexp-in-string
     "\n$" ""
     (shell-command-to-string cmd)
     )    
    )
  )

(defun my/ask-for-path (label starting-directory)
  (nth 0 (let* ((default-directory starting-directory))
	   (find-file-read-args label t)
	   ))  
  )
