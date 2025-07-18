;; This function is used to select one ip from a list of such ips
;; taken with the function 'list-of-IPs'.

(defun ip()
  (interactive)
  (ivy-read "Choose IP: " (list-of-IPs) :action (lambda (x) (kill-new x))))

;; This function returns a list of all the IPv4 addresses of the
;; interfaces activated on the host machine.
(require 'subr-x)

(defun list-of-IPs()
  (interactive)
  (shell-command "ip a")
  (switch-to-buffer "*Shell Command Output*")
  (let ((done nil)
	(start-p -1)
	(end-p -1)
	(list-of-ips nil))

    (while (not done)
      (condition-case ex
	  (search-forward "inet ")
	('error (setq done 't)))

      (when (not done)
	(setq start-p (point))
	(search-forward "/")
	(setq end-p (- (point) 1))
	;; (message "%s" (buffer-substring start-p end-p))
	(setq list-of-ips (cons (string-trim (buffer-substring start-p end-p))
				list-of-ips))))
    (kill-current-buffer)
    (delete-other-windows)
    list-of-ips))
