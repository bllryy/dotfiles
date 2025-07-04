(defun home/tool-wrapper ()
  "Wrapper on the tool script"
  (interactive)
  (shell-command (concat "tool " (read-from-minibuffer "tool: "))))

(defun home/clean-buffers ()
  "Remove all buffers but slack, minded.org and a vterminal"
  (interactive)
  (let ((buffers (buffer-list)))
    (mapcar
     (lambda (buff)
       (unless (member (format "%s" buff) '("*vterminal<1>*" "*scratch*"))
	 (kill-buffer buff)))
     buffers)))

(defun home/filename ()
  "Copies into the kill-ring the full path of the current file"
  (interactive)
  (kill-new (buffer-file-name))
  )

;; Code used to update ibuffer menu with custom filter based on
;; currently opened activities in the buffers list. We iter through
;; the buffer list in order to extract the names of the activities,
;; and then we create a custom filter where at the top of the ibuffer
;; we list out all the different activities with the files organized
;; for each activity.

(defun home/compute-ibuffer-group ()

  ;; list out the names of activity based on currently opened buffers
  (defun get-buffers ()
    (delete-dups
     (remove "" 
	     (remove nil
		     (mapcar
		      (lambda (buffer)
			(with-current-buffer buffer
			  (let* ((directory (split-string default-directory "/"))
				 (is-minded? (and (> (length directory) 6)
						  (string-equal (nth 4 directory) "PROGRAMMING"))))
			    (when is-minded?
			      (nth 5 directory)
			      )
			    )
			  )
			)
		      (buffer-list))
		     )
	     )
     ))

  ;; escape certain characters in order to have proper regex within
  ;; the group configuration
  ;; 
  (defun prepare-name (string)
    (concat ".*" (s-replace "+" "\\+" string) ".*")
    )

  ;; compute the new group filter by appending the new filters
  ;; computed based on activity names and the default filters saved
  ;; within the 'default-ibuffer-saved-filter-groups' variable.
  ;; 
  (let* ((new-filters (mapcar (lambda (x) `(,x (filename . ,(prepare-name x)))) (get-buffers)))
	 (default-filters (cdar default-ibuffer-saved-filter-groups))
	 (new-default-group `( ,(append '("default") new-filters default-filters))))
    new-default-group
    )
  )

;; Search ~denote~ notes by title. Use common structure of filenames to
;; extract name for each notes and let user select which note to open
;; with ~ivy~.

(defun home/denote--find-note-by-title ()
  (interactive)
  (let* ((dir-files (directory-files denote-directory))
	 (note-titles (remove
		       nil
		       (mapcar
			(lambda (entry)
			  (let* ((denote-title (nth 0 (split-string (format "%s" (nth 1 (split-string entry "--"))) "__")))
				 (denote-path (concat denote-directory "/" entry)))			    
			    (if (or (string-equal entry ".") (string-equal entry ".."))
				nil
			      `(,denote-title . ,denote-path))
			    )
			  )
			dir-files)))
	 (selected-title (ivy-read "Title: " note-titles))
	 (selected-path (assoc-default selected-title note-titles))
	 )
    (find-file selected-path)
    )
  )
