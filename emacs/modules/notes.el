;; Search ~denote~ notes by title. Use common structure of filenames to
;; extract name for each notes and let user select which note to open
;; with ~ivy~.

(defun my/denote--find-note-by-title ()
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
