(defun my/txt2hex (beg end)
  "opens new buffer *out* which contains the selected text as
well as the hexadecimal bytes of the text."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if (and beg end)
      (progn
	(let ((word (buffer-substring-no-properties beg end)))
	  (switch-to-buffer-other-window "*out*")
	  (erase-buffer)	  

	  ;; -- first, for each word
	  (dotimes (i (length word))
	    ;; -- write char and hex
	    (insert (format "%c -> 0x%X\n" (aref word i) (aref word i))))
	  
	  (insert "\n")
	  
	  ;; -- then, write each hex byte in a row
	  (insert "0x")
	  (dotimes (i (length word))
	    (insert (format "%X" (aref word i))))

	  ))))

(defun int2bin (i)
  (defun int2bin-rec (i res)
    (cond ((= i 0) (concat "0" res))
	  ((= i 1) (concat "1" res))
	  ('t
	   (int2bin-rec (lsh i -1)
			(concat (format "%s" (logand i 1))
				res)))))
  (int2bin-rec i ""))

(defun my/txt2bin (beg end)
  "opens new buffer *out* which contains the selected text as
  well as the binary representation of the byte used to encode
  each character in the text"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if (and beg end)
      (progn
	(let ((word (buffer-substring-no-properties beg end)))
	  (switch-to-buffer-other-window "*out*")
	  (erase-buffer)

	  ;; -- first, print word -> bin
	  (dotimes (i (length word))
	    (let* ((no-padding-bin-str (int2bin (aref word i)))
		   (padded-bin-str
		    (concat (make-string (- 8 (length no-padding-bin-str)) ?0) no-padding-bin-str)))
	      (insert (format "%c -> %s\n" (aref word i) padded-bin-str))))

	  (insert "\n")

	  ;; -- then, print each bin in a sequence
	  (dotimes (i (length word))
	    (let* ((no-padding-bin-str (int2bin (aref word i)))
		   (padded-bin-str
		    (concat (make-string (- 8 (length no-padding-bin-str)) ?0) no-padding-bin-str)))
	      (insert (format "%s " padded-bin-str))))))))

(defun my/invert-endianness (beg end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))

  (defun invert-word (word)
    (let ((word-length (length word))
	  (i 0)
	  (result nil))
      (while (< (+ i 1) word-length)
	(setq result (cons
		      (format "%c%c" (aref word i) (aref word (+ i 1)))
		      result))
	(setq i (+ i 2))
	)

      (insert (format "%s\n" (string-join result "")))
      )
    )
  
  (if (and beg end)
      (progn
	(let ((region (buffer-substring-no-properties beg end)))
	  (switch-to-buffer-other-window "*out*")
	  (erase-buffer)

	  (mapcar (lambda (element)
		    (invert-word element))
		  (split-string region))

	  ))))
