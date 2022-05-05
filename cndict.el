(require 'cndict-char-data)
(require 'cndict-word-data)

(defvar cndict-short-length 100)

(defun cndict-char-content-detail (str)
  (let ((table (gethash str cndict-char-dict-table))
	(r (format "* %s \n\n" str)))
    (maphash
     #'(lambda (pinyin expl)
	 (setq r
	       (concat
		r
		(format
		 "- %s: %s\n\n"
		 pinyin
		 (let ((num 1)
		       (s ""))
		   (maphash
		    #'(lambda (content detail)
			(setq s (concat
				 s "\n\n  " (number-to-string num) ". "
				 content "; "
				 (when detail
				   (concat
				    "\n     "
				    (string-join detail "; "))))
			      num (1+ num)))
		    expl)
		   s)))))
     table)
    r))

(defun cndict-char-content (str)
  (let ((table (gethash str cndict-char-dict-table))
	(r (format "* %s " str)))
    (maphash
     #'(lambda (pinyin expl)
	 (setq r (concat
		  r
		  (format "%s: %s| "
			  pinyin
			  (let* ((num 0)
				 (contents (hash-table-keys expl))
				 (l (max (/ (- cndict-short-length
					       8 (length pinyin))
					    (length contents))
					 20)))
			    (mapconcat
			     #'(lambda (cont)
				 (setq num (1+ num))
				 (concat
				  (number-to-string num) ". "
				  (if (< (length cont) l)
				      cont
				    (concat (substring cont 0 l)
					    "..."))
				  "; "))
			     contents ""))))))
     table)
    (if (> (length r) cndict-short-length)
	(concat (substring r 0 (- cndict-short-length 3)) "...")
      r)))

(defun cndict-minibuffer (str)
  (interactive (list (funcall region-extract-function nil)))
  (let ((r (or (ignore-errors
		   (string-replace "\n\n  " "" (gethash str cndict-word-dict-table)))
	       (ignore-errors
		 (cndict-char-content
		  (char-to-string (aref str (1- (length str))))))
	       "未找到该词")))
    (message r)))

(defun cndict (str)
  (interactive (list (funcall region-extract-function nil)))
  (let ((r (or (gethash str cndict-word-dict-table)
	       (ignore-errors
		 (cndict-char-content-detail
		  (char-to-string (aref str (1- (length str)))))))))
    (if r
	(progn (with-temp-buffer-window
		   (format "“%s”的释义*"
			   (substring
			    r 2
			    (progn (string-match " \n\n" r)
				   (match-beginning 0))))
		   (list (lambda (_ _) (org-mode) (toggle-word-wrap -1) nil))
		   nil
		 (with-current-buffer standard-output
		   (insert r))))
      (message "未找到该词"))))

(provide 'cndict)
