;;; cndict.el --- Chinese Dictionary -*- lexical-binding: t -*-

;; Copyright (C) 2022 Rosario S.E.

;; Author: Rosario S.E. <ser3vau@gmail.com>
;; URL: https://github.com/3vau/cndict

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; cndict - Chinese Dictionary.
;;
;; It used dictionary data from https://github.com/mapull/chinese-dictionary,
;; Thanks to it's author.
;;
;; Thanks to @manateelazycat(https://emacs-china.org/u/manateelazycat/)
;; for inspiration.
;;
;; Thanks to all my friends in https://emacs-china.org

;;; Code:

(require 'cndict-char-data)
(require 'cndict-word-data)

(defun cndict-char-pinyin (str)
  "输入字符，返回其读音的列表"
  (hash-table-keys (gethash str cndict-char-dict-table)))

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
  "查询选中字词或上一个 kill-ring 记录的字词，通过 minibuffer 输出简短的结果。"
  (interactive (list (or (funcall region-extract-function nil)
			 (current-kill 0 t))))
  (let ((r (or (ignore-errors
		   (string-replace "\n\n  " "" (gethash str cndict-word-dict-table)))
	       (ignore-errors
		 (cndict-char-content
		  (char-to-string (aref str (1- (length str))))))
	       "未找到该词")))
    (message r)))

(defun cndict (str)
  "查询选中字词或上一个 kill-ring 记录的字词，使用临时 buffer 输出完整的结果。"
  (interactive (list (or (funcall region-extract-function nil)
			 (current-kill 0 t))))
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

;;; cndict.el ends here
