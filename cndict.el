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
(require 'cndict-tongyun-data)

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
		   (string-replace "\n\n  " ""
				   (gethash str cndict-word-dict-table)))
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
		   (format "*“%s”的释义*"
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

(defun cndict-char-pinyin (str)
  "输入字符，返回其读音的列表"
  (hash-table-keys (gethash str cndict-char-dict-table)))

(defconst cndict-tune-name-alist
  '((1 . "阴平")
    (2 . "阳平")
    (3 . "上声")
    (4 . "去声")))

(defun cndict-lastn= (n targ str)
  (condition-case err
      (string-match-p targ (char-to-string (elt str (- (length str) n 1))))
    (t nil)))

(defun cnrhy-pinyin-details (pinyin)
  (let ((tune (cond ((string-match-p "[àòèìùǜ]" pinyin) 4)
		    ((string-match-p "[ǎǒěǐǔǚ]" pinyin) 3)
		    ((string-match-p "[áóéíúǘ]" pinyin) 2)
		    ((string-match-p "[āōēīūǖ]" pinyin) 1)
		    (t 0)))
	(tongyun-rhyme))
    (setq pinyin (replace-regexp-in-string "[āáǎàɑ]" "a" pinyin))
    (setq pinyin (replace-regexp-in-string "[ōóǒò]" "o" pinyin))
    (setq pinyin (replace-regexp-in-string "[ēéěè]" "e" pinyin))
    (setq pinyin (replace-regexp-in-string "[īíǐì]" "i" pinyin))
    (setq pinyin (replace-regexp-in-string "[ūúǔù]" "u" pinyin))
    (setq pinyin (replace-regexp-in-string "[ǖǘǚǜü]" "v" pinyin))
    (setq tongyun-rhyme
	  (cond ((cndict-lastn= 0 "a" pinyin) 1)
		((cndict-lastn= 0 "o" pinyin)
		 (if (cndict-lastn= 1 "a" pinyin)
		     9
		   2))
		((cndict-lastn= 0 "e" pinyin) 3)
		((cndict-lastn= 0 "i" pinyin)
		 (cond ((cndict-lastn= 1 "a" pinyin) 7)
		       ((cndict-lastn= 1 "[ue]" pinyin) 8)
		       (t 4)))
		((cndict-lastn= 0 "u" pinyin)
		 (cond ((cndict-lastn= 1 "[oi]" pinyin) 10)
		       ((cndict-lastn= 1 "[jxqy]" pinyin) 6)
		       (t 5)))
		((cndict-lastn= 0 "v" pinyin) 6)
		((cndict-lastn= 0 "n" pinyin)
		 (if (cndict-lastn= 1 "a" pinyin)
		     11
		   12))
		((cndict-lastn= 0 "[gɡ]" pinyin)
		 (cond ((cndict-lastn= 2 "o" pinyin) 15)
		       ((cndict-lastn= 2 "[ie]" pinyin) 14)
		       (t 13)))
		((cndict-lastn= 0 "r" pinyin) 16)
		(t 17)))
    (cons tune tongyun-rhyme)))

(define-button-type 'cndict-button
  'action #'cndict-button)

(defun cndict-button (button)
  (cndict (buffer-substring
	   (button-get button 'begin)
	   (button-get button 'end))))

(defun cndict-rhyme-insert-tune (rhyme tune)
  (insert
   (format "*** %s, %s\n\n"
	   (alist-get rhyme cndict-tongyun-name-alist)
	   (alist-get tune cndict-tune-name-alist)))
  (mapcar
   #'(lambda (char)
       (let ((name (char-to-string char)))
	 (insert-text-button
	  name
	  'type 'cndict-button
	  'begin (point)
	  'end (1+ (point)))
	 (insert "  ")))
   (gethash tune
	    (gethash rhyme cndict-tongyun-table)))
  (insert "\n\n"))

(defun cndict-rhyme (str)
  (interactive (list (or (funcall region-extract-function nil)
			 (current-kill 0 t))))
  (let* ((char (char-to-string ;; 只要最后一个字
		(aref str (1- (length str)))))
	 (r (ignore-errors
	      (cndict-char-content char))))
    (if r
	(progn
	  (with-temp-buffer-window
	      (format "*“%s”的释义*" char)
	      (list (lambda (_ _)
		      (org-mode)
		      (toggle-word-wrap -1)
		      nil))
	      nil
	    (with-current-buffer standard-output
	      (let ((pys (cndict-char-pinyin char)))
		(insert "* ")
		(insert-text-button char
				    'type 'cndict-button
				    'begin (point)
				    'end (1+ (point)))
		(insert "\n\n")
		(insert (format "- 读音: %s\n\n"
				(mapconcat #'identity pys ", ")))
		(insert (format "- 释义: %s\n\n" (substring r 3)))
		(dolist (py pys)
		  (let* ((pinyin-details (cnrhy-pinyin-details py))
			 (tune (car pinyin-details))
			 (rhyme (cdr pinyin-details)))
		    (when (= tune 0) (setq tune 1)) ;; 轻声按照平声处理
		    (insert
		     (format "** %s, %s, %s\n\n"
			     py
			     (alist-get rhyme cndict-tongyun-name-alist)
			     (alist-get tune cndict-tune-name-alist)))
		    (if (memq tune '(2 4)) ;; 这时只需－1, 下面只需＋1
		        (progn (cndict-rhyme-insert-tune rhyme tune)
			       (cndict-rhyme-insert-tune rhyme (1- tune)))
		      (progn (cndict-rhyme-insert-tune rhyme tune)
			     (cndict-rhyme-insert-tune rhyme (1+ tune))))))))))
      (message "未找到该字"))))
