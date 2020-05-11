;;; pp-html-parse.el --- parse all logic tags in sexp.
;;; -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 1.0.0
;; Keywords: parse logic
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: http://github.com/Kinneyzhang/pp-html
;; Package-Requires: ((emacs "26.3") (dash "2.16.0))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;;; Code:
(require 'dash)
(require 'pp-html-utils)
(require 'pp-html-eval)

(defvar pp-html-logic-element-list
  '(:assign :include :if :unless :for :cond :extend)
  "Supported pp-html logic element list.")

(defun pp-html-sexp-replace (old new sexp)
  "Replace all occurences of OLD by NEW in SEXP."
  (let ((lst)
	(el))
    (--tree-map-nodes
     (when (pp-html--var-p it)
       (setq lst (split-string (pp-html--symbol-rest it) "[.]"))
       (setq el (intern (nth 0 lst)))
       (equal el old))
     (if (= 1 (length lst))
	 (progn
	   (eval `(setq ,el ,new))
	   (-setq it (pp-html-eval it)))
       (eval `(setq ,el ',new))
       (-setq it (pp-html-eval it)))
     sexp)))

(defun pp-html--process-logic-assign (sexp)
  "Process :assign logic"
  (let ((alist (pp-html--plist->alist (cdr sexp))))
    (dolist (lst alist)
      (let ((var (car lst))
	    (val (pp-html-eval (cadr lst))))
	(set var val)))))

(defun pp-html--process-logic-include (sexp)
  "Process :include logic."
  (pp-html-eval (cadr sexp)))

(defun pp-html--process-logic-if (sexp)
  "Process :if logic."
  (if (pp-html-eval (nth 1 sexp))
      (pp-html-eval (nth 2 sexp))
    (if (nth 3 sexp)
	(pp-html-eval (nth 3 sexp)))))

(defun pp-html--process-logic-unless (sexp)
  "Process :unless logic."
  (if (not (pp-html-eval (nth 1 sexp)))
      (pp-html-eval (nth 2 sexp))
    (if (nth 3 sexp)
	(pp-html-eval (nth 3 sexp)))))

(defun pp-html--process-logic-cond (sexp)
  "Process :cond logic."
  (let ((len (length sexp))
	(cases (cdr sexp))
	(i 0)
	(res))
    (if (= 0 (% len 2))
	(error "error pp-html :cond syntax!"))
    (catch 'break
      (while (nth i cases)
	(when (pp-html-eval (nth i cases))
	  (setq res (pp-html-eval (nth (1+ i) cases)))
	  (throw 'break res))
	(incf i 2)))))

(defun pp-html--process-logic-for (sexp)
  "Process :for logic."
  (let ((res)
	(el (pp-html-eval (nth 1 sexp)))
	(in (nth 2 sexp))
	(lst (pp-html-eval (nth 3 sexp)))
	(target (pp-html-eval (nth 4 sexp)))
	(len (length sexp)))
    (if (and (= len 5) (eq 'in in))
	(setq res (mapcar (lambda (x)
			    (pp-html-sexp-replace el x target))
			  lst))
      (error "error pp-html :for syntax!"))))

(defun pp-html--process-logic-extend (sexp)
  "Process :extend logic."
  (let* ((base-str (prin1-to-string (pp-html-parse (cadr sexp))))
	 (blocks (cddr sexp))
	 (extend-str base-str)
	 (point))
    (when blocks
      (with-temp-buffer
	(emacs-lisp-mode)
	(insert base-str)
	(setq point (goto-char (point-min)))
	(while point
	  (dolist (item blocks)
	    (let* ((block-name (symbol-name (cadr item)))
		   (extend-block (cadr (cdr item)))
		   (extend-block-str (prin1-to-string (pp-html-eval extend-block))))
	      (setq point (re-search-forward ":block" nil t))
	      (skip-chars-forward "[\" \"\n\t\r]")
	      (when (string= block-name (thing-at-point 'symbol))
		(forward-symbol 1)
		(skip-chars-forward "^(")
		(let ((base-block-str (thing-at-point 'sexp)))
		  (skip-chars-backward "^(")
		  (backward-char)
		  (kill-sexp)
		  (if extend-block
		      (insert extend-block-str)
		    (insert base-block-str)))))
	    (setq extend-str (buffer-string))))))
    (read extend-str)))

(defun pp-html--process-logic (sexp)
  (--tree-map-nodes
   (member (car-safe it) pp-html-logic-element-list)
   (-setq it
     (funcall (read (concat "pp-html--process-logic-" (pp-html--symbol-rest (car-safe it)))) it))
   sexp))

;;;###autoload
(defun pp-html-parse (sexp)
  "Process all logic element."
  (let* ((sexp (pp-html-eval sexp))
	 (new-sexp (pp-html--process-logic sexp)))
    (if (not (equal sexp new-sexp))
	(pp-html-parse new-sexp)
      sexp)))

;; ;;;###autoload
;; (defun pp-html-parse-test (sexp)
;;   (ignore-errors (kill-buffer "*pp-html-parse-test*"))
;;   (with-current-buffer (get-buffer-create "*pp-html-parse-test*")
;;     (emacs-lisp-mode)
;;     (insert (format "%S" (pp-html-parse sexp)))
;;     (goto-char (point-min))
;;     (forward-char)
;;     (setq pos (point))
;;     (while pos
;;       ;; (setq old-pos pos)
;;       (setq pos (re-search-forward "(" nil t))
;;       (backward-char)
;;       (newline)
;;       (forward-char))
;;     (indent-region (point-min) (point-max)))
;;   (view-buffer "*pp-html-parse-test*" 'kill-buffer))

(provide 'pp-html-parse)
;;; pp-html-parse.el ends here
