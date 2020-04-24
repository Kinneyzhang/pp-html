;;; pp-html.el --- Pretty print html using sexps with the function `pp-html'. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang, all rights reserved.

;; Author: Kinneyzhang <kinneyzhang666@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs"26.3"))
;; URL: https://github.com/Kinneyzhang/pp-html.el
;; Keywords: html, list

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a utility for pretty print html with emacs sexps.
;; The function `pp-html' allows you to print a complex
;; html page by using simple elisp list form. Some stuff link Django's
;; template tag feature are also added to elisp lisp when print.

;;; Code:
(require 'dash)
(require 'web-mode)
(require 'pp-html-filter)

(defvar pp-html-html5-element-list
  '("html"
    "head" "title" "base" "link" "meta" "style"
    "script" "noscript" "template"
    "body" "section" "nav" "article" "aside" "h1" "h2" "h3" "h4" "h5" "h6" "header" "footer" "address" "main"
    "p" "hr" "pre" "blockquote" "ol" "ul" "li" "dl" "dt" "dd" "figure" "figcaption" "div"
    "a" "em" "strong" "small" "s" "cite" "q" "dfn" "abbr" "data" "time" "code" "var" "samp" "kbd" "sub" "sup" "mark" "ruby" "rt" "rp" "bdi" "bdo" "span" "br" "wbr"
    "ins" "del"
    "img" "iframe" "embed" "object" "param" "video" "audio" "source" "track" "canvas" "map" "area" "svg" "math"
    "table" "caption" "colgroup" "col" "tbody" "thead" "tfoot" "tr" "td" "th"
    "form" "fieldset" "legend" "label" "input" "button" "select" "datalist" "optgroup" "option" "textarea" "output" "progress" "meter"
    "details" "summary" "menuitem" "menu")
  "html5 element list, from https://developer.mozilla.org/zh-CN/docs/Web/Guide/HTML/HTML5/HTML5_element_list")

(defvar pp-html-empty-element-list
  '("area" "base" "br" "col" "colgroup" "embed" "hr" "img" "input" "link" "meta" "param" "source" "track" "wbr")
  "Html5 empty element list, from https://developer.mozilla.org/zh-CN/docs/Glossary/空元素")

(defvar pp-html-logic-element-list
  '(:assign :include :if :unless :for :cond :extend)
  "Supported pp-html logic element list.")

(defvar pp-html-html-doctype-param '("html")
  "Html doctype.")

(defvar pp-html-xml-p nil
  "Whether to print xml.")

(defun pp-html--symbol-initial (sym)
  "Get the initial charactor of a symbol."
  (if (symbolp sym)
      (substring (symbol-name sym) 0 1)))

(defun pp-html--symbol-rest (sym)
  "Get the rest part of symbol."
  (if (symbolp sym)
      (substring (symbol-name sym) 1)))

;; pp-html evaluation
(defun pp-html--var-p (el)
  "Jude if a element is a variable."
  (if (symbolp el)
      (if (string= "$" (pp-html--symbol-initial el)) t nil)
    nil))

(defun pp-html--func-p (el)
  "Jude if a element is a function."
  (if (listp el)
      (if (eq '$ (car el)) t nil)
    nil))

(defun pp-html--func-param-eval (params)
  "Evaluate function params in pp-html."
  (-tree-map
   (lambda (p)
     (if (pp-html--var-p p)
	 (setq p (pp-html--eval p))
       (setq p p)))
   params))

(defun pp-html--func-eval (el)
  "Evaluate function in pp-html."
  (let ((params (pp-html--func-param-eval (cddr el))))
    (eval `(funcall ',(cadr el) ,@params))))

(defun pp-html--eval (el)
  "General evaluation in pp-html."
  (cond
   ((pp-html--var-p el)
    (eval (intern (pp-html--symbol-rest el))))
   ((pp-html--func-p el)
    (pp-html--func-eval el))
   ((pp-html--filter-p el)
    (pp-html--filter-eval el))
   (t el)))

;; some utilities
(defun pp-html--get-attr-plist (sexp)
  "Get attributes plist of a sexp."
  (let ((i 0)
	(plist))
    (while (and (nth i sexp) (symbolp (pp-html--eval (nth i sexp))))
      (let ((item (pp-html--eval (nth i sexp))))
	(cond
	 ((string= "@" (pp-html--symbol-initial item))
	  (setq plist (append plist (list :id (pp-html--symbol-rest item))))
	  (incf i))
	 ((string= "." (pp-html--symbol-initial item))
	  (setq plist (append plist (list :class (pp-html--symbol-rest item))))
	  (incf i))
	 ((string= ":" (pp-html--symbol-initial item))
	  (let ((attr item)
		(next (pp-html--eval (nth (1+ i) sexp))))
	    (if (or (numberp next) (stringp next))
		(progn
		  (setq plist
			(append plist (list attr next)))
		  (incf i 2))
	      (progn
		(setq plist (append plist (list attr nil)))
		(incf i))))))))
    (list i plist)))

(defun pp-html--whole-attr-plist (sexp)
  "Combine all css selector class."
  (let ((i 0)
	(pos)
	(class-val "")
	(plist (cadr (pp-html--get-attr-plist sexp))))
    (while (nth i plist)
      (if (eq :class (nth i plist))
	  (setq pos (append pos (list i))))
      (incf i 2))
    (when (> (length pos) 1)
      (dolist (p pos)
	(let ((val (nth (1+ p) plist)))
	  (if (numberp val)
	      (setq class-val (concat class-val (number-to-string val) " "))
	    (setq class-val (concat class-val val " ")))))
      (setq class-val (substring class-val 0 -1))
      (setf (nth (1+ (nth 0 pos)) plist) class-val)
      (let* ((remove-pos1 (-remove-at-indices '(0) pos))
	     (remove-pos2 (-map '1+ remove-pos1))
	     (remove-pos (append remove-pos1 remove-pos2)))
	(setq plist (-remove-at-indices remove-pos plist))))
    plist))

(defun pp-html--get-child-sexp (sexp)
  "Get inner content of a html tag."
  (let ((len (car (pp-html--get-attr-plist sexp))))
    (nthcdr len sexp)))

(defun pp-html--plist->alist (plist)
  "Convert plist to alist."
  (if (null plist)
      '()
    (cons
     (list (car plist) (cadr plist))
     (pp-html--plist->alist (cddr plist)))))

(defun pp-html--jump-outside (elem)
  "Jump outside of a html elem"
  (let ((elem (symbol-name elem)))
    (if pp-html-xml-p
	(forward-char (+ 3 (length elem)))
      (if (member elem pp-html-empty-element-list)
	  (forward-char 0)
	(forward-char (+ 3 (length elem)))))))

;; Insert html function.
(defun pp-html--insert-html-attrs (alist)
  "Insert html attributes."
  (dolist (attr alist)
    (let ((key (pp-html--symbol-rest (car attr)))
	  (val (pp-html--eval (cadr attr))))
      (if (null val)
	  (insert (concat " " key))
	(if (numberp val)
	    (insert
	     (concat " " key "=" "\"" (number-to-string val) "\""))
	  (insert
	   (concat " " key "=" "\"" val "\"")))
	))))

(defun pp-html--insert-html-elem (elem &optional plist)
  "Insert html elem with attributes."
  (let ((elem (symbol-name elem))
	(alist (pp-html--plist->alist plist))
	(doctype ""))
    (if pp-html-xml-p
	(progn
	  (insert (concat "<" elem ">" "</" elem ">"))
	  (backward-char (+ 4 (length elem)))
	  (if alist (pp-html--insert-html-attrs alist))
	  (forward-char 1))
      (if (member elem pp-html-empty-element-list)
	  (progn
	    (insert (concat "<" elem "/>"))
	    (backward-char 2)
	    (if alist (pp-html--insert-html-attrs alist))
	    (forward-char 2))
	(progn
	  (if (string= elem "html")
	      (progn
		(dolist (doc-info pp-html-html-doctype-param)
		  (setq doctype (concat doctype " " doc-info)))
		(insert (concat "<!DOCTYPE" doctype ">" "<" elem ">" "</" elem ">")))
	    (if (or (null (read elem)) (member elem pp-html-html5-element-list))
		(insert (concat "<" elem ">" "</" elem ">"))
	      (error (format "Invalid html tag: %s" elem))))
	  (backward-char (+ 4 (length elem)))
	  (if alist (pp-html--insert-html-attrs alist))
	  (forward-char 1))))
    ))

;; Process logic list.
(defun pp-html-sexp-replace (old new sexp)
  "Replace all occurences of OLD by NEW in SEXP."
  (-tree-map
   (lambda (x)
     (if (equal x old)
	 (-setq x new)
       (-setq x x)))
   sexp))

(defun pp-html--process-logic-assign (sexp)
  "Process :assign logic"
  (let ((alist (pp-html--plist->alist (cdr sexp))))
    (dolist (lst alist)
      (let ((var (car lst))
	    (val (pp-html--eval (cadr lst))))
	(set var val)))))

(defun pp-html--process-logic-include (sexp)
  "Process :include logic."
  (pp-html--eval (cadr sexp)))

(defun pp-html--process-logic-if (sexp)
  "Process :if logic."
  (if (pp-html--eval (nth 1 sexp))
      (pp-html--eval (nth 2 sexp))
    (if (nth 3 sexp)
	(pp-html--eval (nth 3 sexp)))))

(defun pp-html--process-logic-unless (sexp)
  "Process :unless logic."
  (if (not (pp-html--eval (nth 1 sexp)))
      (pp-html--eval (nth 2 sexp))
    (if (nth 3 sexp)
	(pp-html--eval (nth 3 sexp)))))

(defun pp-html--process-logic-for (sexp)
  "Process :for logic."
  (let ((res)
	(el (pp-html--eval (nth 1 sexp)))
	(in (nth 2 sexp))
	(lst (pp-html--eval (nth 3 sexp)))
	(target (pp-html--eval (nth 4 sexp)))
	(len (length sexp)))
    (if (and (= len 5) (eq 'in in))
	(progn
	  (dolist (x lst)
	    (setq res (append res (list (pp-html-sexp-replace el x target)))))
	  res)
      (error "error pp-html :for syntax!"))))

;; (defun pp-html--process-logic-block (sexp)
;;   "Process :block logic."
;;   (pp-html--eval (nth 2 sexp)))

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
	    (let ((block-name (symbol-name (cadr item)))
		  (extend-block (cadr (cdr item)))
		  (extend-block-str (prin1-to-string (pp-html--eval (cadr (cdr item))))))
	      (setq point (re-search-forward ":block" nil t))
	      (skip-chars-forward "[\" \"\n\t\r]")
	      (if (string= block-name (thing-at-point 'symbol))
		  (progn
		    (forward-symbol 1)
		    (skip-chars-forward "^(")
		    (let ((base-block-str (thing-at-point 'sexp)))
		      (skip-chars-backward "^(")
		      (backward-char)
		      (kill-sexp)
		      (if extend-block
			  (insert extend-block-str)
			(insert base-block-str))))))
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
  (let* ((sexp (pp-html--eval sexp))
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

;; Process tag sexp.
(defun pp-html--process-elem (sexp)
  "Process html elem."
  (let ((elem (car sexp))
	(plist (pp-html--whole-attr-plist (cdr sexp)))
	(inner (pp-html--get-child-sexp (cdr sexp))))
    (with-current-buffer (get-buffer-create "*pp-html-temp*")
      (pp-html--insert-html-elem elem plist)
      (dolist (item inner)
	(let ((item (pp-html--eval item)))
	  (if (listp item)
	      (pp-html-process-sexp item)
	    (if (numberp item)
		(insert (number-to-string item))
	      (insert item)))))
      (pp-html--jump-outside elem)
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun pp-html-process-sexp (sexp)
  "Process pp-html sexp"
  (let* ((car (pp-html--eval (car-safe sexp))))
    (if (and car (listp car))
	(dolist (item sexp)
	  (pp-html-process-sexp item))
      (pp-html--process-elem sexp))))

;; Format html string.
(defun pp-html--has-child-p ()
  "Judge if a tag has child element."
  (save-excursion
    (web-mode-element-child)))

(defun pp-html--has-context-p ()
  "Judge if a tag has inner context."
  (save-excursion
    (not (eq (skip-chars-forward "^<") 0))))

(defun pp-html--has-context-newline ()
  "Make a newline at the end context."
  (if (pp-html--has-context-p)
      (progn
	(skip-chars-forward "^<")
	(newline))))

(defun pp-html-format-html ()
  "Well format html string."
  (let ((pos (point-min)))
    (with-current-buffer (get-buffer-create "*pp-html-temp*")
      (web-mode)
      (goto-char (point-min))
      (forward-char 2)
      (if (string= "DOCTYPE" (thing-at-point 'word))
	  (progn
	    (skip-chars-forward "^>")
	    (forward-char)
	    (newline)
	    (setq pos (point))))
      (while (< pos (point-max))
	(if (pp-html--has-child-p)
	    (progn
	      (skip-chars-forward "^>")
	      (forward-char)
	      (newline)
	      (pp-html--has-context-newline)
	      (setq pos (point)))
	  (progn
	    (web-mode-element-end)
	    (newline)
	    (pp-html--has-context-newline)
	    (setq pos (point))))))))

;;; Format xml string
(defun pp-html-xml--has-child-p ()
  (save-excursion
    (forward-char)
    (skip-chars-forward "^<")
    (forward-char)
    (if (string= "/" (thing-at-point 'char)) nil t)))

(defun pp-html-xml--close-tag-p ()
  (save-excursion
    (forward-char)
    (if (string= "/" (thing-at-point 'char)) t nil)))

(defun pp-html-format-xml ()
  "Well format xml string."
  (let ((pos (point-min)))
    (with-current-buffer (get-buffer-create "*pp-html-temp*")
      (nxml-mode)
      (goto-char (point-min))
      (while (< pos (point-max))
	(if (pp-html-xml--close-tag-p)
	    (progn
	      (skip-chars-forward "^>")
	      (forward-char)
	      (newline)
	      (pp-html--has-context-newline)
	      (setq pos (point)))
	  (if (pp-html-xml--has-child-p)
	      (progn
		(nxml-down-element)
		(newline)
		(pp-html--has-context-newline)
		(setq pos (point)))
	    (progn
	      (nxml-forward-element)
	      (newline)
	      (pp-html--has-context-newline)
	      (setq pos (point)))))))))

;;;###autoload
(defun pp-html (sexp &optional xml-p)
  "Pretty print html."
  (let ((sexp (pp-html-parse sexp)))
    (setq pp-html-xml-p xml-p)
    (ignore-errors (kill-buffer "*pp-html-temp*"))
    (let ((html (pp-html-process-sexp sexp)))
      (with-current-buffer (get-buffer-create "*pp-html-temp*")
	(if pp-html-xml-p
	    (progn
	      (pp-html-format-xml)
	      (goto-char (point-min))
	      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"))
	  (pp-html-format-html))
	(setq html (buffer-substring-no-properties (point-min) (point-max))))
      (ignore-errors (kill-buffer "*pp-html-temp*"))
      html)))

;;;###autoload
(defun pp-html-test (sexp &optional xml-p)
  "Preview printed html in a view buffer."
  (ignore-errors (kill-buffer "*pp-html-test*"))
  (with-current-buffer (get-buffer-create "*pp-html-test*")
    (if xml-p
	(nxml-mode)
      (web-mode))
    (insert (pp-html sexp xml-p))
    (indent-region (point-min) (point-max)))
  (view-buffer "*pp-html-test*" 'kill-buffer))

(provide 'pp-html)

;;; pp-html.el ends here
