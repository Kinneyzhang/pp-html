;;; pp-html.el --- Pretty print html using sexps. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang, all rights reserved.

;; Author: Kinneyzhang <kinneyzhang666@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3") (s "1.12.0) (dash "2.16.0) (web-mode "16.0.24"))
;; URL: https://github.com/Kinneyzhang/pp-html
;; Keywords: html, sexp

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

;; pp-html is a HTML template library in emacs-lisp.
;; It is convenient to generate simple HTML code or complex HTML page
;; by writing elisp S expression in the form of pp-html syntax.
;; It is worth mentioning that =:include= and =:extend= tag
;; make it possible to build HTML pages by module and reuse HTML blocks.

;;; Code:
(require 's)
(require 'dash)
(require 'web-mode)

(defvar pp-html-filter-list
  '((:abs pp-html-filter-abs)
    (:append pp-html-filter-append)
    (:at_least pp-html-filter-at_least)
    (:at_most pp-html-filter-at_most)
    (:capitalize pp-html-filter-capitalize)
    (:ceil pp-html-filter-ceil)
    (:compact pp-html-filter-compact)
    (:concat pp-html-filter-concat)
    (:date pp-html-filter-date)
    (:default pp-html-filter-default)
    (:divided_by pp-html-filter-divided_by)
    (:downcase pp-html-filter-downcase)
    (:first pp-html-filter-first)
    (:floor pp-html-filter-floor)
    (:join pp-html-filter-join)
    (:last pp-html-filter-last)
    (:lstrip pp-html-filter-lstrip)
    (:map pp-html-filter-map)
    (:minus pp-html-filter-minus)
    (:modulo pp-html-filter-modulo)
    (:plus pp-html-filter-plus)
    (:prepend pp-html-filter-prepend)
    (:replace pp-html-filter-replace)
    (:replace_first pp-html-filter-replace_first)
    (:reverse pp-html-filter-reverse)
    (:round pp-html-filter-round)
    (:rstrip pp-html-filter-rstrip)
    (:size pp-html-filter-size)
    (:slice pp-html-filter-slice)
    (:sort pp-html-filter-sort)
    (:sort_natural pp-html-filter-sort_natural)
    (:split pp-html-filter-split)
    (:strip pp-html-filter-strip)
    (:strip_html pp-html-filter-strip_html)
    (:truncate pp-html-filter-truncate)
    (:truncatewords pp-html-filter-truncatewords)
    (:uniq pp-html-filter-uniq)
    (:upcase pp-html-filter-upcase))
  "Filter list"
  )

(defvar pp-html-logic-element-list
  '(:assign
    :ifequal :ifnotequal :if :unless :case :cond
    :for
    :include :extend)
  "Supported pp-html logic element list.")

(defvar pp-html-html5-elements
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

(defvar pp-html-other-html-elements
  '("meting-js"))

(defvar pp-html-empty-element-list
  '("area" "base" "br" "col" "colgroup" "embed" "hr" "img" "input" "link" "meta" "param" "source" "track" "wbr")
  "Html5 empty element list, from https://developer.mozilla.org/zh-CN/docs/Glossary/空元素")

(defvar pp-html-html-doctype-param '("html")
  "Html doctype.")

(defvar pp-html-xml-p nil
  "Whether to print xml.")

(defvar pp-html-xml-header "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "Xml header.")

;;; --------------------------------------------------
;; helper functions
(defun pp-html--symbol-initial (sym)
  "Get the initial charactor of a symbol."
  (if (symbolp sym)
      (substring (symbol-name sym) 0 1)))

(defun pp-html--symbol-rest (sym)
  "Get the rest part of symbol."
  (if (symbolp sym)
      (substring (symbol-name sym) 1)))

(defun pp-html-property-p (sym)
  "Judge if a symbol is a property."
  (when (symbolp sym)
    (when (string= (pp-html--symbol-initial sym) ":")
      t)))

(defun pp-html--get-alist (plist)
  "Convert standard plist to alist."
  (if (null plist)
      '()
    (cons
     (list (car plist) (cadr plist))
     (pp-html--get-alist (cddr plist)))))

(defun pp-html--plist->alist (plist)
  (let ((i 0)
	alist)
    (while (nth i plist)
      (setq next (nth (1+ i) plist))
      (if (or (numberp next) (stringp next))
	  (progn
	    (setq alist
		  (append alist (list (list (nth i plist) (nth (1+ i) plist)))))
	    (incf i 2))
	(progn
	  (setq alist (append alist (list (list (nth i plist) nil))))
	  (incf i))))
    alist))

(defun pp-html--plist-to-alist (plist)
  "Convert plist to alist."
  (let ((i 0)
	(j 0)
	(key nil)
	(val nil)
	(alist nil))
    (while (nth i plist)
      (when (pp-html-property-p (nth i plist))
	(progn
	  (setq key (nth i plist))
	  (setq j (1+ i))))
      (catch 'break
	(while (nth j plist)
	  (if (not (pp-html-property-p (nth j plist)))
	      (progn
		(setq val (append val (list (nth j plist))))
		(incf j))
	    (throw 'break nil))))
      (setq alist (append alist (list (cons key val))))
      (setq val nil)
      (setq i j))
    alist))

(defun pp-html-sexp-replace (old new sexp)
  "Replace all occurences of OLD by NEW in SEXP."
  (let ((lst)
	(el))
    (--tree-map-nodes
     (when (pp-html--var-p it)
       (setq lst (split-string (pp-html--symbol-rest it) "[.]"))
       (setq el (intern (nth 0 lst)))
       (equal el old))
     (progn
       (eval `(setq ,el ',new))
       (-setq it (pp-html-eval it)))
     sexp)))
;;; --------------------------------------------------
;; filter evaluation, functions.
(defun pp-html--filter-p (el)
  "Judge if a element is a filter."
  (if (listp el)
      (if (and (eq '/ (car el)))
	  t nil)
    nil))

(defun pp-html--filter-eval (sexp)
  "Evalute pp-html filter."
  (let* ((value (pp-html-eval (cadr sexp)))
	 (plist (cddr sexp))
	 (alist (pp-html--plist-to-alist plist)))
    (dolist (item alist)
      (if (null (cadr item))
	  (setq value
		(eval `(,(cadr (assoc (car item) pp-html-filter-list)) ',value)))
	(if (-all? 'listp (cdr item))
	    (setq value
		  (eval `(,(cadr (assoc (car item) pp-html-filter-list)) ',value ',@(cdr item))))
	  (setq value
		(eval `(,(cadr (assoc (car item) pp-html-filter-list)) ',value ,@(cdr item)))))))
    value))

;;;###autoload
(defun pp-html-define-filter (filter func)
  "Add pp-html filter."
  (if (not (assoc filter pp-html-filter-list))
      (add-to-list 'pp-html-filter-list (list filter func))
    (error "Filter %s already defined!" filter)))

;; filter functions
(defun pp-html-filter-abs (value)
  "Return the absolute value of a number."
  (let ((value (if (stringp value)
		   (string-to-number value)
		 value)))
    (abs value)))

(defun pp-html-filter-append (value arg)
  "Append a list to another one."
  ;; (append (pp-html-eval value) (pp-html-eval arg))
  (append value (pp-html-eval arg)))

(defun pp-html-filter-at_least (value arg)
  "Limits a number to a minimum value."
  (if (< value arg)
      arg
    value))

(defun pp-html-filter-at_most (value arg)
  "Limits a number to a maximum value."
  (if (< value arg)
      value
    arg))

(defun pp-html-filter-capitalize (value)
  "Convert the first word’s first character to upper case."
  (s-capitalize value))

(defun pp-html-filter-ceil (value)
  "Rounds the input up to the nearest whole number."
  (if (stringp value)
      (ceiling (string-to-number value))
    (ceiling value)))

(defun pp-html-filter-compact (value)
  "Delete all nil in a list."
  (delete nil value))

(defun pp-html-filter-concat (value arg)
  "Join two string together."
  (concat value (pp-html-eval arg)))

(defun pp-html-filter-date (value arg)
  "Converts a timestamp into another date format.
%Y is the year, %y within the century, %C the century.
%G is the year corresponding to the ISO week, %g within the century.
%m is the numeric month.
%b and %h are the locale’s abbreviated month name, %B the full name.
 (%h is not supported on MS-Windows.)
%d is the day of the month, zero-padded, %e is blank-padded.
%u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
%a is the locale’s abbreviated name of the day of week, %A the full name.
%U is the week number starting on Sunday, %W starting on Monday,
 %V according to ISO 8601.
%j is the day of the year.

%H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
 only blank-padded, %l is like %I blank-padded.
%p is the locale’s equivalent of either AM or PM.
%q is the calendar quarter (1–4).
%M is the minute (00-59).
%S is the second (00-59; 00-60 on platforms with leap seconds)
%s is the number of seconds since 1970-01-01 00:00:00 +0000.
%N is the nanosecond, %6N the microsecond, %3N the millisecond, etc.
%Z is the time zone abbreviation, %z is the numeric form.

%c is the locale’s date and time format.
%x is the locale’s \"preferred\" date format.
%D is like \"%m/%d/%y\".
%F is the ISO 8601 date format (like \"%Y-%m-%d\").

%R is like \"%H:%M\", %T is like \"%H:%M:%S\", %r is like \"%I:%M:%S %p\".
%X is the locale’s \"preferred\" time format.
" ;; 获取时间的通用格式
  (if (or (string= "now" value) (string= "today" value))
      (format-time-string arg (current-time))
    (format-time-string arg value)))

(defun pp-html-filter-default (value arg)
  "If value is nil or null string, set default value."
  (if (or (equal "" value) (null value))
      arg
    value))

(defun pp-html-filter-divided_by (value arg)
  "Divides a number by another number."
  (/ value (pp-html-eval arg)))

(defun pp-html-filter-downcase (value)
  "Convert all chars in string to lower case."
  (downcase value))

;; (defun pp-html-filter-escape (value)
;;   "Escapes a string by replacing characters with escape sequences (so that the string can be used in a URL, for example)."
;;   (xml-escape-string value))

;; (defun pp-html-filter-escape_once (value)
;;   "Escapes a string without changing existing escaped entities. It doesn’t change strings that don’t have anything to escape."
;;   )

(defun pp-html-filter-first (value)
  "Returns the first item of an array."
  (car value))

(defun pp-html-filter-floor (value)
  "Rounds the input down to the nearest whole number.
"
  (if (stringp value)
      (floor (string-to-number value))
    (floor value)))

(defun pp-html-filter-join (value arg)
  "Combines the items in a list into a single string using the argument as a separator"
  (let ((res (nth 0 value)))
    (dolist (item (cdr value))
      (setq res (concat res arg item)))
    res))

(defun pp-html-filter-last (value)
  "Returns the last item of an array."
  (car (last value 1)))

(defun pp-html-filter-lstrip (value)
  "Removes all whitespace (tabs, spaces, and newlines) from the left side of a string. It does not affect spaces between words."
  (s-trim-left value))

(defun pp-html-filter-map (value arg)
  "Creates an array of values by extracting the values of a named property from another object."
  (let ((key (intern (concat ":" arg))))
    (mapcar (lambda (lst)
	      (plist-get lst key))
	    value)))

(defun pp-html-filter-minus (value arg)
  "Subtracts a number from another number."
  (if (stringp value)
      (- (string-to-number value) arg)
    (- value arg)))

(defun pp-html-filter-modulo (value arg)
  "Returns the remainder of a division operation."
  (% value arg))

(defun pp-html-filter-plus (value arg)
  "Adds a number to another number."
  (if (stringp value)
      (+ (string-to-number value) arg)
    (+ value arg)))

(defun pp-html-filter-prepend (value arg)
  "Adds the specified string to the beginning of another string."
  (concat arg value))

(defun pp-html-filter-replace (value arg1 arg2)
  "Replaces every occurrence of the first argument in a string with the second argument."
  (replace-regexp-in-string arg1 arg2 value))

(defun pp-html-filter-replace_first (value arg1 arg2)
  "Replaces only the first occurrence of the first argument in a string with the second argument."
  (with-temp-buffer
    (insert value)
    (goto-char (point-min))
    (re-search-forward arg1 nil t 1)
    (replace-match arg2)
    (buffer-string)))

(defun pp-html-filter-reverse (value)
  "Reverses the order of the items in an array."
  (reverse value))

(defun pp-html-filter-round (value)
  "Rounds a number to the nearest integer."
  (round value))

(defun pp-html-filter-rstrip (value)
  "Removes all whitespace (tabs, spaces, and newlines) from the right side of a string. It does not affect spaces between words."
  (s-trim-right value))

(defun pp-html-filter-size (value)
  "Returns the number of characters in a string or the number of items in an array."
  (length value))

(defun pp-html-filter-slice (STRING &optional FROM TO)
  "Return a new string whose contents are a substring of STRING.
The returned string consists of the characters between index FROM
(inclusive) and index TO (exclusive) of STRING.  FROM and TO are
zero-indexed: 0 means the first character of STRING.  Negative values
are counted from the end of STRING.  If TO is nil, the substring runs
to the end of STRING.
"
  (substring STRING FROM TO))

(defun pp-html-filter-sort (value)
  "Sorts items in an array in case-sensitive order."
  (cond
   ((-all? 'numberp value)
    (sort value '<))
   ((-all? 'stringp value)
    (sort value 'string<))
   (t (error "Items in sorted list must be in one type!"))))

(defun pp-html-filter-sort_natural (value)
  "Sorts items in an array in none case-insensitive order."
  (cond
   ((-all? 'numberp value)
    (sort value '<))
   ((-all? 'stringp value)
    (--sort (string< (downcase it) other) value))
   (t (error "Items in sorted list must be in one type!"))))

(defun pp-html-filter-split (value arg)
  "Divides a string into an array using the argument as a separator. split is commonly used to convert comma-separated items from a string to an array."
  (split-string value arg))

(defun pp-html-filter-strip (value)
  "Removes all whitespace (tabs, spaces, and newlines) from both the left and right sides of a string. It does not affect spaces between words."
  (s-trim value))

;; (defun pp-html-filter-strip_html (value)
;;   "Removes any HTML tags from a string."
;;   (with-temp-buffer
;;     (insert value)
;;     (goto-char (point-min))
;;     (setq res nil)
;;     (while (re-search-forward "\\(<.+?>.*?</.+?>\\|<.+?[ ]*/>\\)" nil t)
;;       (setq res (append res (list (match-string 1)))))
;;     (setq repl (mapcar (lambda (x) (string-trim x "<.+?>" "</.+?>")) res))
;;     (setq new-str (buffer-string))
;;     (dotimes (i (length res))
;;       (setq new-str (replace-regexp-in-string (nth i res) (nth i repl) new-str))))
;;   new-str)

(defun pp-html-filter-truncate (value arg &optional ellipsis)
  "Shortens a string down to the number of characters passed as an argument. If the specified number of characters is less than the length of the string, an ellipsis (…) is appended to the string and is included in the character count."
  (let ((end "..."))
    (when ellipsis (setq end ellipsis))
    (concat (substring value 0 (- arg (length end))) end)))

(defun pp-html-filter-truncatewords (value arg &optional ellipsis)
  "Shortens a string down to the number of words passed as an argument. If the specified number of words is less than the number of words in the string, an ellipsis (…) is appended to the string."
  (let ((end "..."))
    (when ellipsis (setq end ellipsis))
    (setq subwords
	  (with-temp-buffer
	    (insert value)
	    (goto-char (point-min))
	    (subword-kill arg)
	    (erase-buffer)
	    (yank)
	    (buffer-string)))
    (concat subwords end)))

(defun pp-html-filter-uniq (value)
  "Removes any duplicate elements in an array."
  (delete-dups value))

(defun pp-html-filter-upcase (value)
  "Convert all chars in string to upper case."
  (upcase value))

;;; --------------------------------------------------
;; pp-html evaluation, include filter, variable and function.
(defun pp-html--var-p (el)
  "Judge if a element is a variable."
  (if (symbolp el)
      (if (and
	   (string= "$" (pp-html--symbol-initial el))
	   (not (string= "" (pp-html--symbol-rest el))))
	  t nil)
    nil))

(defun pp-html--var-eval (el)
  "Evaluate variable in pp-html."
  (let* ((i 1)
	 (var (intern (pp-html--symbol-rest el)))
	 (var-str (symbol-name var))
	 (lst (split-string var-str "[.]"))
	 (res (eval (intern (nth 0 lst)))))
    (if (= 1 (length lst))
	(setq res (eval var))
      (while (nth i lst)
	(let ((prop (intern (concat ":" (nth i lst)))))
	  (setq res (eval `(plist-get ',res ,prop)))
	  (incf i))))
    res))

(defun pp-html--array-p (el)
  "Judge if"
  (let ((arrayp nil)
	(by 1)
	from to)
    (when (listp el)
      (when (or (= 1 (length el)) (= 3 (length el)))
	(let* ((str (format "%s" (car el)))
	       (dots (string-trim str "[a-zA-Z$0-9\\-]+" "[a-zA-Z$0-9\\-]+"))
	       (nums (split-string str "\\.\\."))
	       (from-str (nth 0 nums))
	       (to-str (nth 1 nums)))
	  (when (and (string= ".." dots) (= 2 (length nums)))
	    (when (and (not (string= "" from-str)) (not (string= "" to-str)))
	      (setq from (pp-html-eval (read from-str)))
	      (setq to (pp-html-eval (read to-str)))
	      (when (and (numberp from) (numberp to))
		(if (= 1 (length el))
		    (progn
		      (setq arrayp t)
		      (when (> from to)
			(setq by -1)))
		  (if (= 3 (length el))
		      (when (and (eq 'by (nth 1 el)) (numberp (nth 2 el)))
			(setq arrayp t)
			(setq by (pp-html-eval (nth 2 el))))))))))))
    (when arrayp (list from to by))))

(defun pp-html--array-eval (el)
  "Evaluate array in pp-html"
  (when (pp-html--array-p el)
    (let* ((array (pp-html--array-p el))
	   (from (nth 0 array))
	   (to (nth 1 array))
	   (by (nth 2 array)))
      (number-sequence from to by))))

(defun pp-html--func-p (el)
  "Judge if a element is a function."
  (if (listp el)
      (if (eq '$ (car el)) t nil)
    nil))

(defun pp-html--func-param-eval (params)
  "Evaluate function params in pp-html."
  (mapcar
   (lambda (x)
     (pp-html-eval x))
   (-tree-map
    (lambda (p)
      (if (pp-html--var-p p)
	  (setq p (pp-html-eval p))
	(setq p p)))
    params)))

(defun pp-html--func-eval (el)
  "Evaluate function in pp-html."
  (let ((params (pp-html--func-param-eval (cddr el))))
    (eval `(funcall ',(cadr el) ,@params))))

;;;###autoload
(defun pp-html-eval (el)
  "General evaluation in pp-html."
  (cond
   ((pp-html--var-p el)
    (pp-html--var-eval el))
   ((pp-html--func-p el)
    (pp-html--func-eval el))
   ((pp-html--filter-p el)
    (pp-html--filter-eval el))
   ((pp-html--array-p el)
    (pp-html--array-eval el))
   (t el)))
;;; --------------------------------------------------
;; pp-html logic tags functions and parse.
(defun pp-html--process-logic-assign (sexp)
  "Process :assign logic"
  (let ((alist (pp-html--get-alist (cdr sexp))))
    (dolist (lst alist)
      (let ((var (car lst))
	    (val (pp-html-eval (cadr lst))))
	(if (listp val)
	    (eval `(setq ,var ',val))
	  (eval `(setq ,var ,val)))))))

;; control flow
(defun pp-html--process-logic-ifequal (sexp)
  "Process :ifequal logic."
  (if (equal (pp-html-eval (nth 1 sexp)) (pp-html-eval (nth 2 sexp)))
      (pp-html-eval (nth 3 sexp))
    (if (nth 4 sexp)
	(pp-html-eval (nth 4 sexp)))))

(defun pp-html--process-logic-ifnotequal (sexp)
  "Process :ifnotequal logic."
  (if (not (equal (pp-html-eval (nth 1 sexp)) (pp-html-eval (nth 2 sexp))))
      (pp-html-eval (nth 3 sexp))
    (if (nth 4 sexp)
	(pp-html-eval (nth 4 sexp)))))

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

(defun pp-html--process-logic-case (sexp)
  "Process :case logic."
  (let ((case-var (pp-html-eval (cadr sexp)))
	(cases (cddr sexp))
	res)
    (dolist (cas cases)
      (if (eq :when (car cas))
	  (when (equal case-var (pp-html-eval (cadr cas)))
	    (setq res (car (last cas 1))))
	(error "Invalid pp-html :case syntax!")))
    res))

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

;; Iteration
(defun pp-html--process-logic-for (sexp)
  "Process :for logic."
  (let ((old (nth 1 sexp))
	(in (nth 2 sexp))
	(seq (pp-html-eval (nth 3 sexp)))
	(parameters nil)
	(len (length sexp))
	target res)
    (if (eq 'in in)
	(if seq
	    (progn
	      (if (eq :else (car (pp-html-parse (car (last sexp 1)))))
		  (progn
		    (setq parameters (-drop-last 2 (-drop 4 sexp)))
		    (setq target (car (last sexp 2))))
		(setq parameters (-drop-last 1 (-drop 4 sexp)))
		(setq target (car (last sexp 1))))
	      (dolist (param (pp-html--plist->alist parameters))
		(cond
		 ((eq :limit (car param))
		  (setq seq (seq-take seq (cadr param))))
		 ((eq :offset (car param))
		  (setq seq (seq-drop seq (cadr param))))
		 ((eq :reversed (car param))
		  (setq seq (reverse seq)))))
	      (catch 'break
		(dolist (new seq)
		  ;; (setq target (pp-html-parse target))
		  (setq new-target (pp-html-sexp-replace old new target))
		  (catch 'continue
		    (cond
		     ((equal (pp-html-parse new-target) '(:break))
		      (throw 'break res))
		     ((equal (pp-html-parse new-target) '(:continue))
		      (throw 'continue res))
		     (t (setq res (append res (list (pp-html-parse new-target))))))))))
	  (if (eq :else (car (pp-html-parse (car (last sexp 1)))))
	      (setq res (cadr (pp-html-parse (car (last sexp 1)))))))
      (error "error pp-html :for syntax!"))
    res))


(defun pp-html--process-logic-include (sexp)
  "Process :include logic."
  (pp-html-eval (cadr sexp)))

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

;;; --------------------------------------------------
;; main functions, generate html.
(defun pp-html--get-attr-plist (sexp)
  "Get attributes plist of a sexp."
  (let ((i 0)
	(plist))
    (while (and (nth i sexp) (symbolp (pp-html-eval (nth i sexp))))
      (let ((item (pp-html-eval (nth i sexp))))
	(cond
	 ((string= "@" (pp-html--symbol-initial item))
	  (setq plist (append plist (list :id (pp-html--symbol-rest item))))
	  (incf i))
	 ((string= "." (pp-html--symbol-initial item))
	  (setq plist (append plist (list :class (pp-html--symbol-rest item))))
	  (incf i))
	 ((string= ":" (pp-html--symbol-initial item))
	  (let ((attr item)
		(next (pp-html-eval (nth (1+ i) sexp))))
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
	  (val (pp-html-eval (cadr attr))))
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
	    (if (or (member elem pp-html-other-html-elements)
		    (member elem pp-html-html5-elements))
		(insert (concat "<" elem ">" "</" elem ">"))
	      (if (null (read elem))
		  (insert "")
		(error (format "Invalid html tag: %s" elem)))))
	  (backward-char (+ 4 (length elem)))
	  (if alist (pp-html--insert-html-attrs alist))
	  (forward-char 1))))
    ))

(defun pp-html--process-elem (sexp)
  "Process html elem."
  (let ((elem (car sexp))
	(plist (pp-html--whole-attr-plist (cdr sexp)))
	(inner (pp-html--get-child-sexp (cdr sexp))))
    (with-current-buffer (get-buffer-create "*pp-html-temp*")
      (pp-html--insert-html-elem elem plist)
      (dolist (item inner)
	(let ((item (pp-html-eval item)))
	  (if (listp item)
	      (pp-html-process-sexp item)
	    (if (numberp item)
		(insert (number-to-string item))
	      (insert item)))))
      (pp-html--jump-outside elem)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun pp-html-process-sexp (sexp)
  "Process pp-html sexp"
  (let* ((car (pp-html-eval (car-safe sexp))))
    (if (listp car)
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
	(newline 1))))

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
      (xml-mode)
      (goto-char (point-min))
      (while (< pos (point-max))
	(if (pp-html-xml--close-tag-p)
	    (progn
	      (skip-chars-forward "^>")
	      (forward-char)
	      (newline 1)
	      (pp-html--has-context-newline)
	      (setq pos (point)))
	  (if (pp-html-xml--has-child-p)
	      (progn
		(nxml-down-element)
		(newline 1)
		(pp-html--has-context-newline)
		(setq pos (point)))
	    (progn
	      (nxml-forward-element)
	      (newline 1)
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
	      (insert pp-html-xml-header))
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
