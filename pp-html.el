;;; pp-html.el --- Pretty print html using sexps with the function `pp-html':

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

;; (pp-html '(link :async nil :href "url"))
;; => "<link async href=\"url\"/>"

;; (pp-html '(div :class "post" "post content ..."))
;; => "<div class=\"post\">post content ...</div>"

;; (pp-html '(div :class "post"
;;             (p :class "parah"
;;               (span "span content"))))
;; => "<div class=\"post\">
;;      <p class=\"parah\">
;;        <span>span content</span>
;;      </p>
;;     </div>"

;;; Code:
(require 'web-mode)

(defvar pp-html-single-tag-list
  '("img" "br" "hr" "input" "meta" "link" "param")
  "Html single tag list.")

(defvar pp-html-logic-tag-list
  '(:include :if :for :block :extend)
  "Supported logic tag.")

;; some utilities
(defun pp-html--get-plist (list)
  "Get attributes plist of a html tag."
  (let* ((i 0)
	 (plist))
    (while (and (nth i list) (symbolp (nth i list)))
      (setq key (nth i list))
      (setq value (nth (1+ i) list))
      (setq plist (append plist (list key value)))
      (incf i 2))
    plist))

(defun pp-html--get-inner (list)
  "Get inner content of a html tag."
  (let* ((i 0)
	 (inner nil)
	 (plist (pp-html--get-plist list)))
    (if (null plist)
	(setq inner list)
      (dolist (p plist)
	(setq inner (remove p list))
	(setq list inner)))
    inner))

(defun pp-html--plist->alist (plist)
  "Convert plist to alist."
  (if (null plist)
      '()
    (cons
     (list (car plist) (cadr plist))
     (pp-html--plist->alist (cddr plist)))))

(defun pp-html--jump-outside (tag)
  "Jump outside of a html tag"
  (let ((tag (symbol-name tag)))
    (if (member tag pp-html-single-tag-list)
	(forward-char 0)
      (forward-char (+ 3 (length tag))))))

;; Insert html function.
(defun pp-html--insert-html-tag (tag &optional attrs)
  "Insert html tag with attributes."
  (let ((tag (symbol-name tag))
	(attrs (pp-html--plist->alist attrs)))
    (if (member tag pp-html-single-tag-list)
	(progn
	  (insert (concat "<" tag "/>"))
	  (backward-char 2)
	  (dolist (attr attrs)
	    (if (null (cadr attr))
		(insert
		 (concat " " (substring (symbol-name (car attr)) 1)))
	      (insert
	       (concat " " (substring (symbol-name (car attr)) 1) "=" "\"" (cadr attr) "\""))))
	  (forward-char 2))
      (progn
	(insert (concat "<" tag ">" "</" tag ">"))
	(backward-char (+ 4 (length tag)))
	(dolist (attr attrs)
	  (if (null (cadr attr))
	      (insert
	       (concat " " (substring (symbol-name (car attr)) 1)))
	    (insert
	     (concat " " (substring (symbol-name (car attr)) 1) "=" "\"" (cadr attr) "\""))))
	(forward-char 1)))
    ))

;; Process logic list.
(defun pp-html--list-replace-all (old new list &optional fn-equal)
  "Replace all occurences of OLD by NEW in LIST.
Return number of elements replaced.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((fn-equal (or fn-equal #'equal)))
    (while list
      (when (listp (car list))
	(my/list-replace-all old new (car list)))
      (when (funcall fn-equal old (car list))
        (setcar list new))
      (setq list (cdr list))
      )))

(defun pp-html--process-logic-include (left)
  "Process :include logic."
  (dolist (item (eval (car left)))
    (pp-html-unformatted item)))

(defun pp-html--process-logic-if (left)
  "Process :if logic."
  (if (car left)
      (pp-html-unformatted (cadr left))
    (pp-html-unformatted (cadr (cdr left)))))

(defun pp-html--process-logic-for (left)
  "Process :for logic"
  (let* ((name (car left))
	 (items (cadr left))
	 (target (cadr (cdr left)))
	 (target-copy (copy-tree target)))
    (dolist (item items)
      (pp-html--list-replace-all name item target)
      (pp-html-unformatted target)
      (setq target target-copy))))

(defun pp-html--process-logic-block (left)
  "Process :block logic."
  (pp-html-unformatted (cadr left)))

(defun pp-html--process-logic-extend (left)
  "Process :extend logic."
  (let* ((base-str (prin1-to-string (car left)))
	 (entend-str
	  (with-temp-buffer
	    (insert base-str)
	    (setq point (goto-char (point-min)))
	    (while point
	      (dolist (item (cdr left))
		(setq name (symbol-name (cadr item)))
		(setq block (prin1-to-string (cadr (cdr item))))
		(setq point (re-search-forward ":block" nil t))
		(skip-chars-forward "[\" \"\n\t\r]")
		(if (string= name (thing-at-point 'word))
		    (progn
		      (skip-chars-forward "[a-zA-Z]")
		      (skip-chars-forward "[\" \"\n\t\r]")
		      (setq sexp-beg (point))
		      (ignore-errors (forward-sexp))
		      (setq sexp-end (point))
		      (kill-region sexp-beg sexp-end)
		      (insert block)))))
	    (buffer-substring-no-properties (point-min) (point-max)))))
    (pp-html-unformatted (read entend-str))))

(defun pp-html--process-logic (list)
  "process template logic"
  (let ((logic (car list))
	(left (cdr list)))
    (if (member logic pp-html-logic-tag-list)
	(funcall (read (concat "pp-html--process-logic-" (substring (symbol-name logic) 1))) left))
    ))

;; Process tag list.
(defun pp-html--process-tag (list)
  "Process html tag."
  (let ((tag (car list))
	(plist (pp-html--get-plist (cdr list)))
	(inner (pp-html--get-inner (cdr list))))
    (with-current-buffer (get-buffer-create "*pp-html-temp*")
      (when tag
	(progn
	  (pp-html--insert-html-tag tag plist)
	  (dolist (item inner)
	    (if (listp item)
		(pp-html-unformatted item)
	      (insert item)))
	  (pp-html--jump-outside tag)
	  (buffer-substring-no-properties (point-min) (point-max)))))
    ))

(defun pp-html-unformatted (list)
  "Process elisp list to unformatted html"
  (let ((car-str (symbol-name (car list))))
    (if (string= (substring car-str 0 1) ":")
	(pp-html--process-logic list)
      (pp-html--process-tag list))
    ))

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
  (let ((pos (goto-char (point-min))))
    (with-current-buffer "*pp-html-temp*"
      (web-mode)
      (while (< pos (point-max))
	(if (pp-html--has-child-p)
	    (progn
	      (skip-chars-forward "^>")
	      (forward-char)
	      (newline)
	      (setq pos (point))
	      (pp-html--has-context-newline)
	      (setq pos (point)))
	  (progn
	    (web-mode-element-end)
	    (newline)
	    (setq pos (point))
	    (pp-html--has-context-newline)
	    (setq pos (point))))))))

;;;###autoload
(defun pp-html (LIST)
  "Pretty print html."
  (let ((html (pp-html-unformatted LIST)))
    (with-current-buffer "*pp-html-temp*"
      (pp-html-format-html)
      (setq html (buffer-substring-no-properties (point-min) (point-max))))
    (kill-buffer "*pp-html-temp*")
    html))

;;;###autoload
(defun pp-html-test (LIST)
  "Preview printed html in a view buffer."
  (ignore-errors (kill-buffer "*pp-html-test*"))
  (with-current-buffer (get-buffer-create "*pp-html-test*")
    (web-mode)
    (insert (pp-html LIST))
    (indent-region-or-buffer))
  (view-buffer "*pp-html-test*"))

(provide 'pp-html)

;;; pp-html.el ends here
