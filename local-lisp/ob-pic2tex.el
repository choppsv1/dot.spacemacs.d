;;; ob-pic2tex.el --- org-babel functions for pic2tex evaluation

;; Copyright (C) 2009-2016 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating pic2tex source code.
;;
;; For information on pic2tex see http://www.graphviz.org/
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in pic2tex
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments
;;
;; 4) there are no variables (at least for now)

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:pic2tex
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a pic2tex source block.")

(defun org-babel-expand-body:pic2tex (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body
		t
		t))))
     vars)
    body))

(defun org-babel-execute:pic2tex (body params)
  "Execute a block of Pic2tex code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (cdr (assoc :result-params params)))
	 (out-file (cdr (or (assoc :file params)
			    (error "You need to specify a :file parameter"))))
	 (cmdline (or (cdr (assoc :cmdline params))
		      (format "-T%s" (file-name-extension out-file))))
   (cmd (or (cdr (assoc :cmd params)) "pic2tex"))
   (in-file (org-babel-temp-file "pic2tex-")))
    (with-temp-file in-file
      (insert (org-babel-expand-body:pic2tex body params)))
    (org-babel-eval
     (concat cmd
	     " " (org-babel-process-file-name in-file)
	     " " cmdline
	     " -o " (org-babel-process-file-name out-file)) "")
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:pic2tex (session params)
  "Return an error because Pic2tex does not support sessions."
  (error "Pic2tex does not support sessions"))

(provide 'ob-pic2tex)

;;; ob-pic2tex.el ends here
