;;; ob-pic.el --- org-babel functions for pic evaluation

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

;; Org-Babel support for evaluating pic source code.
;;
;; For information on pic see http://XXX
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in pic
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments
;;
;; 4) there are no variables (at least for now)

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:pic
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a pic source block.")

(defun org-babel-expand-body:pic (body params)
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

(defun org-babel-execute:pic (body params)
  "Execute a block of pic code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (cdr (assoc :result-params params)))
	 (out-file (cdr (or (assoc :file params)
			    (error "You need to specify a :file parameter"))))
   ;;(cmdline (or (cdr (assoc :cmdline params))
   ;;             (format "-T%s" (file-name-extension out-file))))
   ;; (cmd (or (cdr (assoc :cmd params)) "groff -p"))
         (cmdline (or (cdr (assoc :cmdline params))
                ""))
         (cmd (or (cdr (assoc :cmd params)) "dpic"))
         (in-file (org-babel-temp-file "pic-")))
    (with-temp-file in-file
      (insert (org-babel-expand-body:pic body params)))
    (message "XXX cmd %s cmdline %s params %s" cmd cmdline params)
    (org-babel-eval
     (concat cmd
       " " cmdline
       " " (org-babel-process-file-name in-file)
       " > " (org-babel-process-file-name out-file)) "")
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:pic (session params)
  "Return an error because pic does not support sessions."
  (error "pic does not support sessions"))

(provide 'ob-pic)

;;; ob-pic.el ends here
