;;; poly-rest-mode.el -- Polymode for ReST. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; A mode for interacting with multiple kinds of code inside a ReST host-mode.
;;
;; https://github.com/vspinu/polymode/issues/109
;; https://emacs.stackexchange.com/questions/27679/ergonomic-way-to-edit-source-code-inside-rest-or-markdown-code-blocks/27695#27695
;;
;;; Code:

(require 'polymode)

(defcustom pm-host/ReST
  (pm-bchunkmode "ReST"
                 :mode 'rst-mode
                 :init-functions '(poly-rest-fixes))
  "Restructued Text host chunkmode."
  :group 'hostmodes
  :type 'object)

(defvar pm-ReST-code-regexp
  (regexp-opt '("code" "code-block" "sourcecode" "highlight"))
  "Regular expression to match all possible ReST code blocks.")

(defvar pm-ReST-head-regexp
  (concat "^[^ ]*\\( *\.\. +" pm-ReST-code-regexp "::.*\n"
          "\\(?: +:.+\n\\)*\\)")      ;; Match 0 or more option lines.
  "Regular expression to match the `head' of a ReST code block.")

(defvar pm-ReST-retriever-regexp
  (concat "^[^ ]*\\(?: *\.\. +" pm-ReST-code-regexp ":: \\(.+\\)\\)$")
  "Regular expression to retrieve the mode of the code block.")

(pm-create-indented-block-matchers "ReST" pm-ReST-head-regexp)

(defcustom pm-inner/ReST-code
  (pm-hbtchunkmode-auto "ReST"
                        :head-reg #'pm-ReST-head-matcher
                        :tail-reg #'pm-ReST-tail-matcher
                        :head-mode 'host
                        :tail-mode 'host
                        :retriever-regexp pm-ReST-retriever-regexp
                        :font-lock-narrow t)
  "Restructured Text inner code block mode."
  :group 'innermodes
  :type 'object)


(defcustom pm-poly/ReST
  (pm-polymode-multi-auto "ReST"
                          :hostmode 'pm-host/ReST
                          :auto-innermode 'pm-inner/ReST-code)
  "Restructured Text typical `polymode' configuration."
  :group 'polymodes
  :type 'object)


;;;###autoload (autoload #'poly-rest-mode "poly-rest-mode")
(define-polymode poly-rest-mode pm-poly/ReST)


(defun poly-rest-fixes ()
  "Fix various minor issues that can occur in the poly-ReST-mode."
  (remove-hook 'prog-mode-hook   #'whitespace-mode)
  (remove-hook 'python-mode-hook #'mmm-mode))

(provide 'poly-rest-mode)

;;; poly-rest-mode.el ends here
