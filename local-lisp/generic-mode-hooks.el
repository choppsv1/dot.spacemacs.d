;;
;; December 31 2015, Christian Hopps <chopps@gmail.com>
;;
;; Copyright (c) 2015 by Christian E. Hopps
;; All rights reserved.
;;

;; ===================
;; All text mode hooks
;; ===================

(defun run-all-text-mode-hook ()
  nil
  (progn
    (run-hooks 'all-text-mode-hook)
    (setq fill-column 80)
    (auto-fill-mode)
    ;; (turn-on-fci-mode)
    ))

(dolist (hook '(conf-mode-hook
                diff-mode-hook
                markdown-mode-hook
                org-mode-hook
                rst-mode-hook
                rfc-mode-hook
                ))
  (add-hook hook 'run-all-text-mode-hook))

;; =========================
;; All Programming Mode Hook
;; =========================

(defun run-all-prog-mode-hook ()
  nil
  (progn
    (run-hooks 'all-prog-mode-hook)
    (setq fill-column 100)            ; can be overriden in mode specific hooks added later
    (auto-fill-mode)
    ;; (turn-on-fci-mode)
    ))

(dolist (hook '(c-mode-common-hook
                emacs-lisp-mode-hook
                erlang-mode-hook
                haskell-mode-hook
                html-mode-hook
                json-mode-hook
                lisp-interaction-mode-hook
                lisp-mode-hook
                nxml-mode-hook
                perl-mode-hook
                python-mode-hook
                ruby-mode-hook
                shell-mode-hook
                sgml-mode-hook
                yaml-mode-hook
                yang-mode-hook
                ))
  (add-hook hook 'run-all-prog-mode-hook))

(provide 'generic-mode-hooks)
