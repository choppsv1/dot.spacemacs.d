;;; packages.el --- ietf layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Christian Hopps <chopps@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst ietf-packages '(ietf-docs
                          irfc))

;; for yang mode
;; (irfc :location '(melpa "https://github.com/milkypostman/melpa#recipe-format"))))

(defun ietf/init-ietf-docs ()
  (use-package ietf-docs
    :commands ietf-docs-open-at-point
    :init
    (progn
      (spacemacs/set-leader-keys "f I" 'ietf-docs-open-at-point)
      (setq ietf-docs-cache-directory ietf-docs-cache))))

;; https://github.com/choppsv1/dot.spacemacs.d/blob/6bb0f1682ca3a1accdfc379b797a1ae04c1a7f20/init.el

(defun ietf/init-irfc ()
  (use-package ietf-docs
    :commands irfc-mode
    :init
    (progn
      (setq irfc-directory ietf-docs-cache)
      (setq irfc-assoc-mode t)
      (add-to-list 'auto-mode-alist
                   '("/draft-\\([a-z0-9_]+-\\)+[0-9]+.txt" . irfc-mode))
      (add-to-list 'auto-mode-alist
                   '("/draft-\\([a-z0-9_]+-\\)+[a-z0-9_]+.txt" . irfc-mode)))))
;; '("/draft-\\([a-z0-9_]+-\\)+[0-9]+.txt" . irfc-mode)))))

;;; packages.el ends here
