;;; packages.el --- YANG Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Christian Hopps <chopps@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq lux-packages '(
  (lux-mode :location local)
))

(defun lux/init-lux-mode ()
  "Initialize LUX mode"
  (use-package lux-mode
    :mode ("\\.lux" "\\.luxinc")))
