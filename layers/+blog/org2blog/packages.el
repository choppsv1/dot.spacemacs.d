;;; packages.el --- org2blog layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <chopps@tops.chopps.org>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `org2blog-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org2blog/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org2blog/pre-init-PACKAGE' and/or
;;   `org2blog/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org2blog-packages
  '(
    org
    metaweblog
    xml-rpc
    (org2blog :location (recipe
                         :fetcher github
                         :repo "punchagan/org2blog"))))

(defun org2blog/init-xml-rpc ()
  (use-package xml-rpc))

(defun org2blog/init-metaweblog ()
  (use-package metaweblog))

(defun org2blog/init-org2blog ()
  (use-package org2blog
    ;; Do we want to defer how does it know about orgblog in org files then
    :commands (org2blog/wp-mode org2blog/wp-login)
    ))

;;; packages.el ends here
