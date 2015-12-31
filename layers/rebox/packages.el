;;
;; December 30 2015, Christian Hopps <chopps@gmail.com>
;;
;; Copyright (c) 2015 by Christian E. Hopps
;; All rights reserved.
;;

(setq rebox-packages '(rebox2))

(defun rebox/init-rebox2 ()
  (use-package rebox2
    :commands (rebox-dwim rebox-mode)
    :defer t
    :init (progn
            (setq-default rebox-style-loop '(71 72 73 74 75 76 77 81 82 83 84 85 86 87))
            ;; C-mode comments
            ;; (setq-default '(241 235 243))
            (evil-leader/set-key "cb" 'rebox-dwim)
            (evil-leader/set-key "cB" 'rebox-dwim))
    :config (progn
              (rebox-register-template 71 176 ["?"
                                               "? box123456"
                                               "?"])

              (rebox-register-template 72 176 ["? ---------"
                                               "? box123456"
                                               "? ---------"])

              (rebox-register-template 73 376 ["? ========="
                                               "? box123456"
                                               "? ========="])

              (rebox-register-template 74 176 ["?-----------"
                                               "? box123456 "
                                               "?-----------"])

              (rebox-register-template 75 276 ["?-----------+"
                                               "? box123456 "
                                               "?-----------+"])

              (rebox-register-template 76 376 ["?==========="
                                               "? box123456 "
                                               "?==========="])

              (rebox-register-template 77 576 ["????????????"
                                               "? box123456 "
                                               "????????????"])

              (rebox-register-template 81 176 ["??"
                                               "?? box123456"
                                               "??"])

              (rebox-register-template 82 286 ["?? ---------"
                                               "?? box123456"
                                               "?? ---------"])

              (rebox-register-template 83 486 ["?? ========="
                                               "?? box123456"
                                               "?? ========="])

              (rebox-register-template 84 286 ["??-----------"
                                               "?? box123456 "
                                               "??-----------"])

              (rebox-register-template 85 386 ["??-----------+"
                                               "?? box123456 "
                                               "??-----------+"])

              (rebox-register-template 86 486 ["??==========="
                                               "?? box123456 "
                                               "??==========="])

              (rebox-register-template 87 576 ["?????????????"
                                               "?? box123456 "
                                               "?????????????"])

              )))
