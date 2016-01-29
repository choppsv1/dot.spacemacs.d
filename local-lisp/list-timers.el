;; -*- mode: emacs-lisp -*-
;;
;; Not part of Emacs; GPL, etc....

;; List timers in the current Emacs process in a human-readable table.
;;
;; Mostly for experimentation with tabulated-list-mode, but
;; potentially useful anyway.
;;
;; It does use a bunch of stuff internal to the timer package.
;;

(define-derived-mode timers-menu-mode tabulated-list-mode "Timer Menu"
  "Major mode for displaying the active timers."
  (setq tabulated-list-format [("Time" 20 t)
			       ("Repeat" 8 timers-menu--compare-repeat
				:right-align t)
			       ("Idle" 6 timers-menu--compare-idle
				:right-align t)
			       ("Trig?" 5 t)
			       ("Function" 15 t)
			       ])
  (add-hook 'tabulated-list-revert-hook 'timers-menu--refresh nil t)
  (setq tabulated-list-sort-key (cons "Time" nil))
  (tabulated-list-init-header))

(defun timers-menu--compare-repeat (a b)
  (< (or (timer--repeat-delay (car a)) -1)
     (or (timer--repeat-delay (car b)) -1)))

(defun timers-menu--compare-idle (a b)
  (< (or (timer--idle-delay (car a)) -1)
     (or (timer--idle-delay (car b)) -1)))

(defun timers-menu--format-callback (func args)
  (let ((string (if args
		    (format "%S%S" func args)
		  (format "%S" func))))
    (replace-regexp-in-string "\n" "\\n" string nil t)))

(defun timers-menu--format-one (timer)
  (list timer
	(vector (if (eq 'idle (timer--idle-delay timer))
		    (format "  idle %8.2fs"
			    (time-to-seconds (timer--time timer)))
		  (format-time-string "%Y-%m-%d %T"
				      (timer--time timer)))
		(format "%S" (timer--repeat-delay timer))
		(format "%S" (timer--idle-delay timer))
		(if (timer--triggered timer) " Yes" "  No")
		(timers-menu--format-callback (timer--function timer)
					      (timer--args timer)))))

(defun timers-menu--refresh ()
  (setq tabulated-list-entries
	(append (mapcar 'timers-menu--format-one timer-list)
		(mapcar 'timers-menu--format-one timer-idle-list))))

(defun list-timers ()
  "Display a list of all active timers.

The timer display is NOT automatically updated as timers trigger
and (in some cases) get rescheduled."
  (interactive)
  (let ((buffer (get-buffer-create "*Timer List*")))
    (with-current-buffer buffer
      (timers-menu-mode)
      (timers-menu--refresh)
      (tabulated-list-print))
    (display-buffer buffer)))

(provide 'list-timers)
