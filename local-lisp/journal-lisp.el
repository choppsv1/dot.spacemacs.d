;;
;; October 27 2022, Christian Hopps <chopps@gmail.com>
;;
;; From: http://www.howardism.org/Technical/Emacs/journaling-org.html

(defun split-string-with-number (string)
  "Returns a list of three components of the string, the first is
the text prior to any numbers, the second is the embedded number,
and the third is the rest of the text in the string."
  (let* ((start (string-match "[0-9]+" string))
         (end (string-match "[^0-9]+" string start)))
    (if start
        (list (substring string 0 start)
              (substring string start end)
              (if end  (substring string end)  "")))))

(defun find-file-number-change (f)
  (let* ((filename (buffer-file-name))
         (parts    (split-string-with-number
                    (file-name-base filename)))
         (new-name (number-to-string
                    (funcall f (string-to-number (nth 1 parts))))))
    (concat (file-name-directory filename)
            (nth 0 parts)
            new-name
            (nth 2 parts))))

(defun find-file-increment ()
  "Takes the current buffer, and loads the file that is 'one
more' than the file contained in the current buffer. This
requires that the current file contain a number that can be
incremented."
  (interactive)
  (find-file (find-file-number-change '1+)))

(defun find-file-decrement ()
  "Takes the current buffer, and loads the file that is 'one
less' than the file contained in the current buffer. This
requires that the current file contain a number that can be
decremented."
  (interactive)
  (find-file (find-file-number-change '1-)))

(defun journal-file-insert ()
  "Insert's the journal heading based on the file's name."
  (interactive)
  (when (string-match "\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
                      (buffer-name))
    (let ((year  (string-to-number (match-string 1 (buffer-name))))
          (month (string-to-number (match-string 2 (buffer-name))))
          (day   (string-to-number (match-string 3 (buffer-name))))
          (datim nil))
      (setq datim (encode-time 0 0 0 day month year))
      (insert (format-time-string
               "#+TITLE: Journal Entry- %Y-%b-%d (%A)\n\n" datim)))))

(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d")))
    (expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-today ()
  "Create and load a journal file based on today's date."
  (interactive)
  (find-file (get-journal-file-today)))

(spacemacs/set-leader-keys
  "fJ" 'journal-file-today
  "f+" 'find-file-increment
  "f-" 'find-file-decrement
  "fn" 'find-file-increment
  "fp" 'find-file-decrement)

(setq-default org-journal-dir "~/Dropbox/org-mode/journal/")

(add-to-list 'auto-mode-alist '(".*/journal/[0-9]*$" . org-mode))
(add-to-list 'auto-insert-alist '(".*/journal/[0-9]*$" . journal-file-insert))

(provide 'journal-lisp)
