(defmacro fold-section (description &rest body)
  "A macro allowing for ignoring the first thing"
  (if (bound-and-true-p debug-init-msg) (message "DEBUG-INIT: %s" description))
  (list 'if t (cons 'progn body)))

(defun find-and-close-fold (re-string)
  "Find a given string and fold it"
  (interactive "sFold on regexp:")
  (save-excursion
    (goto-char (point-max))
    (while (re-search-backward re-string nil 'noerror)
      (message "found and folding point: %d" (point))
      (evil-close-fold)
      (goto-char (- (point) 1))
      )))

(defun find-and-open-fold (re-string)
  "Find a given string and fold it"
  (interactive "sFold on regexp:")
  (save-excursion
    (goto-char (point-max))
    (while (re-search-backward re-string)
      (message "found and folding point: %d" (point))
      (evil-open-fold)
      (goto-char (- (point) 1))
      )))

(defun my-adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (require 'cl-lib)
    (cl-adjoin element list)))

(defun remove-last-elt (list)
  (let ((rlist (reverse list)))
    (reverse (cdr rlist))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun dired-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;-------------------------------
;; Disabled commands (not many)
;;-------------------------------

(defun enable-all-commands ()
  "Enable all commands, reporting on which were disabled."
  (interactive)
  (with-output-to-temp-buffer "*Commands that were disabled*"
    (mapatoms
     (function
      (lambda (symbol)
        (when (get symbol 'disabled)
          (put symbol 'disabled nil)
          (prin1 symbol)
          (princ "\n")))))))

(defun increment-numbers-in-rergion ()
  "Find all numbers in the region and increment them by 1."
  (interactive)
  (if (not (use-region-p))
      (error "No region defined"))
  (let* ((start (region-beginning))
         (end (region-end))
         found)
    (save-excursion
      (goto-char start)
      (while (setq found (re-search-forward "[0-9]+" end t))
        (replace-match (number-to-string (+ (string-to-number (match-string 0)) 1)))))))

(defun normalize-numbers-in-rergion ()
  "Find all numbers in the region starting with 0 set them increasing order"
  (interactive)
  (if (not (use-region-p))
      (error "No region defined"))
  (let* ((start (region-beginning))
         (end (region-end))
         (value 0)
         found)
    (save-excursion
      (goto-char start)
      (while (setq found (re-search-forward "\\<[0-9]+\\>" end t))
        (replace-match (number-to-string value))
        (setq value (+ value 1))))))


;; Automatically enable any disabled command the first time it's used.
(defun enable-this-command (&rest args)
  (put this-command 'disabled nil)
  (call-interactively this-command))
(setq disabled-command-function 'enable-this-command)

(defun strip-trailing-whitespace ()
  "Eliminate whitespace at ends of lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t][ \t]*$" nil t)
      (delete-region (match-beginning 0) (point)))))

;; (defun nuke-nroff-bs ()
;;   (interactive)
;;   (let ((old-modified (buffer-modified-p))
;;         (old-point (point)))
;;     (call-interactively (beginning-of-buffer))
;;     (replace-regexp "\\(.\\)^H\\1^H\\1^H\\1" "\\1")
;;     (call-interactively (beginning-of-buffer))
;;     (replace-regexp "\\(.\\)^H\\1^H\\1" "\\1")
;;     (call-interactively (beginning-of-buffer))
;;     (replace-regexp "\\(.\\)^H\\1" "\\1")
;;     (call-interactively (beginning-of-buffer))
;;     (replace-string "_^H" "")
;;     (set-buffer-modified-p old-modified)
;;     (goto-char old-point)))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (let* ((l (length prefix)))
    (string= (substring string 0 l) prefix)))

(defun bh-compile ()
  (interactive)
  (let ((df (directory-files "."))
        (has-proj-file nil)
        )
    (while (and df (not has-proj-file))
      (let ((fn (car df)))
        (if (> (length fn) 10)
            (if (string-equal (substring fn -10) ".xcodeproj")
                (setq has-proj-file t)
              )
          )
        )
      (setq df (cdr df))
      )
    (if has-proj-file
        (compile "xcodebuild -configuration Debug")
      (compile "make")
      )
    )
  )

(defun kill-region-to-ssh ()
  "Copy the region to our ssh clients clipboard"
  (interactive)
  (let ((cmd
         (or (and (not (string= "" (getenv "SSH_CONNECTION")))
                  (concat "ssh -q " (car (split-string (getenv "SSH_CONNECTION"))) " bash -c 'xclip -in || pbcopy' >& /dev/null"))
             ;; (let ((cmd (or (and (not (string= "" (getenv "SSH_CONNECTION"))) (concat "ssh -q " (car (split-string (getenv "SSH_CLIENT"))) " pbcopy"))
             "bash -c 'xclip -in || pbcopy' >& /dev/null")))
    (message "running command: %s" cmd)
    (shell-command-on-region (mark) (point) cmd))
  (deactivate-mark))

(setq lastw-screen-window -1)
(defun bring-screen-window-front ()
  "If running in screen tell screen to switch to our window"
  (let ((window (getenv "WINDOW"))
        (sty (getenv "STY")))
    (if sty
        (shell-command-to-string (concat "screen -X select " window)))))

(defun return-to-last-screen-window ()
  "Return to previous screen window"
  (if (getenv "STY")
      (shell-command-to-string "screen -X other")))
(add-hook 'server-visit-hook 'bring-screen-window-front)
(add-hook 'server-done-hook 'return-to-last-screen-window)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


;; (require 'flymake)

;; (defun flymake-elisp-init ()
;;   (unless (string-match "^ " (buffer-name))
;;     (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                          'flymake-create-temp-inplace))
;;            (local-file  (file-relative-name
;;                          temp-file
;;                          (file-name-directory buffer-file-name))))
;;       (list
;;        (expand-file-name invocation-name invocation-directory)
;;        (list
;;         "-Q" "--batch" "--eval"
;;         (prin1-to-string
;;          (quote
;;           (dolist (file command-line-args-left)
;;             (with-temp-buffer
;;               (insert-file-contents file)
;;               (condition-case data
;;                   (scan-sexps (point-min) (point-max))
;;                 (scan-error
;;                  (goto-char(nth 2 data))
;;                  (princ (format "%s:%s: error: Unmatched bracket or quote\n"
;;                                 file (line-number-at-pos)))))))
;;           )
;;          )
;;         local-file)))))

;; (push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)

;; (add-hook 'emacs-lisp-mode-hook
;;           ;; workaround for (eq buffer-file-name nil)
;;           (function (lambda () (if buffer-file-name (flymake-mode)))))

(defun narrow-to-python-string ()
  "Narrow to the multiline string section that contains the point"
  (interactive)
  (let (sstart
        send
        sstr
        (smatch "\\(\"\"\"\\|\'\'\'\\)"))
    (save-excursion

      (if (not (looking-at smatch))
          (re-search-backward "\\(\"\"\"\\|\'\'\'\\)"))
      (setq sstr (match-string 0))
      (setq sstart (match-end 0))
      (goto-char sstart)
      (message (format "sstart %d" sstart))
      (re-search-forward sstr)
      (setq send (match-beginning 0))
      (message (format "send %d" send)))
    (narrow-to-region sstart send)
    (message (format "narrowed to %d:%d" sstart send))
    sstart))

(defun narrow-to-line ()
  "Narrow to the current line"
  (let (beg end)
    (save-excursion
      (move-end-of-line 1)
      (setq end (point))
      (move-beginning-of-line 1)
      (setq beg (point))
      (message (format "narrow to line %d:%d" beg end)))
    (narrow-to-region beg end)
    (values beg end)))

(defun delete-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line 1))

(defun tr-param ()
  "Translate @param to rst style - ``"
  (interactive)
  (save-excursion
    (save-restriction
      (let (beg end val indent ptype sym (tsym "") (ppos 0) (tpos 0) (npos 0)
                (psmatch "\\(?:@\\(param\\) *\\([[:alnum:]_]+\\) *: *\\|@\\(return\\): *\\)")
                (pmatch "\\(?:@\\(param\\) *\\([[:alnum:]_]+\\) *: *\\|@\\(return\\): *\\(.*\\)\\)"))
                                        ; Operate in the doc-string only.
        (narrow-to-python-string)
        (save-restriction
          (setq val (narrow-to-line))
          (setq beg (nth 0 val))
          (setq end (nth 1 val))
          (goto-char beg)
          (re-search-forward pmatch))
        (setq ptype (match-string 1))
        (if (not ptype)
            (setq ptype (match-string 3))
          (setq sym (match-string 2)))
        (setq ppos (match-beginning 0))
        (setq indent (- ppos beg)) ; indent of param
        (message (format "beginning %d indent %d" ppos indent))
                                        ; Get any type definition and remove the line
        (ignore-errors
          (save-excursion
            (if (equal ptype "param")
                (re-search-forward (concat "@type *" sym " *: *\\(.*\\)"))
              (re-search-forward (concat "@rtype: *\\(.*\\)")))
            (setq tpos (match-beginning 0))
            (setq tsym (match-string 1))
            (setq tsym (replace-regexp-in-string "[tT]rue or [fF]alse" "`bool`" tsym))
            (setq tsym (replace-regexp-in-string "\\<string\\>" "`str`" tsym))
            (setq tsym (replace-regexp-in-string "\\<[Bb]oolean\\>" "`bool`" tsym))
            (setq tsym (replace-regexp-in-string "\\<[Bb]ool\\>" "`bool`" tsym))
            (setq tsym (replace-regexp-in-string "\\<integer\\>" "`int`" tsym))
            (setq tsym (replace-regexp-in-string "\\<int\\>" "`int`" tsym))
            (setq tsym (replace-regexp-in-string "\\<list\\(()\\)?" "`list`" tsym))
            (setq tsym (replace-regexp-in-string "\\<dict\\(()\\)?" "`dict`" tsym))
            (setq tsym (replace-regexp-in-string "L{\\([^}]+\\)}" "`\\1`" tsym))
            (save-excursion
              (goto-char tpos)
              (delete-line))))
        (goto-char beg)
        (re-search-forward psmatch)
        (if (equal ptype "param")
            (replace-match (concat "  - `" sym "` (" tsym ") - "))
          (if (equal tsym "")
              (replace-match (concat ":return: "))
            (replace-match (concat ":return: (" tsym ") "))))
        (condition-case nil
            (progn
              (re-search-forward "@\\(param\\|return\\)" nil)
              (point))
          (error (point-max)))))))
;; re-indent folloiwng lines to our - until we reach a blank line or a line
;; containing @ or the ned of our region

(defun tr-all-param ()
  "Translate all paramters"
  (interactive)
  (save-excursion
    (let (send
          sstart
          (cpos (point)))
      (save-restriction
        (let (indent)
          ;; Operate in the doc-string only.
          (setq sstart (narrow-to-python-string))
          (goto-char sstart)
          (setq cpos sstart)
          (setq send (point-max))
          (message (format "pmax %d" send))
          ;; Find the first param
          (re-search-forward "^\\( +\\)@param")
          (setq indent (match-string 1))
          (replace-match (concat indent ":Parameters:\n" indent "@param"))))
      ;; now run tr-param until we are done
      (while (< cpos send)
        (setq cpos (tr-param))
        (goto-char cpos)))))

(defun read-lines (fPath)
  "Return a list of lines of a file at FPATH."
  (with-temp-buffer
    (insert-file-contents fPath)
    (split-string (buffer-string) "\n" t)))

(eval-after-load "elisp-mode"
  '(progn
     (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
     (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
     ))

(eval-after-load "lisp-mode"
  '(progn
     (modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
     (modify-syntax-entry ?- "w" lisp-mode-syntax-table)
     ))

(defun _url-open-with-buffer (url)
  "Open a URL into a file buffer"
  (interactive "M")
  (let* ((basename (url-file-nondirectory url))
         (url-content-buffer (url-retrieve-synchronously url)))
    (set-buffer url-content-buffer)
    (rename-buffer basename 'unique)))

(defun url-open-with-buffer (url)
  "Open a URL into a file buffer"
  (interactive "M")
  (let* ((basename (url-file-nondirectory url))
         (nbuffer (generate-new-buffer basename)))
    (switch-to-buffer nbuffer)
    (shell-command (concat "curl -s " url) nbuffer)))


(defun org-todo-last-week-done ()
  (interactive)
  (org-tags-view nil "CLOSED>=\"<-1w>\""))

(defun dumpvals (regex)
  "Dump all variables and their values from a given regexp"
  (let ((variables (mapcar (lambda (x) (car x))
                           (remove-if-not (lambda (x) (not (functionp (car x))))
                                          (apropos regex)))))
    (dolist (var variables)
      (if (boundp var)
          (message "%s: \"%s\"" var (eval var t))
        (message "%s: unbound" var)))))

(provide 'generic-lisp)
