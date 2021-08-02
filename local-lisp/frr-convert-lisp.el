;;
;; July 23 2021, Christian Hopps <chopps@gmail.com>
;;
;; Copyright (c) 2021 by Christian E. Hopps
;; All rights reserved.
;;

(defun frr-scan-imports ()
  (interactive)
  (let* ((first-import-line-re "^\\(import .*\\|from .*\\)$")
         (import-line-re "^\\(import .*\\|from .*\\|[ \\t]+[a-zA-Z0-9_ ,]+)?\\)$")
         (blank-line-re "^[ \\t]*$")
         (comment-line-re "^[ \\t]*#.*$")
         (cwd-line-re "^CWD =.*$")
         (path-mod-re "\\(sys.path.append\\|sys.path.insert\\).*$")

         (remove-line-re
          (concat "\\(" (mapconcat 'identity
                                   (list blank-line-re comment-line-re path-mod-re) "\\|")
                  "\\)"))
         (non-break-line-re
          (concat "\\(" (mapconcat 'identity
                                   (list import-line-re blank-line-re comment-line-re cwd-line-re path-mod-re) "\\|")
                  "\\)"))
         cwd-line
         import-start
         import-end)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward first-import-line-re)
      (setq import-start (point))
      (beginning-of-line)
      (while (re-search-forward non-break-line-re (line-end-position) t)
        (beginning-of-line)
        (if (looking-at remove-line-re)
            (delete-line)
          (if (not (looking-at cwd-line-re))
              (forward-line)
            (setq cwd-line (concat (match-string 0) "\n"))
            (delete-line))))
      (save-restriction
        (narrow-to-region import-start (point))
        (goto-char (point-max))
        (insert "\n")
        (goto-char (point-max))
        (if cwd-line (insert cwd-line)))
      )
    )
  (py-isort-buffer)
  (save-buffer)
  (spacemacs/python-remove-unused-imports)
  (save-buffer)
  )

(defun frr-remove-topo ()
  (interactive)
  (let* ((topo-class-re "^class \\([a-zA-Z0-9_]+\\) ?(Topo):.*$")
         (build-def-re "^    def build(.*):.*$")
         (get-tgen-re ".*tgen = get_topogen.*")
         (indent-re "^\\(    \\|$\\)")
         (blank-line-re "^[ \\t]*$")
         (comment-line-re "^[ \\t]*#.*$")
         (remove-line-re (concat "\\(" blank-line-re "\\|" comment-line-re "\\|" get-tgen-re "\\)"))
         topo-class)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward topo-class-re nil t)
        (setq topo-class (match-string 1))
        (delete-line)
        (beginning-of-line)
        (while (not (looking-at build-def-re))
          (delete-line))
        (replace-match "def build_topo(tgen):\n" t)
        (while (looking-at remove-line-re)
          (delete-line))
        (while (looking-at indent-re)
          (if (= (length (match-string 0)) 0)
              (forward-line)
            (replace-match "")
            (beginning-of-line)
            (if (looking-at get-tgen-re)
                (delete-line)
              (forward-line))))
        (message "topo-class: %s" topo-class)
        (re-search-forward (concat "Topogen(" topo-class))
        (replace-match "Topogen(build_topo" t)))))

(defun frr-fixup-test ()
  (interactive)
  (frr-scan-imports)
  (frr-remove-topo))

(spacemacs/set-leader-keys-for-major-mode 'python-mode "9" 'frr-fixup-test)


(fset 'json-convert
   (kmacro-lambda-form [?g ?g ?/ ?j ?s ?o ?n ?F ?i ?l ?e ?\C-m ?k ?m ?a ?j ?j ?j ?j ?j ?j ?d ?\' ?a ?/ ?b ?u ?i ?l ?d ?_ ?t ?o ?p ?o ?\C-m ?0 ?d ?\] ?\] ?n ?0 ?\" ?2 ?P ?d ?d ?\C-i ?c ?w ?j ?s ?o ?n ?_ ?f ?i ?l ?e escape ?j ?d ?d ?d ?d ?d ?d ?d ?d ?d ?d ?n ?c ?w ?j ?s ?o ?n ?_ ?f ?i ?l ?e escape ?0 ?j ?k ?o ?g ?l ?o ?b ?a ?l ?  ?t ?o ?p ?o ?\C-m ?t ?o ?p ?o ?  ?= ?  ?t ?g ?e ?n ?. ?j ?s ?o ?n ?_ ?t ?o ?p ?o escape] 0 "%d"))

(bind-kma
