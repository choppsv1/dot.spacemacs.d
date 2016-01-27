;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; spacemacs-helm
     (auto-completion :variables
                      auto-completion-private-snippets-directory "~/.spacemacs.d/private/snippets")

     ;; ;; better-defaults
     erc
     ;; rcirc
     ;; bb-erc
     ;; eyebrowse blows layouts away!
     gtags
     mu4e
     (osx :variables
           osx-use-option-as-meta t)
     org
     org2blog
     ranger
     rebox
     ;; (rcirc :variables
     ;;        rcirc-enable-authinfo-support t)
     spell-checking
     ;; spotify
     syntax-checking
     ;; version-control

     ;; ;; Langs
     ;; c-c++
     ;; emacs-lisp
     ;; ;; erlang
     ;; git
     ;; ;; go
     ;; html
     ;; ;; java
     ;; javascript
     ;; (latex :variables
     ;;        latex-build-command "latexmk")
     ;; lua
     ;; ;; markdown
     ;; ;; php
     ;; (python :variables
     ;;         python-fill-column 120)
     ;; ;; ruby
     ;; shell-scripts
     ;; yaml

     ;;(ietf :variables ietf-docs-cache "~/ietf-docs-cache")
     ietf

     ;; Languages
     c-c++
     emacs-lisp
     git
     html
     javascript
     lua
     (python :variables
             python-fill-column 120)
     shell-scripts
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(mu4e-maildirs-extension smartparens) ; evil-org
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(misterioso
                         spacemacs-dark
                         spacemacs-light
                         ;; solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   ;; dotspacemacs-default-font '("DejaVu Sans Mono"
   ;;                             :size 28
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.1)
   dotspacemacs-default-font '("Ubuntu Mono-10" :weight normal :width normal :powerline-scale 1.4)

   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ;; Allow for adding to use package configuration.
   ;; use-package-inject-hooks t
   )
  )

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost any
user code here.  The exception is org related code, which should be placed in `dotspacemacs/user-config'."

  ;; =========
  ;; User-init
  ;; =========

  (add-to-list 'load-path (concat dotspacemacs-directory "local-lisp"))
  ;;(require 'iterm-custom-keys)
  (require 'iterm-xterm-extra)
  (require 'generic-lisp)
  (require 'generic-mode-hooks)

  (auto-insert-mode)

  (setq
   debug-init-msg t
   dropbox-directory "~/Dropbox"
   evil-search-wrap nil
   evil-want-C-i-jump nil
   ;; evil-esc-delay 0.001
   ;; js2-basic-offset 2
   ;; js-indent-level 1
   org-directory "~/org"
   org-agenda-files '("~/org")
   org-protocol-default-template-key "t"
   org2blog/wp-shortcode-langs-map '(("emacs-lisp" . "lisp") ("sh" . "bash"))
   rebox-style-loop '(71 72 73))

  (with-eval-after-load "evil-evilified-state"
    (define-key evil-evilified-state-map-original "H" 'evil-window-top)
    (define-key evil-evilified-state-map-original "L" 'evil-window-bottom)
    (define-key evil-evilified-state-map-original "M" 'evil-window-middle))

  ;; =======
  ;; Display
  ;; =======

  ;; (setq fci-rule-character ?\u2509)
  (setq fci-rule-character ?\u250A)
  (setq fci-rule-character-color "#121212")
  ;; (setq fci-rule-color "#222222")

  ;; =================================
  ;; Global Key Bindings and Registers
  ;; =================================

  (fold-section "Keybindings"
                (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

                (global-set-key (kbd "M-W") 'kill-region-to-ssh)
                (global-set-key (kbd "M-Y") 'yank-from-ssh)
                ;; (global-set-key (kbd "M-Q") 'rebox-dwim)

                ;; Find emacs source
                (global-set-key (kbd "C-h C-l") 'find-library)
                (global-set-key (kbd "C-h C-f") 'find-function)
                (global-set-key (kbd "C-h C-k") 'find-function-on-key)
                (global-set-key (kbd "C-h C-v") 'find-variable)

                (spacemacs/set-leader-keys "W" 'spacemacs/workspaces-micro-state)

                (global-set-key (kbd "C-M-1") 'eyebrowse-switch-to-window-config-1)
                (global-set-key (kbd "C-M-2") 'eyebrowse-switch-to-window-config-2)
                (global-set-key (kbd "C-M-3") 'eyebrowse-switch-to-window-config-3)
                (global-set-key (kbd "C-M-4") 'eyebrowse-switch-to-window-config-4)
                (global-set-key (kbd "C-M-5") 'eyebrowse-switch-to-window-config-5)

                (spacemacs/set-leader-keys
                  "oa" 'org-agenda
                  ;; "og" 'helm-org-agenda-files-headings
                  ;; "oo" 'org-clock-out
                  "oc" 'org-capture
                  "oC" 'helm-org-capture-templates ;requires templates to be defined.
                  "ol" 'org-store-link
                  "aL" 'lisp-interaction-mode
                  )
                )
  (fold-section "Registers (files)"
                ;; (set-register ?E `(file . ,emacs-init-source))
                (progn
                  (set-register ?W `(file . ,(concat org-directory "/work.org")))
                  (set-register ?N `(file . ,(concat org-directory "/notes.org")))
                  (set-register ?I `(file . ,(concat org-directory "/ietf.org")))
                  (set-register ?S `(file . ,(concat org-directory "/status.org")))
                  (set-register ?P `(file . ,(concat dropbox-directory "/ts-pass.gpg")))
                  ))

  ;; =================
  ;; Programming Modes
  ;; =================

  ;; (spacemacs|use-package-add-hook rebox2
  ;;   :post-init
  ;;   (progn
  ;;     (setq rebox-style-loop '(81 82 83))
  ;;     ;; (setq rebox-style-loop '(71 72 73 74 75 76 77 81 82 83 84 85 86 87))
  ;;     ;; C-mode comments
  ;;     ;; (setq-default '(241 235 243))
  ;;     (add-hook 'all-prog-mode-hook 'rebox-mode)
  ;;     )
  ;;   )


  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  (progn


    ;; =======
    ;; Display
    ;; =======


    (fold-section "display"
                  (global-hl-line-mode -1)            ; Disable hihglighting of current line.

                  ;; fill-column-mode character doesn't work
                  (set-face-inverse-video-p 'vertical-border nil)
                  (set-face-background 'vertical-border (face-background 'default))
                  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?\u2999))
                  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?\u299A))
                  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?\u2503))

                  (setq spaceline-window-numbers-unicode nil
                        spaceline-workspace-numbers-unicode nil)
                  )

    ;; =======
    ;; Layouts
    ;; =======

    (fold-section "layouts"
                  (spacemacs|define-custom-layout "mail"
                                                  :binding "m"
                                                  :body
                                                  (mu4e))

                  (spacemacs|define-custom-layout "irc"
                    :binding "i"
                    :body
                    (progn
                      (add-hook 'erc-mode-hook #'(lambda ()
                                                   (persp-add-buffer (current-buffer))))
                      (launch-irc-jabber)
                      (split-window-right)
                      (launch-irc-gitter))
                    )
                  (spacemacs|define-custom-layout "W:OCP"
                    :binding "wo"
                    :body
                    (find-file "~/w/ocp/setup.py")
                    )
                  (spacemacs|define-custom-layout "W:JDSU"
                    :binding "wj"
                    :body
                    (find-file "~/w/jdsu-ocm/setup.py")
                    )
                  (spacemacs|define-custom-layout "W:UTIL"
                    :binding "wu"
                    :body
                    (find-file "~/w/tsutil/setup.py")
                    )
                  (spacemacs|define-custom-layout "W:CASSFILE"
                    :binding "wc"
                    :body
                    (find-file "~/w/cassfile/setup.py")
                    )
                  (spacemacs|define-custom-layout "W:ONDATA"
                    :binding "wd"
                    :body
                    (find-file "~/w/optical-network-data/terastream.yang")
                    )
                  )

    ;; ===========
    ;; Keybindings
    ;; ===========

    (fold-section "bindings"
                  (defun dear-leader/swap-keys (key1 key2)
                    (let ((map1 (lookup-key spacemacs-default-map key1))
                          (map2 (lookup-key spacemacs-default-map key2)))
                      (spacemacs/set-leader-keys key1 map2 key2 map1)))
                  (dear-leader/swap-keys "am" "aM")
                  (global-set-key (kbd "C-\\") 'spacemacs//layouts-persp-next-C-l)
                  )

    ;; ==========
    ;; Messaging
    ;; ==========

    (defun my-erc-perspective ()
      "Switch or create to a perspective called 'erc' and connect to IRC"
      (interactive)
      (select-frame (make-frame '((name . "ERC") (minibuffer . t))))
      (persp-switch "freenode.net")
      (erc :server "asimov.freenode.net" :port "6667" :nick "chopps"))

    (when (configuration-layer/layer-usedp 'mu4e)
      (progn
        (defcustom mu4e-spam-folder "/chopps.org/spam-train"
          "Folder for spam email"
          :type '(string :tag "Folder name")
          :group 'mu4e-folders)


        (setq mu4e-maildir "~/Maildir"
              ;; Updating
              ;; mu4e-pre-hook-count 0
              ;; mu4e-full-update-mail-command "bash -c '(cd && offlineimap -l /Users/chopps/.offlineimap/logfile)'"
              ;; mu4e-quick-update-mail-command "bash -c '(cd && offlineimap -q -l /Users/chopps/.offlineimap/logfile)'"
              ;; mu4e-update-pre-hook 'mu4e-pre-hook-udpate-command
              mu4e-mu-binary (executable-find "mu")
              mu4e-update-interval nil
              mu4e-headers-include-related nil

              ;; [b]ookmarks
              mu4e-not-junk-folder-filter " AND NOT ( maildir:/gmail.com/[Gmail].Spam OR maildir:/chopps.org/spam* ) "
              mu4e-inbox-filter-base " ( maildir:/gmail.com/INBOX OR maildir:/chopps.org/INBOX OR maildir:/terastrm.net/INBOX OR maildir:/chopps.org/a-terastream ) "
              mu4e-imp-filter-base " ( maildir:/chopps.org/sw-common OR maildir:/chopps.org/ietf-rtg-yang-dt OR maildir:/chopps.org/ietf-wg-isis OR maildir:/chopps.org/ietf-wg-netmod OR maildir:/chopps.org/ietf-wg-homenet ) "
              mu4e-unread-filter " ( flag:unread AND NOT flag:flagged AND NOT flag:trashed ) "
              mu4e-unread-flagged-filter " ( flag:unread AND flag:flagged AND NOT flag:trashed ) "
              mu4e-bookmarks (append
                              (list (list (concat "flag:unread AND NOT flag:trashed AND " mu4e-inbox-filter-base) "Unread [i]NBOX messages" ?i)

                                    (list (concat "flag:flagged AND NOT flag:trashed AND " mu4e-inbox-filter-base) "[f]lagged INBOX messages" ?f)
                                    (list (concat "flag:flagged AND NOT flag:trashed AND NOT " mu4e-inbox-filter-base mu4e-not-junk-folder-filter) "[F]lagged Non-INBOX messages" ?F)

                                    (list (concat mu4e-unread-filter         mu4e-imp-filter-base) "Unread Important messages" ?n)
                                    (list (concat mu4e-unread-flagged-filter mu4e-imp-filter-base) "Unread-Flagged Important messages" ?N)

                                    (list (concat mu4e-unread-filter         "AND NOT" mu4e-imp-filter-base " AND NOT " mu4e-inbox-filter-base mu4e-not-junk-folder-filter) "Unread [u]nimportant messages" ?u)
                                    (list (concat mu4e-unread-flagged-filter "AND NOT" mu4e-imp-filter-base " AND NOT " mu4e-inbox-filter-base mu4e-not-junk-folder-filter) "Unread-Flagged [U]nimportant messages" ?U)

                                    (list (concat mu4e-unread-filter         " AND NOT " mu4e-inbox-filter-base mu4e-not-junk-folder-filter) "Unread Non-INBOX messages" ?o)
                                    (list (concat mu4e-unread-flagged-filter " AND NOT " mu4e-inbox-filter-base mu4e-not-junk-folder-filter) "Unread-Flagged Non-INBOX messages" ?O)

                                    (list (concat mu4e-unread-filter         mu4e-not-junk-folder-filter) "Unread messages" ?a)
                                    (list (concat mu4e-unread-flagged-filter mu4e-not-junk-folder-filter) "Unread-flagged messages" ?A)

                                    (list "maildir:/chopps.org/spam-probable" "Probable spam messages" ?s)
                                    )
                              (mapcar (lambda (x) (cons (concat (car x) mu4e-not-junk-folder-filter) (cdr x)))
                                      '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                                        ("date:1h..now" "Last hours messages" ?h)
                                        ("date:24h..now" "Today's messages" ?d)
                                        ("date:today..now" "Today's messages" ?t)
                                        ("date:7d..now" "Last 7 days" ?w)
                                        ("mime:7d..now" "Last 7 days" ?w)
                                        ("mime:*pdf" "Messages with PDF" 112)
                                        ("mime:*vcs" "Messages with VCS" 113)
                                        )))


              ;; [j]ump shortcuts
              mu4e-maildir-shortcuts '(("/chopps.org/INBOX" . ?i)
                                       ("/gmail.com/INBOX" . ?g)
                                       ("/terastrm.net/INBOX" . ?w)
                                       ("/chopps.org/receipts" . ?r)
                                       ("/chopps.org/a-terastream" . ?t)
                                       ("/chopps.org/aa-netbsd" . ?n)
                                       ("/chopps.org/ietf-wg-isis" . ?I)
                                       ("/chopps.org/ietf-wg-homenet" . ?H)
                                       ("/chopps.org/ietf-wg-netmod" . ?N)
                                       ("/chopps.org/spam-train" . ?S)
                                       ("/chopps.org/spam-probable" . ?s))

              ;; Visuals
              mu4e-use-fancy-chars nil
              mu4e-view-show-addresses t
              mu4e-headers-visible-lines 15
              mu4e-headers-visible-columns 80
              mu4e-html2text-command 'mu4e-shr2text
              ;; make work better in dark themes
              ;; [[mu4e:msgid:87vb7ng3tn.fsf@djcbsoftware.nl][Re: I find html2markdown the best value for mu4e-html2text-command]]
              shr-color-visible-luminance-min 80
              mu4e-view-html-plaintext-ratio-heuristic 15
              ;; mu4e-html2text-command "html2text -nobs -utf8 -width 120"

              ;; Folders -- most setup per account
              ;; see context below

              ;; mu4e-sent-folder   "/chopps.org/Sent Messages"
              ;; mu4e-drafts-folder "/chopps.org/Drafts"
              ;; mu4e-trash-folder  "/chopps.org/Deleted Messages"

              mu4e-attachment-dir "~/Downloads"

              ;; only complete addresses found in email to one of the below addresses
              mu4e-compose-complete-only-personal t
              mu4e-user-mail-address-list (list "chopps@chopps.org"
                                                "chopps@dev.terastrm.net"
                                                "chopps@gmail.com"
                                                "chopps@netbsd.org"
                                                "chopps@rawdofmt.org")
              ;; This isn't used yet but we'd like it to be for getting
              ;; contact completions from any mail with these addresses in them.
              mu4e-contacts-user-mail-address-list (list
                                                    "chopps@chopps.org"
                                                    "chopps@dev.terastrm.net"
                                                    "chopps@gmail.com"
                                                    "chopps@netbsd.org"
                                                    "chopps@rawdofmt.org"
                                                    ;; we want contacts added from these mailing lists
                                                    "isis-wg@ietf.org"
                                                    "developers@netbsd.org"
                                                    "netbsd-developers@netbsd.org"
                                                    )

              mu4e-compose-signature-auto-include nil
              mu4e-compose-complete-addresses t
              message-completion-alistp '(("^\\(Newsgroups\\|Followup-To\\|Posted-To\\|Gcc\\):" . message-expand-group)
                                          ("^\\(Resent-\\)?\\(To\\|B?Cc\\):" . my-message-expand-name)
                                          ("^\\(Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):" . message-expand-name)
                                          ("^\\(Disposition-Notification-To\\|Return-Receipt-To\\):" . message-expand-name))



              ;; don't keep message buffers around
              message-kill-buffer-on-exit t
              )

        (defun ch:ct (clist)
          "Transform candidate into (display . real)"
          (mapcar (lambda (candidate)
                    (let* ((name (plist-get candidate :name))
                           email (plist-get candidate :mail))
                      (or (and name (format "%s <%s>" name email))
                          email))) clist))
        (defun my-message-expand-name (&optional start)
          (interactive)
          (message "my-message-expand-name called")
          (helm :prompt "contact:" :sources
                (helm-build-sync-source "mu4e contacts"
                  :candidates mu4e~contact-list :candidate-transformer 'ch:ct)))

        (defun my-mu4e-compose-hook ()
          "Setup outgoing messages"
          ;; Add chopps@<account-sending-from> to CC
          ;; Add chopps@chopps.org to Bcc if not sending from @chopps.org
          (let ((buffer-modified (buffer-modified-p)))
            (save-excursion
              (message-add-header (concat "Cc: " user-mail-address))
              (if (not (string= user-mail-address "chopps@chopps.org"))
                  (message-add-header "Bcc: chopps@chopps.org")))
            (set-buffer-modified-p buffer-modified))
          ;; Outgoing mails get format=flowed.
          ;; (use-hard-newlines t 'guess)
          ;; Sign messages by default
          (mml-secure-message-sign-pgpmime))

        (defun mu4e-pre-hook-udpate-command ()
          (let ((check (% mu4e-pre-hook-count 4)))
            (setq mu4e-get-mail-command (if (= check 0)
                                            mu4e-full-update-mail-command
                                          mu4e-full-update-mail-command))
            (setq mu4e-pre-hook-count (1+ mu4e-pre-hook-count))))

        ;; Mark to move to spam folder from headers view.
        (defun mu4e-headers-mark-move-to-spam ()
          (interactive)
          (mu4e-mark-set 'move mu4e-spam-folder)
          (mu4e-headers-next))

        ;; Mark to move to spam folder from message view.
        (defun mu4e-view-mark-move-to-spam ()
          (interactive)
          (mu4e~view-in-headers-context
           (mu4e-headers-mark-move-to-spam)))
        (message "post-init end mu4e eval after load")

        ;; XXX these aren't defined
        ;; (define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
        ;; (define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)

        (with-eval-after-load 'mu4e
          (bind-key (kbd "'") 'mu4e-headers-next 'mu4e-headers-mode-map)
          (bind-key (kbd "\"") 'mu4e-headers-prev 'mu4e-headers-mode-map)
          (bind-key (kbd "\"") 'mu4e-view-headers-prev 'mu4e-view-mode-map)
          (bind-key (kbd "f") 'mu4e-view-go-to-url 'mu4e-view-mode-map)

          (when (require 'mu4e-context nil t)
            (setq mu4e-contexts `( ,(make-mu4e-context
                                     :name "chopps.org"
                                     :enter-func (lambda () (mu4e-message "Home"))
                                     ;; no leave-func
                                     :match-func (lambda (msg)
                                                   (and msg (string-match "/chopps.org/.*" (mu4e-message-field msg :maildir))))
                                     :vars '((user-mail-address  . "chopps@chopps.org")
                                             ;; mu4e
                                             (mu4e-sent-folder   . "/chopps.org/Sent Messages")
                                             (mu4e-trash-folder  . "/chopps.org/Deleted Messages")
                                             (mu4e-drafts-folder . "/chopps.org/Drafts")
                                             (mu4e-sent-messages-behavior   . sent)
                                             ;; smtp
                                             (smtpmail-starttls-credentials . '(("smtp.chopps.org" 9005 nil nil)))
                                             (smtpmail-default-smtp-server  . "smtp.chopps.org")
                                             (smtpmail-smtp-server          . "smtp.chopps.org")
                                             ;; smtpmail-local-domain?
                                             ;; smtpmail-sendto-domain?
                                             (smtpmail-smtp-service         . 9005)))
                                   ,(make-mu4e-context
                                     :name "dev.terastrm.net"
                                     :enter-func (lambda () (mu4e-message "Work"))
                                     ;; no leave-func
                                     :match-func (lambda (msg)
                                                   (and msg (string-match "/terastrm.net/.*" (mu4e-message-field msg :maildir))))
                                     :vars '(
                                             ;; about me
                                             (user-mail-address  . "chopps@dev.terastrm.net")
                                             ;; mu4e
                                             (mu4e-sent-folder   . "/terastrm.net/Sent Messages")
                                             (mu4e-trash-folder  . "/terastrm.net/Deleted Messages")
                                             (mu4e-drafts-folder . "/terastrm.net/Drafts")
                                             (mu4e-sent-messages-behavior   . sent)
                                             ;; smtp
                                             (smtpmail-starttls-credentials . '(("smtp.dev.terastrm.net" 587 nil nil)))
                                             (smtpmail-default-smtp-server  . "smtp.dev.terastrm.net")
                                             (smtpmail-smtp-server          . "smtp.dev.terastrm.net")
                                             ;; smtpmail-local-domain?
                                             ;; smtpmail-sendto-domain?
                                             (smtpmail-smtp-service         . 587)))

                                   ,(make-mu4e-context
                                     :name "gmail.com"
                                     :enter-func (lambda () (mu4e-message "Gmail"))
                                     ;; no leave-func
                                     :match-func (lambda (msg)
                                                   (and msg (string-match "/gmail.com/.*" (mu4e-message-field msg :maildir))))
                                     :vars '(
                                             ;; about me
                                             (user-mail-address  . "chopps@gmail.com")
                                             ;; mu4e
                                             (mu4e-drafts-folder . "/gmail.com/[Gmail].Drafts")
                                             (mu4e-sent-folder   . "/gmail.com/[Gmail].Sent Mail")
                                             (mu4e-trash-folder  . "/gmail.com/[Gmail].Trash")
                                             (mu4e-sent-messages-behavior   . delete)
                                             ;; smtp
                                             (smtpmail-starttls-credentials . '(("smtp.gmail.com" 587 nil nil)))
                                             (smtpmail-default-smtp-server  . "smtp.gmail.com")
                                             (smtpmail-smtp-server          . "smtp.gmail.com")
                                             ;; smtpmail-local-domain?
                                             ;; smtpmail-sendto-domain?
                                             (smtpmail-smtp-service . 587))))))

          (require 'mu4e-contrib)

          ;; XXX also add this back
          ;; (mu4e-alert-enable-mode-line-display)
          ;; (mu4e-alert-enable-notifications)

          (add-hook 'mu4e-headers-mode-hook
                    (lambda () (progn
                                 (make-local-variable 'scroll-conservatively)
                                 (setq
                                  show-trailing-whitespace nil
                                  scroll-conservatively 0
                                  scroll-up-aggressively .8
                                  scroll-down-aggressively .8)
                                 )))
          (add-hook 'mu4e-view-mode-hook
                    (lambda () (setq show-trailing-whitespace nil)))

          (add-hook 'mu4e-compose-mode-hook 'my-mu4e-compose-hook)

          (add-to-list 'mu4e-view-actions
                       '("ViewInBrowser" . mu4e-action-view-in-browser))

          (define-key mu4e-headers-mode-map "d" 'mu4e-headers-mark-for-read)
          (define-key mu4e-view-mode-map "d" 'mu4e-view-mark-for-read)
          (define-key mu4e-headers-mode-map "@" 'mu4e-headers-mark-move-to-spam)
          (define-key mu4e-view-mode-map "@" 'mu4e-view-mark-move-to-spam)
          (define-key mu4e-headers-mode-map "\\" 'mu4e-headers-mark-move-to-spam)
          (define-key mu4e-view-mode-map "\\" 'mu4e-view-mark-move-to-spam)

          (add-to-list 'mu4e-header-info-custom
                       '(:list-or-dir .
                                      (:name "ML or maildir" ;; long name, as seen in message view
                                             :shortname "ML-D"     ;; short name, as seen in the headers view
                                             :help "Mailing list or maildir if not set"
                                             :function
                                             (lambda (msg)
                                               (or (mu4e-message-field msg :mailing-list)
                                                   (mu4e-message-field msg :maildir))))))

          (setq
           ;; "Date         Flgs   List       From                   Subject
           mu4e-headers-fields (quote (
                                       (:flags          .  5)
                                       (:human-date     . 15)
                                       (:from           . 26)
                                       (:list-or-dir    . 30)
                                       (:thread-subject . nil))))

          )
        )
      )
    (when (configuration-layer/layer-usedp 'erc)
      (setq erc-prompt-for-nickserv-password nil
            erc-autojoin-channels-alist '(("irc.gitter.im" "#syl20bnr/spacemacs")
                                          ("mollari.netbsd.org" "#NetBSD")
                                          ("freenode.net" "#org-mode"))
            erc-spelling-mode t
            erc-auto-query 'window
            erc-track-switch-direction 'importance
            erc-hl-nicks-minimum-contrast-ratio 3.5
            ;; erc-hl-nicks-color-contrast-strategy 'contrast
            ;; erc-hl-nicks-color-contrast-strategy 'invert
            erc-hl-nicks-skip-nicks '("gitter")
            erc-notifications-icon (concat user-emacs-directory "./layers/+irc/rcirc/img/irc.png")

            )
      ;; We want to be in normal state most of the time so we can flip in and out.
      (evil-set-initial-state 'erc-mode 'normal)
      (add-hook 'erc-send-post-hook 'evil-normal-state)

        ;; '(erc-autoaway-idle-seconds 600)
        ;; '(erc-autojoin-mode t)
        ;; '(erc-button-mode t)
        ;; '(erc-current-nick-highlight-type (quote all))
        ;; '(erc-fill-mode t)
        ;; '(erc-hl-nicks-mode t)
        ;; '(erc-hl-nicks-trim-nick-for-face nil)
        ;; '(erc-irccontrols-mode t)
        ;; '(erc-kill-buffer-on-part t)
        ;; '(erc-kill-queries-on-quit t)
        ;; '(erc-kill-server-buffer-on-quit t)
        ;; '(erc-list-mode t)
        ;; '(erc-log-channels-directory "/Users/chopps/Dropbox/erclogs" t)
        ;; '(erc-log-mode t)
        ;; '(erc-match-mode t)
        ;; '(erc-menu-mode t)
        ;; '(erc-move-to-prompt-mode t)
        ;; '(erc-netsplit-mode t)
        ;; '(erc-networks-mode t)
        ;; '(erc-noncommands-mode t)
        ;; '(erc-pcomplete-mode t)
        ;; '(erc-prompt (lambda nil (concat "[" (buffer-name) "]")))
        ;; '(erc-readonly-mode t)
        ;; '(erc-ring-mode t)
        ;; '(erc-server-coding-system (quote (utf-8 . utf-8)))
        ;; '(erc-services-mode t)
        ;; '(erc-social-graph-dynamic-graph t)
        ;; '(erc-stamp-mode t)
        ;; '(erc-track-minor-mode t)
        ;; '(erc-track-mode t)
        ;; '(erc-youtube-mode t)

      (defun erc-acct-get-password (user host port)
        (let* ((auth-source-creation-defaults nil)
               (auth-source-creation-prompts '((password . "Enter IRC password for %h:%p")))
               (secret (plist-get (nth 0 (auth-source-search
                                          :type 'netrc
                                          :max 1
                                          :host host
                                          :user user
                                          :port port))
                                  :secret))
               (password (if (functionp secret)
                             (funcall secret)
                           secret)))))

      ;; (setq erc-nickserv-passwords
      ;;       `((freenode (("chopps" . ,(erc-acct-get-password "chopps" "freenode.net" "nickserv"))))
      ;;         (localhost (("chopps" . ,(erc-acct-get-password "chopps" "localhost" "bitlbee")))))
      ;;       )

      (defun launch-irc-gitter ()
        "Launch irc connection to giter.im"
        (interactive)
        (erc :server "192.168.1.5" :port 6669 :nick "choppsv1"
            :password (erc-acct-get-password "choppsv1" "192.168.1.5" 6669)))
        ;; (erc-tls :server "irc.gitter.im" :port 6667 :nick "choppsv1"
        ;;         :password (erc-acct-get-password "choppsv1" "irc.gitter.im" 6667)))

      (defun launch-irc-netbsd ()
        "Launch irc connection to netbsd"
        (interactive)
        (erc-tls :server "mollari.netbsd.org" :port 7001 :nick "chopps" :full-name "Christian E. Hopps"))

      (defun launch-irc-freenode ()
        "Launch irc connection to freenode"
        (interactive)
        (erc-tls :server "asimov.freenode.net" :port 6697 :nick "chopps"
                 :password (erc-acct-get-password "chopps" "freenode.net" 6697)))

      (defun launch-irc-jabber ()
        "Launch irc connection to jabber"
        (interactive)
        (erc :server "localhost" :port 6667 :nick "chopps" :full-name "Christian E. Hopps"))

      (defun launch-erc ()
        "Launch all our connections to IRC"
        (interactive)
        (launch-irc-gitter)
        (launch-irc-freenode)
        (launch-irc-netbsd)
        (launch-irc-jabber))

      (spacemacs/set-leader-keys
        "aif" 'launch-irc-freenode
        "aij" 'launch-irc-jabber
        "ain" 'launch-irc-netbsd
        "aig" 'launch-irc-gitter
        "aiL" 'launch-erc)

      (defun bitlbee-netrc-identify ()
        "Auto-identify for Bitlbee channels using authinfo or netrc.

        The entries that we look for in netrc or authinfo files
        have their 'port' set to 'bitlbee', their 'login' or
        'user' set to the current nickname and 'server' set to
        the current IRC server's name. A sample value that works
        for authenticating as user 'keramida' on server
        'localhost' is:

            machine localhost port bitlbee login keramida password supersecret"

        (interactive)
        (when (string= (buffer-name) "&bitlbee")
          (let ((pass (erc-acct-get-password (erc-current-nick) erc-session-server "bitlbee")))
            (message "Sending privmsg to &bitlbee server %s" erc-session-server)
            (erc-message "PRIVMSG"
                         (format "%s identify %s"
                                 (erc-default-target)
                                 pass)))))
      (add-hook 'erc-join-hook 'bitlbee-netrc-identify)

      ;; We only need this if we aren't using our local bridge
      ;; ;; add a user to the current channel
      ;; (defun add-nick-insert-pre-hook (line)
      ;;   "Add user to ERC channel list"
      ;;   (when (string= erc-session-server "irc.gitter.im")
      ;;     (save-match-data
      ;;       (when (string-match "^<\\([^>]+\\)> .*" line)
      ;;         (let ((nick (match-string 1 line)))
      ;;           (erc-update-current-channel-member nick nick 'add-if-new))))))
      ;; (add-hook 'erc-insert-pre-hook 'add-nick-insert-pre-hook)

      (with-eval-after-load "erc"
        (erc-services-mode 1)
        (erc-spelling-mode 1))
      )

    (when (configuration-layer/layer-usedp 'rcirc)
      (defun get-gitter-password ()
        (let* ((auth-source-creation-defaults nil)
               (auth-source-creation-prompts '((password . "Enter IRC password for %h:%p")))
               (sec (plist-get (nth 0 (auth-source-search
                                       :type 'netrc
                                       :max 1
                                       :host "irc.gitter.im"
                                       :port 6667
                                       :user "choppsv1"))
                               :secret)))
          (if (functionp sec)
              (funcall sec)
            sec)))
      (setq
       rcirc-log-directory "~/Dropbox/logs/rcirclogs"
       rcirc-time-format "%H:%M "
       rcirc-server-alist
       `(
         ("127.0.0.1"
          :user "choppsv1"
          :port "6669"
          :password ,(get-gitter-password)
          ;; :encryption tls
          :channels ("#syl20bnr/spacemacs"))
         ("asimov.freenode.net"
          :user "chopps"
          :port "6697"
          :encryption tls
          :channels ("#org-mode")
          )
         ;; ("mollari.netbsd.org"
         ;;  :user "chopps"
         ;;  :port "7001"
         ;;  :encryption tls
         ;;  :channels ("#netbsd")
         ;; )
         ("localhost"
          :user "chopps"
          :port "6667"
          ;; :channels ("#ts")
          )
         )
       )
      )

    ;; =================
    ;; Programming Modes
    ;; =================

    (when (configuration-layer/layer-usedp 'syntax-checking)
      (with-eval-after-load "flycheck"

        (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
        (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)

        (flycheck-define-checker python-pycheckers
                                 "A python syntax and style checker using flake8 and pylint."
                                 :command ("pycheckers.sh"
                                           (config-file "-8" flycheck-flake8rc)
                                           (config-file "-r" flycheck-pylintrc)
                                           source-inplace)
                                 :error-patterns
                                 ((error line-start
                                         (file-name) ":" line ":" (optional column ":") " "
                                         (message "E" (one-or-more digit) (zero-or-more not-newline))
                                         line-end)
                                  (warning line-start
                                           (file-name) ":" line ":" (optional column ":") " "
                                           (message (or "F"            ; Pyflakes in Flake8 >= 2.0
                                                        "W"            ; Pyflakes in Flake8 < 2.0
                                                        "C")           ; McCabe in Flake >= 2.0
                                                    (one-or-more digit) (zero-or-more not-newline))
                                           line-end)
                                  (info line-start
                                        (file-name) ":" line ":" (optional column ":") " "
                                        (message (or "N"              ; pep8-naming in Flake8 >= 2.0
                                                     "R")             ; re-factor from python.
                                                 (one-or-more digit) (zero-or-more not-newline))
                                        line-end)
                                  )
                                 :modes python-mode)))
    (when (configuration-layer/layer-usedp 'emacs-lisp)
      (with-eval-after-load "lisp-mode"
        (defun rebox-lisp-hook ()
          (message "rebox lisp hook")
          (setq-default rebox-style-loop '(81 82 83)))
        (add-hook 'lisp-mode-hook 'rebox-lisp-hook)
        (add-hook 'emacs-lisp-mode-hook 'rebox-lisp-hook)))
    (when (configuration-layer/layer-usedp 'c-c++)
      (setq c-font-lock-extra-types
            (quote
             ("FILE"
              "\\sw+_st" "\\sw+_t" "\\sw+type" ; procket types
              "\\(u_?\\)?int\\(8\\|16\\|32\\|64\\)_t" "ushort" "uchar"
              "bool" "boolean")))

      (with-eval-after-load "cc-mode"
        ;; (modify-syntax-entry ?_ "w" awk-mode-syntax-table)
        (modify-syntax-entry ?_ "w" c-mode-syntax-table)
        (modify-syntax-entry ?_ "w" objc-mode-syntax-table)
        (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
        ;; (modify-syntax-entry ?_ "w" java-mode-syntax-table)
        ;; (modify-syntax-entry ?_ "w" objc-mode-syntax-table)
        (add-hook 'c-mode-common-hook
                  (function (lambda ()
                              (if (string= (shell-command-to-string "uname -s") "NetBSD\n")
                                  (c-set-style "KNF")
                                (c-set-style "Procket")
                                (setq indent-tabs-mode nil))
                              (c-toggle-auto-hungry-state 1)
                              (setq fill-column 80)
                              (flyspell-prog-mode)
                              )))

        (c-add-style
         "KNF"
         '((indent-tabs-mode . t)
           (c-basic-offset . 8)
           (c-comment-only-line-offset . 0)
           (c-label-minimum-indentation . 0)
           (c-tab-always-indent    . t)
           (c-hanging-semi&comma-criteria (lambda () 'stop))
           (c-hanging-braces-alist . ((class-open) (class-close) (defun-open)
                                      (defun-close) (inline-open) (inline-close)
                                      (brace-list-open) (brace-list-close)
                                      (brace-list-intro) (brace-list-entry)
                                      (block-open) (block-close) (substatement-open)
                                      (statement-case-open) (extern-lang-open)
                                      (extern-lang-close)))
           (c-hanging-colons-alist     . ((access-label)
                                          (case-label)
                                          (label)
                                          (member-init-intro)
                                          (inher-intro)))
                                        ;   (c-cleanup-list             . (scope-operator
                                        ;                                 empty-defun-braces
                                        ;                                 defun-close-semi))
           (c-offsets-alist . ((string                . -1000)
                               (c                     . c-lineup-C-comments)
                               (defun-open            . 0)
                               (defun-close           . 0)
                               (defun-block-intro     . +)
                               (func-decl-cont        . 0)
                                        ; above is ansi        (func-decl-cont        . 0)
                               (knr-argdecl-intro     . 0)
                               (knr-argdecl           . 0)
                               (topmost-intro         . 0)
                               (topmost-intro-cont    . 0)
                               (block-open            . 0)
                               (block-close           . 0)
                               (brace-list-open       . 0)
                               (brace-list-close      . 0)
                               (brace-list-intro      . +)
                               (brace-list-entry      . 0)
                               (statement             . 0)
                               (statement-cont        . 4)
                               (statement-block-intro . +)
                               (statement-case-intro  . +)
                               (statement-case-open   . 0)
                               (substatement          . +)
                               (substatement-open     . 0)
                               (case-label            . 0)
                               (label                 . -)
                               (do-while-closure      . 0)
                               (else-clause           . 0)
                               (comment-intro         . c-lineup-comment)
                               (arglist-intro         . 4)
                               (arglist-cont          . 0)
                               (arglist-cont-nonempty . 4)
                               (arglist-close         . 4)
                               (cpp-macro             . -1000)
                               ))))

        (c-add-style
         "Procket"
         '((c-basic-offset . 4)
           (c-comment-only-line-offset . 0)
           (c-label-minimum-indentation . 0)
           (c-tab-always-indent    . t)
           (c-hanging-semi&comma-criteria (lambda () 'stop))
           (c-hanging-braces-alist . ((class-open) (class-close) (defun-open)
                                      (defun-close) (inline-open) (inline-close)
                                      (brace-list-open) (brace-list-close)
                                      (brace-list-intro) (brace-list-entry)
                                      (block-open) (block-close) (substatement-open)
                                      (statement-case-open) (extern-lang-open)
                                      (extern-lang-close)))
           (c-hanging-colons-alist     . ((access-label)
                                          (case-label)
                                          (label)
                                          (member-init-intro)
                                          (inher-intro)))
                                        ;   (c-cleanup-list             . (scope-operator
                                        ;                                 empty-defun-braces
                                        ;                                 defun-close-semi))
           (c-offsets-alist . ((string                . -1000)
                               (c                     . c-lineup-C-comments)
                               (defun-open            . 0)
                               (defun-close           . 0)
                               (defun-block-intro     . +)
                               (func-decl-cont        . 0)
                                        ; above is ansi        (func-decl-cont        . 0)
                               (knr-argdecl-intro     . 0)
                               (knr-argdecl           . 0)
                               (topmost-intro         . 0)
                               (topmost-intro-cont    . 0)
                               (block-open            . 0)
                               (block-close           . 0)
                               (brace-list-open       . 0)
                               (brace-list-close      . 0)
                               (brace-list-intro      . +)
                               (brace-list-entry      . 0)
                               (statement             . 0)
                               (statement-cont        . c-lineup-math)
                               (statement-block-intro . +)
                               (statement-case-intro  . +)
                               (statement-case-open   . 0)
                               (substatement          . +)
                               (substatement-open     . 0)
                               (case-label            . 0)
                               (label                 . -)
                               (do-while-closure      . 0)
                               (else-clause           . 0)
                               (comment-intro         . c-lineup-comment)
                               (arglist-intro         . 4)
                               (arglist-cont          . 0)
                               (arglist-cont-nonempty . c-lineup-arglist)
                               (arglist-close         . 4)
                               (cpp-macro             . -1000)
                               ))))
        ))
    (when (configuration-layer/layer-usedp 'python)
      (with-eval-after-load "python-mode"
        (setq python-fill-docstring-style 'symmetric
              python-fill-string-function 'my-python-fill-string-function)

        (defun my-python-fill-comment-function (&optional justify)
          (let ((fill-column 80))
            (python-fill-comment justify)))

        ;; (define-key python-mode-map (kbd "C-c Ta") 'nosetests-all)
        ;; (define-key python-mode-map (kbd "C-c Tm") 'nosetests-module)
        ;; (define-key python-mode-map (kbd "C-c To") 'nosetests-one)
        ;; (define-key python-mode-map (kbd "C-c Tpa") 'nosetests-pdb-all)
        ;; (define-key python-mode-map (kbd "C-c Tpm") 'nosetests-pdb-module)
        ;; (define-key python-mode-map (kbd "C-c Tpo") 'nosetests-pdb-one)
        ;; (define-key python-mode-map (kbd "C-c ta") 'pytest-all)
        ;; (define-key python-mode-map (kbd "C-c tm") 'pytest-module)
        ;; (define-key python-mode-map (kbd "C-c to") 'pytest-one)
        ;; (define-key python-mode-map (kbd "C-c td") 'pytest-directory)
        ;; (define-key python-mode-map (kbd "C-c tpa") 'pytest-pdb-all)
        ;; (define-key python-mode-map (kbd "C-c tpm") 'pytest-pdb-module)
        ;; (define-key python-mode-map (kbd "C-c tpo") 'pytest-pdb-one)
        ;; SPC m e i[gnore]
        ;; SPC m e f[ix]
        ;; (define-key python-mode-map (kbd "C-c M-\\") 'pyfixer:ignore-current-line)
        ;; (define-key python-mode-map (kbd "C-c C-\\") 'pyfixer:fix-current-line)
        ;; (define-key python-mode-map (kbd "C-c C-M-\\") 'pyfixer:fix-all-errors)
        ;; (define-key python-mode-map (kbd "C-c 8") 'pyfixer:fix-all-errors)
        ;; (bind-key "C-c C-h" 'pylookup-lookup python-mode-map)

        (spacemacs/declare-prefix-for-mode 'python-mode "e" "errors-prefix")
        (spacemacs/set-leader-keys-for-major-mode 'python-mode "ei" 'pyfixer:ignore-current-line)
        (spacemacs/set-leader-keys-for-major-mode 'python-mode "ef" 'pyfixer:fix-current-line)
        (spacemacs/set-leader-keys-for-major-mode 'python-mode "eF" 'pyfixer:fix-all-errors)
        (spacemacs/set-leader-keys-for-major-mode 'python-mode "en" 'flycheck-next-error)
        (spacemacs/set-leader-keys-for-major-mode 'python-mode "ep" 'flycheck-prev-error)


        ;; Consider _ a part of words for python
        (modify-syntax-entry ?_ "w" python-mode-syntax-table)

        ;; (define-key global-map (kbd "C-c o") 'iedit-mode)

        ;; (if (file-exists-p "/usr/local/bin/python"  )
        ;; (setenv "PYMACS_PYTHON" "/usr/local/bin/python"))

        (defun python-sort-import-list ()
          "Split an single import lines with multiple module imports into separate lines sort results"
          (interactive)
          (if (not (use-region-p))
              (error "No region defined"))
          (let* ((start (region-beginning))
                 (end (region-end))
                 (value 0)
                 found)
            (save-excursion
              (let* (modlist impstart impend bigstr)
                (setq modlist '())
                (goto-char start)
                (when (re-search-forward "^import \\([[:alnum:]_,\\. ]+\\)$" end t)
                  (setq impstart (match-beginning 0))
                  (setq impend (match-end 0))
                  (setq modlist (append modlist (mapcar 's-trim (s-split "," (match-string 1)))))
                  (while (setq found (re-search-forward "^import \\([[:alnum:]_,\\. ]+\\)$" end t))
                    (setq impend (match-end 0))
                    (setq modlist (append modlist (mapcar 's-trim (s-split "," (match-string 1))))))
                  (setq modlist (sort modlist 's-less?))
                  (setq modlist (mapcar (lambda (x) (concat "import " x)) modlist))
                  (setq bigstr (s-join "\n" modlist))
                  (save-restriction
                    (narrow-to-region impstart impend)
                    (delete-region impstart impend)
                    (goto-char impstart)
                    (insert bigstr)))))))

        (defun my-python-mode-hook ()
          (require 'pyfixers)
          (setq comment-column 60)

          ;; spacemacs does?
          ;; (highlight-indentation-mode -1)
          ;; (flyspell-prog-mode)
          ;; (flycheck-mode t)

          ;; This gives and error
          (message "select checker")
          ;; This is required b/c for some reason it's still not loaded at this point.
          ;; (require 'flycheck)
          (flycheck-select-checker 'python-pycheckers)
          (message "post select checker")
          ;; (flycheck-set-checker-executable 'python-flake8 "~/bin/pycheckers.sh")
          ;; (message "select set exec")

          (add-to-list 'compilation-error-regexp-alist '("\\(.*\\):[CEFRW][0-9]+: ?\\([0-9]+\\),[0-9]+: .*" 1 2))
          (message "Python mode hook done"))

        (add-hook 'python-mode-hook 'my-python-mode-hook 'append)
        )
      )

    ;; ===
    ;; Org
    ;; ===

    (when (configuration-layer/layer-usedp 'org)
      (progn
        (message "post-init-start")
        (add-hook 'org-mode-hook #'yas-minor-mode)

        (evil-define-key 'normal evil-org-mode-map "H" 'evil-window-top)
        (evil-define-key 'normal org-mode-map "M" 'evil-window-middle)
        (evil-define-key 'normal org-mode-map "L" 'evil-window-bottom)

        (evil-define-key 'visual org-mode-map "H" 'evil-window-top)
        (evil-define-key 'vidual org-mode-map "M" 'evil-window-middle)
        (evil-define-key 'vidual org-mode-map "L" 'evil-window-bottom)

        (evil-define-key 'motion org-mode-map "H" 'evil-window-top)
        (evil-define-key 'motion org-mode-map "M" 'evil-window-middle)
        (evil-define-key 'motion org-mode-map "L" 'evil-window-bottom)

        ;; :mode (("\\.org\\'" . org-mode)
        ;;        ("\\.o2b\\'" . org-mode))
        ;; :bind (("C-c c" . org-capture)
        ;;        ("C-c a" . org-agenda)
        ;;        ("C-c l" . org-store-link))

        (defun my-org-mode-hook ()
          (if debug-init-msg
              (message "Org-mode-hook"))
          ;; (org-set-local 'yas/trigger-key [tab])
          ;;(yas-minor-mode)

          (turn-on-flyspell)

          ;; (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)
          (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)
          (define-key org-mode-map (kbd "C-c e e") 'org-encrypt-entries)
          (define-key org-mode-map (kbd "C-c e E") 'org-encrypt-entry)
          (define-key org-mode-map (kbd "C-c e d") 'org-decrypt-entries)
          (define-key org-mode-map (kbd "C-c e D") 'org-decrypt-entry)

          (setq org-tags-exclude-from-inheritance '("crypt"))
          (setq org-crypt-disable-auto-save t)
          (and (buffer-file-name)
               (string-match "\\.o2b$" (buffer-file-name))
               (org2blog/wp-mode))
          (setq org-crypt-key "D7B83025"))
        (add-hook 'org-mode-hook 'my-org-mode-hook)

        (defun my-org-confirm-babel-evaluate (lang body)
          (not (or (string= lang "ditaa")
                   (string= lang "dot2tex")
                   (string= lang "dot"))))
        ;; (add-to-list 'org-babel-load-languages '(dot2tex . t))
        (setq
         org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate
         org-src-fontify-natively t
         org-default-notes-file (concat org-directory "/notes.org")
         ;; org-agenda-start-day "-8d"
         org-agenda-start-on-weekday 1
         org-src-window-setup 'current-window
         org-log-done 'time
         org-hide-leading-stars t
         org-refile-use-outline-path 'file
         org-outline-path-complete-in-steps t
         org-export-latex-emphasis-alist (quote (("*" "\\textbf{%s}" nil)
                                                 ("/" "\\emph{%s}" nil)
                                                 ("_" "\\underline{%s}" nil)
                                                 ("+" "\\texttt{%s}" nil)
                                                 ("=" "\\verb=%s=" nil)
                                                 ("~" "\\verb~%s~" t)
                                                 ("@" "\\alert{%s}" nil)))

         ;; capture the search instead of the highlighted message in
         ;; headers view
         org-mu4e-link-query-in-headers-mode t

         org-capture-templates
         '(
           ("t" "Todo" entry (file+headline (concat org-directory "/notes.org") "Tasks")
            "* TODO %?\nCreated: %t\nAnnotation: %a\n")

           ("m" "Mail Followup" entry (file+headline (concat org-directory "/notes.org") "Mail")
            "* Mail TODO Read Mail%? ([f: %:fromname]: %:subject)\n%U\nMessage: %a\n")

           ("c" "Code Todo" entry (file+headline (concat org-directory "/notes.org") "Code Todo")
            "* Code TODO %?\nCreated: %t\nAnnotation: %a\n")

           ("L" "Mac Link Note" entry (file+headline (concat org-directory "/notes.org") "Notes")
            "* NOTE %?\n%u\n%(org-mac-safari-get-frontmost-url)\n")

           ("n" "Generic Note" entry (file+headline (concat org-directory "/notes.org") "Notes")
            "* NOTE %?\n%u\nannotation:%a\nx:%x\n")

           ("s" "Status" entry (file+datetree (concat org-directory "/status.org"))
            "* NOTE %?\n%u\n")

           ("x" "Tramdose 100mg" entry (file+datetree (concat org-directory "/medicine.org") "Tramadol")
            "* NOTE 100mg\nCreated: %U" :immediate-finish)

           ("X" "Tramdose" entry (file+datetree (concat org-directory "/medicine.org") "Tramadol")
            "* NOTE %?\nCreated: %U")


           ("i" "IETF related")
           ;;; XXX these are exact copies of the generic ones different file.
           ("it" "Todo" entry (file+headline (concat org-directory "/ietf.org") "Tasks")
            "* TODO %?\nCreated: %t\nAnnotation: %a\n")

           ("in" "Generic Note" entry (file+headline (concat org-directory "/ietf.org") "Notes")
            "* NOTE %?\n%u\nannotation:%a\nx:%x\n")

           ("w" "Work related")
           ;;; XXX these are exact copies of the generic ones different file.
           ("wt" "Todo" entry (file+headline (concat org-directory "/work.org") "Tasks")
            "* TODO %?\nCreated: %t\nAnnotation: %a\n")

           ("wc" "Code Todo" entry (file+headline (concat org-directory "/work.org") "Code Todo")
            "* Code TODO %?\nCreated: %t\nAnnotation: %a\n")

           ("wn" "Generic Note" entry (file+headline (concat org-directory "/work.org") "Notes")
            "* NOTE %?\n%u\nannotation:%a\nx:%x\n")
           )
         ;; In mail map todo to mail-todo
         org-capture-templates-contexts '(("t" "m" ((in-mode . "mu4e-headers-mode")))
                                          ("t" "m" ((in-mode . "mu4e-view-mode"))))
         )

        ;; (add-to-list 'org-babel-load-languages '(python . t))
        ;; (add-to-list 'org-babel-load-languages '(dot . t))
        ;; (add-to-list 'org-babel-load-languages '(ditaa . t))
        ;; (add-to-list 'org-babel-load-languages '(dot2tex . t))
        (message "post-init-end")

        ;;   ;; XXX latex
        ;;   ;; (require 'ob-latex)
        ;;   (require 'org-crypt)
        ;;   (add-to-list 'org-babel-load-languages '(python . t))
        ;;   (add-to-list 'org-babel-load-languages '(dot . t))
        ;;   (add-to-list 'org-babel-load-languages '(ditaa . t))
        ;;   (add-to-list 'org-babel-load-languages '(dot2tex . t))

        ;;   (require 'ox-latex)
        ;;   (add-to-list 'org-latex-classes
        ;;                '("beamer"
        ;;                  "\\documentclass\[presentation\]\{beamer}\n
        ;;               \\uepackage{listings}
        ;;               \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
        ;; frame=single,
        ;; basicstyle=\\small,
        ;; showspaces=false,showstringspaces=false,
        ;; showtabs=false,
        ;; keywordstyle=\\color{blue}\\bfseries,
        ;; commentstyle=\\color{red},
        ;; }\n"
        ;;                  ("\\section\{%s\}" . "\\section*\{%s\}")
        ;;                  ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
        ;;                  ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

        ;; (require 'calfw-org)
        ;; (org-crypt-use-before-save-magic)

        ;; Languages to interpret in begin_src blocks
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((python . t)
           (ditaa . t)
           (dot2tex . t)
           (dot . t))
         )
        ;;  (dot2tex . t))

        ;; (eval-after-load "org"
        ;;   '(mapc
        ;;     (lambda (face)
        ;;       (set-face-attribute
        ;;        face nil
        ;;        :inherit
        ;;        (my-adjoin-to-list-or-symbol
        ;;         'fixed-pitch
        ;;         (face-attribute face :inherit))))
        ;;     (list 'org-code 'org-block 'org-table 'org-block-background)))

        ;; (add-to-list 'org-modules 'org-mac-message)
        ;; (setq org-mac-mail-account "Work")

        ;; - Vi friendly bindings replacing cursor movement with meta-{hjkl}
        (bind-key "C-c w" 'org-refile-to-datetree)
        (bind-key "M-h" 'org-metaleft org-mode-map)
        (bind-key "M-l" 'org-metaright org-mode-map)
        (bind-key "M-k" 'org-metaup org-mode-map)
        (bind-key "M-j" 'org-metadown org-mode-map)
        (bind-key "M-H" 'org-shiftmetaleft org-mode-map)
        (bind-key "M-L" 'org-shiftmetaright org-mode-map)
        (bind-key "M-K" 'org-shiftmetaup org-mode-map)
        (bind-key "M-J" 'org-shiftmetadown org-mode-map)

        ;; This is for using xelatex
        (require 'ox-latex)
        ;; (setq org-latex-listings t)

        (setq org-latex-listings 'minted)

        (setq org-latex-pdf-process '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                                      "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                                      "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))

        ;; XXX latex
        ;; ;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
        ;; ;; but adapted to use latexmk 4.20 or higher.
        ;; (defun my-auto-tex-cmd ()
        ;;   "When exporting from .org with latex, automatically run latex,
        ;;  pdflatex, or xelatex as appropriate, using latexmk."
        ;;   (let ((texcmd)))
        ;;   ;; default command: oldstyle latex via dvi
        ;;   (setq texcmd "latexmk -dvi -pdfps -quiet %f")
        ;;   ;; pdflatex -> .pdf
        ;;   (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
        ;;       (setq texcmd "latexmk -pdf -quiet %f"))
        ;;   ;; xelatex -> .pdf
        ;;   (if (string-match "LATEX_CMD: xelatex" (buffer-string))
        ;;       (setq texcmd "latexmk -pdflatex=xelatex -pdf -quiet %f"))
        ;;   ;; LaTeX compilation command
        ;;   (setq org-latex-to-pdf-process (list texcmd)))

        ;; (add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)

        (defun org-refile-to-datetree ()
          "Refile a subtree to a datetree corresponding to it's timestamp."
          (interactive)
          (let* ((datetree-date (org-entry-get nil "TIMESTAMP" t))
                 (date (org-date-to-gregorian datetree-date)))
            (when date
              (save-excursion
                (org-cut-subtree)
                (org-datetree-find-date-create date)
                (org-narrow-to-subtree)
                (show-subtree)
                (org-end-of-subtree t)
                (newline)
                (goto-char (point-max))
                (org-paste-subtree 4)
                (widen)
                )
              )
            ))

        ;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
        (setq org-latex-packages-alist
              '(("" "graphicx" t)
                ("" "longtable" nil)
                ("" "minted" nil)
                ("" "float" nil)))

        ;; XXX latex
        ;; (defun my-auto-tex-parameters ()
        ;;   "Automatically select the tex packages to include."
        ;;   ;; default packages for ordinary latex or pdflatex export
        ;;   (setq org-latex-default-packages-alist
        ;;         '(("AUTO" "inputenc" t)
        ;;           ("T1"   "fontenc"   t)
        ;;           (""     "fixltx2e"  nil)
        ;;           (""     "wrapfig"   nil)
        ;;           (""     "soul"      t)
        ;;           (""     "textcomp"  t)
        ;;           (""     "marvosym"  t)
        ;;           (""     "wasysym"   t)
        ;;           (""     "latexsym"  t)
        ;;           (""     "amssymb"   t)
        ;;           (""     "hyperref"  nil)))

        ;;   ;; Packages to include when xelatex is used
        ;;   (if (string-match "LATEX_CMD: xelatex" (buffer-string))
        ;;       (setq org-latex-default-packages-alist
        ;;             '(("" "fontspec" t)
        ;;               ("" "xunicode" t)
        ;;               ("" "url" t)
        ;;               ("" "rotating" t)
        ;;               ("american" "babel" t)
        ;;               ("babel" "csquotes" t)
        ;;               ("" "soul" t)
        ;;               ("xetex" "hyperref" nil)
        ;;               )))

        ;;   (if (string-match "LATEX_CMD: xelatex" (buffer-string))
        ;;       (setq org-latex-classes
        ;;             (cons '("article"
        ;;                     "\\documentclass[11pt,article,oneside]{memoir}"
        ;;                     ("\\section{%s}" . "\\section*{%s}")
        ;;                     ("\\subsection{%s}" . "\\subsection*{%s}")
        ;;                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ;;                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ;;                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ;;                   org-latex-classes))))

        ;; (add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)
        )
      )

    (when (configuration-layer/layer-usedp 'org2blog)
      (with-eval-after-load "org2blog"
        ;; XXX cool addition add after testing
        ;; ;; (add-hook 'org2blog/wp-mode-hook '(lambda () (org-mode t)))
        ;; (add-to-list 'org2blog/wp-sourcecode-langs "lisp")
        ;; (add-to-list 'org2blog/wp-sourcecode-langs "sh")

        ;; (defadvice org-wp-src-block (after ad-org-wp-src-block activate)
        ;;   "Always use space as title if none given"
        ;;   (setq ad-return-value (replace-regexp-in-string "title=\"\"" "title=\" \"" ad-return-value)))
        ;; (ad-activate 'org-wp-src-block)

        ;; Should probably be in layer config
        (setq org2blog/wp-use-sourcecode-shortcode t)
        (setq org2blog/wp-blog-alist `(("hoppsjots.org"
                                        :url "http://hoppsjots.org/xmlrpc.php"
                                        :default-categories ("Development" "Emacs")
                                        :username ,(car (auth-source-user-and-password "hoppsjots.org"))
                                        :password ,(cadr (auth-source-user-and-password "hoppsjots.org"))))))
      )

    ;; ====
    ;; Evil
    ;; ====

    (fold-section "evil"
                  (setq-default evil-shift-width 4)

                  (setq
                   evil-shift-round nil
                   evil-search-wrap nil
                   evil-want-C-i-jump nil
                   evil-esc-delay 0.001
                   )
                  (fold-section "Evil"
                                (defun evil-undefine ()
                                  (interactive)
                                  (let (evil-mode-map-alist)
                                    (call-interactively (key-binding (this-command-keys)))))

                                ;; What does this do?
                                ;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
                                ;; (define-key evil-visual-state-map [escape] 'keyboard-quit)
                                ;; (define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)

                                ;; Undefine vi keys in all modes.
                                ;; Is this screwing us with C-S keys
                                ;; (let ((undef '("\C-a" "\C-e" "\C-n" "\C-p")))
                                ;;   (while undef
                                ;;     (define-key evil-normal-state-map (car undef) 'evil-undefine)
                                ;;     (define-key evil-visual-state-map (car undef) 'evil-undefine)
                                ;;     (define-key evil-insert-state-map (car undef) 'evil-undefine)
                                ;;     (setq undef (cdr undef))))

                                ;; Undefine vi keys in insert mode.
                                (let ((undef '("\C-k")))
                                  (while undef
                                    (define-key evil-insert-state-map (car undef) 'evil-undefine)
                                    (setq undef (cdr undef))))

                                ;; Remove RET and SPC from motion map so they can be overridden by various modes
                                (defun my-move-key (keymap-from keymap-to key)
                                  "Moves key binding from one keymap to another, deleting from the old location. "
                                  (define-key keymap-to key (lookup-key keymap-from key))
                                  (define-key keymap-from key nil))
                                (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))

                                ;; (my-move-key evil-motion-state-map evil-normal-state-map " ")

                                ;; Check what escape does without this.
                                ;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
                                ;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
                                ;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
                                ;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
                                ;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

                                ;; Configure some modes to start in emacs mode.
                                (dolist (mode '(artist-mode
                                                gud-minor-mode
                                                gud-mode
                                                gud
                                                pylookup
                                                pylookup-mode
                                                ))
                                  (evil-set-initial-state mode 'emacs))
                                ;; Configure some modes to start in insert mode.
                                (evil-set-initial-state 'mu4e-compose-mode 'insert)
                                )
                  )

    (global-set-key (kbd "C-c i o") 'ietf-docs-open-at-point)

    (autoload 'yang-mode "yang-mode")
    (add-to-list 'auto-mode-alist '("\\.yang\\'" . yang-mode))

    (add-hook 'org-mode-hook 'evil-normalize-keymaps)

    ;; ---------------------
    ;; Auto insert templates
    ;; ---------------------

    (fold-section "Auto Insert Templates"
                  (defun new-file-header-date ()
                    (concat (format-time-string "%B" (current-time))
                            " "
                            (trim-string (format-time-string " %e" (current-time)))
                            (format-time-string " %Y" (current-time))))

                  (setq work-ai-prefix "/.*/\\(?:\\(?:Documents|Dropbox\\)/[Ww]ork\\|chopps/w\\)/.*/")

                  (with-eval-after-load 'autoinsert
                    (define-auto-insert
                      '("\\.org\\'" . "Home Org mode skeleton")
                      '("Short description: "
                        "#+TITLE: " _ \n
                        > "#+AUTHOR: Christian E. Hopps" \n
                        > "#+EMAIL: chopps@gmail.com" \n
                        > "#+STARTUP: indent" \n
                        > "" \n
                        ))
                    (define-auto-insert
                      '("\\.o2b\\'" . "Home Blog Org mode skeleton")
                      '("Short description: "
                        "#+TITLE: " _ \n
                        > "#+BLOG: hoppsjots.org" \n
                        > "#+AUTHOR: Christian E. Hopps" \n
                        > "#+EMAIL: chopps@gmail.com" \n
                        > "#+CATEGORY: Development" _ \n
                        > "#+OPTIONS: toc:nil num:nil todo:nil pri:nil tags:nil ^:nil TeX:nil" \n
                        > "#+STARTUP: indent" \n
                        > "" \n
                        ))
                    (define-auto-insert
                      '("\\.el\\'" . "Home Lisp comment skeleton")
                      '("Short description: "
                        ";;" \n
                        > ";; " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > ";;" \n
                        > ";; Copyright (c) " (substring (current-time-string) -4) " by Christian E. Hopps" \n
                        > ";; All rights reserved." \n
                        > ";;" \n
                        > _ ))
                    (define-auto-insert
                      '("\\.py\\'" . "# Home python comment skeleton")
                      '("Short description: "
                        "# -*- coding: utf-8 -*-"
                        > "#" \n
                        > "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "#" \n
                        > "# Copyright (c) " (substring (current-time-string) -4) " by Christian E. Hopps." \n
                        > "# All rights reserved." \n
                        > "#" \n
                        > "# REDISTRIBUTION IN ANY FORM PROHIBITED WITHOUT PRIOR WRITTEN" \n
                        > "# CONSENT OF THE AUTHOR." \n
                        > "#" \n
                        > "from __future__ import absolute_import, division, unicode_literals, print_function, nested_scopes" \n
                        > "" \n
                        > _ \n
                        > "" \n
                        > "__author__ = '" (user-full-name) "'" \n
                        > "__date__ = '" (new-file-header-date) "'" \n
                        > "__version__ = '1.0'" \n
                        > "__docformat__ = \"restructuredtext en\"" \n
                        > _ ))
                    (define-auto-insert
                      '("\\.sh\'" . "# Home shell comment skeleton")
                      '("Short description: "
                        "#!/bin/bash" \n
                        > "#" \n
                        > "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "#" \n
                        > "# Copyright (c) " (substring (current-time-string) -4) " by Christian E. Hopps." \n
                        > "# All rights reserved." \n
                        > "#" \n
                        > _ ))
                    (define-auto-insert
                      '("\\.\\(pl\\|tcl\\)" . "# Home comment skeleton")
                      '("Short description: "
                        "#" \n
                        > "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "#" \n
                        > "# Copyright (c) " (substring (current-time-string) -4) " by Christian E. Hopps." \n
                        > "# All rights reserved." \n
                        > "#" \n
                        > _ ))
                    (define-auto-insert
                      '("\\.rst\\'" . "Home ReST skeleton")
                      '("Short description: "
                        ".." \n
                        > ".. " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > ".." \n
                        > ".. Copyright (c) " (substring (current-time-string) -4) " by Christian E. Hopps." \n
                        > ".. All rights reserved." \n
                        > ".." \n
                        > _ ))
                    (define-auto-insert
                      '("\\.\\(h\\|c\\|CC?\\|cc\\|cxx\\|cpp\\|c++\\|m\\)\\'" . "Home C-style skeleton")
                      '("Short description: "
                        "/*" \n
                        > " * " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "*" \n
                        > "* Copyright (c) " (substring (current-time-string) -4) " by Christian E. Hopps." \n
                        > "* All rights reserved." \n
                        > "*" \n
                        > "* REDISTRIBUTION IN ANY FORM PROHIBITED WITHOUT PRIOR WRITTEN" \n
                        > "* CONSENT OF THE AUTHOR." \n
                        > "*/" \n
                        > _ ))


                    ;;-----------+
                    ;;      Work
                    ;;-----------+

                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.org\\'") "Work org mode skeleton")
                      '("Short description: "
                        "#+TITLE: " _ \n
                        > "#+AUTHOR: Christian E. Hopps" \n
                        > "#+EMAIL: chopps@gmail.com" \n
                        > "#+STARTUP: indent" \n
                        > "" \n
                        ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.el\\'") "Work Lisp comment skeleton")
                      '("Short description: "
                        ";;" \n
                        > ";; " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > ";;" \n
                        > _
                        ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.py\\'") "# Work python comment skeleton")
                      '("Short description: "
                        "# -*- coding: utf-8 -*-"
                        > "#" \n
                        > "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "#" \n
                        > "from __future__ import absolute_import, division, unicode_literals, print_function, nested_scopes" \n
                        > _ \n
                        > "__author__ = '" (user-full-name) "'" \n
                        > "__date__ = '" (new-file-header-date) "'" \n
                        > "__version__ = '1.0'" \n
                        > "__docformat__ = \"restructuredtext en\"" \n
                        ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.sh\\'") "# Work comment skeleton")
                      '("Short description: "
                        "#!/bin/bash" \n
                        > "#" \n
                        > "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "#" \n
                        > _ ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.\\(pl\\|tcl\\)\\'") "# Work comment skeleton")
                      '("Short description: "
                        "#" \n
                        > "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "#" \n
                        > _ ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.rst\\'") "Work ReST skeleton")
                      '("Short description: "
                        ".." \n
                        > ".. " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > ".." \n
                        > _ ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.\\(h\\|c\\|CC?\\|cc\\|cxx\\|cpp\\|c++\\|m\\)\\'") "Work C-style skeleton")
                      '("Short description: "
                        "/*" \n
                        > "* " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "*/" \n
                        > _ ))
                    ))

    ;; (message "End: %s" inhibit-startup-screen)
    ;; (if inhibit-startup-screen
    ;;     (quit-window))

    (setq powerline-default-separator 'wave)

    (defun split-window-sensibly-prefer-horizontal (&optional window)
      "Same as `split-window-sensibly' except prefer to split horizontally first."
      (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                 ;; Split window horizontally.
                 (with-selected-window window
                   (split-window-right)))
            (and (window-splittable-p window)
                 ;; Split window vertically.
                 (with-selected-window window
                   (split-window-below)))
            (and (eq window (frame-root-window (window-frame window)))
                 (not (window-minibuffer-p window))
                 ;; If WINDOW is the only window on its frame and is not the
                 ;; minibuffer window, try to split it vertically disregarding
                 ;; the value of `split-height-threshold'.
                 (let ((split-height-threshold 0))
                   (when (window-splittable-p window)
                     (with-selected-window window
                       (split-window-below))))))))

    (setq split-width-threshold 100)
    (setq window-min-width 40)
    (setq split-window-preferred-function 'split-window-sensibly-prefer-horizontal)
    )
  )

;; Local Variables:
;; eval: (find-and-close-fold "\\((fold-section \\|(spacemacs|use\\|(when (configuration-layer\\)")
;; End:

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-shift-width 4)
 '(safe-local-variable-values
   (quote
    ((eval find-and-close-fold "\\((fold-section \\|(spacemacs|use\\|(when (configuration-layer\\)")
     (js2-indent-level . 2)
     (evil-shift-width . 2)
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "make test" projectile-test-cmd-map)))))
 '(send-mail-function (quote smtpmail-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(erc-input-face ((t (:foreground "cornflowerblue"))))
 '(evil-search-highlight-persist-highlight-face ((t (:inherit region :background "yellow3" :foreground "black"))))
 '(mode-line ((t (:background "blue4" :foreground "#eeeeec"))))
 '(mode-line-inactive ((t (:background "grey40" :foreground "grey60"))))
 '(powerline-active1 ((t (:inherit mode-line :background "Deep Sky Blue"))))
 '(powerline-active2 ((t (:inherit mode-line :background "light sky blue")))))
