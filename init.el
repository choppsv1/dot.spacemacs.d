;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

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
     auto-completion
     ;; better-defaults

     ;; Langs
     c-c++
     emacs-lisp
     erlang
     git
     go
     html
     java
     javascript
     latex
     lua
     markdown
     php
     python
     ruby
     shell-scripts
     yaml

     ;; Email
     (mu4e :variables
           mu4e-installation-path "/usr/local/share/emacs/site-lisp/mu4e")

     ;; Operating system -- needs to be conditional
     osx

     org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)

     spell-checking
     syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
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
   dotspacemacs-verbose-loading nil
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
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         ;; solarized-light
                         solarized-dark
                         leuven
                         monokai
                         misterioso
                         zenburn
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
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
   ;; Delete whitespace while saving buffer. Possible values are `all',
   ;; `trailing', `changed' or `nil'. Default is `changed' (cleanup whitespace
   ;; on changed lines) (default 'changed)
   dotspacemacs-whitespace-cleanup 'changed

   ;; Allow for adding to use package configuration.
   use-package-inject-hooks t
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost any
user code here.  The exception is org related code, which should be placed in
`dotspacemacs/user-config'."
  (setq
   debug-init-msg t
   dropbox-directory "~/Dropbox"
   org-directory "~/org")

  (add-to-list 'load-path (concat dotspacemacs-directory "local-lisp"))

  (defun chopps-add-local-keys (&optional frame)
    (let ((keymap function-key-map))    ; was local-function-key-map
      (message "adding keys")
      ;; ;; These are apparently the xterm defaults (there are others for mod combos)
      (define-key keymap "\e[1;2A" [S-up])
      (define-key keymap "\e[1;2B" [S-down])
      (define-key keymap "\e[1;2C" [S-right])
      (define-key keymap "\e[1;2D" [S-left])

      (define-key keymap "\e[1;3A" [M-up])
      (define-key keymap "\e[1;3B" [M-down])
      (define-key keymap "\e[1;3C" [M-right])
      (define-key keymap "\e[1;3D" [M-left])

      (define-key keymap "\e[1;9A" [M-up])
      (define-key keymap "\e[1;9B" [M-down])
      (define-key keymap "\e[1;9C" [M-right])
      (define-key keymap "\e[1;9D" [M-left])

      (define-key keymap "\e[1;5A" [C-up])
      (define-key keymap "\e[1;5B" [C-down])
      (define-key keymap "\e[1;5C" [C-right])
      (define-key keymap "\e[1;5D" [C-left])

      (define-key keymap "\e[1;6A" [C-S-up])
      (define-key keymap "\e[1;6B" [C-S-down])
      (define-key keymap "\e[1;6C" [C-S-right])
      (define-key keymap "\e[1;6D" [C-S-left])

      (define-key keymap "\e[1;4A" [M-S-up])
      (define-key keymap "\e[1;4B" [M-S-down])
      (define-key keymap "\e[1;4C" [M-S-right])
      (define-key keymap "\e[1;4D" [M-S-left])

      (define-key keymap "\e[1;10A" [M-S-up])
      (define-key keymap "\e[1;10B" [M-S-down])
      (define-key keymap "\e[1;10C" [M-S-right])
      (define-key keymap "\e[1;10D" [M-S-left])

      (define-key keymap (kbd "ESC \" 5 R") '[S-return])
      (define-key keymap (kbd "ESC \" 5 r") '[C-return])
      (define-key keymap (kbd "ESC \" 2 R") '[C-S-return])
      (define-key keymap (kbd "ESC \" 5 ;") '[?\C-\;])
      (define-key keymap (kbd "ESC \" 5 :") '[?\C-\:])
      (define-key keymap (kbd "ESC \" 5 ,") '[?\C-\,])
      (define-key keymap (kbd "ESC \" 5 .") '[?\C-\.])
      (define-key keymap (kbd "ESC \" 5 >") '[?\C-\>])
      (define-key keymap (kbd "ESC \" 5 <") '[?\C-\<])
      (define-key keymap (kbd "ESC \" 5 /") '[?\C-\/])
      (define-key keymap (kbd "ESC \" 5 ?") '[?\C-\?])
      (define-key keymap (kbd "ESC \" 5 \'") '[?\C-\'])
      (define-key keymap (kbd "ESC \" 5 \"") '[?\C-\"])
      (define-key keymap (kbd "ESC \" 5 |") '[?\C-|])
      (define-key keymap (kbd "ESC \" 5 \\") '[?\C-\\])

      (define-key keymap (kbd "ESC \" 5 t") '[C-tab])
      (define-key keymap (kbd "ESC \" 5 T") '[C-backtab])


      (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)))

  (chopps-add-local-keys)
  (spacemacs|use-package-add-hook mu4e
    :post-init
    (progn
      (setq mu4e-maildir "~/Maildir"
            ;; Updating
            ;; mu4e-pre-hook-count 0
            ;; mu4e-full-update-mail-command "bash -c '(cd && offlineimap -l /Users/chopps/.offlineimap/logfile)'"
            ;; mu4e-quick-update-mail-command "bash -c '(cd && offlineimap -q -l /Users/chopps/.offlineimap/logfile)'"
            ;; mu4e-update-pre-hook 'mu4e-pre-hook-udpate-command
            mu4e-mu-binary (executable-find "mu")
            mu4e-update-interval nil

            ;; [b]ookmarks
            mu4e-not-junk-folder-filter " AND NOT ( maildir:/gmail.com/[Gmail].Spam OR maildir:/chopps.org/spam* ) "
            mu4e-inbox-filter-base " ( maildir:/gmail.com/INBOX OR maildir:/chopps.org/INBOX OR maildir:/terastrm.net/INBOX OR maildir:/chopps.org/a-terastream ) "
            mu4e-imp-filter-base " ( maildir:/chopps.org/ietf-rtg-yang-dt OR maildir:/chopps.org/ietf-wg-isis OR maildir:/chopps.org/ietf-wg-netmod OR maildir:/chopps.org/ietf-wg-homenet ) "
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

            my-mu4e-account-alist
            '(
              ("chopps.org"
               ;; about me
               (user-mail-address      "chopps@chopps.org")
               ;; mu4e
               (mu4e-sent-folder   "/chopps.org/Sent Messages")
               (mu4e-trash-folder  "/chopps.org/Deleted Messages")
               (mu4e-drafts-folder "/chopps.org/Drafts")
               (mu4e-sent-messages-behavior sent)
               ;; smtp
               (smtpmail-starttls-credentials '(("smtp.chopps.org" 9005 nil nil)))
               (smtpmail-default-smtp-server "smtp.chopps.org")
               (smtpmail-smtp-server "smtp.chopps.org")
               ;; smtpmail-local-domain?
               ;; smtpmail-sendto-domain?
               (smtpmail-smtp-service 9005))

              ("terastrm.net"
               ;; about me
               (user-mail-address      "chopps@dev.terastrm.net")
               ;; mu4e
               (mu4e-sent-folder   "/terastrm.net/Sent Messages")
               (mu4e-trash-folder  "/terastrm.net/Deleted Messages")
               (mu4e-drafts-folder "/terastrm.net/Drafts")
               (mu4e-sent-messages-behavior sent)
               ;; smtp
               (smtpmail-starttls-credentials '(("smtp.dev.terastrm.net" 587 nil nil)))
               (smtpmail-default-smtp-server "smtp.dev.terastrm.net")
               (smtpmail-smtp-server "smtp.dev.terastrm.net")
               ;; smtpmail-local-domain?
               ;; smtpmail-sendto-domain?
               (smtpmail-smtp-service 587))

              ("gmail.com"
               ;; about me
               (user-mail-address      "chopps@gmail.com")
               ;; mu4e
               (mu4e-drafts-folder "/gmail.com/[Gmail].Drafts")
               (mu4e-sent-folder   "/gmail.com/[Gmail].Sent Mail")
               (mu4e-trash-folder  "/gmail.com/[Gmail].Trash")
               (mu4e-sent-messages-behavior delete)
               ;; smtp
               (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
               (smtpmail-default-smtp-server "smtp.gmail.com")
               (smtpmail-smtp-server "smtp.gmail.com")
               ;; smtpmail-local-domain?
               ;; smtpmail-sendto-domain?
               (smtpmail-smtp-service 587))))

      (defun ch:ct (clist)
        "Transform candidate into (display . real)"
        (mapcar (lambda (candidate)
                  (let* ((name (plist-get candidate :name))
                         email (plist-get candidate :mail))
                    (or (and name (format "%s <%s>" name email))
                        email))) clist))
      (defun my-message-expand-name (&optional start)
        (interactive)
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

      (defun my-mu4e-set-account (account)
        "Set account variables up"
        (let ((account-vars (cdr (assoc account my-mu4e-account-alist))))
          (if account-vars
              (mapc #'(lambda (var)
                        (set (car var) (cadr var)))
                    account-vars)
            (error "No email account found"))))

      (defun my-mu4e-set-account-using-message ()
        "Set the account for composing a message."
        (let* ((defchoice (if mu4e-compose-parent-message
                              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                                (string-match "/\\(.*?\\)/" maildir)
                                (match-string 1 maildir))
                            (caar my-mu4e-account-alist)))
               (account
                (completing-read (format "Compose with account: (%s) "
                                         (mapconcat #'(lambda (var) (car var))
                                                    my-mu4e-account-alist "/"))
                                 (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                 nil
                                 t
                                 nil
                                 nil
                                 defchoice))
               (account-vars (cdr (assoc account my-mu4e-account-alist))))
          (if account-vars
              (mapc #'(lambda (var)
                        (set (car var) (cadr var)))
                    account-vars)
            (error "No email account found"))))

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
      )
    :post-config
    (progn
      (message "mu4e eval after load")
      ;; XXX these aren't defined
      ;; (define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
      ;; (define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)

      (require 'mu4e-context)
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
                        (smtpmail-smtp-service . 587)))))

      ;; (require 'mu4e-maildirs-extension)
      ;; XXX we need to add this
      ;; (mu4e-maildirs-extension)
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
      ;; (add-hook 'mu4e-compose-pre-hook
      ;;          'my-mu4e-set-account-using-message)
      (add-hook 'mu4e-compose-mode-hook 'my-mu4e-compose-hook)
      (add-to-list 'mu4e-view-actions
                   '("ViewInBrowser" . mu4e-action-view-in-browser))

      ;; (my-mu4e-set-account "chopps.org")
      (define-key mu4e-headers-mode-map "d" 'mu4e-headers-mark-for-read)
      (define-key mu4e-view-mode-map "d" 'mu4e-view-mark-for-read)
      (define-key mu4e-headers-mode-map "#" 'mu4e-headers-mark-move-to-spam)
      (define-key mu4e-view-mode-map "#" 'mu4e-view-mark-move-to-spam)
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
  (spacemacs|use-package-add-hook org
    :post-init
    (progn
      (add-hook 'org-mode-hook #'yas-minor-mode)

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
       org-agenda-start-on-weekday 5
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

       ;; (setq org-agenda-start-day "-8d")
       org-capture-templates
       '(("d" "Todo" entry (file+headline (concat org-directory "/notes.org") "Tasks")
          "* TODO %?\nSCHEDULED: %T\nDEADLINE: %T\nCreated: %t\nAnnotation: %a\n")

         ("t" "Todo" entry (file+headline (concat org-directory "/notes.org") "Tasks")
          "* TODO %?\nCreated: %t\nAnnotation: %a\n")

         ("x" "Tramdose" entry (file+datetree (concat org-directory "/medicine.org") "Tramadol")
          "* NOTE %?\nCreated: %U")

         ("m" "Mail Todo" entry (file+headline (concat org-directory "/notes.org") "Mail")
          "* TODO Read Mail%? (%:fromname about %:subject)\n%U\n%a\n")

         ("M" "Mail Todo" entry (file+headline (concat org-directory "/notes.org") "Mail")
          "* TODO Followup Mail From %:fromname About %:subject)\nSCHEDULED: %T\nDEADLINE: %T\nCreated: %t\n%a\nExtra Notes: %?")

                                        ; ("M" "Mac Mail Todo" entry (file+headline (concat org-directory "/notes.org") "Mail")
                                        ;  "* TODO %?\n%T\n%(org-mac-message-get-links \"s\")\n")

         ("l" "Link Note" entry (file+headline (concat org-directory "/notes.org") "Notes")
          "* NOTE %?\n%T\n%(org-mac-safari-get-frontmost-url)\n")

         ("n" "Generic Note" entry (file+headline (concat org-directory "/notes.org") "Notes")
          "* NOTE %?\n%T\nannotation:%a\nx:%x\n")

         ("s" "Status" entry (file+datetree (concat org-directory "/status.org"))
          "* NOTE %?\n%T\n")

         ("i" "IETF related")
         ("im" "IETF Todo w/ Mac Mail" entry (file+headline (concat org-directory "/notes.org") "IETF")
          "* TODO %?\n%T\n%(org-mac-message-get-links \"s\")\n")
         ("il" "IETF Note w/ Mac Link " entry (file+headline (concat org-directory "/notes.org") "IETF")
          "* NOTE %?\n%T\n%(org-mac-safari-get-frontmost-url)\n")
         ("in" "IETF Note (ann, clip)" entry (file+headline (concat org-directory "/notes.org") "IETF")
          "* NOTE %?\n%T\nannotation:%a\nx:%x\n")
         ("it" "IETF Todo (with annotation)" entry (file+headline (concat org-directory "/notes.org") "IETF")
          "* TODO %?\n%T\nannotation:%a\n")

         ("T" "Terastream related")
         ("Tm" "Terastram Todo with Mac Mail" entry (file+headline (concat org-directory "/notes.org") "Terastream")
          "* TODO %?\n%T\n%(org-mac-message-get-links \"s\")\n")
         ("Tl" "Terastram Safari Note" entry (file+headline (concat org-directory "/notes.org") "Terastream")
          "* NOTE %?\n%T\n%(org-mac-safari-get-frontmost-url)\n")
         ("Tl" "Terastram Safari Todo" entry (file+headline (concat org-directory "/notes.org") "Terastream")
          "* TODO %?\n%T\n%(org-mac-safari-get-frontmost-url)\n")
         ("Tc" "Terastram Code Todo" entry (file+olp (concat org-directory "/notes.org") "Terastream" "Code Todo")
          "* TODO %?\n%t\n%f:%a\n")
         ("Tt" "Terastream Todo" entry (file+headline (concat org-directory "/notes.org") "Terastream")
          "* TODO %?\n%T\n")
         )
       )

      ;; (add-to-list 'org-babel-load-languages '(python . t))
      ;; (add-to-list 'org-babel-load-languages '(dot . t))
      ;; (add-to-list 'org-babel-load-languages '(ditaa . t))
      ;; (add-to-list 'org-babel-load-languages '(dot2tex . t))
      )
    :post-config
    (progn
      ;; (require 'ob-latex)
      (require 'org-crypt)
      (add-to-list 'org-babel-load-languages '(python . t))
      (add-to-list 'org-babel-load-languages '(dot . t))
      (add-to-list 'org-babel-load-languages '(ditaa . t))
      (add-to-list 'org-babel-load-languages '(dot2tex . t))

      (require 'ox-latex)
      (add-to-list 'org-latex-classes
                   '("beamer"
                     "\\documentclass\[presentation\]\{beamer}\n
                  \\uepackage{listings}
                  \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
    frame=single,
    basicstyle=\\small,
    showspaces=false,showstringspaces=false,
    showtabs=false,
    keywordstyle=\\color{blue}\\bfseries,
    commentstyle=\\color{red},
    }\n"
                     ("\\section\{%s\}" . "\\section*\{%s\}")
                     ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                     ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

      ;; (require 'calfw-org)
      (org-crypt-use-before-save-magic)

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
      (setq org-latex-listings t)

      ;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
      ;; but adapted to use latexmk 4.20 or higher.
      (defun my-auto-tex-cmd ()
        "When exporting from .org with latex, automatically run latex,
       pdflatex, or xelatex as appropriate, using latexmk."
        (let ((texcmd)))
        ;; default command: oldstyle latex via dvi
        (setq texcmd "latexmk -dvi -pdfps -quiet %f")
        ;; pdflatex -> .pdf
        (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
            (setq texcmd "latexmk -pdf -quiet %f"))
        ;; xelatex -> .pdf
        (if (string-match "LATEX_CMD: xelatex" (buffer-string))
            (setq texcmd "latexmk -pdflatex=xelatex -pdf -quiet %f"))
        ;; LaTeX compilation command
        (setq org-latex-to-pdf-process (list texcmd)))

      (add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)

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
              ("" "float" nil)))

      (defun my-auto-tex-parameters ()
        "Automatically select the tex packages to include."
        ;; default packages for ordinary latex or pdflatex export
        (setq org-latex-default-packages-alist
              '(("AUTO" "inputenc" t)
                ("T1"   "fontenc"   t)
                (""     "fixltx2e"  nil)
                (""     "wrapfig"   nil)
                (""     "soul"      t)
                (""     "textcomp"  t)
                (""     "marvosym"  t)
                (""     "wasysym"   t)
                (""     "latexsym"  t)
                (""     "amssymb"   t)
                (""     "hyperref"  nil)))

        ;; Packages to include when xelatex is used
        (if (string-match "LATEX_CMD: xelatex" (buffer-string))
            (setq org-latex-default-packages-alist
                  '(("" "fontspec" t)
                    ("" "xunicode" t)
                    ("" "url" t)
                    ("" "rotating" t)
                    ("american" "babel" t)
                    ("babel" "csquotes" t)
                    ("" "soul" t)
                    ("xetex" "hyperref" nil)
                    )))

        (if (string-match "LATEX_CMD: xelatex" (buffer-string))
            (setq org-latex-classes
                  (cons '("article"
                          "\\documentclass[11pt,article,oneside]{memoir}"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                        org-latex-classes))))

      (add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)
      )
    )

  ;; (set-register ?E `(file . ,emacs-init-source))
  (set-register ?W `(file . ,(concat org-directory "/work.org")))
  (set-register ?N `(file . ,(concat org-directory "/notes.org")))
  (set-register ?I `(file . ,(concat org-directory "/ietf.org")))
  (set-register ?S `(file . ,(concat org-directory "/status.org")))
  (set-register ?P `(file . ,(concat dropbox-directory "/ts-pass.gpg")))

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
