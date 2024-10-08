;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;;
;; April 2 2022, Christian Hopps <chopps@gmail.com>
;;
;; Copyright (c) 2022 by Christian E. Hopps
;; All rights reserved.
;;

(setq debug-init-msg t)
                                        ; (setq debug-on-error t)
(defun debug-init-message (fmt &rest a)
  (and debug-init-msg
       (let ((ts (format-time-string "%S.%N")))
         (apply 'message
                (append (list (concat ts ": DEBUG-INIT: " fmt))
                        a)))))

(defun my-layer-setup ()
  "My layer setup"
  ;;
  ;; Base Layers
  ;;
  (setq
   chopps-layers
   '(
     ;; Choose either ivy or helm as completion framework
     ;; ivy
     (helm :variables
           helm-move-to-line-cycle-in-source nil)

     better-defaults
     ;; (git :variables
     ;;      ;; magit-diff-refine-hunk 'all
     ;;      magit-bury-buffer-function 'magit-restore-window-configuration
     ;;      ;; magit-display-buffer-function 'magit-display-buffer-traditional
     ;;      ;; magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
     ;;      ;; git-enable-magit-delta-plugin nil
     ;;      ;; git-magit-status-fullscreen nil
     ;;      ;; git-enable-magit-gitflow-plugin nil
     ;;      ;; git-enable-magit-todos-plugin nil
     ;;      )
     git

     neotree
     rebox
     theming

     ;; Languages
     emacs-lisp

     ;; File Formats
     csv
     markdown
     multiple-cursors
     shell-scripts
     yaml
     themes-megapack
     )

   chopps-extra-layers
   '(
     (auto-completion :disabled-for org
                      :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip nil
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete)
     colors
     (org :variables
          org-clock-idle-time 15
          org-enable-rfc-support nil
          )
     )

   ;;
   ;; OS Specific Layers
   ;;

   linux-layers
   '(
     systemd
     )

   osx-layers
   '(
     ;; May repeate in development b/c we don't normally include osx in devel machiens
     html
     (ietf :variables ietf-docs-cache "~/ietf-docs-cache")
     (mu4e :variables
           ;; mu4e-enable-async-operations t
           mu4e-enable-notifications t
           ;; mu4e-enable-mode-line nil
           mu4e-use-maildirs-extension nil)
     ;; (org2blog :variables org2blog-name "hoppsjots.org")
     (osx :variables
          osx-use-option-as-meta t)
     (spell-checking :variables enable-flyspell-auto-completion nil)
     )

   ;;
   ;; Development Specific Layers
   ;;

   dev-common-layers
   '(
     (cmake :variables cmake-enable-cmake-ide-support nil)
     dap
     debug
     docker
     gtags
     html
     (ietf :variables ietf-docs-cache "~/ietf-docs-cache")
     (python :variables
             ;; python-backend 'anaconda
             ;; python-lsp-server 'pyright
             python-lsp-server 'pylsp
             python-formatter 'black
             python-fill-column 88
             python-tab-width 8
             python-pipenv-activate nil
             python-poetry-activate t
             python-fill-docstring-style 'pep-257-nn
             ;; primary test runner is pytest use 'spc u' prefix to invoke nose
             python-test-runner '(pytest nose)
             pytest-global-name "python -m pytest --doctest-modules"
             python-sort-imports-on-save nil
             python-enable-yapf-format-on-save nil
             ;;
             python-auto-set-local-pyvenv-virtualenv nil
             python-auto-set-local-pyenv-virtualenv nil
             ;;
             ;; Use .dir-locals.el
             ;; ((python-mode . ((lsp-pylsp-plugins-pylint-args . ["--rcfile=~/w/munet/pyproject.toml"]))))
             ;; lsp-pylsp-plugins-pylint-args ["--rcfile=/home/chopps/w/munet/pyproject.toml"]

             lsp-pylsp-plugins-autopep8-enabled nil
             lsp-pylsp-plugins-black-enabled t
             lsp-pylsp-plugins-flake8-enabled t
             lsp-pylsp-plugins-flake8-ignore ["E203"]
             lsp-pylsp-plugins-flake8-max-line-length 88
             lsp-pylsp-plugins-jedi-completion-enabled t
             lsp-pylsp-plugins-jedi-definition-enabled nil
             lsp-pylsp-plugins-jedi-hover-enabled nil
             lsp-pylsp-plugins-jedi-references-enabled t
             lsp-pylsp-plugins-jedi-signature-help-enabled t
             lsp-pylsp-plugins-jedi-symbols-enabled t
             lsp-pylsp-plugins-mccabe-enabled nil
             lsp-pylsp-plugins-preload-enabled nil
             lsp-pylsp-plugins-pycodestyle-enabled nil
             lsp-pylsp-plugins-pydocstyle-add-ignore []
             lsp-pylsp-plugins-pydocstyle-convention 'google
             lsp-pylsp-plugins-pydocstyle-enabled t
             lsp-pylsp-plugins-pyflakes-enabled nil
             lsp-pylsp-plugins-pylint-enabled t
             lsp-pylsp-plugins-rope-completion-enabled nil
             lsp-pylsp-plugins-yapf-enabled nil
             )
     (rust :variables
           rust-format-on-save t

           lsp-rust-server 'rust-analyzer
           lsp-rust-analyzer-server-display-inlay-hints nil
           lsp-rust-analyzer-cargo-watch-command "clippy"

           ;; ;; enable / disable the hints as you prefer:
           ;; ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
           ;; lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
           ;; lsp-rust-analyzer-display-chaining-hints t
           ;; lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
           ;; lsp-rust-analyzer-display-closure-return-type-hints t
           ;; lsp-rust-analyzer-display-parameter-hints nil
           ;; lsp-rust-analyzer-display-reborrow-hints nil

           )
     (spell-checking :variables enable-flyspell-auto-completion nil)
     (syntax-checking :variables syntax-checking-enable-tooltips t)
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      ;; version-control-diff-tool 'git-gutter+
                      ;; version-control-diff-tool 'diff-hl
                      version-control-diff-side 'right
                      version-control-global-margin t)
     (yang :variables
           yang-pyang-rules "lint"
           yang-pyang-extra-args "--max-line-length=79")
     )

   dev-lite-layers
   '(
     ;; Language Meta-Layers
     (lsp :variables
          ;; lsp-diagnostics-provider :none

          lsp-lens-enable nil
          ;; lsp-signature-auto-activate

          lsp-eldoc-render-all t
          lsp-idle-delay 0.6

          lsp-inlay-hint-enable nil

          lsp-enable-indentation nil
          lsp-enable-file-watchers t
          lsp-file-watch-threshold 20000

          lsp-ui-doc-enable nil
          lsp-ui-peek-always-show t
          lsp-ui-peek-list-width 50
          lsp-ui-peek-peek-height 40
          lsp-ui-sideline-code-actions-prefix " "
          lsp-ui-sideline-show-hover nil
          ;; lsp-ui-sideline-show-code-actions t
          ;; lsp-ui-sideline-show-diagnostics t
          )

     ;; Languages
     (c-c++ :variables
            c-c++-backend 'lsp-ccls
            c-c++-adopt-subprojects t
            c-c++-default-mode-for-headers 'c-mode
            c-c++-enable-clang-format-on-save nil
            c-c++-lsp-enable-semantic-highlight nil)
     )

   dev-layers
   '(
     ;; ditaa
     ;; ess
     ;; nginx
     ;; pandoc
     ;; pdf
     ;; ranger
     ;; treemacs
     sphinx

     ;; File formats
     docker
     graphviz
     groovy
     restructuredtext

     ;; Language Meta-Layers
     (lsp :variables
          ;; lsp-diagnostics-provider :none
          lps-ui-sideline-enable nil)

     ;; (semantic :disabled-for '(emacs-lisp cc-mode c-mode c++-mode))
     (semantic :disabled-for '(emacs-lisp cc-mode c++-mode))

     ;; Languages
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c-mode
            c-c++-enable-clang-format-on-save nil
            c-c++-lsp-enable-semantic-highlight nil

            c-c++-backend 'lsp-ccls
            c-c++-adopt-subprojects t
            c-c++-default-mode-for-headers 'c-mode

            ;; ;; clangd options
            ;; c-c++-backend 'lsp-clangd
            ;; ;; c-c++-adopt-subprojects t
            ;; ;; c-c++-lsp-sem-highlight-rainbow t
            ;; ;; c-c++-enable-clang-support nil

            )

     (go :variables
         go-format-before-save t
         go-use-golangci-lint t
         ;; go-use-gometalinter t
         ;; godoc-at-point-function 'godoc-gogetdoc
         go-backend 'lsp
         )

     ;; javascript
     (latex :variables latex-build-command "latexmk")

     (lua :variables
          lua-default-application "lua5.2"
          lua-backend 'lsp
          lua-lsp-server 'lua-language-server
          ;;      lua-lsp-server 'emmy
          ;;      lsp-clients-emmy-lua-jar-path "/home/chopps/.emacs.d/.cache/lsp/EmmyLua-LS-all.jar" ; default path
          lsp-clients-lua-language-server-bin
          "/home/chopps/.emacs.d/.cache/lsp/lua-language-server/bin/lua-language-server" ; default path
          lsp-clients-lua-language-server-main-location
          "/home/chopps/.emacs.d/.cache/lsp/lua-language-server/main.lua") ; default path
     )

   ;; Normally we do note configure any OSX as a dev system, and only change
   ;; this when we are doing development on the mac.

   ;; all dev-lite are linux
   ;; chopps-dev-lite-systems '("dlk" "jaja" "labnh" "lake" "flk" "ubb" "uff")
   chopps-dev-lite-systems '("dlk" "ja.int.chopps.org" "jaja" "labnh" "lake" "flk" "ubb" "uff" "sal" "varm")
   ;; These systems get full development packages -- the slowest load
   chopps-dev-systems '("cmf-xe-1" "morn1" "tops" "hp13" "rlk" "dak"))

  (setq jaja-machine-id "949972c901cb4c3f9e7e2c6b484bc1bb")

  (when (member system-name chopps-dev-lite-systems)
    (setq chopps-layers (append chopps-layers dev-common-layers dev-lite-layers)))

  (when (member system-name chopps-dev-systems)
    (setq chopps-layers (append chopps-layers chopps-extra-layers dev-common-layers dev-layers)))

  (when (member system-name '("tops"))
    (setq chopps-layers (append chopps-layers tops-layers)))

  ;; OS specific
  (cond ((eq system-type 'darwin)
         ;; add chopps-extra-layers maybe again b/c osx may not be development
         (setq chopps-layers (append chopps-layers chopps-extra-layers osx-layers)))
        ((eq system-type 'gnu/linux)
         (setq chopps-layers (append chopps-layers linux-layers))
         ))
  )

(my-layer-setup)

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   load-prefer-newer t

   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   ;; dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-enable-lazy-installation nil ;; required for .m to not load octave mode
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers chopps-layers

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     base16-theme
     borland-blue-theme
     cobalt
     color-theme-modern

     ;; (copilot :location (recipe
     ;;                     :fetcher github
     ;;                     :repo "zerolfx/copilot.el"
     ;;                     :files ("*.el" "dist")))

     (cov :location (recipe :fetcher github
                            :branch "chopps/add-bg-overlay-tint"
                            :repo "choppsv1/cov"))

     dockerfile-mode
     exec-path-from-shell
     ;; This is very cool but too expensive for large projects
     olivetti
     modus-themes
     monky
     nhexl-mode
     nano-theme
     org-caldav
     ;; org-journal
     org-notify
     package-lint
     persistent-scratch
     polymode
     python-docstring
     protobuf-mode
     rainbow-mode
     sqlite3
     ;; rfcview
     ;; colorsarenice-light
     w3m
     )

   ;; ;; A list of packages that cannot be updated.
   ;; ;; dotspacemacs-frozen-packages '()
   ;; dotspacemacs-frozen-packages
   ;; '(
   ;;   magit
   ;;   magit-section
   ;;   forge
   ;;   )

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     ;; vi-tilde-fringe
     applescript-mode
     helm-ls-git
     erc-yt
     erc-view-log
     evil-mc
     helm-gtags
     irfc
     ;; mu4e-maildirs-extension
     mu4e-alert
     nameless
     ;; powerline
     ;; recentf
     ;; savehist

     ;; HATE PURPOSE MODE
     ;; eyebrowse
     ;; helm-purpose
     ;; ivy-purpose
     ;; window-purpose
     ;; spacemacs-purpose-popwin

     ) ; evil-org
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only)

  (unless (string-prefix-p "hp13" (system-name))
    (setq dotspacemacs-additional-packages
          (append dotspacemacs-additional-packages
                  '(clipetty
                    xclip))))
  )

(defun set-fontsize ()
  (setq ch-def-font "DejaVu Sans Mono")
  (cond
   ((string-equal system-type "darwin") ; Mac OS X
    (setq ch-def-font "Iosevka Thin")
    (setq ch-def-height 17.0)
    (debug-init-message "Setting font to %s:%f" ch-def-font ch-def-height))
   ((string-equal system-type "gnu/linux")
    (let ((xres (shell-command-to-string "xdpyinfo | sed -e '/dimensions/!d;s/.* \\([0-9]*\\)x[0-9]* .*/\\1/'"))
          (dpi (shell-command-to-string "xdpyinfo | sed -e '/dots per inch/!d;s/.* \\([0-9]*\\)x[0-9]* .*/\\1/'"))
          ;; (yres (shell-command-to-string "xdpyinfo | sed -e '/dimensions/!d;s/.* [0-9]*x\\([0-9]*\\) .*/\\1/'")))
          )
      (setq ch-def-font "DejaVu Sans Mono")
      (setq xres (replace-regexp-in-string "\n\\'" "" xres))
      ;; (setq yres (replace-regexp-in-string "\n\\'" "" yres))
      (if (> (string-to-number xres) 5000)
          ;; big display
          (setq ch-def-height 20.0)
        ;; small display
        (if (= (string-to-number xres) 3840)
            (if (> (string-to-number dpi) 240)
                (setq ch-def-height 10.0)
              (setq ch-def-height 13.0))
          ;; small display
          (setq ch-def-height 12.0)))))))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; mDetermine display size to pick font size
  ;; (defadvice smartparens-mode (around disable-smartparens activate)
  ;;   "Disable smartparens-mode completely.")
  (setq custom-file (concat dotspacemacs-directory "custom.el"))
  (load custom-file)
  ;; (dotspacemacs/emacs-custom-settings)

  (set-fontsize)
  (debug-init-message "def height %s" ch-def-height)

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   blink-matching-paren 'jump
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent t

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable t

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(mandm
                         ;; Preferred modes
                         sanityinc-tomorrow-blue
                         ;; misterioso
                         ;; cobalt
                         doom-material

                         ;; Dark modes
                         molokai
                         ;; ef-autumn ;; dark -- good -- warmer
                         ;; ef-bio ;; dark -- good -- cooler
                         ;; ef-cherie ;; dark
                         ;; ef-dark ;; dark -- good
                         ;; ef-duo-dark
                         ;; ef-deuteranopia-dark ;; good
                         ;; ef-elea-dark ;; good
                         ;; ef-maris-dark
                         ;; ef-melissa-dark
                         ef-night
                         ;; ef-symbiosis
                         ;; ef-trio-dark
                         ;; ef-tritanopia-dark
                         ;; ef-winter ;; light --

                         ;; Medium modes
                         borland-blue

                         ;; Light modes
                         ;; ef-cyprus ;; light -- good -- cooler
                         ;; ef-day ;; light
                         ;; ef-deuteranopia-light ;; light -- good -- cooler greyish
                         ;; ef-duo-light ;; light -- good -- warmer with blue
                         ;; ef-elea-light ;; light -- nah -- cooloer with green bg tint
                         ;; ef-frost ;; light -- good -- medium blues and purples and greens
                         ;; ef-light ;; light --
                         ;; ef-maris-light ;; light --
                         ;; ef-melissa-light ;; light --
                         ;; ef-kassio ;; light --
                         ;; ef-spring ;; light --
                         ;; ef-summer ;; light --
                         ;; ef-trio-light ;; light --
                         ;; ef-tritanopia-light ;; light --

                         ;; Light modes
                         ;; sanityinc-solarized-light
                         doom-acario-light
                         gruvbox-light-hard
                         leuven
                         ;; this is nice but unavailable when starting
                         ;; up for some reason
                         ;; jsc-light
                         )
   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state nil

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   ;; dotspacemacs-default-font `("Office Code Pro D" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.4)
   ;; Perfect UTF-8, good sans serif
   ;; dotspacemacs-default-font `("DejaVu Sans Mono" :size ,ch-def-height :weight normal :width normal)
   ;; dotspacemacs-default-font `("DejaVu Sans Mono" :size ,ch-def-height :weight normal :width normal)
   dotspacemacs-default-font `(,ch-def-font :size ,ch-def-height :weight normal :width normal :powerline-scale 1.5)
   ;; Very condensed -- pretty good for coding -- same odd shapes offs UTF as Liberation Mono
   ;; dotspacemacs-default-font `("Ubuntu Mono" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.2)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (add-hook 'grep-mode-hook
            (lambda()
              (kill-local-variable 'compilation-auto-jump-to-next)))

  ;; Do not pop an annoying window up constantly to tell us about async
  ;; compilation warnings
  (setq-default native-comp-async-report-warnings-errors 'silent)

  (defun enable-CSI-u ()
    ;; Take advantage of iterm2's CSI u support (https://gitlab.com/gnachman/iterm2/-/issues/8382).
    (require 'term/xterm)
    (if (boundp 'xterm--init-modify-other-keys)
        (xterm--init-modify-other-keys))
    ;; Courtesy https://emacs.stackexchange.com/a/13957, modified per
    ;; https://gitlab.com/gnachman/iterm2/-/issues/8382#note_365264207
    (defun character-apply-modifiers (c &rest modifiers)
      "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
      (if (memq 'control modifiers) (setq c (if (and (<= ?a c) (<= c ?z))
                                                (logand c ?\x1f)
                                              (logior (lsh 1 26) c))))
      (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
      (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
      (vector c))
    (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
      (let ((c 32))
        (while (<= c 126)
          (mapc (lambda (x)
                  (define-key xterm-function-map (format (car x) c)
                              (apply 'character-apply-modifiers c (cdr x))))
                '(;; with ?.VT100.formatOtherKeys: 0
                  ("\e\[27;3;%d~" meta)
                  ("\e\[27;5;%d~" control)
                  ("\e\[27;6;%d~" control shift)
                  ("\e\[27;7;%d~" control meta)
                  ("\e\[27;8;%d~" control meta shift)
                  ;; with ?.VT100.formatOtherKeys: 1
                  ("\e\[%d;3u" meta)
                  ("\e\[%d;5u" control)
                  ("\e\[%d;6u" control shift)
                  ("\e\[%d;7u" control meta)
                  ("\e\[%d;8u" control meta shift)))
          (setq c (1+ c))))))

  (setq exec-path-from-shell-check-startup-files nil)
  ;; literally wrong to use loopback here
  ;; (setf epa-pinentry-mode nil)
  ;; (setq epg-pinentry-mode 'loopback)
  (setq epg-pinentry-mode nil)

  (debug-init-message "USER-INIT: Start")

  (defun debug-frame-font ()
    (debug-init-message "GOT FONT UPDATE"))
  (add-hook 'debuug-frame-font 'after-setting-font-hooks)

  ;; XXX debug disappearing modeline
  (defun replay-font-set ()
    ""
    (interactive)
    (let* ((font (car dotspacemacs-default-font))
           (props (cdr dotspacemacs-default-font))
           (scale (plist-get props :powerline-scale))
           (font-props (spacemacs/mplist-remove
                        (spacemacs/mplist-remove props :powerline-scale)
                        :powerline-offset))
           (fontspec (apply 'font-spec :name font font-props)))
      (debug-init-message "replay-font-set fontspec: %s" fontspec)
      (set-frame-font fontspec)))

  ;; ---------
  ;; User-init
  ;; ---------
  (setq-default git-magit-status-fullscreen t
                magit-repository-directories '(("/home/chopps/w" . 1)
                                               ("/home/chopps/w-share" . 1)
                                               ("/home/chopps/p" . 1)
                                               ))

  ;; (setq-default cov-coverage-mode t)
  (setq-default gdb-default-window-configuration-file "gdb-window-config")
  (setq-default gdb-window-configuration-directory "~/.spacemacs.d/")

  (add-to-list 'load-path (concat dotspacemacs-directory "local-lisp/"))
  ;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu-mac/mu4e/")
  (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu-mac/mu4e/")

  ;; add all non-theme named subdirs of repos to load path
  (setq load-path
        (append load-path
                (seq-filter (lambda (x) (not (member x custom-theme-load-path)))
                            (directory-files (concat dotspacemacs-directory "repos/") t "[^.].*"))))

  ;; add all theme named subdirs of repos to theme path, b/c this is below the
  ;; load path above they are also added to load path
  (setq custom-theme-load-path
        (append custom-theme-load-path
                (directory-files (concat dotspacemacs-directory "repos/") t ".*theme.*")))

  ;; Load all the themes
  (dolist (themedir (directory-files (concat dotspacemacs-directory "repos/") t ".*theme.*"))
    (let ((theme
           (string-remove-suffix "-emacs" (string-remove-suffix "-theme" (file-name-base themedir)))))
      (load-theme (intern theme) t nil)))

  (add-to-list 'load-path (concat "~/p/ietf-docs"))

  (require 'iterm-xterm-extra)

  (setq-default xterm-extra-capabilities '(modifyOtherKeys reportBackground getSelection setSelection))
  ;; Give up and just use this.
  ;; (setq-default xterm-extra-capabilities '(getSelection setSelection))

  (require 'generic-lisp)
  (require 'generic-mode-hooks)

  (auto-insert-mode)

  (if (file-accessible-directory-p "~/Dropbox")
      (setq dropbox-directory "~/Dropbox")
    (setq dropbox-directory nil))

  (when (file-accessible-directory-p "~/Dropbox/org-mode")
    (setq-default org-directory "~/Dropbox/org-mode/")
    (setq org-agenda-files '("~/Dropbox/org-mode/")))

  (setq labn-365-dir (expand-file-name "~/365.labn.net/OneDrive - LabN Consulting, L.L.C/"))
  (setq labn-us-dir (expand-file-name "~/us.labn.net/OneDrive - LabN Consulting LLC/"))


  ;; -------
  ;; Copilot
  ;; -------

  (unless nil
    ;; accept completion from copilot and fallback to company
    (defun my-copilot-accept-line ()
      (interactive)
      (or (copilot-accept-completion-by-line 1)
          (company-indent-or-complete-common nil)))

    (defun my-copilot-accept-word ()
      (interactive)
      (or (copilot-accept-completion-by-word 1)
          (company-indent-or-complete-common nil)))

    (with-eval-after-load 'company
      ;; disable inline previews
      ;; (delq 'company-preview-if-just-one-frontend company-frontends)
      ;; enable tab completion
      ;; (define-key company-mode-map (kbd "<tab>") 'my-copilot-accept-lines)
      ;; (define-key company-mode-map (kbd "TAB") 'my-copilot-accept-lines)
      (define-key company-mode-map (kbd "C-<tab>") 'my-copilot-accept-line)
      (define-key company-mode-map (kbd "C-TAB") 'my-copilot-accept-line)

      ;; (define-key company-active-map (kbd "<tab>") 'my-copilot-accept-lines)
      ;; (define-key company-active-map (kbd "TAB") 'my-copilot-accept-lines)

      (define-key company-active-map (kbd "C-M-<tab>") 'my-copilot-accept-word)
      (define-key company-active-map (kbd "C-M-TAB") 'my-copilot-accept-word)

      (define-key company-active-map (kbd "M-n") 'copilot-next-completion)
      (define-key company-active-map (kbd "M-p") 'copilot-previous-completion)
      )

    (with-eval-after-load 'copilot
      (add-hook 'prog-mode-hook 'copilot-mode)
      ;; (evil-define-key 'insert copilot-mode-map
      ;;   (kbd "<tab>") #'my/copilot-tab)
      )

    ;; (evil-define-key estate evil-org-mode-map "H" nil)
    ;; (define-key evil-insert-state-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
    ;; (define-key evil-insert-state-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
    )

  ;; -----------
  ;; End-copilot
  ;; -----------

  (setq
   evil-search-wrap nil
   evil-want-C-i-jump nil
   ;; This is very annoying to have to set, visual highlight in evil is hijacking PRIMARY selection
   ;; behavior..
   x-select-enable-primary t
   evil-esc-delay 0
   ;; js2-basic-offset 2
   ;; js-indent-level 1
   org-protocol-default-template-key "t"

   ;; This really should be a file local variable.
   ;; rebox-style-loop '(16 71 72 73)

   ;; (with-eval-after-load "evil-evilified-state"
   ;;   (define-key evil-evilified-state-map-original "H" 'evil-window-top)
   ;;   (define-key evil-evilified-state-map-original "L" 'evil-window-bottom)
   ;;   (define-key evil-evilified-state-map-original "M" 'evil-window-middle))
   projectile-globally-unignored-files '(".gitlab-ci.yml" ".pylintrc" ".travis.yml")
   )

  (setq-default debug-mac-notifications nil)
  (setq-default flycheck-standard-error-navigation nil)
  (setq flycheck-standard-error-navigation nil)
  (setq-default spaceline-buffer-id-max-length 120)


  (with-eval-after-load 'tramp
    (push "/home/chopps/local/bin" tramp-remote-path))

  (debug-init-message "debug-init-msg is %s" debug-init-msg)

  (debug-init-message "debug-init 1")

  (setq comment-delim-color "grey50")

  (setq theming-modifications
        `(
          ;; (mandm (default :background "#011827"))
          (cobalt
           (modeline :background "#111111" :foreground "white")
           (modeline-inactive :background "#222222" :foreground "white")
           (spaceline-evil-emacs :foreground "black" :background "SkyBlue2")
           (spaceline-evil-insert :foreground "black" :background "chartreuse3")
           (spaceline-evil-motion :foreground "black" :background "plum3")
           (spaceline-evil-normal :foreground "black" :background "DarkGoldenrod2")
           (spaceline-evil-replace :foreground "black" :background "chocolate")
           (spaceline-evil-visual :foreground "black" :background "grey"))
          (misterioso (erc-input-face :foreground "cornflowerblue")
                      (font-lock-comment-face :foreground "DarkGrey" :slant italic)
                      (evil-search-highlight-persist-highlight-face :background "#338f86")
                      (lazy-highlight-face :background "#338f86")
                      (font-lock-comment-delimiter-face :foreground ,comment-delim-color))
          (molokai (font-lock-comment-face :foreground "DarkGrey")
                   (evil-search-highlight-persist-highlight-face :background "#338f86")
                   (lazy-highlight-face :background "#338f86")
                   (font-lock-comment-delimiter-face :foreground ,comment-delim-color))
          (monokai (font-lock-comment-face :foreground "#A5A17E" :slant italic)
                   (evil-search-highlight-persist-highlight-face :background "#338f86")
                   (lazy-highlight-face :background "#338f86")
                   (font-lock-doc-face :foreground "#A5A17E" :slant italic)
                   (font-lock-comment-delimiter-face :foreground "#55513E"))
          (light-soap (default :foreground "#474747" :background "#fafad4"))
          (quasi-monochrome (default :height ,(* ch-def-height 10))
                            (font-lock-string-face :foreground "DarkGrey" :slant italic)
                            (font-lock-comment-delimiter-face :foreground ,comment-delim-color))
          (mandm ;; (font-lock-doc-face :foreground "#036A07" :slant normal)
           (font-lock-comment-face :foreground "darkcyan" :slant normal)
           (diff-refine-added :inverse-video t :inherit diff-added)
           (diff-refine-changed :inverse-video t :inherit diff-changed)
           (diff-refine-removed :inverse-video t :inherit diff-removed)
           (font-lock-comment-delimiter-face :foreground "darkcyan" :slant normal))
          ;; (font-lock-comment-delimiter-face :foreground "#BDBDA4" :slant normal))
          ;;  (lsp-face-highlight-read :foreground "#036A08" :slant normal)
          ;;  )

          (leuven ;; (default :background "#F0F0E0")
           (default :background "#ede8da")
           ;;(default :background "#F0F0E5")
           (font-lock-doc-face :foreground "#036A07" :slant italic)
           (font-lock-comment-face :foreground "#6D6D64" :slant italic)
           (font-lock-comment-delimiter-face :foreground "#BDBDA4"))
          (sanityinc-tomorrow-blue
           (highlight :background "#00346e")
           (font-lock-doc-face :foreground "lightgreen" :slant italic)
           (font-lock-keyword-face :foreground "peachpuff")
           (font-lock-function-name-face :foreground "snow" :weight bold)
           (font-lock-variable-name-face :foreground "snow" :weight bold)
           (font-lock-type-face :foreground "lightsteelblue1" :slant italic)
           )
          (solarized-light
           (font-lock-doc-face :foreground "#036A07" :slant italic)
           (font-lock-comment-face :foreground "#6D6D64" :slant italic)
           (font-lock-comment-delimiter-face :foreground "#BDBDA4"))
          ;; (colorsarenice-light (erc-input-face :foreground "cornflowerblue")
          ;;                      (spacemacs-micro-state-header-face :foreground "Black")
          ;;                      (powerline-active1 :background "DarkSlateGrey" :foreground "LightBlue")
          ;;                      (powerline-inactive2 :background "DarkSlateGrey" :foreground "LightRed")
          ;;                      (powerline-inactive1 :background "DarkSlateGrey" :foreground "LightGrey")
          ;;                       (font-lock-comment-face :foreground "grey44" :slant italic)
          ;;                       (font-lock-comment-delimiter-face :foreground "grey77"))
          ))

  ;;'(font-lock-comment-delimiter-face ((t (:foreground "grey33"))))
  ;;'(font-lock-comment-face ((t (:foreground "DarkGrey" :slant italic))))


  ;; XXX what we want actually is to advise this function and temporarily change
  ;; XXX the definition of x-select-text to (x-set-selection 'PRIMARY ...) so
  ;; XXX that the correct thing happens
  (defun _evil-visual-update-x-selection (&optional buffer)
    "Update the X selection with the current visual region."
    (let ((buf (or buffer (current-buffer))))
      ;; (debug-init-message "XXXVISUALSELECT1")
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (evil-visual-state-p)
                     (fboundp 'x-set-selection)
                     (or (not (boundp 'ns-initialized))
                         (with-no-warnings ns-initialized))
                     (not (eq evil-visual-selection 'block)))
            (x-set-selection nil (buffer-substring-no-properties
                                  evil-visual-beginning
                                  evil-visual-end))
            ;; is this last thing right?
            ;; (debug-init-message "XXXVISUALSELECT2")
            (setq x-last-selected-text-primary )
            )))))


  (defun _evil-visual-update-x-selection (&optional buffer)
    "Update the X selection with the current visual region."
    (with-current-buffer (or buffer (current-buffer))
      (when (and (evil-visual-state-p)
                 (fboundp 'x-set-selection)   ; <-- small change, hope it works ;)
                 (or (not (boundp 'ns-initialized))
                     (with-no-warnings ns-initialized))
                 (not (eq evil-visual-selection 'block)))
        (x-set-selection 'PRIMARY (buffer-substring-no-properties
                                   evil-visual-beginning
                                   evil-visual-end))
        (setq x-last-selected-text-primary ))))

  (debug-init-message "debug-init DISPLAY")

  (setq abbrev-file-name (concat spacemacs-cache-directory "abbrev_defs"))

  ;; =======
  ;; Display
  ;; =======

  ;; (setq fci-rule-character ?\u2509)
  (setq fci-rule-character ?\u250A)
  (setq fci-rule-character-color "#121212")
  ;; (setq fci-rule-color "#222222")

  (unless (string-equal system-type "darwin")
    (setq split-width-threshold 160)
    (setq split-height-threshold 48)
    (setq window-min-width 60)
    (setq window-min-height 12)

    ;; (defun split-horizontally-for-temp-buffers ()
    ;;   "Split the window horizontally for temp buffers."
    ;;   (when (and (one-window-p t)
    ;;                   (not (active-minibuffer-window)))
    ;;     (split-window-horizontally)))

    ;; (add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)

    (defun split-window-prefer-horizonally (window)
      "If there's only one window (excluding any possibly active
         minibuffer), then split the current window horizontally."
      (if (and (one-window-p t)
               (not (active-minibuffer-window)))
          (let ((split-height-threshold nil))
            (split-window-sensibly window))
        (split-window-sensibly window)))

    (setq split-window-preferred-function 'split-window-prefer-horizonally)

    )

  ;; =================================
  ;; Global Key Bindings and Registers
  ;; =================================

  (fold-section "Registers"
                (set-register ?t "target remote /tmp/unet-test/tests.errors.test_errors/r1/s/gdbserver")
                (set-register ?T "target remote /tmp/unet-test/tests.errors.test_errors/r2/s/gdbserver")
                )

  (fold-section "Keybindings"
                (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

                (defun activate-iterm ()
                  (interactive)
                  (do-applescript "tell application \"iTerm\" to activate"))

                (global-unset-key (kbd "C-q"))
                (global-set-key (kbd "C-q C-SPC") 'activate-iterm)
                (global-set-key (kbd "C-q C-q") 'quoted-insert)

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
                  "w0" 'delete-window
                  "w1" 'delete-other-windows
                  "w@" 'make-frame-command
                  "oa" 'org-agenda
                  ;; "og" 'helm-org-agenda-files-headings
                  ;; "oo" 'org-clock-out
                  "oc" 'org-capture
                  "oC" 'helm-org-capture-templates ;requires templates to be defined.
                  "ol" 'org-store-link
                  ;; "aL" 'lisp-interaction-mode
                  )
                )

  ;; =================
  ;; Programming Modes
  ;; =================

  (spacemacs|use-package-add-hook rebox2
    :post-init
    (progn
      (setq rebox-style-loop '(81 82 83))
      ;; (setq rebox-style-loop '(71 72 73 74 75 76 77 81 82 83 84 85 86 87))
      ;; C-mode comments
      ;; (setq-default '(241 235 245))
      )
    )

  (debug-init-message "USER-INIT: End"))


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  (debug-init-message "USER-LOAD: Start")
  (debug-init-message "USER-LOAD: End")
  )


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (debug-init-message "USER-CONFIG: Start")

  ;; (unless (display-graphic-p)
  ;;   (add-hook 'after-make-frame-functions 'enable-CSI-u)
  ;;   (enable-CSI-u))

  (let ((repo (concat dotspacemacs-directory "repos/py-snippets/snippets/")))
    (message repo)
    (if (file-directory-p repo)
        (setq yas-snippet-dirs (append yas-snippet-dirs `(,repo)))))


  (when-layer-used 'python
                   ;; Get our hooks into python mode early
                   ;; (message "config stage: current python mode hook %s" python-mode-hook)
                   (when-layer-used 'rebox
                                    (defun rebox-python-hook ()
                                      (interactive)
                                      (message "running rebox-python-hook")
                                      (bind-key "M-q" 'rebox-dwim python-mode-map)
                                      ;; (set (make-local-variable 'rebox-style-loop) '(401 402 403 413 415))
                                      (set (make-local-variable 'rebox-style-loop) '(71 72 73)))
                                    (add-hook 'python-mode-hook 'rebox-python-hook))

                   (defun my-python-before-save-hook ()
                     (if (bound-and-true-p blacken-mode)
                         (py-isort-before-save)))

                   (defun my-config-python-mode-hook ()
                     (setq comment-column 60)
                     (python-docstring-mode 1)
                     ;; Check to see if there's a pylint in the project directory maybe?
                     (add-hook 'before-save-hook 'my-python-before-save-hook)
                     (message "XXX checker set in early config hook")
                     ;; flake8 will chain in pylint
                     ;; (flycheck-select-checker 'python-flake8)
                     ;; (flycheck-select-checker 'python-pylint)
                     (semantic-mode -1))
                   (add-hook 'python-mode-hook 'my-config-python-mode-hook)
                   ;; (message "config stage: current python mode hook %s" python-mode-hook)
                   )

  (unless (string-prefix-p "hp13" (system-name))
    (require 'clipetty)
    (global-clipetty-mode))

  ;; sanityinc-tomorrow-blue
  ;; borland-blue
  ;; sanityinc-solarized-dark
  ;; misterioso
  ;; mandm
  ;; gruvbox-light-hard
  ;; molokai
  ;; leuven

  ;; Put this back
  (cond
   ((string-equal system-type "darwin") ; Mac OS X
    (spacemacs/load-theme 'mandm))
   ((or
     (string-prefix-p "flk" (system-name))
     (string-prefix-p "labnh" (system-name))
     (string-prefix-p "lake" (system-name))
     )
    ;; (spacemacs/load-theme 'sanityinc-tomorrow-blue)
    (spacemacs/load-theme 'mandm)
    )
   ((or
     (string-prefix-p "builder" (system-name))
     (string-prefix-p "cmf-" (system-name))
     (string-prefix-p "dlk" (system-name))
     (string-prefix-p "hp13" (system-name))
     (string-prefix-p "rlk" (system-name))
     (string-prefix-p "ubb" (system-name))
     (string-prefix-p "uff" (system-name))
     )
    (spacemacs/load-theme 'cobalt))
   ((string-equal system-type "gnu/linux")
    (spacemacs/load-theme 'mandm))
   (t (spacemacs/load-theme 'sanityinc-tomorrow-blue)))

  ;; XXX is this going to make everything fail?
  (defun et/semantic-remove-hooks ()
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-notc-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-nolongprefix-completion-at-point-function))

  (add-hook 'semantic-mode-hook #'et/semantic-remove-hooks)


  (defun fix-user-full-name (orig-fun &rest args)
    (let ((res (apply orig-fun args)))
      (if (= (length res) 0)
          (setq res "Christian Hopps")
        (replace-regexp-in-string " E " " E. " (replace-regexp-in-string " - CONTRACTOR" "" res)))
      ))
  (advice-add 'user-full-name :around #'fix-user-full-name)

  (progn
    ;; I *like* being able to get back to package files.
    (with-eval-after-load "recentf"
      (setq recentf-exclude (delete (recentf-expand-file-name package-user-dir) recentf-exclude)))

    (cond ((string-equal system-type "darwin") (progn (exec-path-from-shell-copy-env "PATH")
                                                      (if (string-prefix-p "aarch64" system-configuration)
                                                          (setq insert-directory-program "/opt/homebrew/bin/gls")
                                                        (setq insert-directory-program "/usr/local/bin/gls"))
                                                      ;; (setq insert-directory-program "/bin/ls")
                                                      (setq dired-listing-switchecs "-al --dired")
                                                      ;; (setq dired-listing-switches "-aBhl")
                                                      )
           (or (and (fboundp 'server-running-p) (server-running-p)) (server-start))
           ))

    ;; (cond ((not (string-equal system-type "darwin"))
    ;;        (add-hook after-make-frame-functions (lambda () (load-theme 'mandm)))))

    (if (not (daemonp)) (server-start))
                                        ; (if (daemonp)
                                        ;    (add-hook 'after-make-frame-functions
                                        ;            (lambda (frame)
                                        ;                        (with-selected-frame frame
                                        ;                                          (load-theme 'mandm t))))
                                        ;                                              (load-theme 'mandm t))

    ;; (if (string= (getenv "HOSTNAME") "tops")
    ;;     (load-theme 'mandm))

    (debug-init-message "debug-init USER-CONFIG")

    ;; Add themes from megapack to dotspacemacs-themes
    ;; (setq dotspacemacs-themes
    ;;       (mapcar (lambda (package)
    ;;                 (intern (string-remove-suffix "-theme" (symbol-name package))))
    ;;               themes-megapack-packages))

    ;;
    ;; Indention
    ;;

    ;; Most everything use 4 space indent. This variable is a collection of
    ;; the values used by various modes join-line
    (dolist (e spacemacs--indent-variable-alist)
      (if (symbolp (cdr e))
          (if (not (eq 'lisp-indent-offset (cdr e)))
              (set-default (cdr e) 4))
        (dolist (x (cdr e)) (set-default x 4))))

    (setq-default scroll-margin 5)

    ;; (setq-default lisp-indent-offset nil)
    ;; (setq emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (setq python-indent-offset 4)
    ;; overrides
    (setq-default nxml-child-indent 2)
    (setq-default yaml-indent-offset 2)
    ;; This is annoying it's used for json and js need to split up
    (setq-default js-indent-level 2)

    ;; tabs are 8 characters wide!
    (setq-default tab-width 8)
    ;; 80 columns is the normal fill column
    (setq-default fill-column 80)


    ;; (with-eval-after-load "ispell"
    ;; (setq ispell-program-name "hunspell")

    (setq-default magit-todos-ignored-keywords '("NOTE" "DONE" "FAIL"))
    (setq-default transient-default-level 7)
    (setq-default evil-escape-key-sequence nil)

    ;; take out
    ;; (setq magithub-debug-mode t)

    ;; May be able to remove now
    ;; Hate smart parens but apparently still want code??
    ;; (remove-hook 'prog-mode-hook #'smartparens-mode)
    ;; (spacemacs/toggle-smartparens-globally-off)

    ;; (sp-pair "'" nil :actions :rem)
    ;; (sp-pair "\"" nil :actions :rem)
    ;; (sp-pair "(" nil :actions :rem)
    ;; (sp-pair "\{" nil :actions :rem)
    ;; (sp-pair "\[" nil :actions :rem)

    (run-hook-with-args 'spacemacs--hjkl-completion-navigation-functions
                        (member dotspacemacs-editing-style '(vim)))

    ;; Failue b/c dired-mode-map not defined here
    ;; (define-key dired-mode-map "e" 'dired-ediff-files)
    ;; ;; let's create a dired micro-state?
    ;; ;; (define-key dired-mode-map "?" 'spacemacs/dired-transient-state/body)
    ;; (define-key dired-mode-map "?" 'which-key-show-top-level)

    ;; (xclip-mode 1)
    ;; (when (not (display-graphic-p))
    ;;   (condition-case err
    ;;       (turn-on-xclip)
    ;;     (void-function nil)))

    (with-eval-after-load "browse-url"
      ;;(defun browse-url-can-use-xdg-open ()
      (defadvice browse-url-can-use-xdg-open (after ad-browse-url-can-use-xdg-open activate)
        "Always use xdg-open"
        (setq ad-return-value t)))

    (setq
     spacemacs--hjkl-completion-navigation-functions nil
     browse-url-new-window-flag nil
     browse-url-browser-function #'browse-url-generic
     browse-url-generic-program "openurl-chrome.sh"
     tab-always-indent t
     case-fold-search nil
     )

    (add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
    (add-to-list 'auto-mode-alist '("\\.act\\'" . python-mode))

    ;; =======
    ;; Display
    ;; =======

    (fold-section "display"
                  (global-hl-line-mode -1)            ; Disable hihglighting of current line.
                  (spaceline-toggle-minor-modes-off)
                  (spaceline-toggle-major-mode-off)
                  (spaceline-toggle-point-position-off)
                  (spaceline-toggle-buffer-encoding-on)
                  (spaceline-toggle-buffer-encoding-abbrev-off)
                  (spaceline-toggle-org-clock-on)
                  ;; fill-column-mode character doesn't work
                  (set-face-inverse-video-p 'vertical-border nil)
                  (set-face-background 'vertical-border (face-background 'default))
                  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?\u2999))
                  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?\u299A))
                  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?\u2503))
                  )

    ;; =======
    ;; Layouts
    ;; =======

    (fold-section "layouts"

                  ;; (spacemacs|define-custom-layout "agenda"
                  ;;   :binding "a"
                  ;;   :body
                  ;;   (org-agenda-list))

                  ;; (spacemacs|define-custom-layout "dot"
                  ;;   :binding "d"
                  ;;   :body
                  ;;   (progn
                  ;;     (find-file "~/homeroot/dot.files/dot.profile")
                  ;;     (split-window-right)
                  ;;     (find-file "~/homeroot/dot.files/dot.bashrc")
                  ;;     ))

                  ;; (spacemacs|define-custom-layout "mail"
                  ;;   :binding "m"
                  ;;   :body
                  ;;   (mu4e))

                  (setq persp-add-buffer-on-after-change-major-mode 'free)

                  (spacemacs|define-custom-layout "notes"
                    :binding "n"
                    :body
                    (progn
                      (find-file "~/Dropbox/org-mode/work.org")
                      (split-window-right)
                      (find-file "~/Dropbox/org-mode/notes.org")
                      )
                    )

                  ;; (defun persp-mode-buffer-assoc (buffer layout-name)
                  ;;   (let* ((npersp (or (persp-get-by-name layout-name)
                  ;;                      ;;(and (message "XXX Adding new persp %s" layout-name)
                  ;;                      ;;     (persp-add-new layout-name))))
                  ;;                      (persp-add-new layout-name)))
                  ;;          (cpersp (get-frame-persp)))
                  ;;     ;; (message "XXX persp-mode-buffer-assoc current layout: %s target layout %s target name %s buffer: %s"
                  ;;     ;;  (persp-name cpersp) (persp-name npersp) layout-name buffer)
                  ;;     ;; Add to correct perspective
                  ;;     (if (memq buffer (persp-buffers npersp))
                  ;;         ;; (message "XXX buffer %s already in %s" buffer npersp)
                  ;;         ;; (message "XXX Adding buffer %s to %s" buffer (persp-name npersp))
                  ;;         (persp-add-buffer buffer npersp t))))
                  ;; (spacemacs|define-custom-layout "@ERC"
                  ;;   :binding "i"
                  ;;   :body
                  ;;   (progn
                  ;;     (add-hook 'erc-connect-pre-hook '(lambda (x)
                  ;;                                        (persp-mode-buffer-assoc x "@ERC")))
                  ;;     ;; (launch-irc-jabber)
                  ;;     ;; (launch-irc-netbsd)
                  ;;     ;; (split-window-right)
                  ;;     ;; (launch-irc-freenode)
                  ;;     ;; (split-window-right)
                  ;;     ;; (launch-irc-gitter)
                  ;;     )
                  ;;   )

                  )

    ;; ===========
    ;; Keybindings
    ;; ===========

    (fold-section "bindings"
                  (defun chopps-redefine-key (key func)
                    (let ((map (lookup-key spacemacs-default-map key)))
                      (if map
                          (define-key map key func)
                        (global-set-key key func))))

                  (defun dear-leader/swap-keys (key1 key2)
                    (let ((map1 (lookup-key spacemacs-default-map key1))
                          (map2 (lookup-key spacemacs-default-map key2)))
                      (spacemacs/set-leader-keys key1 map2 key2 map1)))
                  (dear-leader/swap-keys "am" "aM")

                  (global-set-key (kbd "M-n") 'next-error)
                  (global-set-key (kbd "M-p") 'previous-error)

                  (global-set-key (kbd "C-\\") 'spacemacs/layouts-transient-state/persp-next)
                  (global-set-key (kbd "C-]") 'ggtags-find-tag-dwim)
                  )

    ;; =================
    ;; Mac Notifications
    ;; =================

    ;;
    ;; If we have mac notifications then replace the dbus version
    ;;
    (debug-init-message "initializing notifications")
    (when (fboundp 'mac-notification-os-send)
      (require 'mac-notifications))

    ;; (defvar mac-notification-action-hash (make-hash-table :test 'equal)
    ;;   "A hash table for looking up category names by action list")
    ;; (puthash nil "Generic" mac-notification-action-hash)

    ;; (defun mac-notification-get-category (actions)
    ;;   "Get the category for these actions creating if necessary"
    ;;   (let ((category (gethash actions mac-notification-action-hash)))
    ;;     (unless category
    ;;       ;; Create a new category for this unique action list
    ;;       (setq category (symbol-name (cl-gensym)))
    ;;       (mac-notification-add-category category actions)
    ;;       (puthash actions category mac-notification-action-hash))
    ;;     category))

    ;; ;; Override dbus version
    ;; (defun notifications-notify (&rest params)
    ;;   (with-demoted-errors
    ;;       (let* ((actions (plist-get params :actions))
    ;;              (category (mac-notification-get-category actions))
    ;;              (title (plist-get params :title))
    ;;              (body (plist-get params :body)))
    ;;         (setq params (plist-put params :category category))
    ;;         (apply #'mac-notification-send title body params)))))

    ;; ==========
    ;; Messaging
    ;; ==========

    ;; (defun my-erc-perspective ()
    ;;   "Switch or create to a perspective called 'erc' and connect to IRC"
    ;;   (interactive)
    ;;   (select-frame (make-frame '((name . "@ERC") (minibuffer . t))))
    ;;   (persp-switch "freenode.net")
    ;;   (erc :server "irc.freenode.net" :port "6667" :nick "chopps"))

    ;;     (when-layer-used 'erc
    ;;      (setq erc-prompt-for-nickserv-password nil
    ;;             erc-autojoin-channels-alist '(("irc.gitter.im" "#syl20bnr/spacemacs")
    ;;                                           ("192.168.1.6" "#syl20bnr/spacemacs")
    ;;                                           ("mollari.netbsd.org" "#NetBSD")
    ;;                                           ("freenode.net" "#choppstest")
    ;;                                           )
    ;;             erc-auto-query 'window
    ;;             erc-fill-mode nil
    ;;             ;; erc-hl-nicks-minimum-contrast-ratio 3.5
    ;;             ;; erc-hl-nicks-color-contrast-strategy 'contrast
    ;;             ;; erc-hl-nicks-color-contrast-strategy 'invert
    ;;             erc-hl-nicks-skip-nicks '("gitter")
    ;;             erc-join-buffer 'bury

    ;;             ;; Logging
    ;;             erc-log-channels-directory "~/Dropbox/logs/erclogs"
    ;;             ;; erc-log-all-but-server-buffers t
    ;;             erc-log-insert-log-on-open nil ;; this inserts after the prompt which is scary as it
    ;;             ;; might send
    ;;             erc-save-buffer-on-part nil
    ;;             erc-save-queries-on-quit nil
    ;;             erc-log-write-after-send t
    ;;             erc-log-write-after-insert t

    ;;             ;; Notifcations
    ;;             erc-notifications-icon (concat user-emacs-directory "./layers/+chat/rcirc/img/irc.png")
    ;;             erc-spelling-mode t

    ;;             erc-track-switch-direction 'importance
    ;;             )
    ;;       ;; We want to be in normal state most of the time so we can flip in and out.
    ;;       (evil-set-initial-state 'erc-mode 'normal)


    ;;     ;; https://github.com/syl20bnr/spacemacs/issues/11152
    ;;     (setq projectile-keymap-prefix (kbd "C-c C-p"))

    ;;       ;; Simplify this function so it works
    ;;       (with-eval-after-load 'erc-log
    ;;         (defun erc-log-all-but-server-buffers (buffer)
    ;;           (not (erc-server-buffer-p buffer))))


    ;;       ;; Actually we really only want this when we move away from the buffer?
    ;;       ;; (add-hook 'erc-send-post-hook 'evil-normal-state)
    ;;       ;; (remove-hook 'erc-send-post-hook 'evil-normal-state)


    ;;       ;; '(erc-autoaway-idle-seconds 600)
    ;;       ;; '(erc-autojoin-mode t)
    ;;       ;; '(erc-button-mode t)
    ;;       ;; '(erc-current-nick-highlight-type (quote all))
    ;;       ;; '(erc-fill-mode t)
    ;;       ;; '(erc-hl-nicks-mode t)
    ;;       ;; '(erc-hl-nicks-trim-nick-for-face nil)
    ;;       ;; '(erc-irccontrols-mode t)
    ;;       ;; '(erc-kill-buffer-on-part t)
    ;;       ;; '(erc-kill-queries-on-quit t)
    ;;       ;; '(erc-kill-server-buffer-on-quit t)
    ;;       ;; '(erc-list-mode t)
    ;;       ;; '(erc-log-channels-directory "/Users/chopps/Dropbox/erclogs" t)
    ;;       ;; '(erc-log-mode t)
    ;;       ;; '(erc-match-mode t)
    ;;       ;; '(erc-menu-mode t)
    ;;       ;; '(erc-move-to-prompt-mode t)
    ;;       ;; '(erc-netsplit-mode t)
    ;;       ;; '(erc-networks-mode t)
    ;;       ;; '(erc-noncommands-mode t)
    ;;       ;; '(erc-pcomplete-mode t)
    ;;       ;; '(erc-prompt (lambda nil (concat "[" (buffer-name) "]")))
    ;;       ;; '(erc-readonly-mode t)
    ;;       ;; '(erc-ring-mode t)
    ;;       ;; '(erc-server-coding-system (quote (utf-8 . utf-8)))
    ;;       ;; '(erc-services-mode t)
    ;;       ;; '(erc-social-graph-dynamic-graph t)
    ;;       ;; '(erc-stamp-mode t)
    ;;       ;; '(erc-track-minor-mode t)
    ;;       ;; '(erc-track-mode t)
    ;;       ;; '(erc-youtube-mode t)

    ;;       (defun erc-acct-get-password (user host port)
    ;;         (let* ((auth-source-creation-defaults nil)
    ;;                (auth-source-creation-prompts '((password . "Enter IRC password for %h:%p")))
    ;;                (secret (plist-get (nth 0 (auth-source-search
    ;;                                           :type 'netrc
    ;;                                           :max 1
    ;;                                           :host host
    ;;                                           :user user
    ;;                                           :port port))
    ;;                                   :secret)))
    ;;                (if (functionp secret)
    ;;                    (funcall secret)
    ;;                  secret)))

    ;;       (with-eval-after-load 'erc
    ;;        (setq erc-nickserv-passwords
    ;;              `((freenode (("chopps" . ,(erc-acct-get-password "chopps" "freenode.net" "nickserv"))))
    ;;                (localhost (("chopps" . ,(erc-acct-get-password "chopps" "localhost" "bitlbee")))))))

    ;;       (defun launch-irc-gitter ()
    ;;         "Launch irc connection to giter.im"
    ;;         (interactive)
    ;;         (erc :server "localhost" :port 6669 :nick "choppsv1"
    ;;             :password (erc-acct-get-password "choppsv1" "192.168.1.6" 6669)))
    ;;         ;; (erc-tls :server "irc.gitter.im" :port 6667 :nick "choppsv1"
    ;;         ;;         :password (erc-acct-get-password "choppsv1" "irc.gitter.im" 6667)))

    ;;       (defun launch-irc-netbsd ()
    ;;         "Launch irc connection to netbsd"
    ;;         (interactive)
    ;;         (erc-tls :server "mollari.netbsd.org" :port 7001 :nick "chopps" :full-name "Christian E. Hopps"))

    ;;       (defun launch-irc-freenode ()
    ;;         "Launch irc connection to freenode"
    ;;         (interactive)
    ;;         (erc-tls :server "irc.freenode.net" :port 6697 :nick "chopps"
    ;;                  :password (erc-acct-get-password "chopps" "freenode.net" 6697)))

    ;;       (defun launch-irc-jabber ()
    ;;         "Launch irc connection to jabber"
    ;;         (interactive)
    ;;         (erc :server "localhost" :port 6667 :nick "chopps" :full-name "Christian E. Hopps"))

    ;;       (defun launch-erc ()
    ;;         "Launch all our connections to IRC"
    ;;         (interactive)
    ;;         (launch-irc-gitter)
    ;;         (launch-irc-freenode)
    ;;         (launch-irc-netbsd)
    ;;         (launch-irc-jabber))

    ;;       (spacemacs/set-leader-keys
    ;;         "aif" 'launch-irc-freenode
    ;;         "aij" 'launch-irc-jabber
    ;;         "ain" 'launch-irc-netbsd
    ;;         "aig" 'launch-irc-gitter
    ;;         "aiL" 'launch-erc)

    ;;       (defun bitlbee-netrc-identify ()
    ;;         "Auto-identify for Bitlbee channels using authinfo or netrc.

    ;;         The entries that we look for in netrc or authinfo files
    ;;         have their 'port' set to 'bitlbee', their 'login' or
    ;;         'user' set to the current nickname and 'server' set to
    ;;         the current IRC server's name. A sample value that works
    ;;         for authenticating as user 'keramida' on server
    ;;         'localhost' is:

    ;;             machine localhost port bitlbee login keramida password supersecret"

    ;;         (interactive)
    ;;         (when (string= (buffer-name) "&bitlbee")
    ;;           (let ((pass (erc-acct-get-password (erc-current-nick) erc-session-server "bitlbee")))
    ;;             ;; (message "Sending privmsg to &bitlbee server %s" erc-session-server)
    ;;             (erc-message "PRIVMSG"
    ;;                          (format "%s identify %s"
    ;;                                  (erc-default-target)
    ;;                                  pass)))))
    ;;       (add-hook 'erc-join-hook 'bitlbee-netrc-identify)

    ;;       ;; We only need this if we aren't using our local bridge
    ;;       ;; ;; add a user to the current channel
    ;;       ;; (defun add-nick-insert-pre-hook (line)
    ;;       ;;   "Add user to ERC channel list"
    ;;       ;;   (when (string= erc-session-server "irc.gitter.im")
    ;;       ;;     (save-match-data
    ;;       ;;       (when (string-match "^<\\([^>]+\\)> .*" line)
    ;;       ;;         (let ((nick (match-string 1 line)))
    ;;       ;;           (erc-update-current-channel-member nick nick 'add-if-new))))))
    ;;       ;; (add-hook 'erc-insert-pre-hook 'add-nick-insert-pre-hook)

    ;;       ;; (setq erc-modules (delete 'fill erc-modules))

    ;;       ;; XXX debug mode-line muckup
    ;;       ;; (defvar evil-normal-state-on-unfocus-modes
    ;;       ;;   '(erc-mode))
    ;;       ;; (defun evil-normal-state-on-unfocus ()
    ;;       ;;   "Return to normal state when a buffer in a given major mode is unfocussed"
    ;;       ;;   (when (member major-mode evil-normal-state-on-unfocus-modes)
    ;;       ;;     (evil-normal-state)))
    ;;       ;; (add-hook 'unfocus-buffer-hook 'evil-normal-state-on-unfocus)

    ;;       (with-eval-after-load 'erc
    ;;         (erc-fill-disable)
    ;;         ;; (erc-log-enable)
    ;;         (add-to-list 'erc-modules 'notifications)
    ;;         (setq erc-modules (cons 'log (delete 'fill erc-modules))))
    ;;         ;; (erc-services-mode 1)
    ;;         ;; (erc-spelling-mode 1))
    ;;       (with-eval-after-load 'erc-desktop-notifications
    ;;         ;; Redefine this so that we can set a timeout
    ;;         ;; normally it uses the server and we use awesome so it would be nice to
    ;;         ;; fix that there.
    ;;         (defun erc-notifications-notify (nick msg)
    ;;           "Notify that NICK send some MSG.
    ;; This will replace the last notification sent with this function."
    ;;           (dbus-ignore-errors
    ;;             (setq erc-notifications-last-notification
    ;;                   (notifications-notify :title (xml-escape-string nick)
    ;;                                         :timeout 0
    ;;                                         :body (xml-escape-string msg)
    ;;                                         :replaces-id erc-notifications-last-notification
    ;;                                         :app-icon erc-notifications-icon))))

    ;;         )
    ;;       )

    ;; (when-layer-used 'rcirc
    ;;  (defun get-gitter-password ()
    ;;     (let* ((auth-source-creation-defaults nil)
    ;;            (auth-source-creation-prompts '((password . "Enter IRC password for %h:%p")))
    ;;            (sec (plist-get (nth 0 (auth-source-search
    ;;                                    :type 'netrc
    ;;                                    :max 1
    ;;                                    :host "irc.gitter.im"
    ;;                                    :port 6667
    ;;                                    :user "choppsv1"))
    ;;                            :secret)))
    ;;       (if (functionp sec)
    ;;           (funcall sec)
    ;;         sec)))
    ;;   (setq
    ;;    rcirc-log-directory "~/Dropbox/logs/rcirclogs"
    ;;    rcirc-time-format "%H:%M "
    ;;    rcirc-server-alist
    ;;    `(
    ;;      ("127.0.0.1"
    ;;       :user "choppsv1"
    ;;       :port "6669"
    ;;       :password ,(get-gitter-password)
    ;;       ;; :encryption tls
    ;;       :channels ("#syl20bnr/spacemacs"))
    ;;      ("asimov.freenode.net"
    ;;       :user "chopps"
    ;;       :port "6697"
    ;;       :encryption tls
    ;;       :channels ("#org-mode")
    ;;       )
    ;;      ;; ("mollari.netbsd.org"
    ;;      ;;  :user "chopps"
    ;;      ;;  :port "7001"
    ;;      ;;  :encryption tls
    ;;      ;;  :channels ("#netbsd")
    ;;      ;; )
    ;;      ("localhost"
    ;;       :user "chopps"
    ;;       :port "6667"
    ;;       ;; :channels ("#ts")
    ;;       )
    ;;      )
    ;;    )
    ;;   )

    ;; (when-layer-used 'jabber
    ;;  (setq ssl-program-name "gnutls-cli"
    ;;         ssl-program-arguments '("--insecure" "-p" service host)
    ;;         ssl-certificate-verification-policy 1)

    ;;   (setq jabber-account-list '(("choppsv2@localhost"
    ;;                                (:port . 5222)
    ;;                                (:password . "foobar"))))
    ;;                                ;;(:connection-type . ssl))))
    ;;   )

    ;; ======
    ;; helm
    ;; ======

    (when (configuration-layer/package-usedp 'helm)
      (require 'helm-net)
      (push '("Stack Snippet" . (lambda (candidate)
                                  (helm-search-suggest-perform-additional-action
                                   "http://www.stacksnippet.com/#gsc.tab=0&gsc.q=%s"
                                   candidate)))
            helm-google-suggest-actions)
      (defun my-helm-google-suggest ()
        (interactive)
        ;; set debug-on-error to swallow potential network errors
        ;; idea taken from: https://blog.johnregner.com/post/78877988910/fixing-helm-spotify#_=_
        (let ((debug-on-error t))
          (helm-google-suggest))))


    ;; ======
    ;; Email
    ;; ======

    (when-layer-used 'mu4e
                     (defcustom mu4e-spam-folder "/chopps.org/spam-train"
                       "Folder for spam email"
                       :type '(string :tag "Folder name")
                       :group 'mu4e-folders)

                     (setq mu4e-attachment-dir "~/Downloads"
                           mu4e-debug t
                           mu4e-mu-debug nil

                           ;;
                           ;; Indexing
                           ;;
                           mu4e-index-cleanup nil
                           mu4e-index-lazy-check t
                           mu4e-change-filenames-when-moving t
                           mu4e-update-interval nil

                           ;; mu4e-hide-index-messages t

                           ;; -------
                           ;; Viewing
                           ;; -------
                           mu4e-headers-results-limit 1000
                           mu4e-headers-visible-lines 15
                           mu4e-headers-visible-columns 200

                           ;; For searches useful as t to find replies to threads?
                           mu4e-headers-include-related nil
                           mu4e-view-show-addresses t
                           mu4e-view-use-gnus t

                           ;; mm-text-html-renderer 'shr
                           mm-text-html-renderer 'gnus-w3m
                           ;; mm-text-html-renderer 'lynx
                           ;; HTML
                           ;; mu4e-html2text-command 'my-mu4e-shr2text
                           shr-color-visible-luminance-min 80
                           shr-use-fonts nil

                           mu4e-use-fancy-chars t

                           ;; -----------
                           ;; Composition
                           ;; -----------
                           send-mail-function 'smtpmail-send-it
                           message-send-mail-function 'smtpmail-send-it
                           message-cite-reply-position 'below

                           ;; don't keep message buffers around
                           message-kill-buffer-on-exit t
                           mu4e-compose-complete-addresses t
                           ;; mu4e-compose-complete-only-personal t
                           ;; mu4e-compose-complete-only-after "2019-01-01"
                           mu4e-compose-context-policy 'ask-if-none
                           mu4e-compose-format-flowed t
                           ;; This is an interesting value.. it's where soft-newlines will be
                           ;; inserted for wrapping. The suggestion is 66, but that's based on
                           ;; the ancient assumption of very small dumb-terminals with old
                           ;; clients. In fact smart phones (the target now) will support
                           ;; format=flowed. So the affect; viewing on clients that do not
                           ;; support format=flowed (e.g. mu4e emacs!) line-wrapping even if
                           ;; the window is quite large. This sucks so we choose a huge value.
                           fill-flowed-encode-column 100000
                           mu4e-compose-in-new-frame t
                           mu4e-compose-keep-self-cc t
                           mu4e-compose-signature-auto-include nil
                           mu4e-context-policy 'pick-first
                           mm-verify-option 'known

                           ;; ------------------
                           ;; Address Completion
                           ;; ------------------

                           message-completion-alistp '(("^\\(Newsgroups\\|Followup-To\\|Posted-To\\|Gcc\\):" . message-expand-group)
                                                       ("^\\(Resent-\\)?\\(To\\|B?Cc\\):" . message-expand-name)
                                                       ("^\\(Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):" . message-expand-name)
                                                       ("^\\(Disposition-Notification-To\\|Return-Receipt-To\\):" . message-expand-name))

                           ;; -----------
                           ;; [b]ookmark
                           ;; -----------

                           mu4e-inbox-mailbox '("maildir:/labn.net/INBOX"
                                                "maildir:/us.labn.net/INBOX"
                                                "maildir:/chopps.org/INBOX"
                                                "maildir:/nrl.navy.mil/INBOX"
                                                "maildir:/gmail.com/INBOX")

                           mu4e-imp-mailbox '("maildir:/chopps.org/ietf-chairs"
                                              "maildir:/chopps.org/ietf-chairs-rtg"
                                              "maildir:/chopps.org/ietf-rtg-dir"
                                              "maildir:/chopps.org/ietf-wg-ipsec"
                                              "maildir:/chopps.org/ietf-wg-lsr"
                                              "maildir:/chopps.org/ietf-wg-netmod"
                                              "maildir:/chopps.org/ietf-wg-rtg"
                                              "maildir:/chopps.org/linux-ipsec"
                                              )

                           mu4e-junk-mailbox '("maildir:/gmail.com/[Gmail]/Spam"
                                               "maildir:/gmail.com/[Gmail]/Trash"
                                               "maildir:/labn.net/Junk Email"
                                               "maildir:/us.labn.net/Junk Email"
                                               "maildir:/chopps.org/spam-probable"
                                               "maildir:/chopps.org/spam-train"
                                               "maildir:/chopps.org/spam")

                           mu4e-unread-filter "(flag:unread AND NOT flag:flagged AND NOT flag:trashed)"
                           mu4e-not-junk-folder-filter (concat " NOT (" (string-join mu4e-junk-mailbox " OR " ) ")")
                           mu4e-inbox-filter-base (concat "(" (string-join mu4e-inbox-mailbox " OR ") ")")
                           mu4e-imp-filter-base (concat "(" (string-join mu4e-imp-mailbox " OR ") ")")
                           mu4e-unread-filter "(flag:unread AND NOT flag:trashed)"

                           mu4e-bookmarks
                           (append
                            (list (list (concat mu4e-unread-filter " AND " mu4e-inbox-filter-base) "Unread [i]NBOX messages" ?i)
                                  (list (concat mu4e-unread-filter " AND " "maildir:/chopps.org/INBOX") "Unread [c]hopps INBOX messages" ?c)
                                  (list (concat mu4e-unread-filter " AND " mu4e-imp-filter-base) "Unread Important messages" ?n)
                                  (list (concat mu4e-unread-filter " AND " mu4e-not-junk-folder-filter " AND " "maildir:/chopps.org/ietf-wg-lsr") "Unread LSR messages" ?l)
                                  (list (concat mu4e-unread-filter " AND " mu4e-not-junk-folder-filter " AND " "maildir:/chopps.org/ietf-wg-ipsec") "Unread netmod messages" ?I)
                                  (list (concat mu4e-unread-filter " AND " mu4e-not-junk-folder-filter " AND " "maildir:/chopps.org/ietf-*") "Unread IETF messages" ?e)
                                  (list (concat mu4e-unread-filter " AND NOT " mu4e-imp-filter-base " AND NOT " mu4e-inbox-filter-base " AND " mu4e-not-junk-folder-filter) "Unread [u]nimportant messages" ?u)
                                  (list (concat mu4e-unread-filter " AND NOT " mu4e-inbox-filter-base " AND " mu4e-not-junk-folder-filter) "Unread Non-INBOX messages" ?o)
                                  (list (concat mu4e-unread-filter " AND " mu4e-not-junk-folder-filter) "Unread messages" ?a)
                                  (list "(maildir:/chopps.org/spam-probable                                              )" "Probable spam messages" ?s))
                            (mapcar (lambda (x) (cons (concat (car x) " AND " mu4e-not-junk-folder-filter) (cdr x)))
                                    '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                                      ("date:1h..now" "Last hours messages" ?h)
                                      ("date:24h..now" "Today's messages" ?d)
                                      ("date:today..now" "Today's messages" ?t)
                                      ("date:7d..now" "Last 7 days" ?w)
                                      ("date:7d..now AND (from:chopps@chopps.org OR from:chopps@gmail.com OR from:chopps@us.labn.net OR from:chopps@labn.net OR from:christian.hopps.ctr@nrl.navy.mil)" "Last 7 days sent" ?W)
                                      ("date:14d..now AND (from:chopps@chopps.org OR  OR from:chopps@gmail.com OR from:chopps@us.labn.net OR from:chopps@labn.net OR from:christian.hopps.ctr@nrl.navy.mil)" "Last 14 days sent" ?F)
                                      ("mime:application/zip" "Messages with ZIP" ?z)
                                      ("mime:application/pdf" "Messages with PDF" ?p)
                                      ("mime:text/calendar" "Messages with calendar" ?q)
                                      ("mime:*cs" "Messages with VCS" ?Q)
                                      ))
                            )


                           ;; [j]ump shortcuts
                           mu4e-maildir-shortcuts '(("/chopps.org/INBOX" . ?i)
                                                    ("/nrl.navy.mil/INBOX" . ?n)
                                                    ("/labn.net/INBOX" . ?l)
                                                    ("/us.labn.net/INBOX" . ?u)
                                                    ("/gmail.com/INBOX" . ?g)
                                                    ("/chopps.org/aa-netbsd" . ?N)
                                                    ("/chopps.org/ietf-wg-lsr" . ?L)
                                                    ("/chopps.org/spam-train" . ?S)
                                                    ("/chopps.org/spam-probable" . ?s))
                           )

                     (with-eval-after-load 'mu4e

                       (progn
                         (debug-init-message "debug-init MU4E setq")


                         (if (string-prefix-p "aarch64" system-configuration)
                             (setq my-msmtp "/opt/homebrew/bin/msmtp")
                           (setq my-msmtp "/usr/local/bin/msmtp"))

                         (setq mu4e-contexts `(
                                               ,(make-mu4e-context
                                                 :name "chopps.org"
                                                 :match-func (lambda (msg)
                                                               (and msg (string-match "/chopps.org/.*" (mu4e-message-field msg :maildir))))
                                                 :vars '((user-mail-address  . "chopps@chopps.org")
                                                         (user-full-name . "Christian Hopps")
                                                         ;; mu4e
                                                         (mu4e-drafts-folder . "/chopps.org/Drafts")
                                                         (mu4e-sent-folder   . "/chopps.org/Sent Messages")
                                                         (mu4e-trash-folder  . "/chopps.org/Deleted Messages")
                                                         (mu4e-sent-messages-behavior   . sent)
                                                         ;; smtp
                                                         (message-send-mail-function . smtpmail-send-it)
                                                         (smtpmail-starttls-credentials . '(("smtp.chopps.org" 587 nil nil)))
                                                         (smtpmail-default-smtp-server  . "smtp.chopps.org")
                                                         (smtpmail-smtp-server          . "smtp.chopps.org")
                                                         ;;(smtpmail-starttls-credentials . '(("coffee.chopps.org" 25 nil nil)))
                                                         ;;(smtpmail-default-smtp-server  . "coffee.chopps.org")
                                                         ;;(smtpmail-smtp-server          . "coffee.chopps.org")
                                                         (smtpmail-local-domain         .      "chopps.org")
                                                         (smtpmail-stream-type          . starttls)
                                                         (smtpmail-smtp-service         . 587)))
                                               ,(make-mu4e-context
                                                 :name "labn.net"
                                                 :match-func (lambda (msg)
                                                               (and msg (string-match "/labn.net/.*" (mu4e-message-field msg :maildir))))
                                                 :vars '((user-mail-address  . "chopps@labn.net")
                                                         (user-full-name . "Christian Hopps")
                                                         ;; mu4e
                                                         (mu4e-drafts-folder . "/labn.net/Drafts")
                                                         (mu4e-sent-folder   . "/labn.net/Sent Items")
                                                         (mu4e-trash-folder  . "/labn.net/Deleted Items")
                                                         (mu4e-sent-messages-behavior   . sent)
                                                         ;; smtp
                                                         (message-send-mail-function . smtpmail-send-it)
                                                         (smtpmail-starttls-credentials . '(("smtp.office365.com" 587 nil nil)))
                                                         (smtpmail-default-smtp-server  . "smtp.office365.com")
                                                         (smtpmail-smtp-server          . "smtp.office365.com")
                                                         (smtpmail-local-domain         . "labn.net")
                                                         (smtpmail-stream-type          . starttls)
                                                         (smtpmail-smtp-service         . 587)))
                                               ,(make-mu4e-context
                                                 :name "us.labn.net"
                                                 :match-func (lambda (msg)
                                                               (and msg (string-match "/us.labn.net/.*" (mu4e-message-field msg :maildir))))
                                                 :vars '((user-mail-address  . "chopps@us.labn.net")
                                                         (user-full-name . "Christian Hopps")
                                                         ;; mu4e
                                                         (mu4e-drafts-folder . "/us.labn.net/Drafts")
                                                         (mu4e-sent-folder   . "/us.labn.net/Sent Items")
                                                         (mu4e-trash-folder  . "/us.labn.net/Deleted Items")
                                                         (mu4e-sent-messages-behavior   . sent)
                                                         ;; smtp
                                                         (message-send-mail-function . smtpmail-send-it)
                                                         (smtpmail-starttls-credentials . '(("smtp.office365.us" 587 nil nil)))
                                                         (smtpmail-default-smtp-server  . "smtp.office365.us")
                                                         (smtpmail-smtp-server          . "smtp.office365.us")
                                                         (smtpmail-local-domain         . "us.labn.net")
                                                         (smtpmail-stream-type          . starttls)
                                                         (smtpmail-smtp-service         . 587)))
                                               ,(make-mu4e-context
                                                 :name "nrl.navy.mil"
                                                 :match-func (lambda (msg)
                                                               (and msg (string-match "/nrl.navy.mil/.*" (mu4e-message-field msg :maildir))))
                                                 :vars `((user-mail-address  . "christian.hopps.ctr@nrl.navy.mil")
                                                         (user-full-name . "Christian Hopps")
                                                         ;; mu4e
                                                         (mu4e-drafts-folder . "/nrl.navy.mil/Drafts")
                                                         (mu4e-sent-folder   . "/nrl.navy.mil/Sent")
                                                         (mu4e-trash-folder  . "/nrl.navy.mil/Trash")
                                                         (mu4e-sent-messages-behavior   . sent)
                                                         ;; smtp
                                                         (message-send-mail-function . message-send-mail-with-sendmail)

                                                         (sendmail-program . ,my-msmtp)
                                                         (send-mail-function . 'smtpmail-send-it)
                                                         (message-sendmail-extra-arguments . ("--read-envelope-from"))

                                                         ;;ms
                                                         ;; (smtpmail-starttls-credentials . '(("outgoing.cmf.nrl.navy.mil" 587 nil nil)))
                                                         ;; (smtpmail-stream-type          . starttls)
                                                         ;; (smtpmail-smtp-service         . 587)

                                                         (message-sendmail-f-is-evil . t)))
                                               ,(make-mu4e-context
                                                 :name "gmail.com"
                                                 :match-func (lambda (msg)
                                                               (and msg (string-match "/gmail.com/.*" (mu4e-message-field msg :maildir))))
                                                 :vars '((user-mail-address  . "chopps@gmail.com")
                                                         (user-full-name . "Christian Hopps")
                                                         ;; mu4e
                                                         (mu4e-drafts-folder . "/gmail.com/[Gmail]/Drafts")
                                                         (mu4e-sent-folder   . "/gmail.com/[Gmail]/Sent Mail")
                                                         (mu4e-trash-folder  . "/gmail.com/[Gmail]/Trash")
                                                         (mu4e-sent-messages-behavior   . delete)
                                                         ;; smtp
                                                         (message-send-mail-function . smtpmail-send-it)
                                                         (smtpmail-starttls-credentials . '(("smtp.gmail.com" 587 nil nil)))
                                                         (smtpmail-default-smtp-server  . "smtp.gmail.com")
                                                         (smtpmail-smtp-server          . "smtp.gmail.com")
                                                         (smtpmail-local-domain         .      "gmail.com")
                                                         (smtpmail-stream-type          . starttls)
                                                         (smtpmail-smtp-service . 587)))
                                               ))


                         (debug-init-message "debug-init MU4E defuns")

                         (defun mu4e~headers-jump-to-maildir-unread (maildir)
                           "Show the unread messages in maildir.
The user is prompted to ask what maildir.  If prefix arg EDIT is
given, offer to edit the search query before executing it."
                           (interactive
                            (let ((maildir (mu4e-ask-maildir "Jump to maildir: ")))))
                           (when maildir
                             (let* ((query (format "maildir:\"%s\" AND flag:unread" maildir)))
                               (mu4e-mark-handle-when-leaving)
                               (mu4e-headers-search query))))

                         ;; (define-key mu4e-main-mode-map (kbd "J") 'mu4e~headers-jump-to-maildir-unread)
                         ;; (define-key evilified-state--normal-state-map (kbd "J") 'mu4e~headers-jump-to-maildir-unread)
                         ;; (define-key evilified-state--visual-state-map (kbd "J") 'mu4e~headers-jump-to-maildir-unread)

                         (defun ch/mu4e--query-insert-something (something)
                           (interactive)
                           (unless (looking-back "\\(:\\|and\\|or\\|not\\)[ \t]*")
                             (insert " and "))
                           (unless (looking-back "[ \t]")
                             (insert " "))
                           (insert something))

                         (defun ch/mu4e-query-insert-flag:unread ()
                           (interactive)
                           (ch/mu4e--query-insert-something "flag:unread"))

                         (defun ch/mu4e-query-insert-flag:flagged ()
                           (interactive)
                           (ch/mu4e--query-insert-something "flag:flagged"))

                         (defun ch/mu4e-query-insert-flag:attach ()
                           (interactive)
                           (ch/mu4e--query-insert-something "flag:attach"))

                         (defvar ch/mu4e-minibuffer-quick-insert-map
                           (let ((map (make-sparse-keymap)))
                             (define-key map (kbd "u") #'ch/mu4e-query-insert-flag:unread)
                             (define-key map (kbd "f") #'ch/mu4e-query-insert-flag:flagged)
                             (define-key map (kbd "a") #'ch/mu4e-query-insert-flag:attach)
                             map))

                         (define-key mu4e-minibuffer-search-query-map (kbd "C-u")
                                     ch/mu4e-minibuffer-quick-insert-map)


                         ;; Work around a bug with too long names in the spaceline/modeline
                         ;; This is causing an error now in emacs 27.2
                         ;; (defun trim-modeline-string-chopps (str)
                         ;;   (substring str 0 39))
                         ;; (add-function :filter-return (symbol-function 'mu4e~quote-for-modeline) 'trim-modeline-string-chopps)

                         ;; Mu4E Keyboard extras
                         (bind-key (kbd "'") 'mu4e-headers-next 'mu4e-headers-mode-map)
                         ;; XXXerror with vimification
                         ;; (bind-key (kbd "\"") 'mu4e-headers-prev 'mu4e-headers-mode-map)
                         ;; (bind-key (kbd "\"") 'mu4e-view-headers-prev 'mu4e-view-mode-map)
                         (bind-key (kbd "f") 'mu4e-view-go-to-url 'mu4e-view-mode-map)

                         (defun my-mu4e-contact-filter (addr)
                           (let ((re "\\(no[t]?[-\\.]?repl\\(y\\|ies\\)\\|@nrl.navy.mill\\|@dev.terastream.net\\|@nrl.nav.mil\\|@cmf.nrl.navy.mil\\|phoebe.johnson\\|christian.phoebe.hopps\\|phoebe.hopps@helloinnovation.com\\|lberger@fore.com\\)"))
                             (unless (string-match-p re addr) addr)))
                         (setq mu4e-contact-process-function 'my-mu4e-contact-filter)

                         (defun my-mu4e-index-udpated ()
                           (let ((ts (format-time-string "%H:%M:%S.%N")))
                             (message "%s mu4e: full updated complete." ts)))

                         (add-hook 'mu4e-index-updated-hook 'my-mu4e-index-udpated)

                         (defun mu4e-update-index-deep ()
                           "Reindex mu4e with cleanup"
                           (interactive)
                           (let ((mu4e-index-cleanup t)
                                 (mu4e-index-lazy-check t))
                             (let ((ts (format-time-string "%H:%M:%S.%N")))
                               (message "%s mu4e: full index update starting..." ts)
                               (mu4e-update-index))))

                         (bind-key (kbd "U") 'mu4e-update-index-deep 'mu4e-main-mode-map)
                         (bind-key (kbd "u") 'mu4e-update-index 'mu4e-main-mode-map)
                         ;; (bind-key (kbd "U") 'mu4e-update-mail-and-index  'mu4e-main-mode-map)

                         (defun mu4e-deal-with-moved-message ()
                           (if (equal major-mode 'mu4e-loading-mode)
                               (progn
                                 (delete-window)
                                 (mu4e-update-index)
                                 (message "correct!"))))

                         (defun mu4e-error-handler (errcode errmsg)
                           "Handler function for showing an error."
                           ;; don't use mu4e-error here; it's running in the process filter context
                           (cl-case errcode
                             ((102 111) (progn
                                          (mu4e-deal-with-moved-message)
                                          (error "Updating Index: Error %d: %s" errcode errmsg)))
                             (4 (user-error "No matches for this search query."))
                             (t (error "Error %d: %s" errcode errmsg))))


                         ;; (debug-init-message "debug-init MU4E helm")
                         ;; (defun ch:ct (clist)
                         ;;   "Transform candidate into (display . real)"
                         ;;   (mapcar (lambda (candidate)
                         ;;             (let* ((name (plist-get candidate :name))
                         ;;                     email (plist-get candidate :mail))
                         ;;               (or (and name (format "%s <%s>" name email))
                         ;;                 email))) clist))
                         ;; (when (configuration-layer/package-usedp 'helm)
                         ;;   (defun my-message-expand-name (&optional start)
                         ;;     ((interactive "P"))
                         ;;     ;; (message "my-message-expand-name called")
                         ;;     (helm :prompt "; contact:" :sources
                         ;;           (helm-build-sync-source "mu4e contacts"
                         ;;                                   :candidates mu4e~contact-list :candidate-transformer 'ch:ct))))

                         (defun my-mu4e-compose-hook ()
                           "Setup outgoing messages"
                           ;; Add chopps@<account-sending-from> to CC
                           ;; Add chopps@chopps.org to Bcc if not sending from @chopps.org
                           (let ((buffer-modified (buffer-modified-p)))
                             (save-excursion
                               (message-add-header (concat "Cc: " user-mail-address))
                               (if (not (string= user-mail-address "chopps@chopps.org"))
                                   (message-add-header "Bcc: chopps@chopps.org")
                                 ))
                             (set-buffer-modified-p buffer-modified))

                           ;; This should go elsewhere
                           ;; Actually don't like this.
                           ;; (require 'visual-fill-column)
                           ;; (visual-fill-column-mode)

                           ;; Outgoing mails get format=flowed.
                           (use-hard-newlines t 'guess)
                           (mml-secure-message-sign-pgpmime)
                           )
                         (add-hook 'mu4e-compose-mode-hook 'my-mu4e-compose-hook)

                         ;; Mark to move to spam folder from headers view.
                         (defun mu4e-headers-mark-move-to-spam ()
                           (interactive)
                           (let ((maildir (mu4e-message-field (mu4e-message-at-point) :maildir)))
                             (if (string-match-p (regexp-quote "gmail.com") maildir)
                                 (mu4e-mark-set 'move "/gmail.com/[Gmail]/Spam")
                               (mu4e-mark-set 'move mu4e-spam-folder))
                             (mu4e-headers-next)))

                         ;; Mark to move to spam folder from message view.
                         (defun mu4e-view-mark-move-to-spam ()
                           (interactive)
                           (mu4e~view-in-headers-context
                            (mu4e-headers-mark-move-to-spam)))
                         (debug-init-message "post-init end mu4e eval after load")

                         ;; XXX these aren't defined
                         ;; (define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
                         ;; (define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)

                         ;; removed 20200426
                         ;; (require 'mu4e-contrib)

                         ;; XXX also add this back
                         ;;
                         ;; error in process sentinel: mu4e-alert--parse-mails: End of file during parsing
                         ;; error in process sentinel: End of file during parsing
                         ;; (mu4e-alert-enable-mode-line-display)
                         ;; (mu4e-alert-enable-notifications)

                         ;; removed 20200426
                         ;; ;; ;; XXX disabled trying to find hang XXX THIS CAUSED IT
                         ;; (debug-init-message "debug-init MU4E mode hook")
                         ;; (add-hook 'mu4e-headers-mode-hook
                         ;;   (lambda () (progn
                         ;;                (make-local-variable 'scroll-conservatively)
                         ;;                (setq
                         ;;                  show-trailing-whitespace nil
                         ;;                  scroll-conservatively 0
                         ;;                  ;; XXX These two cause hangs
                         ;;                  ;; scroll-up-aggressively .8
                         ;;                  ;; scroll-down-aggressively .8)
                         ;;                  )
                         ;;                )))
                         ;; (add-hook 'mu4e-view-mode-hook
                         ;;           (lambda () (setq show-trailing-whitespace nil)))
                         ;; ;; Need exit hook from headers mode to do an immediate index update.
                         ;; (add-hook 'mu4e-main-mode-hook (lambda () (mu4e-update-index)))

                         ;; ;; XXX causes hangs
                         ;; (add-hook 'mu4e-headers-mode-hook (lambda () (progn (setq scroll-up-aggressively .8))))

                         (debug-init-message "debug-init MU4E add notify actions")

                         (defun open-message-id-in-new-frame (msgid)
                           (interactive "s")
                           (let ((mailp (persp-get-by-name "@Mu4e"))
                                 (nframe (make-frame)))
                             (select-frame nframe)
                             (and (persp-p mailp) (persp-switch "@Mu4e" nframe))
                             (mu4e-view-message-with-message-id msgid)
                             (delete-other-windows)))

                         (defun message-file-to-sexp (path)
                           "Retrieve a mu4e s-expression for the e-mail message at PATH."
                           (car-safe
                            (read-from-string
                             (shell-command-to-string
                              (format "mu view -o sexp %s"
                                      (shell-quote-argument (expand-file-name path)))))))

                         (defun mu4e-mac-notify-action (id action content)
                           (let* ((msgid (plist-get content :msgid)))
                             (when msgid
                               (open-message-id-in-new-frame msgid))))

                         (defun mu4e-notify-new-message (path)
                           (let* ((msg (message-file-to-sexp path))
                                  (msgid (plist-get msg :message-id))
                                  (subject (plist-get msg :subject))
                                  (title (format "%s" (car (plist-get msg :from)))))
                             (mac-notification-send title subject
                                                    :msgid msgid
                                                    :sound-name "Looking Up"
                                                    :on-action 'mu4e-mac-notify-action)))

                         ;; Update the index only if non-destructive, otherwise try later
                         (setq mu4e-careful-update-timer nil)
                         (defun mu4e-careful-update-index ()
                           (if (and
                                (not (get-buffer-window "*Article*"))
                                (fboundp 'mu4e-update-index))
                               (progn
                                 (if mu4e-careful-update-timer
                                     (cancel-timer mu4e-careful-update-retry))
                                 (message "updating-mail-index")
                                 (mu4e-update-index))
                             (unless mu4e-careful-update-timer
                               (message "setting timer to update mail index")
                               (setq mu4e-careful-update-timer
                                     (run-at-time "10 sec" nil
                                                  (lambda ()
                                                    (setq mu4e-careful-update-timer nil)
                                                    (mu4e-careful-update-index)))))))

                         (debug-init-message "debug-init MU4E mode add to gcal")
                         (defun mu4e-action-add-to-gcal (msg)
                           "Add to a calendar"
                           (interactive)
                           (let* ((calendar "Work")    ; fix this to query user or have default I guess
                                  (count (hash-table-count mu4e~view-attach-map))
                                  (attachnums (mu4e-split-ranges-to-numbers "a" count)))
                             (dolist (num attachnums)
                               (let* ((att (mu4e~view-get-attach msg num))
                                      (name (plist-get att :name))
                                      (index (plist-get att :index)))
                                 (if (or (s-suffix? ".vcs" name) (s-suffix? ".ics" name))
                                     (progn
                                       (message "%s %s%s%s" "mu4e-view-pipe-attachment " num " (concat \"gcalcli import --calendar=\"" calendar)
                                       (mu4e-view-pipe-attachment msg num (concat "gcalcli import --calendar=" calendar)))
                                   (message "No .[iv]cs attachment"))))))

                         ;; (let* ((save-info (mu4e~view-temp-action
                         ;;                    (mu4e-message-field msg :docid) index))
                         ;;        (path (plist-get save-info :path))
                         ;;        (calendar "Work"))
                         ;;   (message (concat "gcalcli import --calendar=" calendar " " path))
                         ;;   (message (shell-command-to-string (concat "gcalcli import --calendar=" calendar " " path)))))))))

                         (add-to-list 'mu4e-view-actions
                                      '("GoogleCalendar" . mu4e-action-add-to-gcal))

                         ;; (let ((calendar "Work"))
                         ;;   (shell-command-to-string (concat "gcalcli import --calendar=" calendar " " file))

                         ;;   (let* ((html (mu4e-message-field msg :body-html))
                         ;;          (txt (mu4e-message-field msg :body-txt))
                         ;;          (tmpfile (format "%s%x.html" temporary-file-directory (random t))))
                         ;;     (unless (or html txt)
                         ;;       (mu4e-error "No body part for this message"))
                         ;;     (with-temp-buffer
                         ;;       ;; simplistic -- but note that it's only an example...
                         ;;       (insert (or html (concat "<pre>" txt "</pre>")))
                         ;;       (write-file tmpfile)
                         ;;       (browse-url (concat "file://" tmpfile)))))))


                         (define-key mu4e-headers-mode-map "d" 'mu4e-headers-mark-for-read)
                         (define-key mu4e-view-mode-map "d" 'mu4e-view-mark-for-read)
                         (define-key mu4e-headers-mode-map "@" 'mu4e-headers-mark-move-to-spam)
                         (define-key mu4e-view-mode-map "@" 'mu4e-view-mark-move-to-spam)

                         ;; XXXerror with auto vimification
                         ;; (define-key mu4e-headers-mode-map "\\" 'mu4e-headers-mark-move-to-spam)
                         ;; (define-key mu4e-view-mode-map "\\" 'mu4e-view-mark-move-to-spam)


                         (debug-init-message "debug-init MU4E leader keys")
                         (spacemacs/set-leader-keys-for-major-mode 'mu4e-view-mode
                           "g" 'mu4e-view-go-to-url
                           "h" 'mu4e-view-toggle-html
                           "j" 'mu4e-view-headers-next
                           "k" 'mu4e-view-headers-prev
                           "K" 'mu4e-view-save-url
                           "n" 'mu4e-view-headers-next
                           "p" 'mu4e-view-headers-prev
                           "v" 'mu4e-view-verify-msg-popup
                           "\\" 'mu4e-view-verify-msg-popup
                           ;; "y" 'mu4e- selejjj
                           "s" 'mu4e-view-search-narrow
                           "e" 'mu4e-view-search-edit
                           ;; "b" 'mu4e-view-bookmark-make-record
                           )

                         ;; ;; XXXSLOW
                         ;; (add-to-list 'mu4e-header-info-custom
                         ;;   '(:list-or-dir .
                         ;;      (:name "ML or maildir" ;; long name, as seen in message view
                         ;;        :shortname "ML-D"     ;; short name, as seen in the headers view
                         ;;        :help "Mailing list or maildir if not set"
                         ;;        :function
                         ;;        (lambda (msg)
                         ;;          (or (mu4e-message-field msg :mailing-list)
                         ;;            (mu4e-message-field msg :maildir))))))

                         (setq
                          ;; "Date         Flgs   List       From                   Subject
                          mu4e-headers-fields (quote (
                                                      (:flags          .  4)
                                                      (:human-date     . 12)
                                                      (:from           . 24)
                                                      (:maildir        . 30)
                                                      ;;(:thread-subject . nil)
                                                      (:subject . nil)
                                                      )))
                         ;; XXXSLOW

                         ;; (require 'mu4e-special)

                         (debug-init-message "debug-init MU4E defun compose")
                         (defun compose-attach-marked-files ()
                           "Compose mail and attach all the marked files from a dired buffer."
                           (interactive)
                           (let ((files (dired-get-marked-files)))
                             (compose-mail nil nil nil t)
                             (dolist (file files)
                               (if (file-regular-p file)
                                   (mml-attach-file file
                                                    (mm-default-file-encoding file)
                                                    nil "attachment")
                                 (message "skipping non-regular file %s" file)))))

                         )
                       )
                     )

    ;; ===========
    ;; Basic Modes
    ;; ===========

    (with-eval-after-load 'make-mode
      (delq (assoc "^\t+#" makefile-font-lock-keywords) makefile-font-lock-keywords))

    (with-eval-after-load "sh-script"
      (modify-syntax-entry ?_ "w" sh-mode-syntax-table))

    ;; How to do this right what is 70 then 401 or 80 then 301?
    ;; (when-layer-used 'rebox
    ;;                  (with-eval-after-load "rebox2"
    ;;                    (rebox-register-template 70 165 '("?"
    ;;                                                      "? box123456"
    ;;                                                      "?"))
    ;;                    (rebox-register-template 401 255 '("#"
    ;;                                                       "# box123456"
    ;;                                                       "# *********"))
    ;;                    (rebox-register-template 402 355 '("# ---------"
    ;;                                                       "# box123456"
    ;;                                                       "# ---------"))
    ;;                    (rebox-register-template 403 455 '("# =========="
    ;;                                                       "# box123456"
    ;;                                                       "# =========="))
    ;;                    (rebox-register-template 80 175 '("//"
    ;;                                                      "// box123456"
    ;;                                                      "//"))
    ;;                    (rebox-register-template 301 265 '("//"
    ;;                                                       "// box123456"
    ;;                                                       "// *********"))
    ;;                    (rebox-register-template 302 365 '("// ---------"
    ;;                                                       "// box123456"
    ;;                                                       "// ---------"))
    ;;                    (rebox-register-template 303 465 '("// ========="
    ;;                                                       "// box123456"
    ;;                                                       "// ========="))
    ;;                    ))

    ;; (spacemacs/set-leader-keys-for-major-mode 'c-mode
    ;;   "q" 'rebox-dwim)
    ;; (spacemacs/set-leader-keys-for-major-mode 'cc-mode
    ;;   "q" 'rebox-dwim)
    ;; (spacemacs/set-leader-keys-for-major-mode 'c++-mode
    ;;   "q" 'rebox-dwim))



    ;; =================
    ;; Programming Modes
    ;; =================

    (when-layer-used 'lsp
                     (with-eval-after-load 'lsp-mode
                       (setq-default
                        lsp-file-watch-ignored (append '("/usr/include" ".*/build-root/.*" "/opt/current/include" ) lsp-file-watch-ignored)
                        lsp-enable-indentation nil
                        lsp-enable-file-watchers t
                        lsp-file-watch-threshold 20000

                        lsp-ui-peek-always-show t
                        lsp-ui-peek-list-width 50
                        lsp-ui-peek-peek-height 40
                        lsp-ui-sideline-show-hover t
                        ;; lsp-ui-sideline-show-code-actions t
                        ;; lsp-ui-sideline-show-diagnostics t

                        lsp-diagnostics-attributes '((unnecessary :background "gray20" :foreground "gray70")
                                                     (deprecated :strike-through t))
                        )

                       (with-eval-after-load 'lsp-ui-peek
                         (define-key lsp-ui-peek-mode-map "j" 'lsp-ui-peek--select-next-file)
                         (define-key lsp-ui-peek-mode-map "k" 'lsp-ui-peek--select-prev-file))
                       ))

    (when-layer-used 'syntax-checking
                     (with-eval-after-load "flycheck"
                       ;; (setq flycheck-highlighting-mode 'lines)
                       (add-hook 'flycheck-mode-hook (lambda () (setq next-error-function nil)))
                       (setq flycheck-highlighting-mode 'sexps)
                       ;; DO NOT USE A "." prefix as it screws up pylint
                       ;; relative import checks!!
                       ;; (setq flycheck-temp-prefix "flycheck")

                       ;; (when-layer-used
                       ;;  'lsp
                       ;;  ;; (progn
                       ;;  ;;   (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))
                       ;;  )

                       ;; the pos-tip window doesn't seem to work with my awesome setup (anymore)
                       (setq flycheck-display-errors-function #'flycheck-display-error-messages)
                       ;; (setq-default flycheck-pylint-use-symbolic-id nil)

                       ;; Chain pylint after flake8 to get benefit of both.
                       ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)
                       ;; (flycheck-add-next-checker 'python-pylint 'python-pycompile)

                       ;;                        (flycheck-define-checker python-pyflakes
                       ;;                          "A Python syntax and style checker using the pyflakes utility.
                       ;; To override the path to the pyflakes executable, set
                       ;; `flycheck-python-pyflakes-executable'.
                       ;; See URL `http://pypi.python.org/pypi/pyflakes'."
                       ;;                          :command ("pyflakes" source-inplace)
                       ;;                          :error-patterns
                       ;;                          ((error line-start (file-name) ":" line ":" (message) line-end))
                       ;;                          :modes python-mode)

                       ;;                        (add-to-list 'flycheck-checkers 'python-pyflakes)

                       ;; (setq flycheck-checkers (delq 'python-pycompile flycheck-checkers))
                       ;; (setq flycheck-checkers (cons 'python-pylint (delq 'python-pylint flycheck-checkers)))

                       (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
                       (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)))

    (when-layer-used 'emacs-lisp
                     (with-eval-after-load "lisp-mode"
                       ;; hyphens are words in emacs lisp
                       (modify-syntax-entry ?- "w" lisp-mode-syntax-table)
                       (modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
                       (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
                       (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
                       (when-layer-used 'rebox
                                        (defun rebox-lisp-hook ()
                                          (set (make-local-variable 'rebox-style-loop) '(81 82 83)))
                                        (add-hook 'lisp-mode-hook 'rebox-lisp-hook)
                                        (add-hook 'emacs-lisp-mode-hook 'rebox-lisp-hook))))

    (when-layer-used 'go
                     (with-eval-after-load "go-mode"
                       (when-layer-used 'rebox
                                        (defun rebox-go-hook ()
                                          (set (make-local-variable 'rebox-style-loop) '(81 82 83)))
                                        (add-hook 'go-mode-hook 'rebox-go-hook))))

    (when-layer-used 'yang
                     (add-hook 'yang-mode-hook (function (lambda ()
                                                           (c-set-style "BSD")
                                                           (setq indent-tabs-mode nil)
                                                           (setq c-basic-offset 2)))))

    ;; (with-eval-after-load "transient"
    ;;   (if (file-exists-p (expand-file-name "~/.transient-values.el"))
    ;;     (setq transient-values-file "~/.transient-values.el")))

    ;; (with-eval-after-load "yang-mode"
    ;;   ;; (autoload 'yang-mode "yang-mode")
    ;;   ;; (add-to-list 'auto-mode-alist '("\\.yang\\'" . yang-mode))
    ;;   (defun my-yang-mode-hook ()
    ;;     "Configuration for YANG Mode. Add this to `yang-mode-hook'."
    ;;     ;; (c-set-style "Procket")
    ;;     (c-set-style "BSD")
    ;;     (setq indent-tabs-mode nil)
    ;;     (setq c-basic-offset 2)
    ;;     ;; (setq font-lock-maximum-decoration t)
    ;;     (font-lock-mode t))
    ;;   (add-hook 'yang-mode-hook 'my-yang-mode-hook))


    (when-layer-used 'yaml
                     (add-hook 'yaml-mode-hook
                               (function (lambda () (flyspell-prog-mode)))))

    (when-layer-used 'c-c++
                     (let ((binpath))
                       ;; version 10 is default in ubuntu 20.04 so don't search
                       ;; any lower
                       (dolist (suffix '("-17" "-16" "-15" "-14" "-13" "-12" "-11" ""))
                         (unless binpath
                           (setq binpath (executable-find (concat "clang-format" suffix)))))
                       (when binpath
                         (setq-default clang-format-executable binpath)))

                     (setq c-font-lock-extra-types
                           (quote
                            ("FILE"
                             "\\sw+_st" "\\sw+_t" "\\sw+type" ; procket types
                             "u\\(8\\|16\\|32\\|64\\)"
                             "i\\(8\\|16\\|32\\|64\\)"
                             "ushort" "uchar"
                             "\\(u_?\\)?int\\(8\\|16\\|32\\|64\\)_t" "ushort" "uchar"
                             "uword" "bool" "boolean")))

                     (with-eval-after-load "cc-mode"
                       ;; (modify-syntax-entry ?_ "w" awk-mode-syntax-table)
                       (modify-syntax-entry ?_ "w" c-mode-syntax-table)
                       (modify-syntax-entry ?_ "w" objc-mode-syntax-table)
                       (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
                       ;; (modify-syntax-entry ?_ "w" java-mode-syntax-table)
                       ;; (modify-syntax-entry ?_ "w" objc-mode-syntax-table)

                       (defun vpp-indent-format-buffer ()
                         "Reformat the buffer using 'indent'"
                         (interactive)
                         (let ((oldbuf (current-buffer))
                               fmtbuf
                               errbuf)
                           (with-temp-buffer
                             (insert-buffer-substring oldbuf)
                             (shell-command-on-region (point-min) (point-max) "indent -gnu" (current-buffer) t nil t)
                             (unless (= 0 (compare-buffer-substrings oldbuf nil nil (current-buffer) nil nil))
                               (setq fmtbuf (current-buffer))
                               (with-temp-buffer
                                 (insert-buffer-substring fmtbuf)
                                 (shell-command-on-region (point-min) (point-max) "indent -gnu" (current-buffer) t nil t)
                                 (if (not (= 0 (compare-buffer-substrings fmtbuf nil nil (current-buffer) nil nil)))
                                     (progn
                                       (message "INDENT FAILED second invocation different from first")
                                       (let ((cbuf (current-buffer))
                                             (tmp1 (make-temp-file "vppfmt1"))
                                             (tmp2 (make-temp-file "vppfmt2")))
                                         (unwind-protect
                                             (progn
                                               (with-temp-file tmp2 (insert-buffer-substring cbuf))
                                               (with-temp-file tmp1 (insert-buffer-substring fmtbuf))
                                               (let ((newbuf (get-buffer-create "*vpp-indent-non-deterministic*")))
                                                 (shell-command (format "diff -u %s %s" tmp1 tmp2) newbuf)
                                                 (if (= 0 (buffer-size newbuf))
                                                     (kill-buffer newbuf)
                                                   (setq errbuf newbuf))))
                                           (delete-file tmp1)
                                           (delete-file tmp2))))
                                   (with-current-buffer oldbuf
                                     (if (boundp 'replace-buffer-contents)
                                         ;; emacs 26 retains properties in buffer
                                         (replace-buffer-contents fmtbuf)
                                       (let ((oldpoint (point)))
                                         (erase-buffer)
                                         (insert-buffer-substring fmtbuf)
                                         (goto-char (min (point-max) oldpoint)))))))))
                           (if errbuf (display-buffer errbuf))))

                       (defun vpp-clang-diff-git-root-directory (file)
                         (with-temp-buffer
                           (when (git-gutter+-insert-git-output '("rev-parse" "--show-toplevel") file)
                             (goto-char (point-min))
                             (let ((root (buffer-substring-no-properties (point) (line-end-position))))
                               (unless (string= root "")
                                 (file-name-as-directory root))))))

                       (defun file-name-git-root-relative-fn (filepath)
                         (let ((base-path
                                (let ((vc-backend (ignore-errors (vc-responsible-backend filepath))))
                                  (when vc-backend
                                    (vc-call-backend vc-backend 'root filepath)))))
                           (if base-path (file-relative-name filepath base-path))))

                       ;; start_line = int(match.group(1))
                       ;; line_count = 1
                       ;; if match.group(3):
                       ;; line_count = int(match.group(3))
                       ;; if line_count == 0:
                       ;; continue
                       ;; end_line = start_line + line_count - 1
                       ;; lines_by_file.setdefault(filename, []).extend(['-lines', str(start_line) + ':' + str(end_line)])

                       (defvar diffu-new-lines-re "^@@.*\\+\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?")

                       (defun clang-format-args-from-diff-buffer (&optional buffer)
                         (let ((use-buf (or buffer (current-buffer)))
                               lines)
                           (with-current-buffer use-buf
                             (save-excursion
                               (goto-char (point-max))
                               (while (re-search-backward diffu-new-lines-re nil 'noerror)
                                 (let ((start (string-to-number (match-string 1)))
                                       (count (string-to-number (or (match-string 3) "1"))))
                                   (unless (= count 0)
                                     (setq lines (cons (format "--lines=%d:%d" start (+ start count -1)) lines)))))))
                           lines))

                       (defun clang-format-vc-diff ()
                         "Reformat only the changed lines from VC in the buffer"
                         (interactive)
                         (let* ((code-buffer (current-buffer))
                                (cursor (clang-format--bufferpos-to-filepos (point) 'exact 'utf-8-unix))
                                ;; vc-working-revision has some bizarre cache bug, it
                                ;; returns the revision that was HEAD the first time it is
                                ;; checked in a buffer. If that buffer is saved, and checked
                                ;; the next call to vc-working-revision is not updated, one
                                ;; must reload the file. So instead invoke the backend directly.
                                (head-revision (vc-call-backend (vc-backend buffer-file-name)
                                                                'working-revision
                                                                buffer-file-name))
                                (temp-buffer (generate-new-buffer " *clang-format-temp*"))
                                (temp-file (make-temp-file "clang-format"))
                                diff-line-args
                                head-temp-file)
                           (unwind-protect
                               (if (not head-revision)
                                   (progn
                                     (message "(clang-format-vc-diff: %s not revision controlled, formatting whole buffer"
                                              buffer-file-name)
                                     (clang-format-buffer))
                                 ;; Get the HEAD revision into a temp file
                                 (let ((head-temp-buf (vc-find-revision buffer-file-name head-revision)))
                                   (setq head-temp-file (buffer-file-name head-temp-buf))
                                   (kill-buffer head-temp-buf))
                                 ;; Get the diff of buffer from the HEAD revision
                                 (let ((status (call-process-region
                                                nil nil (executable-find "diff")
                                                nil `(,temp-buffer ,temp-file) nil
                                                "-U"
                                                "0"
                                                head-temp-file
                                                "-"))
                                       (stderr (with-temp-buffer
                                                 (unless (zerop (cadr (insert-file-contents temp-file)))
                                                   (insert ": "))
                                                 (buffer-substring-no-properties
                                                  (point-min) (line-end-position)))))
                                   (cond
                                    ((stringp status)
                                     (error "(clang-format-vc-diff diff killed by signal %s%s)" status stderr))
                                    ((zerop status)
                                     (message "(clang-format-vc-diff no diff"))
                                    ((not (= 1 status))
                                     (error "(clang-format-vc-diff diff failed with code %d%s)" status stderr))))
                                 (setq diff-line-args (clang-format-args-from-diff-buffer temp-buffer))
                                 ;; empty the temp buffer, and delete the temp file
                                 (with-current-buffer temp-buffer (erase-buffer))
                                 ;; Get xml-replacements into now-empty temp-buffer
                                 (if (not diff-line-args)
                                     (message "(clang-format-vc-diff no differences to format)")
                                   (let ((status (apply #'call-process-region
                                                        nil nil clang-format-executable
                                                        nil `(,temp-buffer ,temp-file) nil
                                                        `("-output-replacements-xml"
                                                          "-assume-filename" ,(file-name-nondirectory buffer-file-name)
                                                          "-fallback-style" ,clang-format-fallback-style
                                                          "-cursor" ,(number-to-string cursor)
                                                          ,@diff-line-args)))
                                         (stderr (with-temp-buffer
                                                   (unless (zerop (cadr (insert-file-contents temp-file)))
                                                     (insert ": "))
                                                   (buffer-substring-no-properties
                                                    (point-min) (line-end-position)))))
                                     (cond
                                      ((stringp status)
                                       (error "(clang-format killed by signal %s%s)" status stderr))
                                      ((not (zerop status))
                                       (error "(clang-format failed with code %d%s)" status stderr)))
                                     (if (= 0 (buffer-size temp-buffer))
                                         (message "(clang-format-vc-diff no formatting changes")
                                       (cl-destructuring-bind (replacements cursor incomplete-format)
                                           (with-current-buffer temp-buffer
                                             (clang-format--extract (car (xml-parse-region))))
                                         (save-excursion
                                           (dolist (rpl replacements)
                                             (apply #'clang-format--replace rpl)))
                                         (when cursor
                                           (goto-char (clang-format--filepos-to-bufferpos cursor 'exact
                                                                                          'utf-8-unix)))
                                         (if incomplete-format
                                             (message "(clang-format: incomplete (syntax errors)%s)" stderr)
                                           (message "(clang-format: success%s)" stderr)))))))
                             (ignore-errors (kill-buffer temp-buffer))
                             (ignore-errors (delete-file temp-file))
                             (ignore-errors (delete-file head-temp-file)))))

                       (defun vpp-clang-diff-format-buffer-old ()
                         "Reformat the buffer using 'clang-format-diff.py'"
                         ;; Get HEAD revision of file
                         ;; Save buffer to tmp file
                         ;; Get diff of those 2 files
                         ;; run clang-format-diff.py
                         ;; copy reformatted file to new file
                         ;; run clang-format-diff.py
                         ;; check diff of new file and saved file
                         (interactive)
                         (let* ((oldbuf (current-buffer))
                                (fname (file-name-nondirectory buffer-file-name))
                                (tmp1name (concat ".fmt1." fname))
                                (tmp2name (concat ".fmt2." fname))
                                binpath hfname fmtbuf errbuf)
                           (unwind-protect
                               (progn
                                 (dolist (suffix '("-11" "-10" "-9" "-8" "-7" ""))
                                   (unless binpath
                                     (setq binpath (executable-find (concat "clang-format" suffix)))))
                                 (ignore-errors
                                   (let ((headbuf (vc-find-revision buffer-file-name (vc-working-revision buffer-file-name))))
                                     (setq hfname (buffer-file-name headbuf))
                                     (kill-buffer headbuf)))
                                 (if (not hfname)
                                     (clang-format-buffer)
                                   (with-temp-file tmp1name (insert-buffer-substring oldbuf))
                                   (with-temp-buffer
                                     ;; Format the buffer content into tmp1name
                                     (shell-command (format "diff -u0 %s %s" hfname tmp1name) (current-buffer))
                                     (shell-command-on-region (point-min) (point-max)
                                                              (concat "clang-format-diff.py -i -binary " binpath))
                                     ;; Format the formatted content into tmp1name
                                     (shell-command (format "cp %s %s" tmp1name tmp2name))
                                     (shell-command (format "diff -u0 %s %s" hfname tmp1name) (current-buffer))
                                     (shell-command-on-region (point-min) (point-max)
                                                              (concat "clang-format-diff.py -i -binary " binpath))
                                     (erase-buffer)
                                     (insert-file-contents tmp1name)
                                     ;; If something changed
                                     (unless (= 0 (compare-buffer-substrings oldbuf nil nil (current-buffer) nil nil))
                                       (setq fmtbuf (current-buffer))
                                       ;; Check for non-deterministic results
                                       (let ((newbuf (get-buffer-create "*vpp-clang-non-deterministic*")))
                                         (shell-command (format "diff -u0 %s %s" tmp1name tmp2name) newbuf)
                                         (if (= 0 (buffer-size newbuf))
                                             (kill-buffer newbuf)
                                           (setq errbuf newbuf)
                                           (error "non-deterministic clang-format results")))
                                       ;; Replace buffer content with formatted results
                                       (with-current-buffer oldbuf
                                         (if (boundp 'replace-buffer-contents)
                                             ;; emacs 26 retains properties in buffer
                                             (replace-buffer-contents fmtbuf)
                                           (let ((oldpoint (point)))
                                             (erase-buffer)
                                             (insert-buffer-substring fmtbuf)
                                             (goto-char (min (point-max) oldpoint))))))))
                                 (ignore-errors (delete-file hfname))
                                 (ignore-errors (delete-file tmp1name))
                                 (ignore-errors (delete-file tmp2name)))
                             (if errbuf (display-buffer errbuf)))))

                       (defun vpp-format-buffer (&optional force-clang)
                         (if (and (eq major-mode 'c++-mode)
                                  (boundp 'clang-format-buffer))
                             (clang-format-vc-diff)
                           (if force-clang
                               (clang-format-vc-diff)
                             (vpp-indent-format-buffer))))
                       ;;(clang-format-buffer)))

                       (setq-default clang-maybe-format-buffer-enabled t)
                       (require 'clang-format)

                       (spacemacs|add-toggle format-buffer
                         :status clang-maybe-format-buffer-enabled
                         :on (setq clang-maybe-format-buffer-enabled t)
                         :off (setq clang-maybe-format-buffer-enabled nil)
                         :documentation "Toggle maybe formatting changes in buffer on save"
                         :evil-leader "T\C-f")

                       (defun clang-maybe-format-buffer ()
                         "Reformat buffer if contains VPP magic or has project root level .clang-format config"
                         (when (and clang-maybe-format-buffer-enabled
                                    (save-excursion
                                      (goto-char (point-min))
                                      (re-search-forward "coding-style-patch-verification: \\(ON\\|INDENT\\|CLANG\\)" nil t)
                                      (cond
                                       ((string= "CLANG" (match-string 1)) (vpp-format-buffer t) t)
                                       ;; ((string= "ON" (match-string 1)) (vpp-format-buffer) t)
                                       ((string= "INDENT" (match-string 1)) (vpp-format-buffer) t)
                                       ;; We need to avoid doing this for files with changes in DEFUN/DEFPY
                                       ((f-exists? (concat (projectile-project-root) ".clang-format")) (clang-format-vc-diff) t)
                                       (t (message "didn't find .clang-format in %s" (concat (projectile-project-root) ".clang-format"))))))))

                       ;;                                       (if (f-exists? (concat (projectile-project-root) ".clang-format"))
                       ;;                                           (progn
                       ;;                                             (message "found .clang-format")
                       ;;                                             (clang-format-vc-diff))
                       ;;                                         (goto-char (point-min))
                       ;;                                         (re-search-forward "coding-style-patch-verification: \\(ON\\|INDENT\\|CLANG\\)" nil t)
                       ;;                                         (cond
                       ;;                                          ((string= "CLANG" (match-string 1)) (vpp-format-buffer t) t)
                       ;;                                          ;; ((string= "ON" (match-string 1)) (vpp-format-buffer) t)
                       ;;                                          ((string= "INDENT" (match-string 1)) (vpp-format-buffer) t)))))))
                       ;;

                       (defun clang-maybe-format-buffer-on-save ()
                         (add-hook 'before-save-hook 'clang-maybe-format-buffer 90 t))

                       ;; (add-hook 'c-mode-common-hook 'vpp-maybe-format-buffer-on-save)
                       (add-hook 'c-mode-hook 'clang-maybe-format-buffer-on-save)
                       (add-hook 'c++-mode-hook 'clang-maybe-format-buffer-on-save)

                       ;; Linux
                       (defun c-lineup-arglist-tabs-only (ignored)
                         "Line up argument lists by tabs, not spaces"
                         (let* ((anchor (c-langelem-pos c-syntactic-element))
                                (column (c-langelem-2nd-pos c-syntactic-element))
                                (offset (- (1+ column) anchor))
                                (steps (floor offset c-basic-offset)))
                           (* (max steps 1)
                              c-basic-offset)))

                       (c-add-style
                        "linux-kernel"
                        '("linux"
                          (c-offsets-alist . ((inextern-lang . 0)))))
                       ;; (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist-tabs-only))))

                       (defun my-c-mode-hook ()
                         ;;(message "my-c-mode-hook")

                         (spacemacs/set-leader-keys-for-major-mode 'c-mode
                           "w" 'wrap-region-with-cond-0)
                         (spacemacs/set-leader-keys-for-major-mode 'c++-mode
                           "w" 'wrap-region-with-cond-0)

                         (c-set-style
                          (cond ((string-match-p "vpp" (buffer-file-name))
                                 "gnu")
                                ((string-match-p "linux" (buffer-file-name))
                                 (setq indent-tabs-mode t)
                                 "linux-kernel")
                                ((string-match-p "frr" (buffer-file-name))
                                 (setq indent-tabs-mode t)
                                 "linux-kernel")
                                ("KNF")))
                         ;; (c-toggle-auto-hungry-state 1)
                         ;; (setq c-electric-flag nil)
                         ;; (setq fill-column 80)
                         (setup-flycheck-c-project-paths)
                         (flyspell-prog-mode))

                       (add-hook 'c-mode-hook 'my-c-mode-hook)
                       (add-hook 'c++-mode-hook 'my-c-mode-hook)

                       (defun check-flycheck-c-project-add-path (path)
                         (when (and path (file-exists-p path))
                           (add-to-list
                            (make-variable-buffer-local 'flycheck-clang-include-path)
                            path)
                           (add-to-list
                            (make-variable-buffer-local 'flycheck-cppcheck-include-path)
                            path)
                           (add-to-list
                            (make-variable-buffer-local 'flycheck-gcc-include-path)
                            path)))

                       (defun setup-flycheck-c-project-paths ()
                         (interactive)
                         (let ((root (ignore-errors (projectile-project-root))))
                           ;; VPP
                           (when (and root (file-exists-p (concat root "src/vppinfra")))
                             (dolist (path '("src/" "src/plugins/"
                                             "build-root/install-vpp-native/external/include/dpdk/"
                                             "build-root/install-vpp-native/vpp/include/"
                                             "build-root/install-vpp_debug-native/external/include/dpdk/"
                                             "build-root/install-vpp_debug-native/vpp/include/"
                                             "../openwrt-dd/staging_dir/target-aarch64_cortex-a53+neon-vfpv4_glibc-2.22/root-mvebu64/usr/include/"
                                             "../openwrt/staging_dir/target-aarch64_cortex-a72_glibc/root-mvebu/usr/include/"))

                               (let ((path1 (concat root path)))
                                 (check-flycheck-c-project-add-path path1))))
                           ;; FRR
                           (when (and root (file-exists-p (concat root "lib/libfrr.h")))
                             (dolist (path '("build/" "build/lib/" "lib/" "include/"))
                               (let ((path1 (concat root path)))
                                 (check-flycheck-c-project-add-path path1))))
                           (dolist (path '("/opt/current/include" "/usr/local/include"))
                             (check-flycheck-c-project-add-path path))))

                       (when-layer-used 'rebox
                                        (defun rebox-c-hook ()
                                          (set (make-local-variable 'rebox-style-loop) '(241 213 215))
                                          (bind-key "M-q" 'rebox-dwim c-mode-map))
                                        (add-hook 'c-mode-hook 'rebox-c-hook)

                                        (with-eval-after-load "rebox2"
                                          ;; XXX why are these saying defined twice?
                                          ;; (rebox-register-template 252 286 '("/* ---------"
                                          ;;                                    " * box123456"
                                          ;;                                    " * ---------*/"))
                                          ;; (rebox-register-template 253 386 '("/* ========="
                                          ;;                                    " * box123456"
                                          ;;                                    " * =========*/"))
                                          ;; (rebox-register-template 254 486 '("/* *********"
                                          ;;                                    " * box123456"
                                          ;;                                    " * **********/"))
                                          ;; (rebox-register-template 281 186 '("//"
                                          ;;                                    "// box123456"
                                          ;;                                    "//"))
                                          ;; (rebox-register-template 282 286 '("// ---------"
                                          ;;                                    "// box123456"
                                          ;;                                    "// ---------"))
                                          ;; (rebox-register-template 283 486 '("// ========="
                                          ;;                                    "// box123456"
                                          ;;                                    "// ========="))
                                          )

                                        (spacemacs/set-leader-keys-for-major-mode 'c-mode
                                          "q" 'rebox-dwim)
                                        (spacemacs/set-leader-keys-for-major-mode 'cc-mode
                                          "q" 'rebox-dwim)
                                        (spacemacs/set-leader-keys-for-major-mode 'c++-mode
                                          "q" 'rebox-dwim))

                       (setq-default c-electric-flag nil)

                       ;; (setq c-mode-local-vars-hook (delete 'spacemacs//c-c++-setup-flycheck c-mode-local-vars-hook))

                       ;; XXX with LSP working now this is not correct
                       ;; (add-hook 'c-mode-local-vars-hook
                       ;;           (function (lambda ()
                       ;;                       (flycheck-select-checker 'c/c++-clang)))
                       ;;           t)

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
                                              (inextern-lang         . 0)
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
                                              (inextern-lang         . 0)
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

    (with-eval-after-load 'gdb-mi
      ;; gdb-default-window-configuration-file
      ;; (defun gdb-setup-windows ()
      ;;   "Lay out the window pattern for option `gdb-many-windows'."
      ;;   (if gdb-default-window-configuration-file
      ;;       (gdb-load-window-configuration
      ;;        (if (file-name-absolute-p gdb-default-window-configuration-file)
      ;;            gdb-default-window-configuration-file
      ;;          (expand-file-name gdb-default-window-configuration-file
      ;;                            gdb-window-configuration-directory)))
      ;;     ;; Create default layout as before.
      ;;     ;; Make sure that local values are updated before locals.
      ;;     (gdb-get-buffer-create 'gdb-locals-values-buffer)
      ;;     (gdb-get-buffer-create 'gdb-locals-buffer)
      ;;     (gdb-get-buffer-create 'gdb-stack-buffer)
      ;;     (gdb-get-buffer-create 'gdb-breakpoints-buffer)
      ;;     (set-window-dedicated-p (selected-window) nil)
      ;;     (switch-to-buffer gud-comint-buffer)
      ;;     (delete-other-windows)
      ;;     (let ((win0 (selected-window))
      ;;           (win1 (split-window nil ( / ( * (window-height) 3) 4)))
      ;;           (win2 (split-window nil ( / (window-height) 3)))
      ;;           (win3 (split-window-right)))
      ;;       (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3)
      ;;       (select-window win2)
      ;;       (set-window-buffer win2 (or (gdb-get-source-buffer)
      ;;                                   (list-buffers-noselect)))
      ;;       (setq gdb-source-window-list (list (selected-window)))
      ;;       (let ((win4 (split-window-right)))
      ;;         (gdb-set-window-buffer
      ;;          (gdb-get-buffer-create 'gdb-inferior-io) nil win4))
      ;;       (select-window win1)
      ;;       (gdb-set-window-buffer (gdb-stack-buffer-name))
      ;;       (let ((win5 (split-window-right)))
      ;;         (gdb-set-window-buffer (if gdb-show-threads-by-default
      ;;                                    (gdb-threads-buffer-name)
      ;;                                  (gdb-breakpoints-buffer-name))
      ;;                                nil win5))
      ;;       (select-window win0))))

      ;; (add-hook gdb-mode-hook)
      ;; (add-hook gdb-locals-mode-{map,hook})
      ;; (add-hook gdb-registers-mode-{map,hook})
      ;; (add-hook gdb-threads-mode-{map,hook})
      ;; (add-hook gdb-breakpoints-mode-{map,hook})
      ;; (add-hook gdb-frames-mode-{map,hook})
      )


    (when-layer-used 'restructuredtext
                     (with-eval-after-load 'rst
                       (setq rst-preferred-adornments
                             '((?# over-and-under 0)
                               (?* over-and-under 0)
                               (?= simple 0)
                               (?- simple 0)
                               (?^ simple 0)
                               (34 simple 0)))

                       ;; (require 'poly-rest-mode)
                       ;; (add-hook 'rst-mode-hook 'poly-rest-mode)

                       ;; (defun my-rst-hook ()
                       ;;   (message "my-rst-hook called")
                       ;;   (semantic-mode -1))

                       ;; (add-hook 'rst-mode-hook 'my-rst-hook)

                       ))

    (with-eval-after-load 'paradox
      (setq paradox-github-token
            (cadr(auth-source-user-and-password "api.github.com" "a-schaefers^paradox"))))

    (when-layer-used 'git
                     (with-eval-after-load 'magit
                       ;; (magit-todos-mode 1)
                       ;; (require 'magit-gerrit)
                       (magit-wip-mode 1)

                       ;; (transient-insert-suffix 'magit-pull "-r" '("-f" "Overwrite local branch" "--force"))

                       (spacemacs/set-leader-keys "gT" 'magit-toggle-buffer-lock)

                       (bind-key (kbd "M-RET") 'magit-diff-visit-worktree-file-other-window 'magit-diff-mode-map)
                       (bind-key (kbd "C-j") 'magit-diff-visit-worktree-file 'magit-diff-mode-map)
                       (magit-wip-mode 1)
                       ;; this doesn't work
                       ;; (defun git/pre-init-transient ()
                       ;;   ;; (transient-insert-suffix 'magit-pull "-r" '("-f" "Overwrite local branch" "--force"))
                       ;;   (setq transient-values-file (expand-file-name "transient/values.el"
                       ;;                                                 dotspacemacs-directory))
                       ;;   )
                       )
                     )

    ;; python
    (when-layer-used
     'rust
     ;; (with-eval-after-load 'rust-mode
     ;;   ;; Consider _ a part of words for python
     ;;   (modify-syntax-entry ?_ "w" rust-mode-syntax-table))
     (with-eval-after-load 'rustic
       ;; Consider _ a part of words for python
       (modify-syntax-entry ?_ "w" rust-mode-syntax-table)
       (when-layer-used 'rebox
                        (defun rebox-rust-hook ()
                          (set (make-local-variable 'rebox-style-loop) '(81 82 83)))
                        (add-hook 'rust-mode-hook 'rebox-rust-hook))))

    ;; python
    (when-layer-used
     'python

     (with-eval-after-load 'python
       (autoload 'pycoverage-mode "pycoverage" "python coverage mode" t)
       (require 'pyfixers)

       (spacemacs/declare-prefix-for-mode 'python-mode "e" "errors-prefix")
       ;; (define-key python-mode-map (kbd "C-c M-\\") 'pyfixer:ignore-current-line)
       ;; SPC m e i[gnore]
       (spacemacs/set-leader-keys-for-major-mode 'python-mode "ei" 'pyfixer:ignore-current-line)
       ;; (define-key python-mode-map (kbd "C-c C-\\") 'pyfixer:fix-current-line)
       ;; SPC m e f[ix]
       (spacemacs/set-leader-keys-for-major-mode 'python-mode "ef" 'pyfixer:fix-current-line)
       ;; (define-key python-mode-map (kbd "C-c C-M-\\") 'pyfixer:fix-all-errors)
       ;; (define-key python-mode-map (kbd "C-c 8") 'pyfixer:fix-all-errors)
       (spacemacs/set-leader-keys-for-major-mode 'python-mode "eF" 'pyfixer:fix-all-errors)
       ;; (bind-key "C-c C-h" 'pylookup-lookup python-mode-map)

       ;; (add-to-list 'python-shell-extra-pythonpaths "/opt/Acton/modules")

       ;; Consider _ a part of words for python
       (modify-syntax-entry ?_ "w" python-mode-syntax-table)


       ;; (if (file-exists-p "/usr/local/bin/python"  )
       ;; (setenv "PYMACS_PYTHON" "/usr/local/bin/python"))

       (defun rst-python-statement-is-docstring (begin)
         "Return true if beginning of statiment is :begin"
         (save-excursion
           (save-match-data
             (python-nav-beginning-of-statement)
             (looking-at-p begin))))

       (defun rst-python-front-verify ()
         (rst-python-statement-is-docstring (match-string 0)))

       ;; XXX26 (require 'mmm-mode)
       ;; (add-to-list 'mmm-save-local-variables 'adaptive-fill-regexp)
       ;; (add-to-list 'mmm-save-local-variables 'fill-paragraph-function)
       ;; (mmm-add-classes
       ;;  '((rst-python-docstrings
       ;;     :submode rst-mode
       ;;     :face mmm-comment-submode-face
       ;;     :front "u?\\(\"\"\"\\|\'\'\'\\)"
       ;;     :front-verify rst-python-front-verify
       ;;     :back "~1"
       ;;     :end-not-begin t
       ;;     ;; :creation-hook (lambda () semantic-mode -1)
       ;;     :save-matches 1
       ;;     ;; :front rst-python-docstrings-find-front
       ;;     ;; :back rst-python-docstrings-find-back
       ;;     :insert ((?d embdocstring nil @ "u\"\"\"" @ _ @ "\"\"\"" @))
       ;;     :delimiter-mode nil)))
       ;; (mmm-add-mode-ext-class 'python-mode nil 'rst-python-docstrings)

       ;; (require 'nadvice)
       (defun my-save-kill-ring (fun &rest _args)
         (let ((kill-ring nil))
           (funcall fun)))
       (advice-add 'yapffy-region :around 'my-save-kill-ring)

       )
     )

    ;; remove when added to spacemacs--indent-variable-alist
    (when-layer-used 'lua
                     (with-eval-after-load 'lua-mode
                       (setq-default lua-indent-level 4)))

    ;; ===
    ;; Org
    ;; ===

    (setq image-transform-scale 2.0)

    (when-layer-used 'org
                     (debug-init-message "debug-init org setup")
                     (with-eval-after-load 'org

                       ;; Consider _s parts of words
                       (modify-syntax-entry ?_ "w" org-mode-syntax-table)

                       (set-register ?W `(file . ,(concat org-directory "/work.org")))
                       (set-register ?N `(file . ,(concat org-directory "/notes.org")))
                       (set-register ?I `(file . ,(concat org-directory "/ietf.org")))
                       (set-register ?S `(file . ,(concat org-directory "/status.org")))
                       (set-register ?P `(file . ,(concat dropbox-directory "/ts-pass.gpg")))

                       (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

                       (setq-default org-use-sub-superscripts '{})
                       (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

                       (setq org-taskjuggler-default-global-properties
                             "shift normal \"Full time shift\" { workinghours mon - fri 09:00 - 17:00 } ")
                       ;; Make duration values make a lot more sense
                       (setq org-duration-units
                             `(("min" . 1)
                               ("h" . 60)
                               ("d" . ,(* 60 8))
                               ("w" . ,(* 60 8 5))
                               ("m" . ,(* 60 8 5 4.2))
                               ("m" . ,(* 60 8 5 4.2 12)))
                             org-duration-format '(("h") (special . 2))
                             )
                       (org-duration-set-regexps)

                       ;; bad load
                       ;;                        (require 'ox-latex)
                       )


                     (debug-init-message "debug-init org my-org-mode-hook")

                     (defun my-org-mode-hook ()
                       (yas-minor-mode 1)
                       (electric-indent-mode 0)
                       (setq org-adapt-indentation 'headline-data))

                     (add-hook 'org-mode-hook 'my-org-mode-hook)

                     ;; (require 'journal-lisp)

                     (debug-init-message "debug-init org-id")
                     (debug-init-message "debug-init org clock stuff")

                     ;;
                     ;; Global org mode clock in/out keys
                     ;;
                     (defun my/start-heading-clock (heading)
                       "Start clock programmatically for heading with ID in FILE."
                       ;; (if-let (marker (org-find-exact-heading-in-directory heading org-directory))
                       (if-let (marker (org-find-exact-heading-in-directory heading labn-365-dir))
                           (save-current-buffer
                             (save-excursion
                               (set-buffer (marker-buffer marker))
                               (goto-char (marker-position marker))
                               (org-clock-in)
                               (save-buffer)
                               ))
                         (warn "Clock not started (Could not find heading '%s' in '%s')" heading labn-365-dir)))

                     (defun my/stop-clock-save-file-with-heading (heading)
                       "Stop clock and save file"
                       (if-let (marker (org-find-exact-heading-in-directory heading labn-365-dir))
                           (save-current-buffer
                             (save-excursion
                               (set-buffer (marker-buffer marker))
                               (org-clock-out)
                               (save-buffer)
                               ))
                         (warn "Clock not started (Could not find heading '%s' in '%s')" heading labn-365-dir))
                       )
                     (defun clock-in-busdev () "Clock-IN BusDev" (interactive) (my/start-heading-clock "BusDev"))
                     (defun clock-in-tfs () "Clock-IN TFS" (interactive) (my/start-heading-clock "TFS DO7.012"))
                     (defun clock-in-caas () "Clock-IN CAS" (interactive) (my/start-heading-clock "CAAS DO7.016"))
                     (defun clock-out-save () "Clock-Out Save" (interactive) (my/stop-clock-save-file-with-heading "TFS DO7.012"))
                     (spacemacs/set-leader-keys "oib" 'clock-in-busdev)
                     (spacemacs/set-leader-keys "oic" 'clock-in-caas)
                     (spacemacs/set-leader-keys "oim" 'clock-in-tfs)
                     (spacemacs/set-leader-keys "oit" 'clock-in-tfs)
                     (spacemacs/set-leader-keys "oo" 'clock-out-save)

                     ;; (defun my-org-confirm-babel-evaluate (lang body)
                     ;;   (not (or (string= lang "ditaa")
                     ;;            (string= lang "dot2tex")
                     ;;            (string= lang "dot")
                     ;;            (string= lang "gnuplot")
                     ;;            (string= lang "plantuml")
                     ;;            ;; (string= lang "yang")
                     ;;            )))

                     (debug-init-message "debug-init org setq")

                     (if (file-accessible-directory-p "~/Dropbox/org-mode")
                         (progn
                           (setq-default org-directory "~/Dropbox/org-mode/")
                           (setq org-agenda-files '("~/Dropbox/org-mode/")))
                       (setq-default org-directory "~/org/")
                       (setq org-agenda-files '("~/org/")))

                     (setq
                      ;; Crypt
                      org-tags-exclude-from-inheritance '("crypt")
                      org-crypt-disable-auto-save t
                      org-crypt-key "D7B83025"

                      ;; Display
                      org-display-inline-images t

                      ;; General

                      org-agenda-custom-commands '(("c" . "Custom searches") ("cc" "Closed in the last week" tags "CLOSED>=\"<-1w>\""))
                      org-agenda-start-on-weekday 1
                      org-clock-persist t
                      org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate
                      org-deadline-warning-days 5
                      org-default-notes-file (concat org-directory "/notes.org")
                      org-export-with-sub-superscripts nil
                      org-hide-leading-stars t
                      org-html-doctype "html5"
                      org-log-done 'time
                      org-outline-path-complete-in-steps t
                      org-plantuml-jar-path "/opt/plantuml/plantuml.jar"
                      org-refile-use-outline-path 'file
                      org-src-fontify-natively t
                      org-src-tab-acts-natively t
                      org-src-window-setup 'current-window
                      org-use-sub-superscripts nil

                      ;; Exports
                      org-export-latex-emphasis-alist (quote (("*" "\\textbf{%s}" nil)
                                                              ("/" "\\emph{%s}" nil)
                                                              ("_" "\\underline{%s}" nil)
                                                              ("+" "\\texttt{%s}" nil)
                                                              ("=" "\\verb=%s=" nil)
                                                              ("~" "\\verb~%s~" t)
                                                              ("@" "\\alert{%s}" nil)))

                      org-latex-listings 'minted
                      ;; org-latex-packages-alist '(("" "graphicx" t)
                      ;;                            ("" "longtable" nil)
                      ;;                            ("" "minted" nil)
                      ;;                            ("" "float" nil))

                      ;; we do this above
                      ;; org-latex-packages-alist '(("" "minted" nil))
                      org-latex-create-formula-image-program 'imagemagick

                      org-latex-pdf-process
                      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")

                      ;; capture the search instead of the highlighted message in
                      ;; headers view
                      org-mu4e-link-query-in-headers-mode nil


                      org-capture-templates
                      `(
                        ("t" "Todo" entry (file+headline ,(concat org-directory "/notes.org") "Tasks")
                         "* TODO %^{Title}%?\nDEADLINE: %^t CREATED: %u\nAnnotation: %a\n\n")

                        ("m" "Mail Todo" entry (file+headline ,(concat org-directory "/notes.org") "Mail")
                         "* TODO [Mail] %^{Title|%:subject}%? ([%:from])\nDEADLINE: %^t CREATED: %u\nMessage: %a\n\n")

                        ("M" "Memory" entry (file+headline ,(concat org-directory "/notes.org") "Memories")
                         "* NOTE %?\nCREATED: %u\n\n")

                        ("c" "Code Todo" entry (file+headline ,(concat org-directory "/notes.org") "Code Todo")
                         "* TODO [Code] %^{Title}\nDEADLINE: %^t\nCREATED: %u\nAnnotation: %a\n%?\n\n")

                        ;; ("j" "Journal Note" entry (file ,(get-journal-file-today))
                        ;;  "* Event: %?\n\n  %i\n\n  From: %a" :empty-lines 1)

                        ("n" "Generic Note" entry (file+headline ,(concat org-directory "/notes.org") "Notes")
                         "* NOTE %?\n%u\nannotation:%a\nx:%x\n")

                        ("L" "Mac Link Note" entry (file+headline ,(concat org-directory "/notes.org") "Notes")
                         "* NOTE %?\n%u\n%(org-mac-safari-get-frontmost-url)\n")

                        ("s" "Status" item (file+olp+datetree ,(concat org-directory "/work.org") "Working" "Status Items")
                         "- %^{Status Item} %u\n" :tree-type week :immediate-finish t)

                        ;; ("s" "Status" entry (file+weektree ,(concat org-directory "/status.org"))
                        ;;  "* NOTE %?\n%u\n")

                        ("x" "Food/Medication")
                        ("xa" "Albuteral 180mcg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE Albuteral 180mcg\nCreated: %U" :immediate-finish t)
                        ("xA" "Sum" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE Sum\nCreated: %U" :immediate-finish t)
                        ("xb" "Vitamin B12" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 2500mcg Vitamin B12\nCreated: %U" :immediate-finish t)
                        ("xc" "Compazine 2.5mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 2.5mg Compazine\nCreated: %U" :immediate-finish t)
                        ("xC" "Compazine 5mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 5mg Compazine\nCreated: %U" :immediate-finish t)
                        ("xd" "Vitamin D 50k" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 50ku Vitamin D\nCreated: %U" :immediate-finish t)
                        ("xD" "Vitamin D 50k" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 50ku Vitamin D\nCreated: %U" :immediate-finish t)
                        ("xg" "Glimepiride 4mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 4mg Glimepiride\nCreated: %U" :immediate-finish t)
                        ("xi" "Invokana 300mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 300mg Invokana\nCreated: %U" :immediate-finish t)
                        ("xm" "Metformin 1500mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 1500mg Metformin\nCreated: %U" :immediate-finish t)
                        ("xM" "Morning Pills" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 300mg Invokana,4mg Glimepiride,40mg Protonix\nCreated: %U" :immediate-finish t)
                        ("xn" "Nicoderm" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE Nicoderm\nCreated: %U" :immediate-finish t)
                        ("xo" "Oxycodone 2.5mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 2.5mg Oxycodone\nCreated: %U" :immediate-finish t)
                        ("xp" "Protonix 40mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 40mg Protonix\nCreated: %U" :immediate-finish t)
                        ("xs" "Sucralfate 500mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 500mg Sucralfate\nCreated: %U" :immediate-finish t)
                        ("xt" "Tylenol dose 325" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 325mg Tylenol\nCreated: %U" :immediate-finish t)
                        ("xT" "Tylenol dose 650" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 650mg Tylenol\nCreated: %U" :immediate-finish t)
                        ("x1" "Tramadol 100mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 100mg 2x50 Tramadol\nCreated: %U" :immediate-finish t)
                        ("x/" "Tramadol 75mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 75mg 1.5x50 Tramadol\nCreated: %U" :immediate-finish t)
                        ("x." "Tramadol 50mg" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
                         "* NOTE 50mg Tramadol\nCreated: %U" :immediate-finish t)

                        ("g" "Google Calendars")
                        ("gh" "Todo" entry (file ,(concat org-directory "/calendar/goog-home.org"))
                         "* TODO %?\n%T\nAnnotation: %a\n")

                        ("gf" "Todo" entry (file ,(concat org-directory "/calendar/goog-family.org"))
                         "* TODO %?\n%T\nAnnotation: %a\n")

                        ("gw" "Todo" entry (file ,(concat org-directory "/calendar/goog-work.org"))
                         "* TODO %?\n%T\nAnnotation: %a\n")


                        ("i" "IETF related")
           ;;; XXX these are exact copies of the generic ones different file.
                        ("it" "Todo" entry (file+headline ,(concat org-directory "/ietf.org") "Tasks")
                         "* TODO %?\nCreated: %t\nAnnotation: %a\n")

                        ("in" "Generic Note" entry (file+headline ,(concat org-directory "/ietf.org") "Notes")
                         "* NOTE %?\n%u\nannotation:%a\nx:%x\n")

                        ("w" "Work related")
           ;;; XXX these are exact copies of the generic ones different file.
                        ("wt" "Todo" entry (file+headline ,(concat org-directory "/work.org") "Tasks")
                         "* TODO %?\nCreated: %t\nAnnotation: %a\n")

                        ("wc" "Code Todo" entry (file+headline ,(concat org-directory "/work.org") "Code Todo")
                         "* Code TODO %?\nCreated: %t\nAnnotation: %a\n")

                        ("wn" "Generic Note" entry (file+headline ,(concat org-directory "/work.org") "Notes")
                         "* NOTE %?\n%u\nannotation:%a\nx:%x\n")
                        )
                      )

                     ;; XXX bad load
                     (defun org-update-inline-images ()
                       (when org-inline-image-overlays
                         (org-redisplay-inline-images)))
                     (add-hook 'org-babel-after-execute-hook 'org-update-inline-images)
                     ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;; Refile Or Delete Me, from my grpahiv file.
                     ;; XXX bad load
                     (defun _graphviz/post-init-org ()
                       (with-eval-after-load 'org
                         (add-to-list 'org-src-lang-modes  '("dot" . graphviz-dot))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                     ;; XXX bad load
                     (defun org-dblock-write:rangereport (params)
                       "Display day-by-day time reports."
                       (let* ((ts (plist-get params :tstart))
                              (te (plist-get params :tend))
                              (start (time-to-seconds
                                      (apply 'encode-time (org-parse-time-string ts))))
                              (end (time-to-seconds
                                    (apply 'encode-time (org-parse-time-string te))))
                              day-numbers)
                         (setq params (plist-put params :tstart nil))
                         (setq params (plist-put params :end nil))
                         (while (<= start end)
                           (save-excursion
                             (insert "\n\n"
                                     (format-time-string (car org-time-stamp-formats)
                                                         (seconds-to-time start))
                                     "----------------\n")
                             (org-dblock-write:clocktable
                              (plist-put
                               (plist-put
                                params
                                :tstart
                                (format-time-string (car org-time-stamp-formats)
                                                    (seconds-to-time start)))
                               :tend
                               (format-time-string (car org-time-stamp-formats)
                                                   (seconds-to-time end))))
                             (setq start (+ 86400 start))))))

                     ;; XXX bad load
                     (debug-init-message "debug-init org-babel-do-load-languges setup")
                     (setq org-babel-load-languages
                           '((emacs-lisp . t)
                             (C . t)
                             (calc . t)
                             ;;
                             (dot . t)
                             ;; gnuplot fails with recursive redefine org stuff
                             ;; (gnuplot . t)

                             (ditaa . t)
                             ;; XXX latex fails with void variable
                             ;; (latex . t)
                             (pic . t)
                             (plantuml . t)
                             ;; This loads python mode when org mode loads I dont want that.
                             ;; spacemacs is supposed to add this when python mode loads
                             ;; but does this work if all we have is python babel blocks
                             ;; b/c why would it know to load python-mode then?
                             ;; (python . t)
                             (shell . t)
                             ;; Why did this used to work??
                             ;; (yang . t)
                             ;; (sh . t)
                             )
                           )
                     ;;

                     ;; XXX bad load
                     ;;                     (debug-init-message "debug-init ox setup")

                     ;;                     (with-eval-after-load "ox"
                     ;;                       ;;
                     ;;                       (defconst chopps/org-latex-packages-alist-pre-hyperref
                     ;;                         '(
                     ;;                           ;; This conflicts with Beamer
                     ;;                           ;; ("letterpaper,margin=1.0in" "geometry")
                     ;;
                     ;;                           ;; Prevent an image from floating to a different location.
                     ;;                           ;; http://tex.stackexchange.com/a/8633/52678
                     ;;                           ("" "float")
                     ;;                           ;; % 0 paragraph indent, adds vertical space between paragraphs
                     ;;                           ;; http://en.wikibooks.org/wiki/LaTeX/Paragraph_Formatting
                     ;;                           ("" "parskip"))
                     ;;                         "Alist of packages that have to be loaded before `hyperref'package is loaded ftp://ftp.ctan.org/tex-archive/macros/latex/contrib/hyperref/README.pdf "))



                     ;; XXX bad load
                     ;;                       (defconst chopps/org-latex-packages-alist-post-hyperref
                     ;;                         '(
                     ;;                           ;; Prevent tables/figures from one section to float into another section
                     ;;                           ;; http://tex.stackexchange.com/a/282/52678
                     ;;                           ("section" "placeins")
                     ;;                           ;; Graphics package for more complicated figures
                     ;;                           ("" "tikz")
                     ;;                           ("" "caption")
                     ;;                           ("" "adjustbox")
                     ;;                           ;;
                     ;;                           ;; Packages suggested to be added for previewing latex fragments
                     ;;                           ;; http://orgmode.org/worg/org-tutorials/org-latex-preview.html
                     ;;                           ("mathscr" "eucal")
                     ;;                           ("" "latexsym"))
                     ;;                         "Alist of packages that have to (or can be) loaded after `hyperref' package is loaded.")
                     ;;
                     ;;                       (defvar latex-minted-cachedir (concat temporary-file-directory
                     ;;                                                             (getenv "USER")
                     ;;                                                             "/.minted/\\jobname"))
                     ;;
                     ;;                       (add-to-list 'chopps/org-latex-packages-alist-post-hyperref
                     ;;                                    `(,(concat "cachedir=" ; options
                     ;;                                               latex-minted-cachedir)
                     ;;                                      "minted" ; package
                     ;;                                      ;; If `org-latex-create-formula-image-program'
                     ;;                                      ;; is set to `dvipng', minted package cannot be
                     ;;                                      ;; used to show latex previews.
                     ;;                                      ,(not (eq org-latex-create-formula-image-program 'dvipng)))) ; snippet-flag
                     ;;
                     ;;                       ;; remove hyperref from default package list.
                     ;;                       (setq org-latex-default-packages-alist
                     ;;                             (delq (rassoc '("hyperref" nil) org-latex-default-packages-alist)
                     ;;                                   org-latex-default-packages-alist))
                     ;;
                     ;;                       ;; This is no longer needed
                     ;;                       (setq org-latex-default-packages-alist
                     ;;                             (delq (rassoc '("fixltx2e" nil) org-latex-default-packages-alist)
                     ;;                                   org-latex-default-packages-alist))
                     ;;
                     ;;                       ;; Setup package list with correct placement of hyperref
                     ;;                       (setq org-latex-packages-alist
                     ;;                             (append chopps/org-latex-packages-alist-pre-hyperref
                     ;;                                     '(("" "hyperref" nil))
                     ;;                                     chopps/org-latex-packages-alist-post-hyperref))
                     ;;                       )
                     ;;


                     ;; XXX bad load
                     ;;                     (debug-init-message "debug-init org-refile-to-datatree setup")
                     ;;                     (defun org-refile-to-datetree ()
                     ;;                       "Refile a subtree to a datetree corresponding to it's timestamp."
                     ;;                       (interactive)
                     ;;                       (let* ((datetree-date (org-entry-get nil "TIMESTAMP" t))
                     ;;                              (date (org-date-to-gregorian datetree-date)))
                     ;;                         (when date
                     ;;                           (save-excursion
                     ;;                             (org-cut-subtree)
                     ;;                             (org-datetree-find-date-create date)
                     ;;                             (org-narrow-to-subtree)
                     ;;                             (show-subtree)
                     ;;                             (org-end-of-subtree t)
                     ;;                             (newline)
                     ;;                             (goto-char (point-max))
                     ;;                             (org-paste-subtree 4)
                     ;;                             (widen)
                     ;;                             )
                     ;;                           )
                     ;;                         ))
                     ;;

                     ;; XXX bad load
                     ;;                     (debug-init-message "debug-init pre-org2blog")
                     ;;                     (when-layer-used 'org2blog
                     ;;                                      (with-eval-after-load "org2blog"
                     ;;                                        (setq org2blog/wp-use-sourcecode-shortcode t)
                     ;;                                        )
                     ;;                                      )

                     )


    ;; ====
    ;; TMUX
    ;; ====

    (debug-init-message "debug-init tmux")

    (defun sigusr1-handler ()
      (interactive)
      (dolist (line (split-string (shell-command-to-string "tmux show-environment") "\n" t))
        (if (string/starts-with line "-")
            (progn
              (setq line (seq-subseq line 1))
              (setenv line nil))
          (let ((tup (split-string line "=" t)))
            (setenv (car tup) (cadr tup))
            ))))

    (if (getenv "TMUX")
        (define-key special-event-map [sigusr1] 'sigusr1-handler))

    ;; ====
    ;; Evil
    ;; ====

    (debug-init-message "debug-init evil")
    (fold-section "evil"
                  (setq-default evil-shift-width 4)
                  (setq evil-shift-round nil
                        evil-search-wrap nil
                        evil-want-C-i-jump nil
                        evil-disable-insert-state-bindings t)
                  (when (display-graphic-p)
                    (setq evil-esc-delay 0))
                  ;; Configure some modes to start in different modes.
                  (evil-set-initial-state 'artist-mode 'emacs)
                  (evil-set-initial-state 'mu4e-compose-mode 'insert)
                  ;; Have to use this to avoid rebox taking it over.
                  ;; (global-set-key (kbd "C-y") 'yank-from-ssh)
                  ;; (evil-global-set-key 'insert (kbd "C-y") 'yank-from-ssh)
                  ;; (evil-global-set-key 'emacs (kbd "C-y") 'yank-from-ssh)
                  ;; (evil-global-set-key 'replace (kbd "C-y") 'yank-from-ssh)
                  )

    ;; XXX layouts debug why are layouts so messed up?
    ;; (defun persp-add-buffers-to-all ()
    ;;   (progn
    ;;     (message "Adding buffers")
    ;;     (persp-add-buffer "*Messages*")
    ;;     (persp-add-buffer "*scratch*")))
    ;; (add-hook 'persp-activated-hook 'persp-add-buffers-to-all)


    (add-hook 'org-mode-hook 'evil-normalize-keymaps)

    ;; Use ggtags not generic evil-jump-to-tag, would be nice to simply undefine
    ;; Map it everywhere else as well.

    (define-key evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim)

    ;; ---------------------
    ;; Auto insert templates
    ;; ---------------------

    (fold-section "Auto Insert Templates"
                  (defun new-file-header-date ()
                    (concat (format-time-string "%B" (current-time))
                            " "
                            (trim-string (format-time-string " %e" (current-time)))
                            (format-time-string " %Y" (current-time))))

                  ;; "/home/chopps/w/foobar/baz.py"

                  (setq work-ai-prefix "chopps/w\\(-[^/]+\\)\?/.*")

                  (with-eval-after-load 'autoinsert

                    ;; (define-auto-insert
                    ;;   '("\\.org\\'" . "Home Org mode skeleton")
                    ;;   '("Short description: "
                    ;;     "#+TITLE: " _ \n
                    ;;     > "#+AUTHOR: Christian E. Hopps" \n
                    ;;     > "#+EMAIL: chopps@gmail.com" \n
                    ;;     > "#+STARTUP: indent" \n
                    ;;     > "" \n
                    ;;     ))
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
                        "# -*- coding: utf-8 eval: (blacken-mode 1) -*-" \n
                        > "#" \n
                        > "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "#" \n
                        > "# Copyright (c) " (substring (current-time-string) -4) " by Christian E. Hopps." \n

                        > "# All rights reserved." \n
                        > "#" \n
                        > "" \n
                        > _ ))
                    (define-auto-insert
                      '("\\.sh\'" . "# Home shell comment skeleton")
                      '("Short description: "
                        "#!/bin/bash" \n
                        "#" \n
                        "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        "#" \n
                        "# Copyright (c) " (substring (current-time-string) -4) " by Christian E. Hopps." \n
                        "#" \n
                        "" \n
                        _ ))
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
                      '("\\.\\(go\\|rs\\)\\'" . "Home Go skeleton")
                      '("Short description: "
                        "//" \n
                        > "// -*- coding: utf-8 -*-" \n
                        > "//" \n
                        > "// " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "//" \n
                        > "//" \n
                        > "package " _))
                    (define-auto-insert
                      '("\\.\\(h\\|c\\|CC?\\|cc\\|cxx\\|cpp\\|c++\\|m\\)\\'" . "Home C-style skeleton")
                      '("Short description: "
                        "/*" \n
                        > " * -*- coding: utf-8 -*-"
                        > "*" \n
                        > "* " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@gmail.com>" \n
                        > "*" \n
                        > "*/" \n
                        > _ ))


                    ;; ;;-----------+
                    ;; ;;      Work
                    ;; ;;-----------+
                    (setq labn-copyright-name "LabN Consulting, L.L.C.")

                    ;; (define-auto-insert
                    ;;   (cons (concat work-ai-prefix "\\.org\\'") "Work org mode skeleton")
                    ;;   '("Short description: "
                    ;;     "#+TITLE: " _ \n
                    ;;     > "#+AUTHOR: Christian E. Hopps" \n
                    ;;     > "#+EMAIL: chopps@gmail.com" \n
                    ;;     > "#+STARTUP: indent" \n
                    ;;     > "" \n
                    ;;     ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.el\\'") "Work Lisp comment skeleton")
                      '("Short description: "
                        ";;" \n
                        > ";; " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@labn.net>" \n
                        > ";;" \n
                        > ";; Copyright (c) " (substring (current-time-string) -4) ", " labn-copyright-name \n
                        > ";; All rights reserved." \n
                        > ";;" \n
                        > _
                        ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.py\\'") "# Work python comment skeleton")
                      '("Short description: "
                        "# -*- coding: utf-8 eval: (blacken-mode 1) -*-" \n
                        > "#" \n
                        > "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@labn.net>" \n
                        > "#" \n
                        > "# Copyright (c) " (substring (current-time-string) -4) ", " labn-copyright-name \n
                        > "#" \n
                        > _ \n
                        ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.sh\\'") "# Work comment skeleton")
                      '("Short description: "
                        "#!/bin/bash" \n
                        > "#" \n
                        > "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@labn.net>" \n
                        > "#" \n
                        > "# Copyright (c) " (substring (current-time-string) -4) ", " labn-copyright-name \n
                        > "#" \n
                        > _ ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.\\(pl\\|tcl\\)\\'") "# Work comment skeleton")
                      '("Short description: "
                        "#" \n
                        > "# " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@labn.net>" \n
                        > "#" \n
                        > "# Copyright (c) " (substring (current-time-string) -4) ", " labn-copyright-name \n
                        > "#" \n
                        > _ ))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.rst\\'") "Work ReST skeleton")
                      '("Short description: "
                        ".. -*- coding: utf-8 -*-" \n
                        > ".." \n
                        > ".. " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@labn.net>" \n
                        > ".." \n
                        > ".. Copyright (c) " (substring (current-time-string) -4) ", " labn-copyright-name \n
                        > ".." \n
                        > _ ))
                    (define-auto-insert
                      '("\\.\\(go\\|rs\\)\\'" . "Work Go/Rust skeleton")
                      '("Short description: "
                        "// -*- coding: utf-8 -*-" \n
                        > "//" \n
                        > "// " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@labn.net>" \n
                        > "//" \n
                        > "// Copyright (c) " (substring (current-time-string) -4) ", " labn-copyright-name \n
                        > "//" \n
                        > "/// Module implementing " _))
                    (define-auto-insert
                      (cons (concat work-ai-prefix "\\.\\(h\\|c\\|CC?\\|cc\\|cxx\\|cpp\\|c++\\|m\\)\\'") "Work C-style skeleton")
                      '("Short description: "
                        "/*" \n
                        > "* " (new-file-header-date) ", " (user-full-name) " <" (user-login-name) "@labn.net>" \n
                        > "*" \n
                        > "* Copyright (c) " (substring (current-time-string) -4) ", " labn-copyright-name \n
                        > "*" \n
                        > "*/" \n
                        > _ ))
                    ))

    ;; (if inhibit-startup-screen
    ;;     (quit-window))

    ;; (setq powerline-default-separator 'wave)
    ;; (setq powerline-default-separator 'utf-8)

    ;; (defun split-window-sensibly-prefer-horizontal (&optional window)
    ;;   "Same as `split-window-sensibly' except prefer to split horizontally first."
    ;;   (let ((window (or window (selected-window))))
    ;;     (or (and (window-splittable-p window t)
    ;;              ;; Split window horizontally.
    ;;              (with-selected-window window
    ;;                (split-window-right)))
    ;;         (and (window-splittable-p window)
    ;;              ;; Split window vertically.
    ;;              (with-selected-window window
    ;;                (split-window-below)))
    ;;         (and (eq window (frame-root-window (window-frame window)))
    ;;              (not (window-minibuffer-p window))
    ;;              ;; If WINDOW is the only window on its frame and is not the
    ;;              ;; minibuffer window, try to split it vertically disregarding
    ;;              ;; the value of `split-height-threshold'.
    ;;              (let ((split-height-threshold 0))
    ;;                (when (window-splittable-p window)
    ;;                  (with-selected-window window
    ;;                    (split-window-below))))))))

    (debug-init-message "list-timers")

    (require 'list-timers)
    (evil-set-initial-state 'timers-menu-mode 'insert)

    ;; left-arrow, right-arrow
    ;; Used to indicate truncated lines.

    ;; left-curly-arrow, right-curly-arrow
    ;; Used to indicate continued lines.

    ;; right-triangle, left-triangle
    ;; The former is used by overlay arrows. The latter is unused.

    ;; up-arrow, down-arrow, top-left-angle top-right-angle
    ;; bottom-left-angle, bottom-right-angle
    ;; top-right-angle, top-left-angle
    ;; left-bracket, right-bracket, top-right-angle, top-left-angle
    ;; Used to indicate buffer boundaries.

    ;; filled-rectangle, hollow-rectangle
    ;; filled-square, hollow-square
    ;; vertical-bar, horizontal-bar
    ;; Used for different types of fringe cursors.

    ;; UTF-8 left-arrow, right-arrow (for truncated lines), left-curly-arrow, and right-curly-arrow
    ;; → ←  ↩ ↪ ⬅ ↺ ↻ ↷ ⟲ ⟳ ⤵⤴ ⤷ ⤶
    ;; custom graphics that works nice with half-width fringes


    (with-eval-after-load 'git-gutter+-disabled-foo
      (debug-init-message "gutter")

      ;; .....................
      ;; ..............##.....
      ;; ..............###....
      ;; ...............###...
      ;; ................###..
      ;; ####################.
      ;; #####################
      ;; #####################
      ;; ................####.
      ;; ...............####..
      ;; ..............####...
      ;; ..............###....
      ;; ...............#.....

      ;; .....................
      ;; .....##..............
      ;; ....###..............
      ;; ...###...............
      ;; ..###................
      ;; .####################
      ;; #####################
      ;; #####################
      ;; .####................
      ;; ..####...............
      ;; ...####..............
      ;; ....###..............
      ;; .....#...............




      (fringe-helper-define 'left-curly-arrow nil
                            "........................."
                            "........................."
                            ".................#......."
                            ".................####...."
                            ".................#####..."
                            "..................####..."
                            ".......##..........####.."
                            "......###...........###.."
                            ".....###...........####.."
                            "....###............###..."
                            "...###################..."
                            "..###################...."
                            "..##################....."
                            "...####.................."
                            "....####................."
                            ".....####................"
                            "......###................"
                            ".......#................."
                            "........................."
                            "........................."
                            )

      (fringe-helper-define 'right-curly-arrow nil
                            "........................."
                            ".......#................."
                            "....####................."
                            "...#####................."
                            "...####.................."
                            "..####..........##......."
                            "..###...........###......"
                            "..###............###....."
                            "...###............###...."
                            "...###################..."
                            "....###################.."
                            ".....##################.."
                            "..................####..."
                            ".................####...."
                            "................####....."
                            "................###......"
                            ".................#......."
                            "........................."
                            )




      (fringe-helper-define 'git-gutter-fr:added nil
                            ".......XXXXX......."
                            ".......XXXXX......."
                            ".......XXXXX......."
                            ".......XXXXX......."
                            ".......XXXXX......."
                            ".......XXXXX......."
                            ".......XXXXX......."
                            "XXXXXXXXXXXXXXXXXXX"
                            "XXXXXXXXXXXXXXXXXXX"
                            "XXXXXXXXXXXXXXXXXXX"
                            "XXXXXXXXXXXXXXXXXXX"
                            "XXXXXXXXXXXXXXXXXXX"
                            ".......XXXXX......."
                            ".......XXXXX......."
                            ".......XXXXX......."
                            ".......XXXXX......."
                            ".......XXXXX......."
                            ".......XXXXX......."
                            ".......XXXXX......."


                            )
      (fringe-helper-define 'git-gutter-fr:deleted nil
                            "......................"
                            "......................"
                            "......................"
                            "......................"
                            "......................"
                            "......................"
                            "XXXXXXXXXXXXXXXXXXXXXX"
                            "XXXXXXXXXXXXXXXXXXXXXX"
                            "XXXXXXXXXXXXXXXXXXXXXX"
                            "XXXXXXXXXXXXXXXXXXXXXX"
                            "......................"
                            "......................"
                            "......................"
                            "......................"
                            "......................"
                            "......................"
                            )
      (fringe-helper-define 'git-gutter-fr:modified nil
                            "........................."
                            "........................."
                            "........................."
                            "..........XXXXX.........."
                            ".........XXXXXXX........."
                            "........XXXXXXXXX........"
                            ".......XXXXXXXXXXX......."
                            ".......XXXXXXXXXXX......."
                            ".......XXXXXXXXXXX......."
                            ".......XXXXXXXXXXX......."
                            ".......XXXXXXXXXXX......."
                            "........XXXXXXXXX........"
                            ".........XXXXXXX........."
                            "..........XXXXX.........."
                            "........................."
                            "........................."
                            "........................."
                            "........................."
                            )
      )


  ;;; To save the clock history across Emacs sessions, use
    (if (display-graphic-p)
        (progn
          (if (not (file-exists-p org-clock-persist-file))
              (shell-command (concat "touch " org-clock-persist-file)))
          (org-clock-persistence-insinuate)
          ))

    (when (and (or (daemonp)
                   (and (fboundp 'server-running-p) (server-running-p)))
               (string-equal system-type "darwin"))

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Terminal notifier
      ;; requires 'brew install terminal-notifier'
      ;; stolen from erc-notifier

      ;; Example use of alerter
      ;; alerter -sound "Second Glance" -sender org.gnu.Emacs -title Foo -subtitle Bar actions YES -closeLabel Close  -message "This is the message"
      (defvar terminal-alert-command (executable-find "alerter") "The path to the alerter binary.")

      ;; (defvar terminal-notifier-command (executable-find "terminal-notifier") "The path to terminal-notifier.")
      ;;                                  ; (terminal-notifier-notify "Emacs notification" "Something amusing happened")

      ;; (defun terminal-notifier-notify (title message)
      ;;   "Show a message with terminal-notifier-command ."
      ;;   (start-process "terminal-notifier"
      ;;                  "terminal-notifier"
      ;;                  terminal-notifier-command
      ;;                  "-title" title
      ;;                  ; "-sender" "org.gnu.Emacs"
      ;;                  "-appIcon" "/Applications/Emacs.app/Contents/Resources/etc/images/icons/hicolor/48x48/apps/emacs.png"
      ;;                  "-message" message
      ;;                  "-sound" "Gentle Roll"
      ;;                  "-activate" "org.gnu.Emacs"))



      (defun timed-notification (time msg)
        (interactive "sNotification when (e.g: 2 minutes, 60 seconds, 3 days): \nsMessage: ")
        (run-at-time time nil (lambda (msg) (notifications-notify :title "Emacs" :body msg)) msg))


      (debug-init-message "before require org notify")
      (require 'org-notify)
      (debug-init-message "after require org notify")

      (defun my-action-act (plist key)
        "User wants to see action."
        (let ((file (plist-get plist :file))
              (begin (plist-get plist :begin)))
          (if (string-equal key "show")
              (progn
                (switch-to-buffer (find-file-noselect file))
                (org-with-wide-buffer
                 (goto-char begin)
                 (outline-show-entry))
                (goto-char begin)
                (search-forward "DEADLINE: <")
                (search-forward ":")
                (if (display-graphic-p)
                    (x-focus-frame nil)))
            (save-excursion
              (with-current-buffer (find-file-noselect file)
                (org-with-wide-buffer
                 (goto-char begin)
                 (search-forward "DEADLINE: <")
                 (cond
                  ((string-equal key "done")  (org-todo))
                  ((string-equal key "hour")  (org-timestamp-change 60 'minute))
                  ((string-equal key "day")   (org-timestamp-up-day))
                  ((string-equal key "week")  (org-timestamp-change 7 'day)))))))))

      (defvar todo-action-hash (make-hash-table :test 'equal)
        "A hash table for looking up notifications by id")

      (defun my-action (id key)
        (let ((plist (gethash id todo-action-hash)))
          (my-action-act plist key)
          (remhash id todo-action-hash)))

      (debug-init-message "in-server-running-notificiation-setup")

      (defun org-notify-action-notify-mac (plist)
        "Pop up a notification window."
        ;; (require 'notifications)
        (let* ((duration (plist-get plist :duration))
               (urgency (plist-get plist :urgency))
               (id (notifications-notify
                    :title     (plist-get plist :heading)
                    :body      (org-notify-body-text plist)
                    :urgency   (or urgency 'normal)
                    :timeout   (if duration (* duration 1000))
                    :actions   org-notify-actions
                    :on-action 'my-action)))
          (puthash id plist todo-action-hash)
          ))
      (debug-init-message "org notify add")
      (org-notify-add 'default
                      '(:time "15m"
                              :period "15m"
                              :duration 0
                              :urgency 'critical
                              :app-icon (concat (configuration-layer/get-layer-path 'org)
                                                "img/org.png")
                              :actions org-notify-action-notify-mac))
      (debug-init-message "org notify start")
      (org-notify-start)
      (debug-init-message "org notify started")
      )
    (debug-init-message "USER-CONFIG done")
    )
  )
;; ;; Local Variables:
;; noteval: (find-and-close-fold "\\((fold-section \\|(spacemacs|use\\|(when-layer-used\\|(when (configuration-layer\\)")"
;; End:

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
