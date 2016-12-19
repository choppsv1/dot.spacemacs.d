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
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
      ;; Choose either ivy or helm as completion framework
      ;; ivy
      helm

      (auto-completion :variables
                       auto-completion-private-snippets-directory "~/.spacemacs.d/private/snippets"
                       ;; auto-completion-tab-key-behavior 'complete
                       auto-completion-tab-key-behavior 'cycle
                       )
      ;; (auto-completion :variables
      ;;   auto-completion-private-snippets-directory "~/.spacemacs.d/private/snippets"
      ;;   auto-completion-enable-sort-by-usage t
      ;;   auto-completion-enable-snippets-in-popup t
      ;;   auto-completion-tab-key-behavior 'complete
      ;;   )
      ;; company-complete vs complete-at-point
      better-defaults
      github
      graphviz
      gtags
      ;;(ietf :variables ietf-docs-cache "~/ietf-docs-cache")
      ietf
      jabber
      mu4e
      org
      (org2blog :variables org2blog-name "hoppsjots.org")
      ;; pandoc
      (osx :variables
           osx-use-option-as-meta t)
      pdf-tools
      ranger

      rebox

      (shell :variables
              ;; shell-default-shell 'shell
              ;; shell-default-position 'bottom
              ;; shell-default-height 30
              )

      ;; nginx
      spell-checking
      ;; spotify
      syntax-checking
      theming
      themes-megapack
      ;; version-control

      ;; ---------
      ;; Languages
      ;; ---------

      php ;; this is here I think to avoid a bug if we put it in alpha order
      csv
      (c-c++ :variables
             c-c++-default-mode-for-headers 'c-mode
             c-c++-enable-clang-support t)
      emacs-lisp
      git
      go
      html
      javascript
      (latex :variables latex-build-command "latexmk")
      lua
      markdown
      ;; Primary test runner is pytest use 'SPC u' prefix to invoke nose
      (python :variables python-fill-column 120
                         python-test-runner '(pytest nose))
      ;; disable emacs-lisp due to completionion in comments parsing tons
      ;; of .el files https://github.com/syl20bnr/spacemacs/issues/7038
      (semantic :disabled-for emacs-lisp)
      shell-scripts
      systemd
      yaml


      ;; -----------------------------
      ;; Let's keep this later. (why?)
      ;; -----------------------------
      (erc :variables
           erc-server-list
           '(("irc.freenode.net"
              :port "6697"
              :ssl t
              :nick "chopps"
             ))
       )
      ;; bb-erc

      ;; rcirc
      ;; (rcirc :variables
      ;;        rcirc-enable-authinfo-support t)
      ;; eyebrowse blows layouts away!
      ;; vim-empty-lines
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(dockerfile-mode monky org-caldav persistent-scratch)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     ;; vi-tilde-fringe
     erc-yt
     erc-view-log
     mu4e-maildirs-extension
     ;; projectile
     ;; projectile-mode
     ;; recentf
     ;; savehist
     smartparens
     ) ; evil-org
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; mDetermine display size to pick font size

  (setq ch-def-height 9.5)
  (let ((xres (shell-command-to-string "xdpyinfo | sed -e '/dimensions/!d;s/.* \\([0-9]*\\)x[0-9]* .*/\\1/'"))
        ;; (yres (shell-command-to-string "xdpyinfo | sed -e '/dimensions/!d;s/.* [0-9]*x\\([0-9]*\\) .*/\\1/'")))
        )
    (setq xres (replace-regexp-in-string "\n\\'" "" xres))
    ;; (setq yres (replace-regexp-in-string "\n\\'" "" yres))
    (when (<= (string-to-number xres) 3000)
      (setq ch-def-height 9.0)))
  ;; (message "def height %s" ch-def-height)


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
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(hybrid :variables
                                       ;; if t then some modes will use motion inplace of normal
                                       ;; hjkl commands
                                       hybrid-mode-enable-hjkl-bindings nil
                                       ;; if nil this forces evil-emacs-state when trying to enter
                                       ;; evilified state
                                       hybrid-mode-enable-evilified-state t
                                       ;; Default evil state when hybrid editing is enabled
                                       hybrid-mode-default-state 'normal)
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         ;; colorsarenice-light
                         misterioso
                         mandm
                         leuven
                         ;; quasi-monochrome
                         molokai
                         monokai
                         ;; phoenix-dark-pink
                         ;; phoenix-dark-mono
                         spacemacs-dark
                         spacemacs-light
                         ;; solarized-light
                         solarized-dark
                         zenburn
                         ;; ;; theme-test
                         ;; aalto-dark
                         ;; aalto-light
                         ;; abyss
                         ;; afternoon
                         ;; alect-black-alt
                         ;; alect-black
                         ;; alect-dark-alt
                         ;; alect-dark
                         ;; alect-light-alt
                         ;; alect-light
                         ;; aliceblue
                         ;; Amelie
                         ;; ample-flat
                         ;; ample-light
                         ;; ample
                         ;; ample-zen
                         ;; andreas
                         ;; anti-zenburn
                         ;; arjen-grey
                         ;; arjen
                         ;; assemblage
                         ;; atom-one-dark
                         ;; aurora
                         ;; autumn-light
                         ;; badger
                         ;; badwolf
                         ;; base16-3024-dark
                         ;; base16-3024-light
                         ;; base16-apathy-dark
                         ;; base16-apathy-light
                         ;; base16-ashes-dark
                         ;; base16-ashes-light
                         ;; base16-ateliercave-dark
                         ;; base16-ateliercave-light
                         ;; base16-atelierdune-dark
                         ;; base16-atelierdune-light
                         ;; base16-atelierestuary-dark
                         ;; base16-atelierestuary-light
                         ;; base16-atelierforest-dark
                         ;; base16-atelierforest-light
                         ;; base16-atelierheath-dark
                         ;; base16-atelierheath-light
                         ;; base16-atelierlakeside-dark
                         ;; base16-atelierlakeside-light
                         ;; base16-atelierplateau-dark
                         ;; base16-atelierplateau-light
                         ;; base16-ateliersavanna-dark
                         ;; base16-ateliersavanna-light
                         ;; base16-atelierseaside-dark
                         ;; base16-atelierseaside-light
                         ;; base16-ateliersulphurpool-dark
                         ;; base16-ateliersulphurpool-light
                         ;; base16-bespin-dark
                         ;; base16-bespin-light
                         ;; base16-brewer-dark
                         ;; base16-brewer-light
                         ;; base16-bright-dark
                         ;; base16-bright-light
                         ;; base16-chalk-dark
                         ;; base16-chalk-light
                         ;; base16-codeschool-dark
                         ;; base16-codeschool-light
                         ;; base16-colors-dark
                         ;; base16-colors-light
                         ;; base16-darktooth-dark
                         ;; base16-darktooth-light
                         ;; base16-default-dark
                         ;; base16-default-light
                         ;; base16-eighties-dark
                         ;; base16-eighties-light
                         ;; base16-embers-dark
                         ;; base16-embers-light
                         ;; base16-flat-dark
                         ;; base16-flat-light
                         ;; base16-google-dark
                         ;; base16-google-light
                         ;; base16-grayscale-dark
                         ;; base16-grayscale-light
                         ;; base16-greenscreen-dark
                         ;; base16-greenscreen-light
                         ;; base16-harmonic16-dark
                         ;; base16-harmonic16-light
                         ;; base16-hopscotch-dark
                         ;; base16-hopscotch-light
                         ;; base16-irblack-dark
                         ;; base16-irblack-light
                         ;; base16-isotope-dark
                         ;; base16-isotope-light
                         ;; base16-londontube-dark
                         ;; base16-londontube-light
                         ;; base16-macintosh-dark
                         ;; base16-macintosh-light
                         ;; base16-marrakesh-dark
                         ;; base16-marrakesh-light
                         ;; base16-mocha-dark
                         ;; base16-mocha-light
                         ;; base16-monokai-dark
                         ;; base16-monokai-light
                         ;; base16-ocean-dark
                         ;; base16-oceanicnext-dark
                         ;; base16-oceanicnext-light
                         ;; base16-ocean-light
                         ;; base16-paraiso-dark
                         ;; base16-paraiso-light
                         ;; base16-phd-dark
                         ;; base16-phd-light
                         ;; base16-pop-dark
                         ;; base16-pop-light
                         ;; base16-railscasts-dark
                         ;; base16-railscasts-light
                         ;; base16-setiui-dark
                         ;; base16-setiui-light
                         ;; base16-shapeshifter-dark
                         ;; base16-shapeshifter-light
                         ;; base16-solarized-dark
                         ;; base16-solarized-light
                         ;; base16-summerfruit-dark
                         ;; base16-summerfruit-light
                         ;; base16-tomorrow-dark
                         ;; base16-tomorrow-light
                         ;; base16-twilight-dark
                         ;; base16-twilight-light
                         ;; base16-yesterdaybright-dark
                         ;; base16-yesterdaybright-light
                         ;; base16-yesterday-dark
                         ;; base16-yesterday-light
                         ;; base16-yesterdaynight-dark
                         ;; base16-yesterdaynight-light
                         ;; beige-diff
                         ;; beige-eshell
                         ;; bharadwaj-slate
                         ;; bharadwaj
                         ;; billw
                         ;; birds-of-paradise-plus
                         ;; blackboard
                         ;; black-on-gray
                         ;; blippblopp
                         ;; bliss
                         ;; blue-erc
                         ;; blue-eshell
                         ;; blue-gnus
                         ;; blue-mood
                         ;; blue-sea
                         ;; borland-blue
                         ;; boron
                         ;; brin
                         ;; bubbleberry
                         ;; busybee
                         ;; calmer-forest
                         ;; calm-forest
                         ;; caroline
                         ;; charcoal-black
                         ;; cherry-blossom
                         ;; clarity
                         ;; classic
                         ;; clues
                         ;; cobalt
                         ;; colonoscopy
                         ;; colorsarenice-dark
                         ;; colorsarenice-light
                         ;; comidia
                         ;; cyberpunk
                         ;; dakrone
                         ;; dark-blue2
                         ;; dark-blue
                         ;; darkburn
                         ;; dark-erc
                         ;; dark-font-lock
                         ;; dark-gnus
                         ;; dark-green
                         ;; dark-info
                         ;; dark-krystal
                         ;; dark-laptop
                         ;; dark-mint
                         ;; darktooth
                         ;; deep-blue
                         ;; deep-thought
                         ;; desert
                         ;; Deviant
                         ;; digital-ofs1
                         ;; distinguished
                         ;; django
                         ;; dorsey
                         ;; dracula
                         ;; dream
                         ;; eltbus
                         ;; emacs-21
                         ;; emacs-nw
                         ;; erosiond
                         ;; euphoria
                         ;; evenhold
                         ;; faff
                         ;; farmhouse-dark
                         ;; farmhouse-light
                         ;; feng-shui
                         ;; firebelly
                         ;; firecode
                         ;; fischmeister
                         ;; flatland-black
                         ;; flatland
                         ;; flatui
                         ;; fogus
                         ;; gandalf
                         ;; github
                         ;; gnome2
                         ;; gnome
                         ;; goldenrod
                         ;; gotham
                         ;; graham
                         ;; grandshell
                         ;; granger
                         ;; gray1
                         ;; gray30
                         ;; green-phosphor
                         ;; greiner
                         ;; greymatters
                         ;; gruber-darker
                         ;; gruvbox
                         ;; gtk-ide
                         ;; hemisu-dark
                         ;; hemisu-light
                         ;; hemisu
                         ;; heroku
                         ;; hickey
                         ;; high-contrast
                         ;; hipster
                         ;; hober
                         ;; hydandata-light
                         ;; idea-darkula
                         ;; infodoc
                         ;; inkpot
                         ;; iodine
                         ;; ir-black
                         ;; jazz
                         ;; jbeans
                         ;; jb-simple
                         ;; jedit-grey
                         ;; jonadabian-slate
                         ;; jonadabian
                         ;; jsc-dark
                         ;; jsc-light2
                         ;; jsc-light
                         ;; julie
                         ;; junio
                         ;; katester
                         ;; kingsajz
                         ;; kooten
                         ;; late-night
                         ;; lavender
                         ;; lawrence
                         ;; ld-dark
                         ;; lethe
                         ;; leuven
                         ;; light-soap
                         ;; lush
                         ;; mac-classic
                         ;; majapahit-dark
                         ;; majapahit-light
                         ;; marine
                         ;; marquardt
                         ;; material-light
                         ;; material
                         ;; matrix
                         ;; mbo70s
                         ;; mccarthy
                         ;; meacupla
                         ;; mellow
                         ;; midnight
                         ;; minimal-light
                         ;; minimal
                         ;; mistyday
                         ;; moe-dark
                         ;; moe-light
                         ;; moe
                         ;; molokai
                         ;; monochrome-bright
                         ;; monochrome
                         ;; monokai
                         ;; montz
                         ;; mustang
                         ;; mustard
                         ;; my-solarized-dark
                         ;; my-solarized-light
                         ;; naquadah
                         ;; niflheim
                         ;; noctilux
                         ;; obsidian
                         ;; occidental
                         ;; odersky
                         ;; oldlace
                         ;; organic-green
                         ;; oswald
                         ;; paper
                         ;; parus
                         ;; pastelmac
                         ;; pastels-on-dark
                         ;; peacock
                         ;; phoenix-dark-mono
                         ;; phoenix-dark-pink
                         ;; pierson
                         ;; planet
                         ;; pok-wob
                         ;; pok-wog
                         ;; professional
                         ;; purple-haze
                         ;; quasi-monochrome
                         ;; radiance
                         ;; railscasts
                         ;; railscast
                         ;; ramangalahy
                         ;; raspopovic
                         ;; renegade
                         ;; resolve
                         ;; retro-green
                         ;; retro-orange
                         ;; reverse
                         ;; reykjavik
                         ;; rippedcasts
                         ;; ritchie
                         ;; robin-hood
                         ;; rotor
                         ;; ryerson
                         ;; salmon-diff
                         ;; salmon-font-lock
                         ;; sanityinc-tomorrow-blue
                         ;; sanityinc-tomorrow-bright
                         ;; sanityinc-tomorrow-day
                         ;; sanityinc-tomorrow-eighties
                         ;; sanityinc-tomorrow-night
                         ;; scintilla
                         ;; seoul256
                         ;; seti
                         ;; shaman
                         ;; simple-1
                         ;; sitaramv-nt
                         ;; sitaramv-solaris
                         ;; slime
                         ;; smyx
                         ;; snowish
                         ;; snow
                         ;; soft-charcoal
                         ;; soft-morning
                         ;; soft-stone
                         ;; solarized-dark
                         ;; solarized-light
                         ;; solarized
                         ;; soothe
                         ;; spacegray
                         ;; spolsky
                         ;; standard-ediff
                         ;; standard
                         ;; stekene-dark
                         ;; stekene-light
                         ;; subatomic256
                         ;; subatomic
                         ;; subdued
                         ;; subtle-blue
                         ;; subtle-hacker
                         ;; sunburst
                         ;; sunny-day
                         ;; suscolors
                         ;; taming-mr-arneson
                         ;; tango-2
                         ;; tango-plus
                         ;; tangotango
                         ;; tao-yang
                         ;; tao-yin
                         ;; taylor
                         ;; tommyh
                         ;; tomorrow-night-paradise
                         ;; tronesque
                         ;; tron
                         ;; tty-dark
                         ;; twilight-bright
                         ;; twilight
                         ;; ubuntu
                         ;; ujelly
                         ;; underwater
                         ;; vim-colors
                         ;; waher
                         ;; warm-night
                         ;; whateveryouwant
                         ;; wheat
                         ;; white-sand
                         ;; wilson
                         ;; word-perfect
                         ;; xemacs
                         ;; xp
                         ;; zen-and-art
                         ;; zenburn
                         zonokai-blue
                         ;; zonokai-red
                         zonokai
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;;
   ;; Bit wider than Ubuntu, pretty good as well though in being more courier/serif like
   ;; dotspacemacs-default-font `("Cousine" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.4)
   ;; dotspacemacs-default-font `("Courier" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.4)
   ;; Perfect UTF-8, good sans serif
   dotspacemacs-default-font `("DejaVu Sans Mono" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.4)
   ;; Droid has odd spaced UTF-8
   ;; dotspacemacs-default-font `("Droid Sans Mono" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.4)
   ;; Monoid has odd spaced UTF-8
   ;; dotspacemacs-default-font `("Monoid" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.4)
   ;; Very light strokes, can look nice but dim a bit wider again than cousine and ubuntu
   ;; dotspacemacs-default-font `("Nimbus Mono" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.4)
   ;; Looks pretty nice, some UTF graphics don't quite match up
   ;; dotspacemacs-default-font `("Liberation Mono" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.4)
   ;; dotspacemacs-default-font `("Source Code Pro for Powerline" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.4)
   ;; Very condensed -- pretty good for coding -- same odd shapes offs UTF as Liberation Mono
   ;;  dotspacemacs-default-font `("Ubuntu Mono" :size ,ch-def-height :weight normal :width normal :powerline-scale 1.4)
    ;; dotspacemacs-default-font '("Source Code Pro"
    ;;                              :size 16.0
    ;;                              :weight normal
    ;;                              :width normal
    ;;                              :powerline-scale 1.4)

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
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
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
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
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

  ;; ---------
  ;; User-init
  ;; ---------

  (add-to-list 'load-path (concat dotspacemacs-directory "local-lisp/"))
  (add-to-list 'load-path (concat dotspacemacs-directory "themes-test/"))
  (add-to-list 'custom-theme-load-path (concat dotspacemacs-directory "local-lisp/"))
  (add-to-list 'custom-theme-load-path (concat dotspacemacs-directory "themes-test/"))
  (add-to-list 'load-path (concat "~/p/ietf-docs"))
  ;;(require 'iterm-custom-keys)
  (require 'iterm-xterm-extra)
  (require 'generic-lisp)
  (require 'generic-mode-hooks)

  (auto-insert-mode)

  (if (file-accessible-directory-p "~/Dropbox")
      (setq dropbox-directory "~/Dropbox")
    (setq dropbox-directory nil))

  (if (file-accessible-directory-p "~/Dropbox/org-mode")
      (setq org-directory "~/Dropbox/org-mode"
            org-agenda-files '("~/Dropbox/org-mode"))
    (setq org-directory "~/org"))

  (setq
   debug-init-msg nil
   evil-search-wrap nil
   evil-want-C-i-jump nil
   ;; This is very annoying to have to set, visual highlight in evil is hijacking PRIMARY selection
   ;; behavior..
   x-select-enable-primary t
   ;; evil-esc-delay 0.001
   ;; js2-basic-offset 2
   ;; js-indent-level 1
   org-protocol-default-template-key "t"

   ;; This really should be a file local variable.
   rebox-style-loop '(71 72 73)

  ;; (with-eval-after-load "evil-evilified-state"
  ;;   (define-key evil-evilified-state-map-original "H" 'evil-window-top)
  ;;   (define-key evil-evilified-state-map-original "L" 'evil-window-bottom)
  ;;   (define-key evil-evilified-state-map-original "M" 'evil-window-middle))

   )
  (setq theming-modifications `(
                                (misterioso (erc-input-face :foreground "cornflowerblue")
                                            (font-lock-comment-face :foreground "DarkGrey" :slant italic)
                                            (font-lock-comment-delimiter-face :foreground "grey33"))
                                (molokai (font-lock-comment-face :foreground "DarkGrey")
                                         (font-lock-comment-delimiter-face :foreground "grey30"))
                                (monokai (font-lock-comment-face :foreground "#A5A17E" :slant italic)
                                         (font-lock-doc-face :foreground "#A5A17E" :slant italic)
                                         (font-lock-comment-delimiter-face :foreground "#55513E"))
                                (quasi-monochrome (default :height ,(* ch-def-height 10))
                                                  (font-lock-string-face :foreground "DarkGrey" :slant italic)
                                                  (font-lock-comment-delimiter-face :foreground "darkslategray"))
                                (leuven ;; (default :background "#F0F0E0")
                                 (default :background "#ede8da")
                                        ;;(default :background "#F0F0E5")
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


  ;; XXX what we want actually is to advise this function and temporarily change
  ;; XXX the definition of x-select-text to (x-set-selection 'PRIMARY ...) so
  ;; XXX that the correct thing happens
  (defun _evil-visual-update-x-selection (&optional buffer)
    "Update the X selection with the current visual region."
    (let ((buf (or buffer (current-buffer))))
      ;; (message "XXXVISUALSELECT1")
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
            ;; (message "XXXVISUALSELECT2")
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
                  "w0" 'delete-window
                  "w1" 'delete-other-windows
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

  ;; (when (and (configuration-layer/layer-usedp 'python)
  ;;            (configuration-layer/layer-usedp 'gtags))
  ;;            (configuration-layer/layer-usedp 'gtags))

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
    (persistent-scratch-setup-default)
  ;;   (when (and (configuration-layer/layer-usedp 'python)
  ;;              (configuration-layer/layer-usedp 'gtags))
  ;;     (add-hook 'python-mode-hook '(lambda () (ggtags-mode 1))))

  ;;   (when (and (configuration-layer/layer-usedp 'emacs-lisp)
  ;;              (configuration-layer/layer-usedp 'gtags))
  ;;     (add-hook 'emacs-lisp-mode-hook '(lambda () (ggtags-mode 1))))



    ;; have to allow-emacs-pinentry in gpg-agent.conf?
    ;; (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
    ;; (pinentry-start)

    ;; Most everything use 4 space indent. This variable is a collection of
    ;; the values used by various modes join-line
    (dolist (e spacemacs--indent-variable-alist)
      (if (symbolp (cdr e))
          (if (not (eq 'lisp-indent-offset (cdr e)))
              (set-default (cdr e) 4))
        (dolist (x (cdr e)) (set-default x 4))))

    ;; (setq-default lisp-indent-offset nil)
      ;; (setq emacs-lisp-mode lisp-mode) . lisp-indent-offset)

    (setq python-indent-offset 4)

    ;; overrides
    (setq-default nxml-child-indent 2)

    ;; tabs are 8 characters wide!
    (setq-default tab-width 8)

    (run-hook-with-args 'spacemacs--hjkl-completion-navigation-functions
                        (member dotspacemacs-editing-style '(vim)))
    (setq
     spacemacs--hjkl-completion-navigation-functions nil
     browse-url-new-window-flag t
     tab-always-indent t
     )

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

      (spacemacs|define-custom-layout "agenda"
        :binding "a"
        :body
        (org-agenda-list))

      (spacemacs|define-custom-layout "emacs"
        :binding "^e"
        :body
        (find-file "~/.emacs.d/"))

      (spacemacs|define-custom-layout "mail"
        :binding "m"
        :body
        (mu4e))

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

      (spacemacs|define-custom-layout "notes"
        :binding "n"
        :body
        (progn
          (find-file "~/Dropbox/org-mode/work.org")
          (split-window-right)
          (find-file "~/Dropbox/org-mode/notes.org")
          )
        )
      (spacemacs|define-custom-layout "W:CASSFILE"
        :binding "wc"
        :body
        (find-file "~/w/cassfile/cassfile/main.py")
        )
      (spacemacs|define-custom-layout "W:JDSUAPP"
        :binding "wa"
        :body
        (ranger "~/w/jdsu-util-ts/")
        )
      (spacemacs|define-custom-layout "W:JDSU"
        :binding "wj"
        :body
        (find-file "~/w/jdsu-ocm/jdsuocm/main.py")
        )
      (spacemacs|define-custom-layout "W:ONDATA"
        :binding "wd"
        :body
        (find-file "~/w/optical-network-data/terastream.yang")
        )
      (spacemacs|define-custom-layout "W:OCP"
        :binding "wo"
        :body
        (find-file "~/w/ocp/ocp/main.py")
        )
      (spacemacs|define-custom-layout "W:UTIL"
        :binding "wu"
        :body
        (find-file "~/w/tsutil/setup.py")
        )
      (spacemacs|define-custom-layout "P:OrgBeamerSandbox"
        :binding "po"
        :body
        (find-file "~/p/org-beamer-sandbox")
        )
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

    ;; ==========
    ;; Messaging
    ;; ==========

    ;; (defun my-erc-perspective ()
    ;;   "Switch or create to a perspective called 'erc' and connect to IRC"
    ;;   (interactive)
    ;;   (select-frame (make-frame '((name . "@ERC") (minibuffer . t))))
    ;;   (persp-switch "freenode.net")
    ;;   (erc :server "irc.freenode.net" :port "6667" :nick "chopps"))

    (when (configuration-layer/layer-usedp 'erc)
      (setq erc-prompt-for-nickserv-password nil
            erc-autojoin-channels-alist '(("irc.gitter.im" "#syl20bnr/spacemacs")
                                          ("192.168.1.6" "#syl20bnr/spacemacs")
                                          ("mollari.netbsd.org" "#NetBSD")
                                          ("freenode.net" "#choppstest")
                                          )
            erc-auto-query 'window
            erc-fill-mode nil
            ;; erc-hl-nicks-minimum-contrast-ratio 3.5
            ;; erc-hl-nicks-color-contrast-strategy 'contrast
            ;; erc-hl-nicks-color-contrast-strategy 'invert
            erc-hl-nicks-skip-nicks '("gitter")
            erc-join-buffer 'bury

            ;; Logging
            erc-log-channels-directory "~/Dropbox/logs/erclogs"
            ;; erc-log-all-but-server-buffers t
            erc-log-insert-log-on-open nil ;; this inserts after the prompt which is scary as it
            ;; might send
            erc-save-buffer-on-part nil
            erc-save-queries-on-quit nil
            erc-log-write-after-send t
            erc-log-write-after-insert t

            ;; Notifcations
            erc-notifications-icon (concat user-emacs-directory "./layers/+chat/rcirc/img/irc.png")
            erc-spelling-mode t

            erc-track-switch-direction 'importance
            )
      ;; We want to be in normal state most of the time so we can flip in and out.
      (evil-set-initial-state 'erc-mode 'normal)

      ;; Simplify this function so it works
      (with-eval-after-load 'erc-log
        (defun erc-log-all-but-server-buffers (buffer)
          (not (erc-server-buffer-p buffer))))


      ;; Actually we really only want this when we move away from the buffer?
      ;; (add-hook 'erc-send-post-hook 'evil-normal-state)
      ;; (remove-hook 'erc-send-post-hook 'evil-normal-state)


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
                                  :secret)))
               (if (functionp secret)
                   (funcall secret)
                 secret)))

      (with-eval-after-load 'erc
       (setq erc-nickserv-passwords
             `((freenode (("chopps" . ,(erc-acct-get-password "chopps" "freenode.net" "nickserv"))))
               (localhost (("chopps" . ,(erc-acct-get-password "chopps" "localhost" "bitlbee")))))))

      (defun launch-irc-gitter ()
        "Launch irc connection to giter.im"
        (interactive)
        (erc :server "localhost" :port 6669 :nick "choppsv1"
            :password (erc-acct-get-password "choppsv1" "192.168.1.6" 6669)))
        ;; (erc-tls :server "irc.gitter.im" :port 6667 :nick "choppsv1"
        ;;         :password (erc-acct-get-password "choppsv1" "irc.gitter.im" 6667)))

      (defun launch-irc-netbsd ()
        "Launch irc connection to netbsd"
        (interactive)
        (erc-tls :server "mollari.netbsd.org" :port 7001 :nick "chopps" :full-name "Christian E. Hopps"))

      (defun launch-irc-freenode ()
        "Launch irc connection to freenode"
        (interactive)
        (erc-tls :server "irc.freenode.net" :port 6697 :nick "chopps"
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
            ;; (message "Sending privmsg to &bitlbee server %s" erc-session-server)
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

      ;; (setq erc-modules (delete 'fill erc-modules))

      (defvar evil-normal-state-on-unfocus-modes
        '(erc-mode))
      (defun evil-normal-state-on-unfocus ()
        "Return to normal state when a buffer in a given major mode is unfocussed"
        (when (member major-mode evil-normal-state-on-unfocus-modes)
          (evil-normal-state)))

      (add-hook 'unfocus-buffer-hook 'evil-normal-state-on-unfocus)

      (with-eval-after-load 'erc
        (erc-fill-disable)
        ;; (erc-log-enable)
        (add-to-list 'erc-modules 'notifications)
        (setq erc-modules (cons 'log (delete 'fill erc-modules))))
        ;; (erc-services-mode 1)
        ;; (erc-spelling-mode 1))
      (with-eval-after-load 'erc-desktop-notifications
        ;; Redefine this so that we can set a timeout
        ;; normally it uses the server and we use awesome so it would be nice to
        ;; fix that there.
        (defun erc-notifications-notify (nick msg)
          "Notify that NICK send some MSG.
This will replace the last notification sent with this function."
          (dbus-ignore-errors
            (setq erc-notifications-last-notification
                  (notifications-notify :title (xml-escape-string nick)
                                        :timeout 0
                                        :body (xml-escape-string msg)
                                        :replaces-id erc-notifications-last-notification
                                        :app-icon erc-notifications-icon))))

        )
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

    (when (configuration-layer/layer-usedp 'jabber)
      (setq ssl-program-name "gnutls-cli"
            ssl-program-arguments '("--insecure" "-p" service host)
            ssl-certificate-verification-policy 1)

      (setq jabber-account-list '(("choppsv2@localhost"
                                   (:port . 5222)
                                   (:password . "foobar"))))
                                   ;;(:connection-type . ssl))))
      )

    ;; ======
    ;; Email
    ;; ======

    (when (configuration-layer/layer-usedp 'mu4e)
      (defcustom mu4e-spam-folder "/chopps.org/spam-train"
        "Folder for spam email"
        :type '(string :tag "Folder name")
        :group 'mu4e-folders)

      ;; REMOVE ME XXXX
      (defun mu4e-clear-caches ()
        "Clear any cached resources."
        (progn
          (message "XXX mu4e-clear-caches called")
          (setq
           mu4e-maildir-list nil
           mu4e~contacts nil)))

      (if (not (member ?. (string-to-list (system-name))))
          (setq smtpmail-local-domain "chopps.org"))

      (setq mu4e-maildir "~/Documents/imap-accounts"
        ;; Updating
        ;; mu4e-pre-hook-count 0
        ;; mu4e-full-update-mail-command "bash -c '(cd && offlineimap -l /Users/chopps/.offlineimap/logfile)'"
        ;; mu4e-quick-update-mail-command "bash -c '(cd && offlineimap -q -l /Users/chopps/.offlineimap/logfile)'"

        ;; mu4e-update-pre-hook 'mu4e-pre-hook-udpate-command
        mu4e-change-filenames-when-moving t
        mu4e-mu-binary (executable-find "mu")
        mu4e-update-interval nil
        mu4e-headers-include-related nil

        ;; Stop mu4e from blowing away message buffer all the time
        mu4e-hide-index-messages t

        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask-if-none
        mu4e-compose-complete-only-after "2015-01-01"

        ;; -----------
        ;; [b]ookmarks
        ;; -----------

        mu4e-inbox-mailbox '("maildir:/gmail.com/INBOX"
                              "maildir:/chopps.org/INBOX"
                              "maildir:/dev.terastrm.net/INBOX"
                              "maildir:/chopps.org/a-terastream")

        mu4e-imp-mailbox '("maildir:/chopps.org/ietf-chairs"
                            "maildir:/chopps.org/ietf-chairs-rtg"
                            "maildir:/chopps.org/ietf-dt-netmod-ds"
                            "maildir:/chopps.org/ietf-rtg-dir"
                            "maildir:/chopps.org/ietf-rtg-yang-dt"
                            "maildir:/chopps.org/ietf-wg-isis"
                            "maildir:/chopps.org/ietf-wg-netmod")

        mu4e-drafts-mailbox '("maildir:/chopps.org/ietf-announce"
                              "maildir:/chopps.org/ietf-ann-proto-action"
                              "maildir:/chopps.org/ietf-ann-id-actions")

        mu4e-junk-mailbox '("maildir:/gmail.com/[Gmail].Spam"
                             "maildir:/chopps.org/spam-probable"
                             "maildir:/chopps.org/spam-train"
                             "maildir:/chopps.org/spam")

        mu4e-unread-filter "(flag:unread AND NOT flag:flagged AND NOT flag:trashed)"
        mu4e-not-junk-folder-filter
        (concat " AND NOT (" (string-join mu4e-junk-mailbox " OR " ) ")")

        mu4e-inbox-filter-base
        (concat "(" (string-join mu4e-inbox-mailbox " OR ") ")")

        mu4e-imp-filter-base
        (concat "(" (string-join mu4e-imp-mailbox " OR ") ")")

        mu4e-unread-filter "(flag:unread AND NOT flag:flagged AND NOT flag:trashed)"
        mu4e-unread-flagged-filter "(flag:unread AND flag:flagged AND NOT flag:trashed)"

        mu4e-bookmarks
        (append
          (list (list (concat "flag:unread AND NOT flag:trashed AND " mu4e-inbox-filter-base) "Unread [i]NBOX messages" ?i)
            (list (concat "flag:unread AND NOT flag:trashed" mu4e-not-junk-folder-filter " AND maildir:/chopps.org/ietf-*") "Unread IETF messages" ?I)

            (list (concat "flag:flagged AND NOT flag:trashed AND " mu4e-inbox-filter-base) "[f]lagged INBOX messages" ?f)
            (list (concat "flag:flagged AND NOT flag:trashed AND NOT " mu4e-inbox-filter-base mu4e-not-junk-folder-filter) "[F]lagged Non-INBOX messages" ?F)

            (list (concat mu4e-unread-filter         mu4e-imp-filter-base) "Unread Important messages" ?n)
            (list (concat mu4e-unread-flagged-filter mu4e-imp-filter-base) "Unread-Flagged Important messages" ?N)

            (list (concat mu4e-unread-filter         " AND NOT " mu4e-imp-filter-base " AND NOT " mu4e-inbox-filter-base mu4e-not-junk-folder-filter) "Unread [u]nimportant messages" ?u)
            (list (concat mu4e-unread-flagged-filter " AND NOT " mu4e-imp-filter-base " AND NOT " mu4e-inbox-filter-base mu4e-not-junk-folder-filter) "Unread-Flagged [U]nimportant messages" ?U)

            (list (concat mu4e-unread-filter         " AND NOT " mu4e-inbox-filter-base mu4e-not-junk-folder-filter) "Unread Non-INBOX messages" ?o)
            (list (concat mu4e-unread-flagged-filter " AND NOT " mu4e-inbox-filter-base mu4e-not-junk-folder-filter) "Unread-Flagged Non-INBOX messages" ?O)


            (list (concat mu4e-unread-filter         mu4e-not-junk-folder-filter) "Unread messages" ?a)
            (list (concat mu4e-unread-flagged-filter mu4e-not-junk-folder-filter) "Unread-flagged messages" ?A)

            (list "maildir:/chopps.org/spam-probable" "Probable spam messages" ?s))
          (mapcar (lambda (x) (cons (concat (car x) mu4e-not-junk-folder-filter) (cdr x)))
            '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
               ("date:1h..now" "Last hours messages" ?h)
               ("date:24h..now" "Today's messages" ?d)
               ("date:today..now" "Today's messages" ?t)
               ("date:7d..now" "Last 7 days" ?w)
               ("date:7d..now from:chopps" "Last 7 days sent" ?W)
               ("mime:*pdf" "Messages with PDF" ?p)
               ("mime:*calendar" "Messages with calendar" ?q)
               ("mime:*cs" "Messages with VCS" ?Q)
               )))


        ;; [j]ump shortcuts
        mu4e-maildir-shortcuts '(("/chopps.org/INBOX" . ?i)
                                  ("/gmail.com/INBOX" . ?g)
                                  ("/dev.terastrm.net/INBOX" . ?w)
                                  ("/chopps.org/receipts" . ?r)
                                  ("/chopps.org/a-terastream" . ?t)
                                  ("/chopps.org/aa-netbsd" . ?n)
                                  ("/chopps.org/ietf-wg-isis" . ?I)
                                  ("/chopps.org/ietf-wg-homenet" . ?H)
                                  ("/chopps.org/ietf-wg-netmod" . ?N)
                                  ("/chopps.org/spam-train" . ?S)
                                  ("/chopps.org/spam-probable" . ?s))

        ;; Visuals
        mu4e-view-show-addresses t
        mu4e-headers-visible-lines 15
        mu4e-headers-visible-columns 80

        ;; XXX Try running w/o this to see if hangs go away.
        ;; mu4e-html2text-command 'mu4e-shr2text
        ;; mu4e-html2text-command "html2text -nobs -utf8 -width 120"
        ;; mu4e-html2text-command "html2text --unicode-snob | grep -v '&nbsp_place_holder;'"
        ;; mu4e-html2text-command "html2text -b 0 --unicode-snob"
        mu4e-html2text-command "w3m -dump -cols 120 -T text/html"

        ;; used ofr HTML in email ;; <#part type="message/rfc822" filename="/home/chopps/Documents/imap-accounts/chopps.org/sw-common/cur/1460249763.39525_8697.tops,U=242:2,S" disposition=attachment description="Re: Mail not correctly displayed"> <#/part>

        ;; make work better in dark themes
        ;; [[mu4e:msgid:87vb7ng3tn.fsf@djcbsoftware.nl][Re: I find html2markdown the best value for mu4e-html2text-command]]

        shr-color-visible-luminance-min 80

        ;; this is keeping it from toggling I think
        ;; mu4e-view-html-plaintext-ratio-heuristic 15
        mu4e-view-html-plaintext-ratio-heuristic 0

        mu4e-use-fancy-chars nil
        ;; mu4e-headers-has-child-prefix    '(" ┬●")  ; Parent
        ;; mu4e-headers-empty-parent-prefix '(" ─●")  ; Orphan
        ;; mu4e-headers-first-child-prefix  '("└─●")  ; First child
        ;; mu4e-headers-duplicate-prefix    '("└≡")   ; Duplicate
        ;; mu4e-headers-default-prefix      '("  ●") ; Default.


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

      (with-eval-after-load 'mu4e
        (progn
          (setq mu4e-contexts `( ,(make-mu4e-context
                                    :name "chopps.org"
                                    :match-func (lambda (msg)
                                                  (and msg (string-match "/chopps.org/.*" (mu4e-message-field msg :maildir))))
                                    :vars '((user-mail-address  . "chopps@chopps.org")
                                            (user-full-name . "Christian Hopps")
                                             ;; mu4e
                                             (mu4e-sent-folder   . "/chopps.org/Sent Messages")
                                             (mu4e-trash-folder  . "/chopps.org/Deleted Messages")
                                             (mu4e-drafts-folder . "/chopps.org/Drafts")
                                             (mu4e-sent-messages-behavior   . sent)
                                             ;; smtp
                                             (smtpmail-starttls-credentials . '(("smtp.chopps.org" 587 nil nil)))
                                             (smtpmail-default-smtp-server  . "smtp.chopps.org")
                                             (smtpmail-smtp-server          . "smtp.chopps.org")
                                             (smtpmail-smtp-service         . 587)))
                                 ,(make-mu4e-context
                                    :name "dev.terastrm.net"
                                    :match-func (lambda (msg)
                                                  (and msg (string-match "/dev.terastrm.net/.*" (mu4e-message-field msg :maildir))))
                                    :vars '((user-mail-address  . "chopps@dev.terastrm.net")
                                            (user-full-name . "Christian Hopps")
                                             ;; mu4e
                                             (mu4e-sent-folder   . "/dev.terastrm.net/Sent Messages")
                                             (mu4e-trash-folder  . "/dev.terastrm.net/Deleted Messages")
                                             (mu4e-drafts-folder . "/dev.terastrm.net/Drafts")
                                             (mu4e-sent-messages-behavior   . sent)
                                             ;; smtp
                                             (smtpmail-starttls-credentials . '(("smtp.dev.terastrm.net" 587 nil nil)))
                                             (smtpmail-default-smtp-server  . "smtp.dev.terastrm.net")
                                             (smtpmail-smtp-server          . "smtp.dev.terastrm.net")
                                             (smtpmail-smtp-service         . 587)))
                                 ,(make-mu4e-context
                                    :name "gmail.com"
                                    :match-func (lambda (msg)
                                                  (and msg (string-match "/gmail.com/.*" (mu4e-message-field msg :maildir))))
                                    :vars '((user-mail-address  . "chopps@gmail.com")
                                            (user-full-name . "Christian Hopps")
                                             ;; mu4e
                                             (mu4e-drafts-folder . "/gmail.com/[Gmail].Drafts")
                                             (mu4e-sent-folder   . "/gmail.com/[Gmail].Sent Mail")
                                             (mu4e-trash-folder  . "/gmail.com/[Gmail].Trash")
                                             (mu4e-sent-messages-behavior   . delete)
                                             ;; smtp
                                             (smtpmail-starttls-credentials . '(("smtp.gmail.com" 587 nil nil)))
                                             (smtpmail-default-smtp-server  . "smtp.gmail.com")
                                             (smtpmail-smtp-server          . "smtp.gmail.com")
                                             (smtpmail-smtp-service . 587)))))


          ;; Mu4E Keyboard extras

          (bind-key (kbd "'") 'mu4e-headers-next 'mu4e-headers-mode-map)
          (bind-key (kbd "\"") 'mu4e-headers-prev 'mu4e-headers-mode-map)
          (bind-key (kbd "\"") 'mu4e-view-headers-prev 'mu4e-view-mode-map)
          (bind-key (kbd "f") 'mu4e-view-go-to-url 'mu4e-view-mode-map)


          (defun ch:ct (clist)
            "Transform candidate into (display . real)"
            (mapcar (lambda (candidate)
                      (let* ((name (plist-get candidate :name))
                              email (plist-get candidate :mail))
                        (or (and name (format "%s <%s>" name email))
                          email))) clist))

          (defun my-message-expand-name (&optional start)
            (interactive)
            ;; (message "my-message-expand-name called")
            (helm :prompt "contact:" :sources
              (helm-build-sync-source "mu4e contacts"
                :candidates mu4e~contact-list :candidate-transformer 'ch:ct)))


          (defun my-mu4e-view-hook ()
            "Possibly switch to htlm mode"
            t)
          (add-hook 'mu4e-view-mode-hook 'my-mu4e-view-hook)

          (defun my-mu4e-compose-hook ()
            "Setup outgoing messages"
            ;; Add chopps@<account-sending-from> to CC
            ;; Add chopps@chopps.org to Bcc if not sending from @chopps.org
            (let ((buffer-modified (buffer-modified-p)))
              (save-excursion
                ;; (message-add-header (concat "Cc: " user-mail-address))
                (if (not (string= user-mail-address "chopps@chopps.org"))
                    (message-add-header "Bcc: chopps@chopps.org")
                  ))
              (set-buffer-modified-p buffer-modified))
            ;; Outgoing mails get format=flowed.
            ;; (use-hard-newlines t 'guess)
            ;; Sign messages by default

            ;; If we have no contacts try filling them again.
            (if (not mu4e~contacts)
                (mu4e~request-contacts))

            (mml-secure-message-sign-pgpmime))
          (add-hook 'mu4e-compose-mode-hook 'my-mu4e-compose-hook)

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
          ;; (message "post-init end mu4e eval after load")

          ;; XXX these aren't defined
          ;; (define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
          ;; (define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)

          (require 'mu4e-contrib)

          ;; XXX also add this back
          ;;
          ;; error in process sentinel: mu4e-alert--parse-mails: End of file during parsing
          ;; error in process sentinel: End of file during parsing
          ;; (mu4e-alert-enable-mode-line-display)
          ;; (mu4e-alert-enable-notifications)

          ;; ;; XXX disabled trying to find hang XXX THIS CAUSED IT
          (add-hook 'mu4e-headers-mode-hook
            (lambda () (progn
                         (make-local-variable 'scroll-conservatively)
                         (setq
                           show-trailing-whitespace nil
                           scroll-conservatively 0
                           ;; XXX These two cause hangs
                           ;; scroll-up-aggressively .8
                           ;; scroll-down-aggressively .8)
                           )
                         )))

          ;; ;; XXX causes hangs
          ;; (add-hook 'mu4e-headers-mode-hook (lambda () (progn (setq scroll-up-aggressively .8))))

          (add-hook 'mu4e-view-mode-hook
                    (lambda () (setq show-trailing-whitespace nil)))


          (defun mu4e-pre-hook-udpate-command ()
            (let ((check (% mu4e-pre-hook-count 4)))
              (setq mu4e-get-mail-command (if (= check 0)
                                            mu4e-full-update-mail-command
                                            mu4e-full-update-mail-command))
              (setq mu4e-pre-hook-count (1+ mu4e-pre-hook-count))))


          (add-to-list 'mu4e-view-actions
                       '("ViewInBrowser" . mu4e-action-view-in-browser))

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
          (define-key mu4e-headers-mode-map "\\" 'mu4e-headers-mark-move-to-spam)
          (define-key mu4e-view-mode-map "\\" 'mu4e-view-mark-move-to-spam)

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
            "b" 'mu4e-view-bookmark-make-record
            )

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
                                         (:flags          .  4)
                                         (:human-date     . 12)
                                         (:from           . 18)
                                         (:list-or-dir    . 20)
                                         (:thread-subject . nil)
                                         )))

          )
        )
      )

    ;; =================
    ;; Programming Modes
    ;; =================

    (when (configuration-layer/layer-usedp 'syntax-checking)
      (with-eval-after-load "flycheck"
        (setq flycheck-highlighting-mode 'lines)
        ;; the pos-tip window doesn't seem to work with my awesome setup (anymore)
        (setq flycheck-display-errors-function #'flycheck-display-error-messages)

        ;; Chain pylint after flake8 to get benefit of both.
        (flycheck-add-next-checker 'python-flake8 'python-pylint)

        (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
        (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)))

    (when (configuration-layer/layer-usedp 'emacs-lisp)
      (with-eval-after-load "lisp-mode"
        ;; hyphens are words in emacs lisp
        (modify-syntax-entry ?- "w" lisp-mode-syntax-table)
        (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
        (defun rebox-lisp-hook ()
          (set (make-local-variable 'rebox-style-loop) '(81 82 83)))
        (add-hook 'lisp-mode-hook 'rebox-lisp-hook)
        (add-hook 'emacs-lisp-mode-hook 'rebox-lisp-hook)
        ))

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
      (with-eval-after-load 'python

        ;; (setq python-fill-docstring-style 'symmetric
        ;;       python-fill-string-function 'my-python-fill-string-function)

        ;; (defun my-python-fill-comment-function (&optional justify)
        ;;   (let ((fill-column 80))
        ;;     (python-fill-comment justify)))

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

        (spacemacs/set-leader-keys-for-major-mode 'python-mode "en" 'flycheck-next-error)
        (spacemacs/set-leader-keys-for-major-mode 'python-mode "ep" 'flycheck-prev-error)


        ;; Consider _ a part of words for python
        (modify-syntax-entry ?_ "w" python-mode-syntax-table)

        ;; (define-key global-map (kbd "C-c o") 'iedit-mode)

        ;; (if (file-exists-p "/usr/local/bin/python"  )
        ;; (setenv "PYMACS_PYTHON" "/usr/local/bin/python"))

        ;; XXX Hack to get rid of warning, need to fix this differently.
        (setq python-shell-completion-native-enable nil)

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

        ;; (defun my-python-mode-hook ()
        ;;   (setq comment-column 60)

        ;;   ;; This gives and error
        ;;   ;; (message "select checker")
        ;;   ;; This is required b/c for some reason it's still not loaded at this point.
        ;;   ;; (require 'flycheck)

        ;;   ;; not needed now that we chain
        ;;   ;; (flycheck-select-checker 'python-pycheckers)
        ;;   ;; (message "post select checker")

        ;;   ;; (flycheck-set-checker-executable 'python-flake8 "~/bin/pycheckers.sh")
        ;;   ;; (message "select set exec")
        ;;   ;; (add-to-list 'compilation-error-regexp-alist '("\\(.*\\):[CEFRW][0-9]+: ?\\([0-9]+\\),[0-9]+: .*" 1 2))

        ;;   (message "Python mode hook done"))

        ;; (add-hook 'python-mode-hook 'my-python-mode-hook 'append)
        )
      )

    ;; remove when added to spacemacs--indent-variable-alist
    (when (configuration-layer/layer-usedp 'lua)
      (with-eval-after-load 'lua-mode
        (setq-default lua-indent-level 4)))

    ;;   (progn
    ;;     (delq (assoc "^\t+#" makefile-font-lock-keywords) makefile-font-lock-keywords)))
    (with-eval-after-load 'make-mode
      (delq (assoc "^\t+#" makefile-font-lock-keywords) makefile-font-lock-keywords))

    ;; ============
    ;; Org Exports
    ;; ============
    (setq image-transform-scale 2.0)

    ;; ===
    ;; Org
    ;; ===

    (when (configuration-layer/layer-usedp 'org)
      (progn
        (if debug-init-msg
            (message "post-init-start"))

        ;; Do we want this?
        (add-hook 'org-mode-hook #'yas-minor-mode)

        ;; Custom Agenda View
        (setq org-agenda-custom-commands
              '(("c" . "Custom searches") ; describe prefix "h"
                ("cc" "Closed in the last week" tags "CLOSED>=\"<-1w>\"")))

        ;; key desc (cmd1 cmd2 ...) settings-for-whole-set files


        ;; This is for using xelatex
        (with-eval-after-load "org"
          ;; (dolist (estate '(normal visual motion))
          ;;   (evil-define-key estate evil-org-mode-map "H" nil)
          ;;   (evil-define-key estate evil-org-mode-map "L" nil)
          ;;   (evil-define-key estate org-mode-map "H" nil)
          ;;   (evil-define-key estate org-mode-map "M" nil)
          ;;   (evil-define-key estate org-mode-map "L" nil))
          (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

          (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

          (require 'ox-latex))

        ;; (with-eval-after-load "org-agenda"
        ;;   (define-key org-agenda-mode-map (kbd "RET") 'org-agenda-switch-to))

        (defun my-org-mode-hook ()
          (if debug-init-msg
              (message "Org-mode-hook"))
          ;; (org-set-local 'yas/trigger-key [tab])
          ;; (yas-minor-mode)
          ;; Probably done now.
          ;; (turn-on-flyspell)

          ;; (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)
          ;; XXX need to redefine this for firefox in archlinux
          (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)
          (define-key org-mode-map (kbd "C-c e e") 'org-encrypt-entries)
          (define-key org-mode-map (kbd "C-c e E") 'org-encrypt-entry)
          (define-key org-mode-map (kbd "C-c e d") 'org-decrypt-entries)
          (define-key org-mode-map (kbd "C-c e D") 'org-decrypt-entry)

          )

        ;; (setq TeX-view-program-selection
        ;;       (append
        ;;        (delq (assoc 'output-pdf TeX-view-program-selection) TeX-view-program-selection)
        ;;        '((output-pdf "PDF Tools"))))

        (add-hook 'org-mode-hook 'my-org-mode-hook)

        ;; (defun th/pdf-view-revert-buffer-maybe (file)
        ;;   (when-let ((buf (find-buffer-visiting file)))
        ;;             (with-current-buffer buf
        ;;               (when (derived-mode-p 'pdf-view-mode)
        ;;                 (pdf-view-revert-buffer nil t)))))

        ;; (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
        ;;           #'th/pdf-view-revert-buffer-maybe)

        (defun my-org-confirm-babel-evaluate (lang body)
          (not (or (string= lang "ditaa")
                   (string= lang "dot2tex")
                   (string= lang "dot")
                   (string= lang "gnuplot")
                   (string= lang "plantuml")
                   )))
        ;; (add-to-list 'org-babel-load-languages '(dot2tex . t))

        (setq
         ;; Crypt
         org-tags-exclude-from-inheritance '("crypt")
         org-crypt-disable-auto-save t
         org-crypt-key "D7B83025"
         org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate
         org-src-fontify-natively t
         org-default-notes-file (concat org-directory "/notes.org")
         ;; Display
         org-display-inline-images t

         ;; General
         ;; org-agenda-start-day "-8d"
         org-agenda-start-on-weekday 1
         org-hide-leading-stars t
         org-log-done 'time
         org-outline-path-complete-in-steps t
         org-plantuml-jar-path "/opt/plantuml/plantuml.jar"
         org-refile-use-outline-path 'file
         org-src-tab-acts-natively t
         org-src-window-setup 'current-window

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

         ;; XXX investigate this more
         ;; org-icalendar-include-todo t
         ;; org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
         ;; org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
         ;; org-icalendar-with-timestamps t

         ;; (setq org-capture-templates
         ;; '(
         ;;   ("m" "Mail options")

         ;;   ("mt" "mailtodo"
         ;;    entry (file+datetree "~/s/notes/tasks.org")
         ;;    "* TODO %^{Task} : %:subject %^G\nSCHEDULED: %t\n- From :: %:from\n- Subject :: %:subject\n- Email :: %a\n\n%?" :kill-buffer t)

         ;;   ("mn" "mailnote"
         ;;    entry (file+headline "~/s/notes/notes.org" "general notes and tasks")
         ;;    "* %^{Title} : %:subject %^G\n- From :: %:from\n- Subject :: %:subject\n- Email :: %a\n\n%?\n\n%U")
         ;;   ))

         org-capture-templates
         '(
           ("t" "Todo" entry (file+headline (concat org-directory "/notes.org") "Tasks")
            "* TODO %^{Title}%?\nDEADLINE: %^t CREATED: %u\nAnnotation: %a\n\n")

           ("m" "Mail Todo" entry (file+headline (concat org-directory "/notes.org") "Mail")
            "* TODO [Mail] %^{Title|%:subject}%? ([%:from])\nDEADLINE: %^t CREATED: %u\nMessage: %a\n\n")

           ("c" "Code Todo" entry (file+headline (concat org-directory "/notes.org") "Code Todo")
            "* TODO [Code] %^{Title}\nDEADLINE: %^t\nCREATED: %u\nAnnotation: %a\n%?\n\n")

           ("n" "Generic Note" entry (file+headline (concat org-directory "/notes.org") "Notes")
            "* NOTE %?\n%u\nannotation:%a\nx:%x\n")

           ("L" "Mac Link Note" entry (file+headline (concat org-directory "/notes.org") "Notes")
            "* NOTE %?\n%u\n%(org-mac-safari-get-frontmost-url)\n")

           ("s" "Status" entry (file+datetree (concat org-directory "/status.org"))
            "* NOTE %?\n%u\n")

           ("x" "Tramdose 100mg" entry (file+datetree (concat org-directory "/tramadol.org") "Tramadol")
            "* NOTE 100mg\nCreated: %U\nPain Level: 3-4" :immediate-finish)

           ("y" "Advil dose 200mg" entry (file+datetree (concat org-directory "/advil.org") "Advil")
            "* NOTE 200mg\nCreated: %U\nPain Level: 1-2" :immediate-finish)

           ("X" "Tramdose" entry (file+datetree (concat org-directory "/medicine.org") "Tramadol")
            "* NOTE %?\nCreated: %U\nPain Level: 3")

           ("g" "Google Calendars")
           ("gh" "Todo" entry (file (concat org-directory "/goog-home.org"))
            "* TODO %?\n%T\nAnnotation: %a\n")

           ("gf" "Todo" entry (file (concat org-directory "/goog-family.org"))
            "* TODO %?\n%T\nAnnotation: %a\n")

           ("gw" "Todo" entry (file (concat org-directory "/goog-work.org"))
            "* TODO %?\n%T\nAnnotation: %a\n")


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
           ))

        (defun org-update-inline-images ()
          (when org-inline-image-overlays
            (org-redisplay-inline-images)))
        (add-hook 'org-babel-after-execute-hook 'org-update-inline-images)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Refile Or Delete Me, from my grpahiv file.
        (defun _graphviz/post-init-org ()
          (with-eval-after-load 'org
            (message "XXXRAN")
            (add-to-list 'org-src-lang-modes  '("dot" . graphviz-dot))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


        ;; not defined
        ;; (org-crypt-use-before-save-magic)

        ;; In mail map todo to mail-todo
        ;; org-capture-templates-contexts '(("t" "m" ((in-mode . "mu4e-headers-mode")))
        ;;                                  ("t" "m" ((in-mode . "mu4e-view-mode"))))
        (with-eval-after-load "ox"
          ;;
          (defconst chopps/org-latex-packages-alist-pre-hyperref
            '(
              ;; This conflicts with Beamer
              ;; ("letterpaper,margin=1.0in" "geometry")

              ;; Prevent an image from floating to a different location.
              ;; http://tex.stackexchange.com/a/8633/52678
              ("" "float")
              ;; % 0 paragraph indent, adds vertical space between paragraphs
              ;; http://en.wikibooks.org/wiki/LaTeX/Paragraph_Formatting
              ("" "parskip"))
            "Alist of packages that have to be loaded before `hyperref'package is loaded ftp://ftp.ctan.org/tex-archive/macros/latex/contrib/hyperref/README.pdf ")

          ;; Need to only load geometry if not a beamer file
          ;; ,(not (eq org-latex-create-formula-image-program 'dvipng)))) ; snippet-flag

          (defconst chopps/org-latex-packages-alist-post-hyperref
            '(
              ;; Prevent tables/figures from one section to float into another section
              ;; http://tex.stackexchange.com/a/282/52678
              ("section" "placeins")
              ;; Graphics package for more complicated figures
              ("" "tikz")
              ("" "caption")
              ("" "adjustbox")
              ;;
              ;; Packages suggested to be added for previewing latex fragments
              ;; http://orgmode.org/worg/org-tutorials/org-latex-preview.html
              ("mathscr" "eucal")
              ("" "latexsym"))
            "Alist of packages that have to (or can be) loaded after `hyperref' package is loaded.")

          (defvar latex-minted-cachedir (concat temporary-file-directory
                                                (getenv "USER")
                                                "/.minted/\\jobname"))

          (add-to-list 'chopps/org-latex-packages-alist-post-hyperref
                       `(,(concat "cachedir=" ; options
                                  latex-minted-cachedir)
                         "minted" ; package
                         ;; If `org-latex-create-formula-image-program'
                         ;; is set to `dvipng', minted package cannot be
                         ;; used to show latex previews.
                         ,(not (eq org-latex-create-formula-image-program 'dvipng)))) ; snippet-flag

          ;; remove hyperref from default package list.
          (setq org-latex-default-packages-alist
                (delq (rassoc '("hyperref" nil) org-latex-default-packages-alist)
                      org-latex-default-packages-alist))

          ;; This is no longer needed
          (setq org-latex-default-packages-alist
                (delq (rassoc '("fixltx2e" nil) org-latex-default-packages-alist)
                      org-latex-default-packages-alist))

          ;; Setup package list with correct placement of hyperref
          (setq org-latex-packages-alist
                (append chopps/org-latex-packages-alist-pre-hyperref
                        '(("" "hyperref" nil))
                        chopps/org-latex-packages-alist-post-hyperref))
          )

        )


      ;; Languages to interpret in begin_src blocks
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((ditaa . t)
         (emacs-lisp . t)
         (dot . t)
         (gnuplot . t)
         (latex . t)
         (pic . t)
         (plantuml . t)
         (python . t)
         (sh . t)
         ;; this can't be evaluated. (yang . nil)
         )
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
      ;; (bind-key "C-c w" 'org-refile-to-datetree)
      ;; (bind-key "M-h" 'org-metaleft org-mode-map)
      ;; (bind-key "M-l" 'org-metaright org-mode-map)
      ;; (bind-key "M-k" 'org-metaup org-mode-map)
      ;; (bind-key "M-j" 'org-metadown org-mode-map)
      ;; (bind-key "M-H" 'org-shiftmetaleft org-mode-map)
      ;; (bind-key "M-L" 'org-shiftmetaright org-mode-map)
      ;; (bind-key "M-K" 'org-shiftmetaup org-mode-map)
      ;; (bind-key "M-J" 'org-shiftmetadown org-mode-map)

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
      (with-eval-after-load "org-caldav"
        ;;https://calendar.google.com/calendar/ical/j2nmb305oqb7n6428m4pf1rctk%40group.calendar.google.com/private-c46e82d6b4bae1f85fe4415a769d225b/basic.ics
        ;;https://calendar.google.com/calendar/ical/
        ;; USERNAME: j2nmb305oqb7n6428m4pf1rctk%40group.calendar.google.com/
        ;; PRIVATE: private-c46e82d6b4bae1f85fe4415a769d225b/basic.ics
        ;; ID: j2nmb305oqb7n6428m4pf1rctk@group.calendar.google.com
        (setq org-icalendar-timezone "US/Eastern"
              ;; org-caldav-url "https://www.google.com/calendar/dav"
              ;; org-caldav-calendar-id "naqenfju9vq9tr0r4nnh7eaiic@group.calendar.google.com"
              ;; org-caldav-inbox "/home/chopps/Dropbox/org-mode/goog-work.org"
              ;; org-caldav-files '()
              org-caldav-calendars
              '((:calendar-id "naqenfju9vq9tr0r4nnh7eaiic@group.calendar.google.com"
                              :url "https://www.google.com/calendar/dav"
                              :files ()
                              :inbox "/home/chopps/Dropbox/org-mode/goog-work.org")
                (:calendar-id "l8cjg3irk2h5a8gk5ch9mtp6ls@group.calendar.google.com"
                              :url "https://www.google.com/calendar/dav"
                              :files ()
                              :inbox "/home/chopps/Dropbox/org-mode/goog-family.org")
                ;;(:calendar-id "f1jltqbvdp88o8htcjkbg920sc@group.calendar.google.com"
                ;;              :files ()
                ;;              :inbox "~/org/goog-home.org")
                ;;(:calendar-id "v8eda33vlrn98c9oj2hefjld7s@group.calendar.google.com"
                ;;              :files ()
                ;;              :inbox "~/org/goog-ietf.org")
                ;; (:calendar-id "v8eda33vlrn98c9oj2hefjld7s@group.calendar.google.com"
                ;;               :files ()
                ;;               :inbox "/home/chopps/Dropbox/org-mode/goog-ietf.org")
                )
              )
        ;; (setq org-caldav-principal-url "https://p25-caldav.icloud.com/65837734/principal"
        ;;       org-caldav-url "https://p25-caldav.icloud.com/65837734/calendars"
        ;;       org-caldav-calendar-id "AF7013C4-D5A4-4885-BF8B-0B11FB3A1488"
        ;;       org-caldav-inbox "/home/chopps/org/orgmode-caldav.org"
        ;;       org-caldav-files '()
        ;;       org-icalendar-timezone "US/Eastern"))

        )


      (when (configuration-layer/layer-usedp 'org2blog)
        (with-eval-after-load "org2blog"
          ;; (defadvice org-wp-src-block (after ad-org-wp-src-block activate)
          ;;   "Always use space as title if none given"
          ;;   (setq ad-return-value (replace-regexp-in-string "title=\"\"" "title=\" \"" ad-return-value)))
          ;; (ad-activate 'org-wp-src-block)

          (setq org2blog/wp-use-sourcecode-shortcode t)
          )
        )

      )

    ;; XXX need to change this
    (when (or (daemonp) (server-running-p))
      (require 'org-notify)
      (defun org-notify-action-notify-urgency (plist)
        "Pop up a notification window."
        (message "XXX org-notify-action-notify-urgency enter")
        (require 'notifications)
        (let* ((duration (plist-get plist :duration))
               (urgency (pilst-get plist :urgency))
               (id (notifications-notify
                    :title     (plist-get plist :heading)
                    :body      (org-notify-body-text plist)
                    :urgency   (or urgency 'normal)
                    :timeout   (if duration (* duration 1000))
                    :actions   org-notify-actions
                    :on-action 'org-notify-on-action-notify)))
          (setq org-notify-on-action-map
                (plist-put org-notify-on-action-map id plist))
          (message "XXX org-notify-action-notify-urgency exit")
          ))

      (org-notify-add '('default
                         :time "1h"
                         :period "30m"
                         :duration 0
                         :urgency 'critical
                         :app-icon (concat (configuration-layer/get-layer-path 'org)
                                           "img/org.png")
                         :actions 'org-notify-action-notify-urgency))
      (org-notify-start)
      )

    ;; ====
    ;; Evil
    ;; ====

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
                  )

    (defun persp-add-buffers-to-all ()
      (progn
        (message "Adding buffers")
        (persp-add-buffer "*Messages*")
        (persp-add-buffer "*scratch*")))

    (add-hook 'persp-activated-hook 'persp-add-buffers-to-all)

    (autoload 'yang-mode "yang-mode")
    (add-to-list 'auto-mode-alist '("\\.yang\\'" . yang-mode))

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

                  (setq work-ai-prefix "/.*/\\(?:\\(?:Documents|Dropbox\\)/[Ww]ork\\|chopps/w\\)/.*/")

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

    ;; (setq split-width-threshold 100)
    ;; (setq window-min-width 40)
    ;; (setq split-window-preferred-function 'split-window-sensibly-prefer-horizontal)


    (require 'list-timers)
    (evil-set-initial-state 'timers-menu-mode 'insert)
    )
  )

 ;; '(font-lock-comment-delimiter-face ((t (:foreground "grey33"))))
 ;; '(font-lock-comment-face ((t (:foreground "DarkGrey" :slant italic)))))
 ;; '(variable-pitch ((t (:family "DejaVu Sans Mono")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "cornflowerblue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "grey33"))))
 '(font-lock-comment-face ((t (:foreground "DarkGrey" :slant italic))))
 '(irfc-head-name-face ((t (:inherit org-level-1))))
 '(irfc-head-number-face ((t (:inherit org-level-1))))
 '(irfc-rfc-link-face ((t (:inherit org-link)))))

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
 '(evil-want-Y-yank-to-eol nil)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (jabber fsm go-guru sourcerer-theme insert-shebang hide-comnt helm-purpose window-purpose imenu-list pug-mode magit-gh-pulls github-search github-clone github-browse-file gist gh marshal logito pcache zonokai-theme zenburn-theme zen-and-art-theme yapfify yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit systemd sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stickyfunc-enhance stekene-theme srefactor spacemacs-theme spaceline powerline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode reverse-theme reveal-in-osx-finder restart-emacs rebox2 ranger rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme professional-theme popwin planet-theme pip-requirements phpunit phpcbf php-extras php-auto-yasnippets phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode persistent-scratch pdf-tools tablist pcre2el pbcopy pastels-on-dark-theme paradox spinner osx-trash osx-dictionary orgit organic-green-theme org2blog org-projectile org-present org-pomodoro org-plus-contrib org-download org-caldav org org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mwim mustang-theme multi-term mu4e-alert ht alert log4e gntp move-text monokai-theme monochrome-theme monky molokai-theme moe-theme mmm-mode minimal-theme metaweblog xml-rpc material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow macrostep lush-theme lua-mode lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint light-soap-theme less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme jade-mode irfc ir-black-theme inkpot-theme info+ indent-guide ietf-docs ido-vertical-mode hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-gtags helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme graphviz-dot-mode grandshell-theme gotham-theme google-translate golden-ratio go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md ggtags gandalf-theme flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck pkg-info epl flx-ido flx flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight espresso-theme eshell-z eshell-prompt-extras esh-help erc-social-graph erc-image erc-hl-nicks emmet-mode elisp-slime-nav dumb-jump drupal-mode php-mode dracula-theme dockerfile-mode django-theme disaster diminish darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-go go-mode company-c-headers company-auctex company-anaconda company column-enforce-mode colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmake-mode clues-theme clean-aindent-mode clang-format cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-map bind-key badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed auctex apropospriate-theme anti-zenburn-theme anaconda-mode pythonic f dash s ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup quelpa package-build)))
 '(safe-local-variable-values
   (quote
    ((docker-image-name . "hyperv")
     (evil-shift-width . 2)
     (eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "make test" projectile-test-cmd-map))
     (org-confirm-babel-evaluate)
     (eval find-and-close-fold "\\((fold-section \\|(spacemacs|use\\|(when (configuration-layer\\)"))))
 '(send-mail-function (quote smtpmail-send-it)))

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  )
;; Local Variables:
;; eval: (find-and-close-fold "\\((fold-section \\|(spacemacs|use\\|(when (configuration-layer\\)")
;; End:
