;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation nil

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; helm
     ;; auto-completion
     ;; better-defaults
	 html
     (python :variables
     		 python-backend 'anaconda
     		 python-fill-column 80
             python-test-runner 'pytest
             python-sort-imports-on-save t)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     emacs-lisp
     sql
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 40)
     shell-scripts
     semantic
     imenu-list
     neotree
     ivy
     ranger
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      :disabled-for org markdown)

     (better-defaults :variables
                      better-defaults-move-to-end-of-code-first t)
     evil-commentary
     (vinegar :variables
              vinegar-reuse-dired-buffer t)
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     (org :variables
          org-want-todo-bindings t)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     ;; git
     ;; markdown

     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(websocket
                                      request
                                      request-deferred
                                      dash
                                      s
                                      skewer-mode
                                      smartrep
                                      all-the-icons
                                      all-the-icons-dired
                                      all-the-icons-ivy
                                      beacon
                                      default-text-scale
                                      zenburn-theme
                                      gruvbox-theme
                                      flymd
                                      zeal-at-point)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    ;; vi-tilde-fringe
                                    firebelly-theme
                                    niflheim-theme
                                    pastels-on-dark-theme
                                    tronesque-theme
                                    zonokai-theme)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

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

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

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
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         ;; ayu
                         ;; zenburn
                         gruvbox-light-medium
                         gruvbox-dark-soft
                         ;; spacemacs-dark
                         ;; spacemacs-light
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("monaco"
                               :size 17.5
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

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
   dotspacemacs-major-mode-leader-key "\'"

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

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
   dotspacemacs-auto-save-file-location 'nil

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.01

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
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
   dotspacemacs-fullscreen-at-startup t

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
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

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
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

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

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
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; customize the startup-banner image.
  (setq-default dotspacemacs-startup-banner '"~/.spacemacs.d/images/matcha.png")
  ;; redirect the download site to domestic resource site
  (setq configuration-layer-elpa-archives
    '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
      ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
      ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

    (defconst spacemacs-buffer-logo-title "[  E M B R A C I N G   E M A C S  ⋮⋮⋮  H A C K I N G   S P A C E M A C S  ]"
    "The title displayed beneath the logo.")

    ;; fix the loading theme problem when themes require dash & autothemer package be loaded.
  ;; solution from https://github.com/syl20bnr/spacemacs/issues/8090
  ;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;; get dash package
  ;; wget https://raw.githubusercontent.com/magnars/dash.el/master/dash.el
  ;; get autothemer.el
  ;; wget https://raw.githubusercontent.com/sebastiansturm/autothemer/master/autothemer.el
  (add-to-list 'load-path "~/.spacemacs.d/lisp/")
  (require 'dash)
  (require 'autothemer)

  ;; set python-environment path to anaconda virtual environment folder
  ;; make sure the your_env_path/Scripts/ do have pythonw.exe (windows) python.exe (unix)
  ;; in cmd with sudo type the following command:(py27 is the folder holds my python2.7)
  ;; mklink /h C:\Anaconda3\envs\py27\Scripts\pythonw.exe C:\Anaconda3\envs\py27\pythonw.exe
  ;; create a new ./Scripts/pythonw.exe has a hard link with ./pythonw.exe
  ;; with the above steps you can have auto-completion,
  ;; set c/c++ default style to Allman
  ;; https://www.emacswiki.org/emacs/IndentingC
  (setq-default c-default-style "bsd")
  (setq-default c-basic-offset 4)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode t)
  ;; eldoc and so on in your virtual environment
  (setenv "WORKON_HOME" "C:/Anaconda3/envs")
  ;; speed up the start-up time
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  ;; utf-8-unix save file only has \n instead of (\r\n in windows)
  ;; (set-buffer-file-coding-system 'utf-8-unix)
  (add-to-list 'file-coding-system-alist '("\\.py" . utf-8-unix))

  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;; (setq exec-path (append exec-path '("C:/global/bin")))
  ;; activate abbrev-mode in emacs-lisp-mode and text-mode
  ;; (setq-default abbrev-mode t)
  ;; (setq system-uses-terminfo nil)
  ;; flymd configuration for markdown
  ;; use flymd-flyit to open brower (firefox)

  (spacemacs/set-leader-keys "oh" 'zeal-at-point)
  ;; (global-set-key "\C-cd" 'zeal-at-point)
  (add-hook 'python-mode-hook
            (lambda () (setq zeal-at-point-docset '("python" "django" "twisted" "sphinx" "flask" "tornado" "sqlalchemy" "numpy" "scipy" "pandas" "matplotlib" "salt" "cvp"))))
  ;; use tab -> select completion, C-n -> select next, C-p -> select previous in company mode
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)
    (define-key company-active-map (kbd "TAB") #'company-complete-selection)
    (define-key company-active-map [tab] #'company-complete-selection)
    )

  (setq flymd-close-buffer-delete-temp-files t)
  (setq flymd-refresh-interval 0.1)
  (add-hook 'markdown-mode-hook (lambda ()
                                  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
                                    "cP" 'flymd-flyit)))

  ;; https://www.emacswiki.org/emacs/font-lock+.el
  (require 'font-lock+)
  (all-the-icons-ivy-setup)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  ;; 代码折叠
  ;; (add-hook 'prog-mode-hook 'hs-minor-mode)
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (defun my-level-fold-1 ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (outline-hide-sublevels 1)))
  (defun my-level-fold-2 ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (outline-hide-sublevels 2)))
  (defun my-level-fold-3 ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (outline-hide-sublevels 3)))

  (spacemacs/declare-prefix "of" "code-(un)folding")
  ;; spc-o-f-U : 展开全部代码
  (spacemacs/set-leader-keys "of0" 'outline-show-all)
  ;; spc-o-f-1 : 折叠所有1层代码
  (spacemacs/set-leader-keys "of1" 'my-level-fold-1)
  ;; spc-o-f-2 : 折叠所有2层代码
  (spacemacs/set-leader-keys "of2" 'my-level-fold-2)
  ;; spc-o-f-3 : 折叠所有3层代码
  (spacemacs/set-leader-keys "of3" 'my-level-fold-3)
  ;; spc-o-f-f : 折叠该节点代码
  (spacemacs/set-leader-keys "off" 'outline-hide-subtree)
  ;; spc-o-f-u : 展开该节点代码
  (spacemacs/set-leader-keys "ofu" 'outline-show-subtree)
  ;; spc-o-f-l : 折叠盖层的子层代码
  (spacemacs/set-leader-keys "ofl" 'outline-hide-leaves)
  ;; spc-o-f-o : 折叠所在代码以外的代码
  (spacemacs/set-leader-keys "ofo" 'outline-hide-other)

  ;; highlight the indentation in python mode
  ;; (require 'highlight-indentation)

  (defun my-pipenv-workon ()
    "switch python virtualenvironment and restart anaconda server"
    (interactive)
    (call-interactively 'pyvenv-workon)
    (setq python-shell-virtualenv-path pyvenv-virtual-env))

  (defun my-pipenv-activate ()
    "switch python virtualenvironment and restart anaconda server"
    (interactive)
    (call-interactively 'pyvenv-activate)
    (setq python-shell-virtualenv-path pyvenv-virtual-env))

  (defun my-pipenv-deactivate ()
    "deactivate pyvenv & anaconda virtual enironment"
    (interactive)
    (pyvenv-deactivate)
    (pythonic-deactivate))

  (add-hook 'python-mode-hook (lambda ()
                                (highlight-indentation-mode 1)
                                (highlight-indentation-current-column-mode 1)
                                ;; (spacemacs/set-leader-keys "mgb" 'xref-pop-marker-stack)
                                ;; fix anaconda-mode use xref issue
                                (use-package anaconda-mode
                                  :defer t
                                  :init
                                  (progn
                                    (setq anaconda-mode-installation-directory
                                          (concat spacemacs-cache-directory "anaconda-mode"))
                                    (add-hook 'python-mode-hook 'anaconda-mode)
                                    (add-to-list 'spacemacs-jump-handlers-python-mode
                                                 '(anaconda-mode-find-definitions :async t)))
                                  :config
                                  (progn
                                    (spacemacs/set-leader-keys-for-major-mode 'python-mode
                                      "hh" 'anaconda-mode-show-doc
                                      "ga" 'anaconda-mode-find-assignments
                                      "gb" 'xref-pop-marker-stack  ;; remove anaconda-mode-go-back
                                      "gu" 'anaconda-mode-find-references
                                      "Va" 'my-pipenv-activate
                                      "Vd" 'my-pipenv-deactivate
                                      "Vw" 'my-pipenv-workon)

                                    (evilified-state-evilify anaconda-view-mode anaconda-view-mode-map
                                      (kbd "q") 'quit-window)

                                    (spacemacs|hide-lighter anaconda-mode)

                                    (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
                                      (evil--jumps-push))))
                                ))

  ;; set c/c++ tab width to 4 whitespaces
  (add-hook 'c-mode-common-hook (lambda ()
                                  (highlight-indentation-mode 1)
                                  (highlight-indentation-current-column-mode 1)
                                  (setq-local tab-width 4)))
  ;; (set-face-background 'highlight-indentation-face "#e3e3d3")
  ;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

  (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)

  (dolist (hook '(emacs-lisp-mode-hook
                  text-mode-hook))
    (add-hook hook #'abbrev-mode))
  (define-abbrev-table 'global-abbrev-table '(
                                              ;; signature
                                              ("9zm" "peterchou")
                                              ))
  ;; make new frame fullscreen as default
  ;; use spc-T-F to manually turn off this feature.
  (add-to-list 'default-frame-alist '(fullscreen . fullboth))
  ;; revert the buffer automatically when the filed is modified outside emcas
  (global-auto-revert-mode t)
  ;; toggle off the minor-mode on modeline as default
  (spacemacs/toggle-mode-line-minor-modes-off)
  ;; Use Spacemacs as the $EDITOR for git commits
  ;; (global-git-commit-mode t)
  ;; show time on powerline
  ;; copy from https://github.com/syl20bnr/spacemacs/issues/9458
  (setq display-time-24hr-format t)
  (setq display-time-format "%H:%M:%S")        ; add seconds
  (setq display-time-interval 1)               ; update every second
  (setq display-time-default-load-average nil) ; don't show load average
  (setq display-time-mail-string "")           ; don't show mail
  (display-time-mode 1)                        ; show time in mode line on startup
  ;; disable creating lockfiles.
  (setq create-lockfiles nil)
  ;; set yasnippet snippet directory from github
  ;; https://github.com/AndreaCrotti/yasnippet-snippets
  (setq yas-snippet-dirs
        '("~/.spacemacs.d/yasnippet-snippets/snippets"
          ))
  ;; Prevent the visual selection overriding my system clipboard
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; activate global beacon-mode which acts
  ;; Whenever the window scrolls a light will shine on top of your curso
  ;; binding C-M-- to decrease font size, C-M-=to increase font size
  ;; and change the font size globally within default-text-scale-mode
  (default-text-scale-mode 1)

  ;; try to make company completion quick.
  ;; if still not good. use C-M-i to manually open the completion list
  (setq company-dabbrev-downcase 0)
  ;; (setq company-idle-delay 0)
  ;; start autocomplete when start typing
  ;; (setq-default company-minimum-prefix-length 1)
  ;; (setq-default auto-completion-complete-with-key-sequence-delay 0.001)

  ;; so you know where it is
  (beacon-mode 1)
  ;; enable anzu-mode in mode-line
  (anzu-mode +1)
  ;; set emcas runing as deamon,
  ;; and rebinding spc q q to frame-killer to keep deamon alive
  ;; still can kill emacs using spc q Q shortcuts
  (server-start)
  ;; binding spc q q to kill the frame and keep buffer alive
  (evil-leader/set-key "qq" 'spacemacs/frame-killer)
  (evil-leader/set-key "qh" 'suspend-frame)
  ;; save buffer and kill the frame
  (defun spacemacs/save-buffer-and-kill-frame ()
    "kill the current buffer and the current frame"
    (interactive)
    (save-buffer)
    (kill-buffer)
    (spacemacs/frame-killer))
  ;; binding spc q w to save-buffer-and-kill-frame function
  (evil-leader/set-key "qw" 'spacemacs/save-buffer-and-kill-frame)
  ;; set new-tree icons
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  ;; active the function ',' in vim (repeat f,F,t,T in reverse)
  (define-key evil-normal-state-map
    "," 'evil-repeat-find-char-reverse)
  ;; customize some settings on ace-window packages
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 4.0)))))
  ;; fix the align problem between chinese/english in org table
  (when (configuration-layer/layer-usedp 'chinese)
    (when (and (spacemacs/system-is-mac) window-system)
      (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))
  ;; fix the delay when showing text in chinese
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Microsoft Yahei" :size 20)))
  ;; highlight the selected line
  (global-hl-line-mode t)
  ;; set backspace to clear highlight in normal mode.
  (define-key evil-normal-state-map
    (kbd "<backspace>") 'spacemacs/evil-search-clear-highlight)
  ;; set the appearance of menu bar as arrow shape
  (setq powerline-default-separator 'arrow)
  ;; activate hungry delete mode
  (global-hungry-delete-mode t)
  ;; fix the issue that Deleting left of empty smartparen pair doesn't delete right when hungry-delete is enabled
  ;; https://github.com/syl20bnr/spacemacs/issues/6584
  (defadvice hungry-delete-backward (before sp-delete-pair-advice activate) (save-match-data (sp-delete-pair (ad-get-arg 0))))
  ;; set fd to escape evil mode in 0.3
  (setq-default evil-escape-delay 0.3)
  ;;set up fly-check to ignore the E501 error
  (setq-default flycheck-flake8-maximum-line-length 160)
  ;; tunning the dired-mode
  ;; always take recursive action without further permission
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  ;; force horizontal split window
  (setq split-width-threshold 120)
  ;; activate org mode to markdown choice
  (with-eval-after-load "org" '(require 'ox-md nil t))
  ;; fix problem of exporting chinese pdf
  ;; add the following lines on the top of your org file
  ;; you could change font Maison Neue to other fonts
  ;; #+LaTeX_HEADER: \usepackage{fontspec}
  ;; #+LATEX_HEADER: \setmonofont[Scale=0.9]{Input Mono}
  ;; #+LATEX_HEADER: \setromanfont{Maison Neue}
  ;; #+LATEX_HEADER: \linespread{1.5}
  ;; #+LATEX_HEADER: \usepackage[margin=1.25in]{geometry}
  ;; #+TITLE: Document Title Here
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                                "xelatex -interaction nonstopmode %f"))

  ;;Don't ask me when kill process buffer
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  ;; fix the malfunction of c-e in better-defaults
  (define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-motion-state-map (kbd "C-e") 'mwim-end-of-code-or-line)

  ;; if file exceed 500kb, it will be opened in fundamental-mode to speed up the loading
  (defun spacemacs/check-large-file ()
    (when (> (buffer-size) 500000)
      (progn (fundamental-mode)
             (hl-line-mode -1)))
    (if (and (executable-find "wc")
             (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
                5000))
        (linum-mode -1)))

  (add-hook 'find-file-hook 'spacemacs/check-large-file)

  ;; https://emacs-china.org/t/ranger-golden-ratio/964/2
  ;; enable ranger function with disable the golden ratio mode
  ;; set quit funtion to key q in normal mode
  (defun my-ranger ()
    (interactive)
    (if golden-ratio-mode
        (progn
          (golden-ratio-mode -1)
          (ranger)
          (setq golden-ratio-previous-enable t))
      (progn
        (ranger)
        (setq golden-ratio-previous-enable nil))))

  (defun my-quit-ranger ()
    (interactive)
    (if golden-ratio-previous-enable
        (progn
          (ranger-close)
          (golden-ratio-mode 1))
      (ranger-close)))

  (with-eval-after-load 'ranger
    (progn
      (define-key ranger-normal-mode-map (kbd "q") 'my-quit-ranger)))

  (spacemacs/set-leader-keys "ar" 'my-ranger)

  ;; Do not show ^M in files containing mixed UNIX and DOS line endings.
  (defun hidden-dos-eol ()
    (interactive)
    (unless buffer-display-table
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^M []))
  ;; Replace DOS eolns CR LF with Unix eolns CR
  (defun remove-dos-eol ()
    (interactive)
    (goto-char (point-min))
    (while (search-forward "\r" nil t) (replace-match "")))
  ;; set eol features under SPC-o-d key binding.
  (spacemacs/declare-prefix "od" "doc-EOF")
  ;; set spc-o -d-h to hide the ^M
  (spacemacs/set-leader-keys "odh" 'hidden-dos-eol)
  ;; set spc-o-d-d to delete the ^M
  (spacemacs/set-leader-keys "odd" 'remove-dos-eol)
  ;; set spc-o-w to cleaning the whitespace
  (spacemacs/set-leader-keys "ow" 'whitespace-cleanup)

  ;; show indent level within vertical ellipsis 「⋮」 ===> useful in python mode.
  ;; from http://www.wilkesley.org/~ian/xah/emacs/whitespace-mode.html
  ;; just show tab mark with basic coloring
  ;;(setq whitespace-style (quote (spaces tabs newline tab-mark)))
  ;;show all marks with basic coloring
  ;;(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
  (setq whitespace-display-mappings
        ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
        '(
          ;; (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          ;; (newline-mark 10 [9166 10]) ; 10 LINE FEED 9166 --	RETURN SYMBOL 「⏎」
          (tab-mark 9 [8942 9] [92 9]) ; 9 TAB, 8942 vertical ellipsis 「⋮」
          ))
  ;; tabify indent space to tabs in this buffer, vice versa.
  ;; this switch won't affect whitespaces / tab not maching "^ +"" or "^\t+" regular expression.
  (defun peterchou/leading-space-tab-switcher ()
    (interactive)
    (let ((space-indent-count (how-many "^ +" (point-min) (point-max)))
          (tab-indent-count (how-many "^\t+" (point-min) (point-max))))
      (if (> tab-indent-count space-indent-count)
          (progn
            (setq indent-tabs-mode nil)
            (save-excursion
              (beginning-of-buffer)
              (while (> tab-indent-count 0)
                (let ((end-char-location (re-search-forward "^\t+" nil t))
                      (first-char-location (re-search-backward "^\t+" nil t)))
                  (untabify first-char-location end-char-location))
                (setq-local tab-indent-count (- tab-indent-count 1))))
            (message " * untabify this buffer --> now is leading-SPACE-indent *"))
        (progn
          (setq indent-tabs-mode t)
          (save-excursion
            (beginning-of-buffer)
            (while (> space-indent-count 0)
              (let ((end-char-location (re-search-forward "^ +" nil t))
                    (first-char-location (re-search-backward "^ +" nil t)))
                (tabify first-char-location end-char-location))
              (setq-local space-indent-count (- space-indent-count 1))))
          (message " * tabify this buffer --> now is leading-TAB-indent *")))))
  ;; set bindings under SPC-o-i maping
  (spacemacs/declare-prefix "ot" "toggles")
  (spacemacs/set-leader-keys "ott" 'peterchou/leading-space-tab-switcher)

  ;; enable whitespace by default
  (spacemacs/toggle-whitespace-globally-on)
  (setq-default whitespace-line-column 160)

  ;; activate cygwin-terminal from emacs
  (defun open-cygwin-mintty-terminal ()
    (interactive)
    (progn
      (shell-command "mintty --window=max &")
      (delete-window)))
  (spacemacs/declare-prefix "oc" "cygwin-terminal")
  (spacemacs/set-leader-keys "oco" 'open-cygwin-mintty-terminal)

  ;; make emacs knows cygwin path(e.g. /cygdrive/c/ <==> c:/)
  ;; http://www.khngai.com/emacs/cygwin.php
  (when (eq system-type 'windows-nt)
    (setq system-uses-terminfo nil)
    (setq exec-path (cons "c:/cygwin/bin/" exec-path))
    (setq cygwin-mount-cygwin-bin-directory "c:/cygwin/bin/")
    (require 'cygwin-mount)
    (cygwin-mount-activate)
    (add-hook 'comint-output-filter-functions
              'shell-strip-ctrl-m nil t)
    (add-hook 'comint-output-filter-functions
              'comint-watch-for-password-prompt nil t)
    (setq explicit-shell-file-name "bash.exe")
                                        ; For subprocesses invoked via the shell
                                        ; (e.g., "shell -c command")
    (setq shell-file-name explicit-shell-file-name)
    (spacemacs/set-leader-keys "oc'" 'shell))

  (defun peterchou-clear-shell-buffer ()
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (spacemacs/set-leader-keys "occ" 'peterchou-clear-shell-buffer)

  ;; fix the problem of parsing tons of .el files when typing in elisp mode.
  ;; https://github.com/company-mode/company-mode/issues/525
  (defun semantic-completion-advice (adviced-f &rest r)
    "Check if POINT it's inside a string or comment before calling semantic-*"
    (if (or (inside-string-q) (inside-comment-q))
        (not (message "Oleeee! do not call function, we're inside a string or comment!"))
      (apply adviced-f r)))

  ;; fix elisp complete problem
  ;; (advice-add 'semantic-analyze-completion-at-point-function :around #'semantic-completion-advice)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme zeal-at-point yasnippet-snippets yapfify xterm-color ws-butler winum wgrep websocket web-mode web-beautify volatile-highlights vmd-mode vi-tilde-fringe uuidgen unfill toc-org tagedit symon string-inflection stickyfunc-enhance srefactor sql-indent fish-mode which-key use-package spaceline-all-the-icons smex smeargle smartrep slim-mode skewer-mode shell-pop scss-mode sass-mode restart-emacs request-deferred ranger rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode popwin pippel pipenv pip-requirements persp-mode pcre2el password-generator paradox overseer orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file neotree nameless mwim multi-term move-text mmm-mode markdown-toc magit-svn magit-gitflow macrostep lorem-ipsum live-py-mode link-hint ivy-yasnippet ivy-xref ivy-rtags ivy-purpose ivy-hydra insert-shebang indent-guide importmagic impatient-mode ibuffer-projectile hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-make gruvbox-theme google-translate google-c-style golden-ratio gnuplot gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy font-lock+ flymd flycheck-rtags flycheck-pos-tip flycheck-bashate flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-commentary evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav editorconfig dumb-jump dotenv-mode disaster diminish define-word default-text-scale cython-mode counsel-projectile counsel-css company-web company-statistics company-shell company-rtags company-c-headers company-anaconda column-enforce-mode clean-aindent-mode clang-format centered-cursor-mode beacon auto-yasnippet auto-highlight-symbol auto-compile all-the-icons-ivy all-the-icons-dired aggressive-indent ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) nil) (((class color) (min-colors 89)) (:background "#1c1c1c" :foreground "#eeeeee"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 4.0))))
 '(font-lock-function-name-face ((t (:bold t))))
 '(font-lock-keyword-face ((t (:bold t))))
 '(font-lock-type-face ((t (:bold t))))
 '(whitespace-indentation ((t (:background nil))))
 '(whitespace-space ((t (:background nil))))
 '(whitespace-tab ((t (:background nil :bold t))))
 )
)
