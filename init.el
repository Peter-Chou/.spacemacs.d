;; -*- mode: emacs-lisp -*-
;; set memory trash colletion threshold to 150mb to speed up the startup time
;; (setq gc-cons-threshold 120000000)

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
     (python :variables
             python-test-runner '(pytest nose)
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
     ;; need to install shellcheck to make linting works
     ;; https://github.com/koalaman/shellcheck
     shell-scripts
     semantic
     imenu-list
     ;; ipython-notebook
     ;; chrome
     ivy
     ;; helm
     ranger
     ;; (spacemacs-layouts :variables layouts-enable-autosave nil
     ;;                    layouts-autosave-delay 300)
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      :disabled-for org markdown)
     ;; ycmd
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
     ;; (colors :variables
     ;;         colors-enable-nyan-cat-progress-bar (display-graphic-p))
     ;; spell-checking

     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(websocket
                                      request
                                      request-deferred
                                      dash
                                      s
                                      skewer-mode
                                      smartrep
                                      all-the-icons
                                      beacon
                                      default-text-scale
                                      gruvbox-theme)
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
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         gruvbox-dark-hard
                         gruvbox-light-soft
                         ;; solarized-light
                         ;; subatomic256
                         ;; flatland
                         ;; misterioso
                         ;; gruber-darker
                         ;; spacemacs-dark
                         ;; spacemacs-light
                         ;; spacemacs-light
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("CamingoCode"
                               :size 16.5
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
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
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
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
   dotspacemacs-auto-save-file-location 'nil
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
   dotspacemacs-which-key-delay 0.01
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
   dotspacemacs-maximized-at-startup t
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
   dotspacemacs-persistent-server t
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
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; customize the startup-banner image.
  (setq-default dotspacemacs-startup-banner '"~/.spacemacs.d/images/starship_troopers_warship.png")
  ;; redirect the download site to domestic resource site
  (setq configuration-layer--elpa-archives
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
  (load-file "~/.spacemacs.d/dash.el")
  (load-file "~/.spacemacs.d/autothemer.el")
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
  ;; (setenv "WORKON_HOME" "C:/Anaconda3/envs")
  ;; speed up the start-up time
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  ;; utf-8-unix save file only has \n instead of (\r\n in windows)
  (set-buffer-file-coding-system 'utf-8-unix)

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  ;; (setq exec-path (append exec-path '("C:/global/bin")))
  ;; activate abbrev-mode in emacs-lisp-mode and text-mode
  ;; (setq-default abbrev-mode t)
  ;; (setq system-uses-terminfo nil)

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
  (add-hook 'python-mode-hook (lambda ()
                                (highlight-indentation-mode 1)
                                (highlight-indentation-current-column-mode 1)))

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
  ;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))
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

  (advice-add 'semantic-analyze-completion-at-point-function :around #'semantic-completion-advice)

  ;; customize mode-line ui from https://github.com/zilongshanren/spacemacs-private/blob/develop/layers/zilongshanren-ui/funcs.el
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ayu-theme traad virtualenvwrapper insert-shebang fish-mode company-shell flycheck-ycmd company-ycmd ycmd imenu-list company-quickhelp ob-ipython seq white-sand-theme rebecca-theme org-mime exotica-theme default-text-scale gruvbox-dark-hard-theme colorsarenice-theme beacon yasnippet-snippets ranger all-the-icons memoize font-lock+ dracula-theme birds-of-paradise-plus-theme evil-commentary company-jedi smartrep jedi jedi-core python-environment epc ctable concurrent helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag ace-jump-helm-line gruber-theme alect-theme material-dark-theme color-theme-sanityinc-tomorrow-theme try darcula-theme stickyfunc-enhance srefactor web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode haml-mode emmet-mode company-web web-completion-data org2ctex zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rainbow-mode rainbow-identifiers railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme espresso-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help ibuffer-projectile gmail-message-mode ham-mode html-to-markdown flymd edit-server vmd-mode company-auctex auctex sql-indent centered-window-mode minimap sublimity ein skewer-mode request-deferred websocket deferred js2-mode simple-httpd disaster company-c-headers cmake-mode clang-format unfill smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-download mwim mmm-mode markdown-toc markdown-mode magit-gitflow htmlize gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md evil-magit magit magit-popup git-commit ghub let-alist with-editor fuzzy flyspell-correct-ivy flyspell-correct flycheck-pos-tip pos-tip flycheck company-statistics company-anaconda company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional cython-mode anaconda-mode pythonic evil-unimpaired ws-butler winum which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smex restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-make helm helm-core google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump popup f dash s diminish define-word counsel-projectile projectile pkg-info epl counsel swiper ivy column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed async aggressive-indent adaptive-wrap ace-window ace-link avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) nil) (((class color) (min-colors 89)) (:background "#1c1c1c" :foreground "#eeeeee"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 4.0))))
 '(whitespace-indentation ((t (:background nil))))
 '(whitespace-space ((t (:background nil))))
 '(whitespace-tab ((t (:background nil :bold t)))))
