;;; packages.el --- peter-display layer packages file for Spacemacs.
;;
;; Copyright (c) 2018-2019 Peter Chou
;;
;; Author:  <Peter Chou>
;; URL: https://github.com/Peter-Chou/spacemacs-settings
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(setq peter-display-packages
      '(
        all-the-icons
        all-the-icons-ivy
        ;; (all-the-icons-dired :requires font-lock+)
        all-the-icons-dired
        company-box
        diredfl
        doom-modeline
        (font-lock+ :step pre
                    :location (recipe :fetcher github
                                      :repo emacsmirror/font-lock-plus))
        (prettify-utils :location (recipe :fetcher github
                                          :repo "Ilazki/prettify-utils.el"))
        nyan-mode
        treemacs-icons-dired
        treemacs-magit

        ;; Elsehwere-owned packages
        which-key
        eshell-prompt-extras

        vim-empty-lines-mode
        virtualenvwrapper  ;; needed by eshell-prompt-extras

        (pretty-code     :location local)
        (pretty-fonts    :location local)
        (pretty-magit    :location local)
        ))

(defun peter-display/init-all-the-icons ()
  (use-package all-the-icons
    :ensure t))

(defun peter-display/init-all-the-icons-ivy ()
  (use-package all-the-icons-ivy
    :config
    (progn
      ;; Fix icon prompt alignment in ivy prompts
      (advice-add 'all-the-icons-ivy-file-transformer :override
                  'all-the-icons-ivy-file-transformer-stdized)

      ;; Add behavior to counsel projectile funcs too
      (advice-add 'counsel-projectile-find-file-transformer :filter-return
                  'all-the-icons-ivy-file-transformer-stdized)
      (advice-add 'counsel-projectile-transformer :filter-return
                  'all-the-icons-ivy-file-transformer-stdized)

      (all-the-icons-ivy-setup)
      (setq all-the-icons-ivy-file-commands
            '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
      )))

(defun peter-display/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :if (eq dired-icons-backend 'all-the-icons)
    :defer t
    :hook (dired-mode . all-the-icons-dired-mode)
    :init
    (require 'font-lock+)))

(defun peter-display/init-company-box ()
  (use-package company-box
    :if (and (not (version< emacs-version "26.1")) (display-graphic-p))
    :diminish
    :functions (all-the-icons-faicon
                all-the-icons-material
                all-the-icons-octicon
                all-the-icons-alltheicon)
    :hook (company-mode . company-box-mode)
    :config
    (setq company-box-backends-colors nil)

    (with-eval-after-load 'all-the-icons
      (setq company-box-icons-unknown
            (all-the-icons-octicon "file-text" :v-adjust -0.05))

      (setq company-box-icons-elisp
            (list
             (all-the-icons-faicon "cube" :v-adjust -0.0575 :face 'font-lock-constant-face)       ; Function
             (all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-keyword-face)         ; Variable
             (all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'font-lock-warning-face)         ; Feature
             (all-the-icons-material "palette" :v-adjust -0.2)      ; Face
             ))

      (setq company-box-icons-yasnippet
            (all-the-icons-faicon "code" :v-adjust -0.0575))       ; Snippet

      (setq company-box-icons-lsp
            `(( 1  . ,(all-the-icons-faicon "file-text-o" :v-adjust -0.0575))     ; Text
              ( 2  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575 :face font-lock-constant-face))            ; Method
              ( 3  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575 :face font-lock-constant-face))            ; Function
              ( 4  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575 :face font-lock-constant-face))            ; Constructor
              ( 5  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-warning-face))             ; Field
              ( 6  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-warning-face))             ; Variable
              ( 7  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'font-lock-warning-face))             ; Class
              ( 8  . ,(all-the-icons-faicon "cogs" :v-adjust -0.0575))            ; Interface
              ( 9  . ,(all-the-icons-alltheicon "less"))                          ; Module
              (10  . ,(all-the-icons-faicon "wrench" :v-adjust -0.0575))          ; Property
              (11  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Unit
              (12  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-keyword-face))             ; Value
              (13  . ,(all-the-icons-faicon "file-text-o" :v-adjust -0.0575 :face 'font-lock-warning-face))     ; Enum
              (14  . ,(all-the-icons-material "format_align_center" :v-adjust -0.2))             ; Keyword
              (15  . ,(all-the-icons-material "content_paste" :v-adjust -0.2))    ; Snippet
              (16  . ,(all-the-icons-material "palette" :v-adjust -0.2))          ; Color
              (17  . ,(all-the-icons-faicon "file" :v-adjust -0.0575))            ; File
              (18  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Reference
              (19  . ,(all-the-icons-faicon "folder" :v-adjust -0.0575))          ; Folder
              (20  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-keyword-face))             ; EnumMember
              (21  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'font-lock-keyword-face))             ; Constant
              (22  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'font-lock-warning-face))             ; Struct
              (23  . ,(all-the-icons-faicon "bolt" :v-adjust -0.0575 :face 'font-lock-warning-face))            ; Event
              (24  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Operator
              (25  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'font-lock-warning-face))             ; TypeParameter
              ))
      )))

(defun peter-display/init-diredfl ()
  (use-package diredfl
    :init
    (diredfl-global-mode 1)))

(defun peter-display/init-doom-modeline ()
  (use-package doom-modeline
    :ensure t
    :hook ((after-init . doom-modeline-mode)
           (doom-modeline-mode . setup-custom-doom-modeline))
    :config
    (progn
      (setq
       find-file-visit-truename t  ; display the real names for symlink files
       ;; doom-modeline-height 21
       doom-modeline-lsp nil
       doom-modeline-persp-name nil
       doom-modeline-github nil
       doom-modeline-buffer-file-name-style 'truncate-with-project
       doom-modeline-major-mode-color-icon t)

      (doom-modeline-def-segment my-python-venv
        "The current python virtual environment state."
        (when (eq major-mode 'python-mode)
          (if (eq python-shell-virtualenv-root nil)
              ""
            (propertize
             (let ((base-dir-name (file-name-nondirectory (substring python-shell-virtualenv-root 0 -1))))
               (if (< 10 (length base-dir-name))
                   (format " (%s..)" (substring base-dir-name 0 8))
                 (format " (%s)" base-dir-name)))
             'face (if (doom-modeline--active) 'doom-modeline-buffer-major-mode)))))

      (doom-modeline-def-modeline 'my-modeline-layout
        '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
        '(misc-info persp-name lsp irc mu4e github debug minor-modes input-method buffer-encoding my-python-venv process vcs checker))

      (defun setup-custom-doom-modeline ()
        (doom-modeline-set-modeline 'my-modeline-layout 'default))
      )))
(defun peter-display/init-font-lock+ ())

(defun peter-display/post-init-eshell-prompt-extras ()
  (when (configuration-layer/package-used-p 'eshell-prompt-extras)
    (with-eval-after-load "esh-opt"
      (require 'virtualenvwrapper)
      (venv-initialize-eshell)
      (autoload 'epe-theme-pipeline "eshell-prompt-extras")
      (setq eshell-highlight-prompt nil
            ;; add new line adhead of tty
            eshell-prompt-function (lambda ()
                                     (concat "\n" (epe-theme-pipeline)))
            ;; eshell-prompt-function 'epe-theme-lambda
            ))))

;;;; Prettify-utils

(defun peter-display/init-prettify-utils ()
  (use-package prettify-utils))


(defun peter-display/init-nyan-mode ()
  (use-package nyan-mode
    :ensure t
    :init
    (setq nyan-animate-nyancat t
          nyan-animation-frame-interval 0.1
          nyan-minimum-window-width 112
          nyan-bar-length 20
          nyan-wavy-trail t)
    :config
    (when enable-nyan-cat-animation
      (nyan-mode 1))))

;;; Unowned Packages
;;;; Which-key

(defun peter-display/post-init-which-key ()
  (when (and (configuration-layer/package-used-p 'pretty-fonts) enable-font-ligature)
    (setq which-key-separator " ")
    (setq which-key-prefix-prefix " ")))

;;;; Pretty-code

(defun peter-display/init-pretty-code ()
  (use-package pretty-code
    :if enable-pretty-code
    :config
    (progn
      (pretty-code-add-hook 'emacs-lisp-mode-hook '((:def "defun")
                                                    (:lambda "lambda")))
      ;; (pretty-code-add-hook 'python-mode-hook     '((:def "def")
      ;;                                               (:lambda "lambda")))
      )))

;;;; Pretty-fonts

(defun peter-display/init-pretty-fonts ()
  (use-package pretty-fonts
    :if enable-font-ligature
    :config
    ;; !! This is required to avoid segfault when using emacs as daemon !!
    (spacemacs|do-after-display-system-init
     (pretty-fonts-add-hook 'prog-mode-hook pretty-fonts-fira-code-alist)
     (pretty-fonts-add-hook 'org-mode-hook  pretty-fonts-fira-code-alist)

     (pretty-fonts-set-fontsets-for-fira-code)
     (pretty-fonts-set-fontsets
      '(;; All-the-icons fontsets
        ("fontawesome"
         ;;                         
         #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

        ("all-the-icons"
         ;;    
         #xe907 #xe928)

        ("github-octicons"
         ;;                               
         #xf091 #xf059 #xf076 #xf075 #xe192  #xf016 #xf071)

        ("material icons"
         ;;              
         #xe871 #xe918 #xe3e7  #xe5da
         ;;              
         #xe3d0 #xe3d1 #xe3d2 #xe3d4))))))

;;;; Pretty-magit

(defun peter-display/init-pretty-magit ()
  (use-package pretty-magit
    :config
    (progn
      ;; (pretty-magit-add-leaders
      ;;  '(("Feature" ? (:foreground "slate gray" :height 1.2))
      ;;    ("Add"     ? (:foreground "#375E97" :height 1.2))
      ;;    ("Fix"     ? (:foreground "#FB6542" :height 1.2))
      ;;    ("Clean"   ? (:foreground "#FFBB00" :height 1.2))
      ;;    ("Docs"    ? (:foreground "#3F681C" :height 1.2))))

      (pretty-magit-setup)
      )))

(defun peter-display/init-treemacs-icons-dired ()
  (use-package treemacs-icons-dired
    :if (eq dired-icons-backend 'treemacs)
    :defer t
    :hook ((dired-mode . treemacs-icons-dired-mode))))

(defun peter-display/init-treemacs-magit ()
    (use-package treemacs-magit
      :ensure t
      ))

(defun peter-display/init-vim-empty-lines-mode ()
  (use-package vim-empty-lines-mode
    :ensure t
    :hook ((eshell-mode . (lambda () (vim-empty-lines-mode -1))))
    :config
    (global-vim-empty-lines-mode)))

(defun peter-display/init-virtualenvwrapper ()
  (use-package virtualenvwrapper
    :ensure t))


;;; packages.el ends here
