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
        (all-the-icons-dired :requires font-lock+)
        company-box
        diredfl
        (prettify-utils :location (recipe :fetcher github
                                          :repo "Ilazki/prettify-utils.el"))
        nyan-mode
        treemacs-icons-dired
        ;; Elsehwere-owned packages
        which-key

        (pretty-code     :location local)
        (pretty-eshell   :location local)
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

      (all-the-icons-ivy-setup))))

(defun peter-display/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :if (eq dired-icons-backend 'all-the-icons)
    :defer t
    :init
    (require 'font-lock+)
    :hook (dired-mode . all-the-icons-dired-mode)))

(defun peter-display/init-company-box ()
  (use-package company-box
    :if (and (not (version< emacs-version "26.1")) (display-graphic-p))
    :defer t
    :hook (company-mode . company-box-mode)
    :config
    (with-eval-after-load 'all-the-icons
      (setq company-box-icons-unknown
            (all-the-icons-octicon "file-text" :v-adjust -0.05))

      (setq company-box-icons-elisp
            (list
             (all-the-icons-faicon "cube" :v-adjust -0.0575)        ; Function
             (all-the-icons-faicon "tag" :v-adjust -0.0575)         ; Variable
             (all-the-icons-faicon "cog" :v-adjust -0.0575)         ; Feature
             (all-the-icons-material "palette" :v-adjust -0.2)      ; Face
             ))

      (setq company-box-icons-yasnippet
            (all-the-icons-faicon "code" :v-adjust -0.0575))       ; Snippet

      (setq company-box-icons-lsp
            `(( 1  . ,(all-the-icons-faicon "file-text-o" :v-adjust -0.0575))     ; Text
              ( 2  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575))            ; Method
              ( 3  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575))            ; Function
              ( 4  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575))            ; Constructor
              ( 5  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Field
              ( 6  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Variable
              ( 7  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575))             ; Class
              ( 8  . ,(all-the-icons-faicon "cogs" :v-adjust -0.0575))            ; Interface
              ( 9  . ,(all-the-icons-alltheicon "less"))                          ; Module
              (10  . ,(all-the-icons-faicon "wrench" :v-adjust -0.0575))          ; Property
              (11  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Unit
              (12  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Value
              (13  . ,(all-the-icons-material "content_copy" :v-adjust -0.2))     ; Enum
              (14  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Keyword
              (15  . ,(all-the-icons-material "content_paste" :v-adjust -0.2))    ; Snippet
              (16  . ,(all-the-icons-material "palette" :v-adjust -0.2))          ; Color
              (17  . ,(all-the-icons-faicon "file" :v-adjust -0.0575))            ; File
              (18  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Reference
              (19  . ,(all-the-icons-faicon "folder" :v-adjust -0.0575))          ; Folder
              (20  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; EnumMember
              (21  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Constant
              (22  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575))             ; Struct
              (23  . ,(all-the-icons-faicon "bolt" :v-adjust -0.0575))            ; Event
              (24  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Operator
              (25  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575))             ; TypeParameter
              )))))

(defun peter-display/init-diredfl ()
  (use-package diredfl
    :init
    (diredfl-global-mode 1)))

;;;; Prettify-utils

(defun peter-display/init-prettify-utils ()
  (use-package prettify-utils))


(defun peter-display/init-nyan-mode ()
  (use-package nyan-mode
    :ensure t
    :init
    (progn
      (when enable-nyan-cat-animation
        (setq nyan-animate-nyancat t
              nyan-animation-frame-interval 0.1
              nyan-minimum-window-width 112
              nyan-wavy-trail t))
      (setq nyan-bar-length 20))
    :config
    (nyan-mode 1)))

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

;;;; Pretty-eshell

(defun peter-display/init-pretty-eshell ()
  (use-package pretty-eshell
    :init
    (progn
      ;; Change default banner message
      (setq eshell-banner-message (s-concat (s-repeat 20 "---") "\n\n"))

      ;; More prompt styling
      (setq pretty-eshell-header "\n︳")
      (setq pretty-eshell-prompt-string " "))

    :config
    (progn
      ;; Directory
      (pretty-eshell-section
       esh-dir
       "\xf07c"  ; 
       (abbreviate-file-name (eshell/pwd))
       '(:foreground "#268bd2" :bold bold :underline t))

      ;; Git Branch
      (pretty-eshell-section
       esh-git
       "\xe907"  ; 
       (magit-get-current-branch)
       '(:foreground "#8D6B94"))

      ;; Python Virtual Environment
      (pretty-eshell-section
       esh-python
       "\xe928"  ; 
       pyvenv-virtual-env-name)

      ;; Time
      (pretty-eshell-section
       esh-clock
       "\xf017"  ; 
       (format-time-string "%H:%M" (current-time))
       '(:foreground "forest green"))

      ;; Prompt Number
      (pretty-eshell-section
       esh-num
       "\xf0c9"  ; 
       (number-to-string pretty-eshell-prompt-num)
       '(:foreground "brown"))

      (setq pretty-eshell-funcs
            (list esh-dir esh-git esh-python esh-clock esh-num)))))


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

;;; packages.el ends here
