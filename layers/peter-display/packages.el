;;; Display Layer -*- lexical-binding: t; -*-
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
        all-the-icons-dired
        company-box
        eshell-git-prompt
        (prettify-utils :location (recipe :fetcher github
                                          :repo "Ilazki/prettify-utils.el"))
        treemacs-icons-dired
        ;; Elsehwere-owned packages
        which-key

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
    :hook (dired-mode . all-the-icons-dired-mode)))

(defun peter-display/init-company-box ()
  (use-package company-box
    :if (display-graphic-p)
    :defer t
    :hook (company-mode . company-box-mode)))


(defun peter-display/init-eshell-git-prompt ()
  (use-package eshell-git-prompt
    :ensure t
    :config
    (with-eval-after-load 'eshell
      (eshell-git-prompt-use-theme 'powerline))
    ))


;;;; Prettify-utils

(defun peter-display/init-prettify-utils ()
  (use-package prettify-utils))

;;; Unowned Packages
;;;; Which-key

(defun peter-display/post-init-which-key ()
  (when (and (configuration-layer/package-used-p 'pretty-fonts) enable-font-ligature)
    (setq which-key-separator " ")
    (setq which-key-prefix-prefix " ")))

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
    :if enable-pretty-magit
    :config
    (progn
      (pretty-magit-add-leaders
       '(("Feature" ? (:foreground "slate gray" :height 1.2))
         ("Add"     ? (:foreground "#375E97" :height 1.2))
         ("Fix"     ? (:foreground "#FB6542" :height 1.2))
         ("Clean"   ? (:foreground "#FFBB00" :height 1.2))
         ("Docs"    ? (:foreground "#3F681C" :height 1.2))))

      (pretty-magit-setup))))

(defun peter-display/init-treemacs-icons-dired ()
  (use-package treemacs-icons-dired
    :if (eq dired-icons-backend 'treemacs)
    :defer t
    :hook ((dired-mode . treemacs-icons-dired-mode))))

;;; packages.el ends here
