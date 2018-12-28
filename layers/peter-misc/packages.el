;;; packages.el --- peter-misc layer packages file for Spacemacs.
;;
;; Copyright (c) 2018 Peter Chou
;;
;; Author:  <Peter-Chou>
;; URL: https://github.com/Peter-Chou/spacemacs-repository
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst peter-misc-packages
  '(
    all-the-icons
    all-the-icons-ivy
    all-the-icons-dired
    beacon
    default-text-scale
    electric-operator
    evil-vimish-fold
    fontify-face
    highlight-indent-guides
    prettify-greek
    smart-semicolon))

(defun peter-misc/init-all-the-icons ()
  (use-package all-the-icons
    :defer t))


(defun peter-misc/init-all-the-icons-ivy ()
  (use-package all-the-icons-ivy
    :ensure t
    :init
    ;; (require 'font-lock+)
    :config
    (all-the-icons-ivy-setup)))

(defun peter-misc/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :defer t
    :hook ((dired-mode . all-the-icons-dired-mode))))

(defun peter-misc/init-beacon ()
  (use-package beacon
    :ensure t
    :config
    (beacon-mode 1)))

(defun peter-misc/init-default-text-scale ()
  (use-package default-text-scale
    :ensure t
    :config
    (default-text-scale-mode 1)))

(defun peter-misc/init-electric-operator ()
  (use-package electric-operator
    :defer t
    :init
    (progn
    (add-hook 'python-mode-hook #'electric-operator-mode)
    (add-hook 'c-mode-common-hook #'electric-operator-mode)
    ;; ignore *, & operator in c/c++ mode
    (add-hook 'electric-operator-mode-hook (lambda ()
                                             (electric-operator-add-rules-for-mode 'c++-mode
                                                                                   (cons "*" nil)
                                                                                   (cons "&" nil))

                                             (electric-operator-add-rules-for-mode 'c-mode
                                                                                   (cons "*" nil)))))))

(defun peter-misc/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :ensure t
    :config
    (evil-vimish-fold-mode 1)))

(defun peter-misc/init-fontify-face ()
  (use-package fontify-face
    :defer t))

(defun peter-misc/init-highlight-indent-guides ()
  (use-package highlight-indent-guides
    :defer t
    :hook ((prog-mode . highlight-indent-guides-mode))
    :config
    (progn
      (setq highlight-indent-guides-method 'character)
      ;; , ⋮, ┆, ┊, ┋, ┇
      (setq highlight-indent-guides-character ?\┋)
      (setq highlight-indent-guides-responsive 'top)
      (setq highlight-indent-guides-auto-enabled nil)
      (set-face-foreground 'highlight-indent-guides-character-face "#8f9091")
      (set-face-foreground 'highlight-indent-guides-top-character-face "#fe5e10")
      (setq highlight-indent-guides-auto-character-face-perc 10)
      (setq highlight-indent-guides-auto-top-character-face-perc 20))))

(defun peter-misc/init-prettify-greek ()
  (use-package prettify-greek
    :ensure t))

(defun peter-misc/init-smart-semicolon ()
  (use-package smart-semicolon
    :defer t
    :hook ((c-mode-common . smart-semicolon-mode))))

;;; packages.el ends here
