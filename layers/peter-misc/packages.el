;;; packages.el --- peter-misc layer packages file for Spacemacs.
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

(defconst peter-misc-packages
  '(
    beacon
    carbon-now-sh
    default-text-scale
    electric-operator
    fontify-face
    highlight-indent-guides
    modern-cpp-font-lock
    smart-semicolon
    symbol-overlay
    ))

(defun peter-misc/init-beacon ()
  (use-package beacon
    :ensure t
    :config
    (beacon-mode 1)))

(defun peter-misc/init-carbon-now-sh ()
  (use-package carbon-now-sh
    :ensure t))


(defun peter-misc/init-default-text-scale ()
  (use-package default-text-scale
    :ensure t
    :config
    (default-text-scale-mode 1)))

(defun peter-misc/init-electric-operator ()
  (use-package electric-operator
    :if enable-electric-operator
    :defer t
    :hook ((c-mode-common . electric-operator-mode)
           (python-mode . electric-operator-mode)
           (electric-operator-mode . (lambda ()
                                       (electric-operator-add-rules-for-mode 'c++-mode
                                                                             (cons "*" nil)
                                                                             (cons "&" nil))
                                       (electric-operator-add-rules-for-mode 'c-mode
                                                                             (cons "*" nil)))))))

(defun peter-misc/init-fontify-face ()
  (use-package fontify-face
    :defer t))

(defun peter-misc/init-highlight-indent-guides ()
  (use-package highlight-indent-guides
    :defer t
    :hook ((python-mode . highlight-indent-guides-mode)
           ;; (prog-mode . highlight-indent-guides-mode)
           (highlight-indent-guides-mode . (lambda ()
                                             (set-face-foreground 'highlight-indent-guides-character-face "#8f9091")
                                             (set-face-foreground 'highlight-indent-guides-top-character-face "#fe5e10"))))
    :config
    (progn
      (setq highlight-indent-guides-method 'character
            
            highlight-indent-guides-character ?\┋ ;; candidates: , ⋮, ┆, ┊, ┋, ┇
            highlight-indent-guides-responsive 'top
            highlight-indent-guides-auto-enabled nil
            highlight-indent-guides-auto-character-face-perc 10
            highlight-indent-guides-auto-top-character-face-perc 20))))

(defun peter-misc/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :defer t
    :hook ((c++-mode . modern-c++-font-lock-mode))))

(defun peter-misc/init-smart-semicolon ()
  (use-package smart-semicolon
    :defer t
    :hook ((c-mode-common . smart-semicolon-mode))))

(defun peter-misc/init-symbol-overlay ()
  (use-package symbol-overlay
    :defer t
    :hook ((prog-mode . symbol-overlay-mode))
    :config
    (progn
      (global-set-key (kbd "M-i") 'symbol-overlay-put)
      (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
      (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
      (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
      (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)
      (define-key symbol-overlay-map (kbd "<") 'spacemacs//symbol-overlay-switch-first)
      (define-key symbol-overlay-map (kbd ">") 'spacemacs//symbol-overlay-switch-last))))

;;; packages.el ends here
