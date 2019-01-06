;;; packages.el --- peter-misc layer packages file for Spacemacs.
;;
;; Copyright (c) 2018-2019 Peter Chou
;;
;; Author:  <Peter Chou>
;; URL: https://github.com/Peter-Chou/spacemacs-repository
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

;; symbol-overlay

(defun spacemacs//symbol-overlay-switch-first ()
  (interactive)
  (let* ((symbol (symbol-overlay-get-symbol))
         (keyword (symbol-overlay-assoc symbol))
         (a-symbol (car keyword))
         (before (symbol-overlay-get-list a-symbol 'car))
         (count (length before)))
    (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count))))

(defun spacemacs//symbol-overlay-switch-last ()
  (interactive)
  (let* ((symbol (symbol-overlay-get-symbol))
         (keyword (symbol-overlay-assoc symbol))
         (a-symbol (car keyword))
         (after (symbol-overlay-get-list a-symbol 'cdr))
         (count (length after)))
    (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count 1))))
