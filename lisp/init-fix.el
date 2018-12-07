
;; ------ global --------------------------------------------------------------


;; ------ emacs mode ----------------------------------------------------------
;; fix the problem of parsing tons of .el files when typing
;; https://github.com/company-mode/company-mode/issues/525
(defun semantic-completion-advice (adviced-f &rest r)
  "Check if POINT it's inside a string or comment before calling semantic-*"
  (if (or (inside-string-q) (inside-comment-q))
      (not (message "Oleeee! do not call function, we're inside a string or comment!"))
    (apply adviced-f r)))
(advice-add 'semantic-analyze-completion-at-point-function :around #'semantic-completion-advice)


;; ------ evil mode -----------------------------------------------------------
;; fix the malfunction of c-e in better-defaults
(define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-motion-state-map (kbd "C-e") 'mwim-end-of-code-or-line)


;; ------ ranger mode ---------------------------------------------------------
;; enable ranger function with disable the golden ratio mode
;; set quit funtion to key q in normal mode
(defun my-ranger ()
  (interactive)
  (if golden-ratio-mode
      (prog
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


;; ------ chinese -------------------------------------------------------------
;; fix the delay when showing text in chinese
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "Microsoft Yahei" :size 20)))


;; ------ python mode ---------------------------------------------------------
;; fix cannot read anaconda-mode server response issue
(setq anaconda-mode-localhost-address "localhost")


;; ------ hungre-delete mode --------------------------------------------------
;; fix the issue that Deleting left of empty smartparen pair doesn't delete right when hungry-delete is enabled
;; https://github.com/syl20bnr/spacemacs/issues/6584
(defadvice hungry-delete-backward (before sp-delete-pair-advice activate) (save-match-data (sp-delete-pair (ad-get-arg 0))))

(provide 'init-fix)
