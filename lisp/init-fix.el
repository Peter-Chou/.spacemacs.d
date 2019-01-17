;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; ------ global --------------------------------------------------------------


;; ------ ranger mode ---------------------------------------------------------
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
  (if (and (boundp 'golden-ratio-previous-enable) golden-ratio-previous-enable)
      (progn
        (ranger-close)
        (golden-ratio-mode 1))
    (ranger-close)))

(with-eval-after-load 'ranger
  (progn
    (define-key ranger-normal-mode-map (kbd "q") 'my-quit-ranger)))
(spacemacs/set-leader-keys "ar" 'my-ranger)


;; ------ vc-mode -------------------------------------------------------------
;; fix not changing branch in modeline
(setq
 auto-revert-check-vc-info t)


;; ------ chinese -------------------------------------------------------------
;; fix the delay when showing text in chinese
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    ;; (font-spec :family "Microsoft Yahei" :size 18.5))
                    (font-spec :family "等距更纱黑体 SC" :size 18.5))
  )


;; ------ python mode ---------------------------------------------------------
;; fix cannot read anaconda-mode server response issue
(setq anaconda-mode-localhost-address "localhost")

;; fix messy output when print chinese
(defun my-python-execute-file (arg)
  "execute python file & produce correct chinese outputs"
  (interactive "P")
  (set-language-environment 'Chinese-GB18030)
  (save-some-buffers t)
  (spacemacs/python-execute-file arg)
  (set-language-environment 'UTF-8))

(defun my-python-execute-file-focus (arg)
  "EXECUTE PYTHON FILE & SWITCH TO THE SHELL BUFFER IN INSERT STATE & PRODUCE CORRECT CHINESE OUTPUTS"
  (interactive "P")
  (set-language-environment 'Chinese-GB18030)
  (save-some-buffers t)
  (spacemacs/python-execute-file-focus arg)
  (set-language-environment 'UTF-8))

(defun my-python-start-or-switch-repl ()
  (interactive)
  (set-language-environment 'Chinese-GB18030)
  (spacemacs/python-start-or-switch-repl)
  (other-window -1))

(defun my-python-quit-repl ()
  (interactive)
  (switch-to-buffer "*Python*")
  (comint-quit-subjob)
  (kill-buffer-and-window)
  (set-language-environment 'UTF-8))

(defun my-python-interrupt-repl ()
  (interactive)
  (switch-to-buffer "*Python*")
  (comint-interrupt-subjob)
  (other-window -1))

(when (eq system-type 'windows-nt)
  (with-eval-after-load 'anaconda-mode
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "'" 'my-python-start-or-switch-repl
      "cc" 'my-python-execute-file
      "cC" 'my-python-execute-file-focus
      "si" 'my-python-start-or-switch-repl
      "sq" 'my-python-quit-repl
      "sk" 'my-python-interrupt-repl)
    (define-key python-mode-map (kbd "C-c C-p") 'my-python-start-or-switch-repl)
    )
  )


;; ------ hungre-delete mode --------------------------------------------------
;; fix the issue that Deleting left of empty smartparen pair doesn't delete right when hungry-delete is enabled
;; https://github.com/syl20bnr/spacemacs/issues/6584
(defadvice hungry-delete-backward (before sp-delete-pair-advice activate) (save-match-data (sp-delete-pair (ad-get-arg 0))))


(provide 'init-fix)
