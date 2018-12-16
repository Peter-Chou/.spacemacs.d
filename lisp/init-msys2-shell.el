(setq system-uses-terminfo nil)

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:/msys64/usr/bin/zsh.exe")
  (setq shell-file-name "zsh")
  ;; (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
  (setq explicit-shell-args '("--login" "-i"))
  (setenv "SHELL" shell-file-name)
  (setq comint-buffer-maximum-size 5000)
  (setq comint-scroll-show-maximum-output t)
  (add-hook 'comint-output-filter-functions
            'comint-strip-ctrl-m)
  (add-hook 'comint-output-filter-functions
            'comint-truncate-buffer)

  (defun my-buffer-face-mode-variable ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "Droid Sans Mono Awesome"))
    (buffer-face-mode))

  ;; use awesome fonts for powerline9k theme in zsh
  (add-hook 'comint-mode-hook 'my-buffer-face-mode-variable)
  )


(provide 'init-msys2-shell)
