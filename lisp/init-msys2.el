
(when (eq system-type 'windows-nt)
  (setq system-uses-terminfo nil)
(setq exec-path (cons "c:/msys64/usr/bin/" exec-path))
(setq explicit-shell-file-name "c:/msys64/usr/bin/bash.exe")

  (add-hook 'comint-output-filter-functions
            'shell-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt nil t)
  (setq explicit-shell-file-name "bash.exe")
  ;; For subprocesses invoked via the shell
  ;; (e.g., "shell -c command")
  (setq shell-file-name explicit-shell-file-name)
  (setq explicit-shell-args '("--login" "-i"))

  (defun open-mintty-shell ()
    "open external msys2-mintty shell from emacs"
    (interactive)
    (progn
      (shell-command "mintty --window=max &")
      (delete-window)))
  )


(provide 'init-msys2)
