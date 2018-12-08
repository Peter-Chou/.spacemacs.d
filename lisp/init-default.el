
;; ------ global -------------------------------------------------------------
;; disable lock files  .#file-name
(setq create-lockfiles nil)

;; toggle off the minor-mode on modeline as default
(spacemacs/toggle-mode-line-minor-modes-off)

;; revert the buffer automatically when the filed is modified outside emcas
(global-auto-revert-mode t)

;; Prevent the visual selection overriding my system clipboard
(fset 'evil-visual-update-x-selection 'ignore)

;; set the appearance of menu bar as arrow shape
(setq powerline-default-separator 'arrow)

;; make new frame fullscreen as default
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; force horizontal split window
(setq split-width-threshold 120)

;;Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; if file exceed 500kb, it will be opened in fundamental-mode to speed up the loading
(defun spacemacs/check-large-file ()
  (when (> (buffer-size) 500000)
    (progn (fundamental-mode)
           (hl-line-mode -1)))
  (if (and (executable-find "wc")
           (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
              5000))
      (linum-mode -1)))
(add-hook 'find-file-hook 'spacemacs/check-large-file)

;; show time on powerline
;; ;; copy from https://github.com/syl20bnr/spacemacs/issues/9458
(setq display-time-24hr-format t)
(setq display-time-format "%H:%M:%S")        ; add seconds
(setq display-time-interval 1)               ; update every second
(setq display-time-default-load-average nil) ; don't show load average
(setq display-time-mail-string "")           ; don't show mail
(display-time-mode 1)                        ; show time in mode line on startup


;; ------ evil mode -----------------------------------------------------------
;; set fd to escape evil mode in 0.3
(setq-default evil-escape-delay 0.3)


;; ------ flycheck mode -------------------------------------------------------
;;set up fly-check to ignore the E501 error
(setq-default flycheck-flake8-maximum-line-length 160)


;; ------ whitespace mode -------------------------------------------------------
;; set max width = 160
(setq-default whitespace-line-column 160)


;; ------ dired mode ----------------------------------------------------------
;; always take recursive action without further permission
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)


;; ------ python mode ---------------------------------------------------------
(add-hook 'python-mode-hook (lambda ()
                              ;; activate hightlight indentation mode
                              (highlight-indentation-mode 1)
                              (highlight-indentation-current-column-mode 1)
                              (require 'py-autopep8)
                              (setq indent-tabs-mode nil
                                    tab-width 2
                                    ;; E121 Fix indentation to be a multiple of four
                                    ;; E402 Fix module level import not at top of file
                                    ;; E401 Put imports on separate lines
                                    py-autopep8-options '("--max-line-length=80" "--indent-size=2" "--ignore=E121" "--ignore=E402" "--ignore=E401"))
                              ;; turn on fill-column-indicator when python mode is active
                              (fci-mode 1)
                              ))
;; activate autopep8 on save
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)


;; ------ c/c++ mode ----------------------------------------------------------
(add-hook 'c-mode-common-hook (lambda ()
                                (highlight-indentation-mode 1)
                                (highlight-indentation-current-column-mode 1)
                                (fci-mode 1)
                                ;; (setq-local tab-width 4)
                                ))


(provide 'init-default)
