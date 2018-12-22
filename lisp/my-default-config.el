
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
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

;; if file exceed 500kb, it will be opened in fundamental-mode to speed up the loading
(defun spacemacs/check-large-file ()
  (when (> (buffer-size) 500000)
    (progn
      (fundamental-mode)
      (hl-line-mode -1)))
  (if (and (executable-find "wc")
           (> (string-to-number (shell-command-to-string (format "wc -l %s"
                                                                 (buffer-file-name)))) 5000))
      (linum-mode -1)))
(add-hook 'find-file-hook 'spacemacs/check-large-file)


;; ------ display-time mode --------------------------------------------------------
;; show time on powerline
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-format "%m.%d %a %H:%M:%S")
;; update every second
(setq display-time-interval 1)
;; don't show load average
(setq display-time-default-load-average nil)
;; don't show mail
(setq display-time-mail-string "")
;; show time in mode line on startup
(display-time-mode 1)


;; ------ company mode --------------------------------------------------------
;; use M-number to choose the candidates
(setq company-show-numbers t)


;; ------ evil mode -----------------------------------------------------------
;; set fd to escape evil mode in 0.3
(setq-default evil-escape-delay 0.3)

;; key binding as vim-surround
(with-eval-after-load 'evil-surround
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute))


;; ------ ranger mode ---------------------------------------------------------
;; show dotfiles at ranger startup
(setq ranger-show-hidden t)

;; ignore certain files when moving over them
(setq ranger-ignored-extensions '("mkv" "iso" "mp4"))

;; set the max files size (in MB)
(setq ranger-max-preview-size 10)


;; ------ avy mode ------------------------------------------------------------
;; instant start avy match
(setq avy-timeout-seconds 0.0)


;; ------ electric-operator mode -----------------------------------------------
(add-hook 'python-mode-hook #'electric-operator-mode)
(add-hook 'c-mode-common-hook #'electric-operator-mode)

;; ignore *, & operator in c/c++ mode
(add-hook 'electric-operator-mode-hook (lambda ()
                            (electric-operator-add-rules-for-mode 'c++-mode
                                                                  (cons "*" nil)
                                                                  (cons "&" nil))

                            (electric-operator-add-rules-for-mode 'c-mode
                                                                  (cons "*" nil))
                            )
          )


;; ------ smart semicolon mode -----------------------------------------------
(add-hook 'c-mode-common-hook #'smart-semicolon-mode)


;; ------ flyspell mode -------------------------------------------------------
;; set default spell checker to aspell
(setq ispell-program-name "aspell")


;; ------ whitespace mode -------------------------------------------------------
;; set max width = 160
(setq-default whitespace-line-column 160)


;; ------ prettify mode -------------------------------------------------------
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(lambda\\)\\>" (0 (prog1 ()
                                                   (compose-region (match-beginning 1)
                                                                   (match-end 1)
                                                                   ?λ))))))


;; ------ fira code symbol mode -----------------------------------------------
;; activate global fira code symbol mode if fira-code-symbol is required in init.el
(if (fboundp 'global-fira-code-symbol-mode)
    (global-fira-code-symbol-mode 1))


;; ------ dired mode ----------------------------------------------------------
;; always take recursive action without further permission
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)


;; ------ fci mode ------------------------------------------------------------
;; set color for fci rule
(setq fci-rule-color "#FFA631")
(setq fci-rule-use-dashes t)
;; activate fci-mode when in programming mode
(add-hook 'prog-mode-hook (lambda ()
                            (fci-mode 1)
                            (fci-update-all-windows t)
                            ))


;; ------ hightlight-indent-guides mode ---------------------------------------
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
;; , ⋮, ┆, ┊, ┋, ┇
(setq highlight-indent-guides-character ?\┋)
(setq highlight-indent-guides-responsive 'top)
(setq highlight-indent-guides-auto-enabled nil)
(set-face-foreground 'highlight-indent-guides-character-face "#8f9091")
(set-face-foreground 'highlight-indent-guides-top-character-face "#fe5e10")
(setq highlight-indent-guides-auto-character-face-perc 10)
(setq highlight-indent-guides-auto-top-character-face-perc 20)


;; ------ treemacs mode -------------------------------------------------------
(setq treemacs-silent-refresh t)
(setq treemacs-silent-filewatch t)


;; ------ python mode ---------------------------------------------------------
;; set two space indent
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq python-indent-offset 2)
            )
          )


(provide 'my-default-config)
