
;; ------ global -------------------------------------------------------------
;; disable lock files  .#file-name
(setq create-lockfiles nil)

;; update version control info in modeline
;; (setq auto-revert-check-vc-info t)

;; toggle off the minor-mode on modeline as default
(cond ((eq (car dotspacemacs-mode-line-theme) 'spacemacs)
       ;; spacemacs specific configuration
       (spacemacs/toggle-mode-line-minor-modes-off)
       )
      ((eq (car dotspacemacs-mode-line-theme) 'doom)
       ;; doom specific configuration
       (setq
        doom-modeline-lsp nil
        doom-modeline-persp-name nil
        doom-modeline-github nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon t)

       ;; set major mode face color
       (set-face-attribute 'doom-modeline-buffer-major-mode nil :weight 'bold :foreground "#fd780f")
       ;; set dictory name face color
       (set-face-attribute 'doom-modeline-buffer-path nil :weight 'bold :foreground "#1da1f2")

       ;; add a customized venv segment
       (doom-modeline-def-segment my-python-venv
         "The current python virtual environment state."
         (when (eq major-mode 'python-mode)
           (if (eq python-shell-virtualenv-root nil)
               ""
             (propertize
              (let ((base-dir-name (file-name-nondirectory (substring python-shell-virtualenv-root 0 -1))))
                (if (< 10 (length base-dir-name))
                    (format " (%s..)" (substring base-dir-name 0 8))
                  (format " (%s)" base-dir-name)))
              'face (if (doom-modeline--active) 'doom-modeline-buffer-major-mode)))))

       (doom-modeline-def-modeline 'main
         '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches " " buffer-info remote-host buffer-position " " selection-info)
         '(misc-info persp-name lsp github debug minor-modes input-method buffer-encoding major-mode my-python-venv process vcs flycheck))
       )
      )

;; revert the buffer automatically when the filed is modified outside emcas
(global-auto-revert-mode t)

;; Prevent the visual selection overriding my system clipboard
(fset 'evil-visual-update-x-selection 'ignore)

;; set the appearance of menu bar as arrow shape
(setq
 powerline-default-separator 'arrow)

;; make new frame fullscreen as default
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; force horizontal split window
(setq
 split-width-threshold 120)

;;Don't ask me when kill process buffer
(setq
 kill-buffer-query-functions (remq 'process-kill-buffer-query-function
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


;; ------ display-time mode ---------------------------------------------------
;; show time on powerline
(setq
 display-time-24hr-format t
 display-time-day-and-date t
 display-time-format "%m.%d %a %H:%M"
 ;; (setq display-time-format "%m.%d %a %H:%M:%S")
 ;; update every second
 display-time-interval 1
 ;; don't show load average
 display-time-default-load-average nil
 ;; don't show mail
 display-time-mail-string ""
 )
;; show time in mode line on startup
(display-time-mode 1)


;; ------ company mode --------------------------------------------------------
;; use M-number to choose the candidates
(setq
 company-show-numbers t)


;; ------ lsp mode ------------------------------------------------------------
(setq
 lsp-ui-sideline-enable nil)


;; ------ eldoc mode ----------------------------------------------------------
(setq
 eldoc-idle-delay 0.3)


;; ------ writeroom mode ------------------------------------------------------
(setq
 writeroom-width 90
 writeroom-fullscreen-effect 'maximized
 )


;; ------ magit mode ----------------------------------------------------------
;; for ediff just show two windows
;; (setq magit-revert-buffers 'silent)
(setq
 magit-diff-refine-hunk t
 ;; magit-ediff-dwim-show-on-hunks t
;; don't display fringe in magit status
 magit-section-visibility-indicator nil)
;; (magit-auto-revert-mode 1)


;; ------ evil mode -----------------------------------------------------------
;; set fd to escape evil mode in 0.3
(setq-default evil-escape-delay 0.3)

;; key binding as vim-surround
(with-eval-after-load 'evil-surround
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute))


;; ------ ranger mode ---------------------------------------------------------
;; show dotfiles at ranger startup
(setq
 ranger-show-hidden t
 ;; ignore certain files when moving over them
 ranger-ignored-extensions '("mkv" "iso" "mp4")
 ;; set the max files size (in MB)
 ranger-max-preview-size 10)


;; ------ avy mode ------------------------------------------------------------
;; instant start avy match
(setq
 avy-timeout-seconds 0.0)


;; ------ flyspell mode -------------------------------------------------------
;; set default spell checker to aspell
(setq
 ispell-program-name "aspell")


;; ------ whitespace mode -------------------------------------------------------
;; set max width = 160
(setq-default
 whitespace-line-column 160)


;; ------ prettify mode -------------------------------------------------------
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(lambda\\)\\>" (0 (prog1 ()
                                                   (compose-region (match-beginning 1)
                                                                   (match-end 1)
                                                                   ?λ))))))


;; ------ fira code symbol mode -----------------------------------------------
;; activate global fira code symbol mode if fira-code-symbol is required in init.el
;; (add-hook 'prog-mode-hook 'fira-code-symbol-hook)


;; ------ dired mode ----------------------------------------------------------
;; always take recursive action without further permission
(setq
 dired-recursive-copies 'always
 dired-recursive-deletes 'always)


;; ------ fci mode ------------------------------------------------------------
;; set color for fci rule
(setq
 fci-rule-color "#FFA631"
 fci-rule-use-dashes t)
;; activate fci-mode when in programming mode
(add-hook 'prog-mode-hook (lambda ()
                            (fci-mode 1)
                            (fci-update-all-windows t)
                            ))


;; ------ treemacs mode -------------------------------------------------------
(setq
 treemacs-silent-refresh t
 treemacs-silent-filewatch t)


;; ------ python mode ---------------------------------------------------------
;; set two space indent
(add-hook 'python-mode-hook (lambda ()
                              (setq indent-tabs-mode nil
                                    tab-width 2
                                    python-indent-offset 2)))


;; ------ java mode -----------------------------------------------------------
;; download java ls from https://projects.eclipse.org/projects/eclipse.jdt.ls
;; unzip to ~/eclipse.jdt.ls/server/
(setq
 lsp-java-server-install-dir (expand-file-name "~/eclipse.jdt.ls/server/")
 lsp-java-workspace-dir (expand-file-name "~/eclipse.jdt.ls/workspace/"))


(provide 'my-default-config)
