;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; ------ global -------------------------------------------------------------
;; disable lock files  .#file-name
(setq create-lockfiles nil)

;; show file size (Emacs 22+)
;; (size-indication-mode t)

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
        doom-modeline-height 21
        doom-modeline-lsp nil
        doom-modeline-persp-name nil
        doom-modeline-github nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; doom-modeline-buffer-file-name-style 'truncate-except-project
        doom-modeline-major-mode-color-icon t)

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

       ;; add my-python-venv segment and remove major-mode segment from modeline
       (doom-modeline-def-modeline 'main
         '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
         '(misc-info persp-name lsp github debug minor-modes input-method buffer-encoding my-python-venv process vcs checker))
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
 ;; display-time-format "%m.%d %a %H:%M"
 display-time-format "%H:%M"
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


;; ------ eshell mode ---------------------------------------------------------
;; add alias to eshell
(defvar my-eshell-alias-flag nil
  "if not nil, it will not set alias to alias files")
(add-hook 'eshell-mode-hook (lambda ()
                              (when (eq my-eshell-alias-flag nil)
                                (setq my-eshell-alias-flag t)
                                (eshell/alias ".." "cd ..")
                                (eshell/alias "..." "cd ../..")
                                (eshell/alias "...." "cd ../../..")

                                (eshell/alias "ll" "ls -lsA --color")
                                (eshell/alias "lh" "ls -lsAd --color .[^.]*")

                                (eshell/alias "d" "dired .")

                                (eshell/alias "g" "git $*")
                                (eshell/alias "goto" "git checkout $*")

                                (eshell/alias "mkdir" "mkdir -pv $*")
                                (eshell/alias "mv" "mv -v $*")
                                (eshell/alias "cp" "cp -v $*")
                                (eshell/alias "wget" "wget -c $*")
                                (eshell/alias ":q" "exit"))))


;; ------ flymd mode ----------------------------------------------------------
;; use latest mathjax from https://www.mathjax.org
;; replace http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML
;; in ~/.emacs.d/elpa/26.1/develop/flymd-20160617.1214/flymd.html
;; with https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/latest.js?config=TeX-AMS-MML_HTMLorMML

;; preview md using firefox since chrome prevents jQuery from loading local files
;; install firefox and put it into PATH
(defun my-flymd-browser-function (url)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (if (string-equal system-type "windows-nt")
        (setq url (concat "file:///" url)))
    (browse-url url)))

(setq
 flymd-browser-open-function 'my-flymd-browser-function
 flymd-output-directory (expand-file-name "~/.emacs.d/.cache")
 flymd-close-buffer-delete-temp-files t)


;; ------ lsp mode ------------------------------------------------------------
(setq
 lsp-ui-sideline-enable nil)


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


;; When 'C-c C-c' is pressed in the magit commit message buffer,
;; delete the magit-diff buffer related to the current repo.
(defun kill-magit-diff-buffer-in-current-repo (&rest _)
  "Delete the magit-diff buffer related to the current repo"
  (let ((magit-diff-buffer-in-current-repo
         (magit-mode-get-buffer 'magit-diff-mode)))
    (kill-buffer magit-diff-buffer-in-current-repo)))

(add-hook 'git-commit-setup-hook
          (lambda ()
            (add-hook 'with-editor-post-finish-hook
                      #'kill-magit-diff-buffer-in-current-repo
                      nil t))) ; the t is important

;; kill magit status buffer when quitting magit status
(define-key magit-mode-map (kbd "q") (lambda()
                                       (interactive)
                                       (magit-mode-bury-buffer t)))


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
 ;; let deer show details
 ranger-deer-show-details t
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


;; ------ dired mode ----------------------------------------------------------
;; always take recursive action without further permission
(setq
 dired-listing-switches "-alh"  ;; show human readable file size
 dired-recursive-copies 'always
 dired-recursive-deletes 'top) ;; “top” means ask once

;; ignore . in dired buffer
(add-hook 'dired-mode-hook 'dired-omit-mode)
;; just show .. in dired-omit-mode
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")
              ;; (seq bol ".." eol)
              (seq bol "." eol))))

;; show diectory first
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

;; force dired use current buffer only
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "f") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-director

;; kill current buffer when leaving dired mode
(define-key dired-mode-map (kbd "q") (lambda () (interactive) (kill-this-buffer)))  ; was dired-up-director


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

(with-eval-after-load 'treemacs-evil
  (define-key evil-treemacs-state-map (kbd "F") 'treemacs-create-file)
  (define-key evil-treemacs-state-map (kbd "+") 'treemacs-create-dir))


;; ------ emacs lisp mode -----------------------------------------------------
;; Enable aggressive indent for emacs-lisp mode
(add-hook 'emacs-lisp-mode-hook #'spacemacs/toggle-aggressive-indent-on)


;; ------ python mode ---------------------------------------------------------
;; set two space indent
(add-hook 'python-mode-hook (lambda ()
                              (setq indent-tabs-mode nil
                                    tab-width 2
                                    python-indent-offset 2)))


;; ------ java mode -----------------------------------------------------------
;; download java ls from https://projects.eclipse.org/projects/eclipse.jdt.ls/downloads
;; unzip to ~/eclipse.jdt.ls/server/
(setq
 lsp-java-server-install-dir (expand-file-name "~/eclipse.jdt.ls/server/")
 lsp-java-workspace-dir (expand-file-name "~/eclipse.jdt.ls/workspace/"))


(provide 'my-default-config)
