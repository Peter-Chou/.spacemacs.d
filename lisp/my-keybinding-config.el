
;; ------ global -------------------------------------------------------------
;; <SPC>-d go back where jumps
(spacemacs/set-leader-keys "d" 'xref-pop-marker-stack)

;; <SPC>-' use shell-here command
(spacemacs/set-leader-keys "'" 'my-shell-here)

;; enable dired-hide-details-mode when opening dired
(spacemacs/set-leader-keys "fj" 'my-dired-jump)
(spacemacs/set-leader-keys "jd" 'my-deer)

;; spacemacs quit bindings
(evil-leader/set-key "qw" 'spacemacs/save-buffer-and-kill-frame)
(evil-leader/set-key "qq" 'spacemacs/frame-killer)
(evil-leader/set-key "qh" 'suspend-frame)

;; append gitignore pattern from https://github.com/github/gitignore
(evil-leader/set-key "gfa" 'gitignore-templates-insert)

;; ------ spacemacs SPC-o -----------------------------------------------------
;; SPC-o-d family binding
(spacemacs/declare-prefix "od" "doc-EOF")
(spacemacs/set-leader-keys "odh" 'hidden-dos-eol)
(spacemacs/set-leader-keys "odd" 'remove-dos-eol)

(spacemacs/set-leader-keys "ow" 'whitespace-cleanup)

;; SPC-o-l family binding
(spacemacs/declare-prefix "ol" "layout")
(spacemacs/set-leader-keys "oll" 'peter/load-my-layout)
(spacemacs/set-leader-keys "ols" 'peter/save-my-layout)


;; SPC-o-s family binding
(spacemacs/declare-prefix "os" "shell")
(spacemacs/set-leader-keys "osc" 'my-clear-shell-buffer)
(spacemacs/set-leader-keys "oso" 'open-mintty-terminal)

;; SPC-o-t family binding
(spacemacs/declare-prefix "ot" "toggle")
;; set spc-o-t-s to toggle on/off fira code symbol
(if (fboundp 'global-fira-code-symbol-mode)
    (spacemacs/set-leader-keys "ots" 'global-fira-code-symbol-mode))


;; ------ evil mode -----------------------------------------------------------
;; emacs keybinding
(define-key evil-insert-state-map (kbd "C-f") 'forward-char)
(define-key evil-insert-state-map (kbd "C-b") 'backward-char)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)


;; ------ company mode --------------------------------------------------------
;; use tab -> select completion, C-n -> select next, C-p -> select previous
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map [tab] #'company-complete-selection))

  ;; use Ctrl-Meta-; to complete the path manually
  (global-set-key (kbd "C-M-;") 'company-files)

;; start auto complete when start typing
(setq-default company-minimum-prefix-length 1)


;; ------ python mode -----------------------------------------------------------
;; fix python keybindings
(with-eval-after-load 'anaconda-mode
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    ;; "hd" 'sphinx-doc ;; auto-docstring
    "Va" 'my-pipenv-activate
    "Vd" 'my-pipenv-deactivate
    "Vw" 'my-pipenv-workon
    "db" 'my-python-toggle-breakpoint
    "ck" 'my-quit-subjob
    "sk" 'my-quit-interactive-subjob)

  ;; use C-/ to show candidates in Helm or Ivy (for fuzzy searching)
  (define-key anaconda-mode-map (kbd "C-M-i") #'company-complete)
  )


(provide 'my-keybinding-config)
