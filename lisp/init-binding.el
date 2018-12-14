
;; ------ global -------------------------------------------------------------
;; <SPC>-d go back where jumps
(spacemacs/set-leader-keys "d" 'xref-pop-marker-stack)

;; <SPC>-' use shell-here command
(spacemacs/set-leader-keys "'" 'my-shell-here)

;; spacemacs quit bindings
(evil-leader/set-key "qw" 'spacemacs/save-buffer-and-kill-frame)
(evil-leader/set-key "qq" 'spacemacs/frame-killer)
(evil-leader/set-key "qh" 'suspend-frame)

;; ------ spacemacs SPC-o -----------------------------------------------------
;; SPC-o-d family binding
(spacemacs/declare-prefix "od" "doc-EOF")
(spacemacs/set-leader-keys "odh" 'hidden-dos-eol)
(spacemacs/set-leader-keys "odd" 'remove-dos-eol)

(spacemacs/set-leader-keys "ow" 'whitespace-cleanup)

;; SPC-o-s family binding
(spacemacs/declare-prefix "os" "shell")
(spacemacs/set-leader-keys "osc" 'my-clear-shell-buffer)
(spacemacs/set-leader-keys "oso" 'open-mintty-terminal)


;; ------ evil mode -----------------------------------------------------------
;; emacs keybinding
(define-key evil-insert-state-map (kbd "C-f") 'forward-char)
(define-key evil-insert-state-map (kbd "C-b") 'backward-char)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)

;; set orignal evil-surrounding keybinding
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)


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
    "sk" 'my-quit-interactive-subjob))


  (provide 'init-binding)
