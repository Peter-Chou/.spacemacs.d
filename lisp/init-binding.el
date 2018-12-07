
;; ------ global -------------------------------------------------------------
;; <SPC>-d go back where jumps
(spacemacs/set-leader-keys "d" 'xref-pop-marker-stack)
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


;; ------ company mode --------------------------------------------------------
;; use tab -> select completion, C-n -> select next, C-p -> select previous
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map [tab] #'company-complete-selection))


;; ------ evil mode -----------------------------------------------------------
;; set orignal evil-surrounding keybinding
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)


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
