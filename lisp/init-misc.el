
(beacon-mode 1)

;; enable anzu-mode in mode-line
(anzu-mode 1)

;; binding C-M-- / C-M-= to decrease / increase font size globally
(default-text-scale-mode 1)

;; activate hungry delete mode
(global-hungry-delete-mode t)

;; activate evil vimish fold mode
(evil-vimish-fold-mode 1)

;; activate all-the-icon famliy
;; (require 'font-lock+)
(all-the-icons-ivy-setup)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)


(provide 'init-misc)
