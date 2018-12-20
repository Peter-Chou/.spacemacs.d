;; -*- mode: emacs-lisp; lexical-binding: t -*-

(with-eval-after-load 'org
  (setq org-startup-folded 'content)

  )

;; add project's TODO files to the agenda automatically
(with-eval-after-load 'org-agenda
  (require 'org-projectile)
  (mapcar '(lambda (file)
             (when (file-exists-p file)
               (push file org-agenda-files)))
          (org-projectile-todo-files)))

(provide 'my-org-config)
