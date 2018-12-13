
;; ------ global --------------------------------------------------------------
(defun spacemacs/save-buffer-and-kill-frame ()
  "kill the current buffer and the current frame"
  (interactive)
  (save-buffer)
  (kill-buffer)
  (spacemacs/frame-killer))

(defun hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings"
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun remove-dos-eol ()
  ";; Replace DOS eol CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun my-clear-shell-buffer ()
  "clear shell buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun open-mintty-terminal ()
  (interactive)
  (progn
    (shell-command "mintty --window=max &")
    (delete-window)))


;; ------ python mode ---------------------------------------------------------
(defun my-pipenv-workon ()
  "switch python virtualenvironment and restart anaconda server"
  (interactive)
  (call-interactively 'pyvenv-workon)
  (setq python-shell-virtualenv-path pyvenv-virtual-env))

(defun my-pipenv-activate ()
  "switch python virtualenvironment and restart anaconda server"
  (interactive)
  (call-interactively 'pyvenv-activate)
  (setq python-shell-virtualenv-path pyvenv-virtual-env))

(defun my-pipenv-deactivate ()
  "deactivate pyvenv & anaconda virtual enironment"
  (interactive)
  (pyvenv-deactivate)
  (pythonic-deactivate))

(defun my-python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond ((spacemacs/pyenv-executable-find "wdb") "import wdb; wdb.set_trace() # XXX BREAKPOINT")
                     ((spacemacs/pyenv-executable-find "ipdb") "import ipdb; ipdb.set_trace() # XXX BREAKPOINT")
                     ((spacemacs/pyenv-executable-find "pudb") "import pudb; pudb.set_trace() # XXX BREAKPOINT")
                     ((spacemacs/pyenv-executable-find "ipdb3") "import ipdb; ipdb.set_trace() # XXX BREAKPOINT")
                     ((spacemacs/pyenv-executable-find "pudb3") "import pudb; pudb.set_trace() # XXX BREAKPOINT")
                     (t "import pdb; pdb.set_trace() # XXX BREAKPOINT")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line)))))
(defun my-quit-subjob ()
  (interactive)
  (save-excursion
    (setq peter-current-buffer-name (buffer-name))
    (previous-buffer)
    (setq peter-previous-buffer-name (buffer-name))
    (switch-to-buffer "*compilation*")
    (comint-quit-subjob)
    (switch-to-buffer peter-previous-buffer-name)
    (switch-to-buffer peter-current-buffer-name)))

(defun my-quit-subjob ()
  "quit runing job in python buffer"
  (interactive)
  (save-excursion
    (setq peter-current-buffer-name (buffer-name))
    (previous-buffer)
    (setq peter-previous-buffer-name (buffer-name))
    (switch-to-buffer "*compilation*")
    (comint-quit-subjob)
    (switch-to-buffer peter-previous-buffer-name)
    (switch-to-buffer peter-current-buffer-name)))

(defun my-quit-interactive-subjob ()
  "quit REPL runing job in python buffer"
  (interactive)
  (save-excursion
    (setq peter-current-buffer-name (buffer-name))
    (previous-buffer)
    (setq peter-previous-buffer-name (buffer-name))
    (switch-to-buffer "*Python*")
    (comint-quit-subjob)
    (switch-to-buffer peter-previous-buffer-name)
    (switch-to-buffer peter-current-buffer-name)))


(provide 'init-function)
