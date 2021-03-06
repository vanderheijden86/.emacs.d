;; pip install ipython[notebook] && apt-get install python-matplotlib && ipython notebook --ip=0.0.0.0

;; ELPY YAPF NEEDS TO BE IN RECENT RELEASE
;; IMPORTMAGIC UPGRADE


;; Read: http://elpy.readthedocs.org/en/latest/ide.html

;; PYFLYMAKE.PY
;; This could be quite more optimized, since I would never used other than Pylint!!!
;; Try to convert it to a raw one, speed up is 10%
;; pyflakes: too light
;; pycheckers: evals a module; not good
;; pep8: done by autosave

;;; consider reimplementing the wrap shifting

(require 'python-mode)

(require 'python)

(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun new-python-eval (arg)
  ;; NOTE THAT I CHANGED IPYTHON's
  ;; /Library/Python/2.7/site-packages/ipython-2.0.0_dev-py2.7.egg/IPython/core/magics/execution.py
  ;; /Library/Python/2.7/site-packages/ipython-2.0.0_dev-py2.7.egg/IPython/terminal/interactiveshell.py
  ;; IN ORDER TO PREVENT SILLY PRINTING
  (interactive "p")
  (when (eq arg 0)
    (setq python-python-command "ipython")
    (setq  pybuffname (concat "*IPython" (int-to-string arg) "*")))
   ;; (setq py-ipython-command-args "--pylab --automagic"))
  (when (eq arg 2)
    (setq python-python-command "ipython2")
    (setq  pybuffname (concat "*IPython" (int-to-string arg) "*"))
    (setq realgud:ipdb-command-name "python2 -m ipdb"))
  (when (eq arg 3)
    (setq python-python-command "ipython3")
    (setq  pybuffname (concat "*IPython" (int-to-string arg) "*"))
    (setq realgud:ipdb-command-name "python2 -m ipdb"))
    ;;(setq py-ipython-command-args "--pylab --automagic --simple-prompt"))
  (when (eq arg 9)
    ;; make it so that pypy's ipython is linked to "ipypy" in your bin, yes.. ipypy is a new name,
    ;; e.g.: (setq python-python-command "/Users/pascal/Downloads/pypy-2.5.1-osx64/bin/ipython")
    (setq python-python-command "ipypy")
    (setq  pybuffname (concat "*IPyPy" (int-to-string arg) "*")))
  (new-python-get-text)
  (if (get-buffer pybuffname)
      (switch-to-buffer-other-window pybuffname)
    (delete-other-windows)
    (if (not (string-match "pypy" python-python-command))
        (progn
          (py-shell nil t python-python-command pybuffname)
          (switch-to-buffer-other-window pybuffname))
      (split-window-right)
      (switch-window)
      (ansi-term "ipypy" (concat "IPyPy" (int-to-string arg)))
      ))
  (end-of-buffer)
  (insert "%time %paste")
  (if (eq major-mode 'term-mode)
      (term-send-input)
    (comint-send-input)
    (comint-add-to-input-history (s-trim-right (substring-no-properties (car kill-ring)))))
  (setq kill-ring (cdr kill-ring))
  (sleep-for 0.01)
  (search-backward "%time %paste")
  (delete-region (line-beginning-position) (+ 1 (line-end-position)))
  (end-of-buffer)
  (other-window -1))

(defun new-python-eval2 (arg)
  ;; NOTE THAT I CHANGED IPYTHON's
  ;; /Library/Python/2.7/site-packages/ipython-2.0.0_dev-py2.7.egg/IPython/core/magics/execution.py
  ;; /Library/Python/2.7/site-packages/ipython-2.0.0_dev-py2.7.egg/IPython/terminal/interactiveshell.py
  ;; IN ORDER TO PREVENT SILLY PRINTING
  (interactive "p")
  (when (eq arg 0)
    (setq python-python-command "ipython")
    (setq  pybuffname (concat "*IPython" (int-to-string arg) "*")))
  (when (eq arg 2)
    (setq python-python-command "ipython2")
    (setq  pybuffname (concat "*IPython" (int-to-string arg) "*")))

  (when (eq arg 3)
    (setq python-python-command "ipython3")
    (setq  pybuffname (concat "*IPython" (int-to-string arg) "*")))
  (when (eq arg 9)
    ;; make it so that pypy's ipython is linked to "ipypy" in your bin, yes.. ipypy is a new name,
    ;; e.g.: (setq python-python-command "/Users/pascal/Downloads/pypy-2.5.1-osx64/bin/ipython")
    (setq python-python-command "ipypy")
    (setq  pybuffname (concat "*IPyPy" (int-to-string arg) "*")))
  (new-python-get-text)
  (if (get-buffer pybuffname)
      (switch-to-buffer-other-window pybuffname)
    (delete-other-windows)
    (if (not (string-match "pypy" python-python-command))
        (progn
          (py-shell nil t python-python-command pybuffname)
          (switch-to-buffer-other-window pybuffname))
      (split-window-right)
      (switch-window)
      (ansi-term "ipypy" (concat "IPyPy" (int-to-string arg)))
      ))
  (end-of-buffer)
  (insert (car kill-ring))
  (if (eq major-mode 'term-mode)
      (term-send-input)
    (comint-send-input t))
  (accept-process-output (get-buffer-process (current-buffer)))
  (setq kill-ring (cdr kill-ring))
  (end-of-buffer)
  (comint-send-input nil t)
  (end-of-buffer)
  (comint-goto-process-mark)
  (other-window -1))


(defun python-send-buffer2 ()
  (interactive)
  (switch-to-buffer "*Python*")
  (ignore-errors (process-kill-without-query (get-buffer-process (current-buffer))))
  (kill-buffer "*Python*")
  (delete-other-windows)
  (python-send-buffer)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer "*Python*")
  (other-window -1))

;; (add-hook 'python-mode-hook
;;           '(lambda()
;;              (define-key python-mode-map (kbd "C-<return>") 'new-python-eval)))

(define-key python-mode-map (kbd "C-c c") 'python-send-my-buffer)

;;(define-key python-mode-map (kbd "<tab>") 'py-shift-right)
(define-key python-mode-map [backtab] 'py-shift-left)

(define-key python-mode-map (kbd "C-<backspace>") 'backward-delete-word)
(define-key py-ipython-shell-mode-map (kbd "C-<backspace>") 'backward-delete-word)

(define-key python-mode-map [backspace] 'backspace-blank-lines-or-char)
(define-key python-mode-map [delete] 'delete-blank-lines-or-char)

(defun add-inferior-python-keywords()
  "adds a few keywords for inferior-python mode"
  (font-lock-add-keywords nil
                          '(
                            ("time:.+" . 'font-lock-variable-name-face)
                            )
                          )
  )

(add-hook 'comint-mode-hook 'add-inferior-python-keywords)

(define-key python-mode-map (kbd "C-c p") 'shell-this-file)

(defun shell-this-file ()
  (interactive)
  (save-buffer)
  (when (eq major-mode 'python-mode)
    (when (< (count-windows) 2)
      (split-window-horizontally))
    (let ((buf (buffer-file-name) ))
      (other-window 1)
      (eshell)
      (insert (concat "python3 " buf)))
    (eshell-send-input)
    (other-window 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq python-indent 4)))

(defun run-all-functions ()
  (interactive)
  (let ((a t))
    (while ay
      (when (eq major-mode 'ess-mode)
        (setq a (search-forward-regexp "[-=] function" nil t)))
      (if a (ess-eval-function-or-paragraph-and-step t))
      (when (eq major-mode 'python-mode)
        (setq a (search-forward-regexp "^def " nil t))
        (if a (ess-eval-function-or-paragraph-and-step t))))
    )
  (goto-char (point-min))
  )

(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(defun python-send-my-buffer (arg)
  (interactive "p")
  (if (not (eq arg 1))
      (setq smart-eval-ess-expression (replace-regexp-in-string "\[[0-9]+\]" (concat "[" (int-to-string arg) "]") smart-eval-ess-expression)))
  (save-buffer)
  (py-execute-buffer)
  (switch-to-buffer-other-window "*IPython*")
  (comint-interrupt-subjob)
  (my-clear)
  (other-window -1)
  (smart-eval-ess-executer)
  )







(setenv "LC_CTYPE" "UTF-8")

(setenv "PYTHONPATH" "/Users/pascal/watson/WEX/API/")

(defun pgrep (str)
  (interactive "sgrep for: ")
  (shell-command (concat "grep -rsnH \"" str "\" . --include=\"*.py\"&"))
  )

(defun phgrep (str)
  (interactive "sgrep for: ")
  (shell-command (concat "grep -rsnH \"" str "\" ~/egoroot --include=\"*.py\"&"))
  )


;; ("\\<\\(object\\|str\\|else\\|except\\|finally\\|try\\|\\)\\>" 0 py-builtins-face)
(font-lock-add-keywords 'python-mode '(("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)
                                       ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'font-lock-constant-face)))

(defun python-makecurrent ()
  (interactive)
  (let ((buffname (file-name-directory (buffer-file-name))))
    (other-window -1)
    (insert (concat "import os; os.chdir('" buffname "')\n"))
    (comint-send-input)
    (other-window -1)
    )
  )

(require 'sphinx-doc)
(add-hook 'python-mode-hook 'sphinx-doc-mode)



(add-hook 'elpy-mode-hook
          '(lambda ()
             (progn
               (add-to-list 'flymake-err-line-patterns '("\\([^|]+\\)| \\([^:]+\\):\\([0-9]+\\)$" 2 3 nil 1))
                (set (make-local-variable 'flymake-warning-predicate) "^.[^EF]")
               )))
(require 'expand-region)
(require 'python-mode-expansions)

(setq py--imenu-create-index-p t)
(setq py-split-windows-on-execute-function (quote split-window-horizontally))
;;(py-exception-name-face ((t (:foreground "#94bff3"))))


(provide 'emp-python)


(require 'linum)
(require 'pycoverage)

(defun my-coverage ()
  (interactive)
  (when (derived-mode-p 'python-mode)
    (progn
      (linum-mode)
      (pycoverage-mode))))
