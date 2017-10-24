
(defun hrs/view-buffer-name ()
  "Display the filename of the current buffer."
  (interactive)
  (message (buffer-file-name)))

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun hrs/de-unicode ()
  "Tidy up a buffer by replacing all special Unicode characters
     (smart quotes, etc.) with their more sane cousins"
  (interactive)
  (let ((unicode-map '(("[\u2018\|\u2019\|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("\u2013" . "--")
                       ("\u2014" . "---")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
    (save-excursion
      (loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

(defun hrs/beautify-json ()
  "Pretty-print the JSON in the marked region. Currently shells
     out to `jsonpp'--be sure that's installed!"
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "jsonpp" (buffer-name) t)))

(defun hrs/unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun hrs/visit-last-dired-file ()
  "Open the last file in an open dired buffer."
  (end-of-buffer)
  (previous-line)
  (dired-find-file))

(defun hrs/visit-last-migration ()
  "Open the last file in 'db/migrate/'. Relies on projectile. Pretty sloppy."
  (interactive)
  (dired (expand-file-name "db/migrate" (projectile-project-root)))
  (hrs/visit-last-dired-file)
  (kill-buffer "migrate"))

(defun hrs/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun hrs/find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun hrs/insert-random-string (len)
  "Insert a random alphanumeric string of length len."
  (interactive)
  (let ((mycharset "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstyvwxyz"))
    (dotimes (i len)
      (insert (elt mycharset (random (length mycharset)))))))

(defun hrs/generate-password ()
  "Insert a good alphanumeric password of length 30."
  (interactive)
  (hrs/insert-random-string 30))

(global-prettify-symbols-mode t)

(setq exec-path (append exec-path '("/usr/local/bin")))

(add-hook 'after-init-hook 'global-company-mode)

;  (setq-default indent-tabs-mode nil)

(setq yas-snippet-dirs '(
"~/emacs/emp-25.2/.emacs.d/snippets/"
"~/emacs/emp-25.2/.emacs.d/packages/yasnippet-20170624.803/snippets/"
))
  (yas-global-mode 1)

(setq yas/indent-line nil)

(define-abbrev-table 'global-abbrev-table
  '((";name" "Harry R. Schwartz")
    (";email" "hello@harryrschwartz.com")
    (";tb" "harry@thoughtbot.com")
    (";site" "http://harryrschwartz.com")))

(setq-default abbrev-mode t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1) ; better/faster matching
(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(define-key yafolding-mode-map (kbd "<s-S-return>") 'yafolding-hide-parent-element)
(define-key yafolding-mode-map (kbd "<s-M-return>") 'yafolding-toggle-all) 
(define-key yafolding-mode-map (kbd "<s-return>") 'yafolding-toggle-element)

(defun my-dired-mode-hook ()
  ;; Let us have a key that puts the dired buffer into interactive renaming mode
  (helm-mode 0)
  )

(add-hook 'dired-mode-hook
          'my-dired-mode-hook)

(setq dired-use-ls-dired nil)

(setq diredp-hide-details-initially-flag nil)

(toggle-diredp-find-file-reuse-dir nil)

(require 'dired-x)
(require 'dired+)
(require 'dired-open)

(setq dired-open-extensions
      '(("pdf" . "evince")
        ("mkv" . "vlc")
        ("mp4" . "vlc")
        ("avi" . "vlc")))

(setq dired-clean-up-buffers-too t)

(setq dired-recursive-copies 'always)

(setq dired-recursive-deletes 'top)

;;(add-to-list 'company-backends 'company-jedi)
(setq company-global-modes '(not python-mode not ruby-mode))

(with-eval-after-load "auto-complete"
     (setq ac-auto-show-menu t)
     (setq ac-auto-start t)
     (setq completion-at-point-functions '(auto-complete))
     (set-face-background 'popup-summary-face "lightgrey")
     (set-face-foreground 'popup-summary-face "black")
     (set-face-background 'popup-menu-summary-face "lightgrey")
     (set-face-underline 'popup-summary-face "lightgrey")
     (set-face-background 'popup-tip-face "lightgrey")
)

(smex-initialize)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-hook 'gfm-mode-hook 'flyspell-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'gfm-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(global-set-key (kbd "C-c q") 'auto-fill-mode)

(setq-default fill-column 100)

(setq help-window-select t)

(helm-mode 1)

(require 'flycheck)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(add-to-list 'flycheck-checkers 'proselint)

(add-hook 'markdown-mode-hook #'flycheck-mode)
(add-hook 'gfm-mode-hook #'flycheck-mode)
(add-hook 'text-mode-hook #'flycheck-mode)
(add-hook 'org-mode-hook #'flycheck-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)

(eval-after-load 'grep
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

(eval-after-load 'wgrep
  '(define-key grep-mode-map
    (kbd "C-c C-c") 'wgrep-finish-edit))

(setq wgrep-auto-save-buffer t)

(wrap-region-global-mode t)
(wrap-region-add-wrapper "/" "/" nil 'ruby-mode)
(wrap-region-add-wrapper "`" "`" nil '(markdown-mode ruby-mode))

(projectile-global-mode)

(require 'engine-mode)

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(engine-mode t)

(setq mouse-autoselect-window nil)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(set-keyboard-coding-system nil)

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(setq org-hide-emphasis-markers t)

(setq org-startup-with-inline-images t)

(setq org-image-actual-width 600)

(setq org-agenda-skip-scheduled-if-done t)

(setq org-directory "~/org")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file "~/org/inbox.org")
(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(defun hrs/copy-tasks-from-inbox ()
  (when (file-exists-p org-inbox-file)
    (save-excursion
      (find-file org-index-file)
      (goto-char (point-max))
      (insert-file-contents org-inbox-file)
      (delete-file org-inbox-file))))

(setq org-agenda-files (list org-index-file))

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)

(setq org-log-done 'time)

(defun take-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  ;(org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert (concat "[[file:" filename "]]"))))

  (define-key global-map "\C-x\p" 'take-org-screenshot)

(setq org-capture-templates
      '(("b" "Blog idea"
         entry
         (file (org-file-path "blog-ideas.org"))
         "* TODO %?\n")

        ("g" "Groceries"
         checkitem
         (file (org-file-path "groceries.org")))

        ("l" "Today I Learned..."
         entry
         (file+datetree (org-file-path "til.org"))
         "* %?\n")

        ("r" "Reading"
         checkitem
         (file (org-file-path "to-read.org")))

        ("t" "Todo"
         entry
         (file org-index-file)
         "* TODO %?\n")))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(defun open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (hrs/copy-tasks-from-inbox)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'open-index-file)

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "M-n") 'org-capture-todo)
(add-hook 'gfm-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))
(add-hook 'haskell-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))

(define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-ct" 'org-deadline)

(with-eval-after-load "org" (define-key org-mode-map (kbd "s-i") 'org-toggle-inline-images))

(defun open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (hrs/copy-tasks-from-inbox)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'open-index-file)

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "M-n") 'org-capture-todo)
(add-hook 'gfm-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))
(add-hook 'haskell-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))

(with-eval-after-load "org" (define-key org-mode-map (kbd "C-k") nil)
 (define-key org-mode-map [backspace] nil)
 (define-key org-mode-map (kbd "C-'") nil)
 (define-key org-mode-map (kbd "C-,") nil)
 (define-key org-mode-map (kbd "<M-RET>") nil))

(require 'ox-md)
(require 'ox-beamer)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (gnuplot . t)
   (js . t)
   (go . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-export-with-smart-quotes t)

(setq org-html-postamble nil)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq TeX-parse-self t)

(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-math-mode)
            (setq TeX-master t)))

(defvar org-electric-pairs '((?/ . ?/) (?= . ?=)
                             (?\_ . ?\_) (?~ . ?~) (?+ . ?+)) "Electric pairs for org-mode.")

(defun org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook 'org-add-electric-pairs)

;;(add-hook 'go-mode-hook 'gorepl-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
;;(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'git-gutter-mode)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; (add-hook 'go-mode-hook 'flycheck-gometalinter-setup)
;;(remove-hook 'go-mode-hook 'flymake-mode)

;  (add-hook 'go-mode-hook
;            (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))

(require 'go-flycheck)

; Use goimports instead of go-fmt
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(with-eval-after-load "gorepl-mode"
 (define-key gorepl-mode-map (kbd "C-<return>") 'gorepl-eval-line)
)

(defun my-go-mode-hook ()
  (define-key global-map (kbd "M-.") nil)
  (define-key global-map (kbd "M-,") nil)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-M-.") 'godef-jump-other-window)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (go-guru-hl-identifier-mode)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

(exec-path-from-shell-copy-env "GOPATH")

'(eval-after-load 'company
  (add-to-list 'company-backends 'company-go)
 (add-to-list 'company-backends 'company-elisp))

(defun my-python-mode-hook ()
                     (elpy-enable)
                     ;; Disable flymake in Elpy.
                     (setq elpy-modules
                           (quote (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults))
                           )
                     (elpy-mode)
                     (flycheck-mode)
                     (setq elpy-rpc-python-command "python3")
                     (elpy-use-ipython)
                     (setq elpy-rpc-backend "jedi")
                     (company-mode 0)
                     (auto-complete-mode t)
                     (jedi:setup) 
                     (setq python-check-command (concat emacsd "pyflymake.py"))
                     (define-key elpy-mode-map (kbd "C-<return>") 'new-python-eval)
                     (setq elpy-test-runner 'elpy-test-pytest-runner)
                     (setq jedi:complete-on-dot t)
)


                     
  (add-hook 'python-mode-hook 'my-python-mode-hook)
  ;; (flymake-mode t)
  ;;                   (setq-local flymake-start-syntax-check-on-newline t)
  ;;                   (setq flymake-no-changes-timeout 10000)

(defun my-python-keybindings-hook ()
   (define-key python-mode-map (kbd "<tab>") 'py-indent-line)
   (local-set-key (kbd "M-,") 'pop-tag-mark)
               (local-set-key "\C-ca" 'pytest-all)
               (local-set-key "\C-c0" 'pytest-pdb-one)
               (local-set-key "\C-c1" 'pytest-one)
;               (local-set-key (kbd "s-<return>") 'iterm-send-text-clipboard)
   )
   (add-hook 'python-mode-hook 'my-python-keybindings-hook)

(with-eval-after-load "python"
               (define-key python-mode-map  (kbd "s-<return>") nil)
               (define-key python-mode-map (kbd "s-<return>") 'iterm-send-text-clipboard)
)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun my-js2-mode-hook ()
  (yafolding-mode)
  (company-mode-on)
  (helm-mode)
  )
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(with-eval-after-load "nodejs-repl-mode"
  (require 'nodejs-repl-eval)
)

(defun my-js2-mode-hook ()
  (require 'nodejs-repl-eval)
  (local-set-key (kbd "C-<return>") 'nodejs-repl-eval-dwim)
  (define-key js2-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-sexp)
  (define-key js2-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
  (define-key js2-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
  (define-key js2-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)
)

  (add-hook 'js2-mode-hook 'my-js2-mode-hook)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(add-hook 'json-mode-hook 'yafolding-mode)

(defun my-json-mode-hook ()
  (local-set-key (kbd "C-c C-j") 'jq-interactively)
  (flycheck-mode))
(add-hook 'json-mode-hook 'my-json-mode-hook)

(defun my-ruby-mode-hook ()
;;  (define-key global-map (kbd "M-.") nil)
;;  (define-key global-map (kbd "M-,") nil)
(inf-ruby-minor-mode t)
(robe-mode t)
; (inf-ruby)
(local-set-key (kbd "C-<return>") 'ruby-send-line)
; (robe-start)
(auto-complete-mode t)
(yafolding-mode t)
)


(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-inf-ruby 'company-robe))

(defun my-typescript-mode-hook ()
               (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              (tide-hl-identifier-mode 0)
              (yafolding-mode)
              ;; company is an optional dependency. You have to
              ;; install it separately via package-install
              (company-mode-on)
              (helm-mode)
  )
  (add-hook 'typescript-mode-hook 'my-typescript-mode-hook)
(setq tide-tssserver-executable "~/.nvm/versions/node/v6.10.3/bin/tsserver")
(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

(with-eval-after-load "typescript"
 (define-key typescript-mode-map (kbd "s-n") 'tide-nav)
 (define-key yafolding-mode-map  (kbd "s-<return>") 'yafolding-toggle-element)
 (define-key typescript-mode-map (kbd "C-<return>") 'iterm-send-text-clipboard)
)

(with-eval-after-load "shell"
 (define-key sh-mode-map (kbd "M-.") 'ffap)
 (define-key yafolding-mode-map  (kbd "s-<return>") 'yafolding-toggle-element)
 (define-key sh-mode-map (kbd "C-<return>") 'iterm-send-text)
)

(define-key global-map (kbd "s-c") 'kill-ring-save)
(define-key global-map (kbd "s-a") 'mark-whole-buffer)
(define-key global-map (kbd "s-l") 'goto-line)
(define-key global-map (kbd "M-f") 'company-files)
(define-key global-map (kbd "M-s-.") 'ffap)
(define-key global-map (kbd "<f10>") 'maximize-frame-toggle)
(define-key global-map (kbd "<end>") 'org-end-of-line)
(define-key global-map (kbd "<home>") 'org-beginning-of-line)
(define-key global-map (kbd "M-d") nil)
(key-chord-define-global "xj" 'helm-mini)
(global-set-key (kbd "<f5>") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "<f6>") 'hrs/split-window-right-and-switch)
(global-set-key (kbd "<f7>") 'other-window)
(global-set-key (kbd "<f8>") 'delete-window)
(global-set-key (kbd "<f11>") 'helm-all-mark-rings)
(global-set-key (kbd "<f12>") 'helm-semantic-or-imenu)
(global-set-key (kbd "s-s") 'helm-ag)
(global-set-key (kbd "C-c r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-x M-f") 'helm-find-files)
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-d") 'iterm-goto-filedir-or-home)

(setq magit-refs-show-commit-count nil)
(setq magit-log-arguments '("-n256" "--graph" "--decorate" "--color"))
;(setq magit-refs-margin nil)

(defun my-magit-mode-hook ()
            (helm-mode 0)
)
(add-hook 'magit-mode-hook 'my-magit-mode-hook)

(global-git-gutter-mode t)

(global-discover-mode)

(discover-add-context-menu
 :context-menu '(isearch
              (description "Isearch, occur and highlighting")
              (lisp-switches
               ("-cf" "Case should fold search" case-fold-search t nil))
              (lisp-arguments
               ("=l" "context lines to show (occur)"
                "list-matching-lines-default-context-lines"
                (lambda (dummy) (interactive) (read-number "Number of context lines to show: "))))
              (actions
               ("Isearch"
                ("_" "isearch forward symbol" isearch-forward-symbol)
                ("w" "isearch forward word" isearch-forward-word))
               ("Occur"
                ("o" "occur" occur))
               ("More"
                ("h" "highlighters ..." makey-key-mode-popup-isearch-highlight))))
 :bind "M-d s")

(discover-add-context-menu
 :context-menu '(yafolding
              (description "Isearch, occur and highlighting")
              (lisp-switches)
              (lisp-arguments)
              (actions
               ("yafolding"
                ("h" "hide element" yafolding-hide-element)
                ("s" "show element" yafolding-show-element)
                ("t" "toggle element" yafolding-toggle-element)
                ("H" "hide all" yafolding-hide-all)
                ("S" "show all" yafolding-show-all)
                ("T" "toggle all" yafolding-toggle-all)
                ("p" "go parent element" yafolding-go-parent-element)
                ("P" "hide parent element" yafolding-hide-parent-element)))) 
 :bind "M-d y"
 :mode 'yafolding
 :mode-hook 'yafolding-mode-hook 
)

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(define-key global-map (kbd "M-e") 'company-yasnippet)

(define-key global-map (kbd "C-h b") 'helm-descbinds)

(define-key global-map (kbd "M-s h") 'helm-ag)
(eval-after-load 'helm-ag
  (custom-set-variables
   '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
   '(helm-ag-command-option "--all-text")
   '(helm-ag-insert-at-point 'symbol)
   '(helm-follow-mode-persistent t)))

(eval-after-load 'helm-semantic-or-imenu
  (custom-set-variables
   '(helm-follow-mode-persistent t)))

(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(defvar frame-maximized 1)
(defun maximize-frame-toggle ()
"Doc-string for `my-switch` function."
(interactive)
  (cond
   ((= frame-maximized 0)
    (maximize-frame) 
      (setq frame-maximized 1))
   ((= frame-maximized 1)
     (restore-frame)
      (setq frame-maximized 0)) ) )

(defun get-file-dir-or-home ()
  "If inside a file buffer, return the directory, else return home"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
    "~/"
      (file-name-directory filename))))

(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n" (get-file-dir-or-home))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
                      (shell-command "sleep 0.5")
                      (do-applescript "tell application \"System Events\" to keystroke {tab} using {command down}")
(message (concat (get-file-dir-or-home) " opened in iTerm2."))
  )

;;; iterm.el - Send text to a running iTerm instance

(require 'pcase)
(require 'thingatpt)

;; To match SublimeText's key binding:
;; (global-set-key (kbd "<C-return>") 'iterm-send-text)

(defvar iterm-default-thing 'line
  "The \"thing\" to send if no region is active.
Can be any symbol understood by `bounds-of-thing-at-point'.")

(defvar iterm-empty-line-regexp "^[[:space:]]*$"
  "Regexp to match empty lines, which will not be sent to iTerm.
Set to nil to disable removing empty lines.")

(defun iterm-escape-string (str)
  (let* ((str (replace-regexp-in-string "\\\\" "\\\\" str nil t))
         (str (replace-regexp-in-string "\"" "\\\"" str nil t)))
    str))

(defun iterm-last-char-p (str char)
  (let ((length (length str)))
    (and (> length 0)
         (char-equal (elt str (- length 1)) char))))

(defun iterm-chop-newline (str)
  (let ((length (length str)))
    (if (iterm-last-char-p str ?\n)
        (substring str 0 (- length 1))
      str)))

(defun iterm-maybe-add-newline (str)
  (if (iterm-last-char-p str ? )
      (concat str "\n")
    str))

  (defun enclose-brackets (str)
    ;;        (let (result ""))
    (setq str (concatenate  'string str "\n"))
        str)


(defun iterm-handle-newline (str)
  (iterm-maybe-add-newline (iterm-chop-newline str)))

(defun iterm-maybe-remove-empty-lines (str)
  (if iterm-empty-line-regexp
      (let ((regexp iterm-empty-line-regexp)
            (lines (split-string str "\n")))
        (mapconcat #'identity
                   (delq nil (mapcar (lambda (line)
                                       (unless (string-match-p regexp line)
                                         line))
                                     lines))
                   "\n"))
    str))

(defun iterm-send-string (str)   
          "Send STR to a running iTerm instance."
          (let* ((str (iterm-maybe-remove-empty-lines str))
                 (str (iterm-handle-newline str))
                 (str (iterm-escape-string str)))
            (shell-command (concat "osascript "
                                   "-e 'tell app \"iTerm2\"' "
                                   "-e 'tell current window' "
                                   "-e 'tell current session' "
                                   "-e 'write text \"" str "\"' "
                                   "-e 'end tell' "
                                   "-e 'end tell' "
                                   "-e 'end tell' ")))
  ;    (do-applescript "tell application \"iTerm2\" to activate")
  ;    (shell-command "sleep 1.5")
  ;    (do-applescript "tell application \"System Events\" to keystroke {tab} using {command down}")
    )
          (defun iterm-send-text-clipboard ()
            (interactive) 
            (copy-region-as-kill 0 0 t)
;;            (new-python-get-text)
        ;; Could cut op the osa script into seperate file. 
                            (shell-command (concat "osascript "
            ;                                     "-e 'set the clipboard to \"" str "\"' "
        ;                                         "-e 'tell application \"iTerm2\"' "
        ;                                         "-e 'activate' "
        ;                                         "-e 'end tell' "
                                                 "-e 'tell application \"iTerm\" to activate' "
                                                 "-e 'tell application \"System Events\" to tell process \"iTerm2\"' "
                                                 "-e 'keystroke \"v\" using {command down}' "
        ;                                         "-e 'key down {return}' "
        ;                                         "-e 'key up {return}' "
        ;                                         "-e 'keystroke \"v\" using {command down}' "
                                                 "-e 'end tell' "
        ;                                         "-e 'end tell' "
                                                 ))
                            (shell-command "sleep 0.5")
                            (do-applescript "tell application \"System Events\" to tell process \"iTerm2\" to keystroke return")
                            (shell-command "sleep 0.5")
                            (do-applescript "tell application \"System Events\" to keystroke {tab} using {command down}")
                            (message "Command pasted and executed into iTerm2.")
                            )


                      (defun iterm-text-bounds ()
                        (pcase-let ((`(,beg . ,end) (if (use-region-p)
                                                        (cons (region-beginning) (region-end))
                                                      (bounds-of-thing-at-point
                                                       iterm-default-thing))))
                          (list beg end)))

                      (defun iterm-send-text (beg end)
                        "Send buffer text in region from BEG to END to iTerm.
                      If called interactively without an active region, send text near
                      point (determined by `iterm-default-thing') instead."
                        (interactive (iterm-text-bounds))
                        (let ((str (buffer-substring-no-properties beg end)))
                          (iterm-send-string str))
                        (forward-line 1)
                         (message "Command written into iTerm2.")
      )

                      (defun iterm-send-text-brackets (beg end)
                        "Send buffer text in region from BEG to END to iTerm.
                      If called interactively without an active region, send text near
                      point (determined by `iterm-default-thing') instead."
                        (interactive (iterm-text-bounds))
                        (let ((str (buffer-substring-no-properties beg end)))
                          (setq str (enclose-brackets str))
                          (message str)
                        (forward-line 1)))
                      (provide 'iterm)

(defun new-python-get-start ()
  (interactive)
  (ignore-errors
    (while (looking-at "[ ]*$")
      (next-line)
      ))
  (move-end-of-line 1)
  (search-backward-regexp "^[@a-zA-Z0-9#\[\{]" 0 t)
  (when (or (looking-at "else") (looking-at "elif") (looking-at "except") (looking-at "finally"))
    (search-backward-regexp "^if" 0 t)
    )
  (ignore-errors
    (previous-line)
    (while (looking-at "[@a-zA-Z]")
      (previous-line))
    (next-line)
    )
  (point)
  )

(defun new-python-get-end ()
  (interactive)
  (ignore-errors
    (while (looking-at "^[@a-zA-Z0-9#\[\{]")
      (next-line)))
  (if (search-forward-regexp "^[@a-zA-Z0-9#\[\{]" (point-max) t)
      (progn (move-beginning-of-line 1)
             (when (or (looking-at "elif") (looking-at "else"))
               (search-forward-regexp "^else" (point-max) t)
               (search-forward-regexp "^[@a-zA-Z0-9#\[\{]" (point-max) t)
               (left-char 1))
             (when (or (looking-at "except"))
               (search-forward-regexp "^except" (point-max) t)
               (search-forward-regexp "^[@a-zA-Z0-9#\[\{]" (point-max) t)
               (left-char 1))
             (when (or (looking-at "finally"))
               (search-forward-regexp "^finally" (point-max) t)
               (search-forward-regexp "^[@a-zA-Z0-9#\[\{]" (point-max) t)
               (left-char 1))
             (point))
    (point-max))
  )

(defun new-python-get-text ()
  (interactive)
  (ignore-errors
    (let ((start (new-python-get-start))
          (end (new-python-get-end)))
      (when (eq (point-max) end)
        (goto-char end))
      (kill-ring-save start end)
      ))
  )
