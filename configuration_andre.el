
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
"~/emp-24.5/.emacs.d/snippets/text-mode/" 
"/Users/avdh/emp-24.5/.emacs.d/packages/yasnippet-20170310.1724/snippets"
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

(smex-initialize)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-hook 'gfm-mode-hook 'flyspell-mode)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'gfm-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(global-set-key (kbd "C-c q") 'auto-fill-mode)

(setq-default fill-column 100)

(add-hook 'temp-buffer-show-hook (lambda () (other-window 1)))
;;remove-hook 'temp-buffer-window-show-hook (lambda () (other-window 1))
;;(remove-hook 'help-mode-hook (lambda () (other-window 1)))

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

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(setq org-hide-emphasis-markers t)

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

(setq dired-use-ls-dired nil)

(setq diredp-hide-details-initially-flag nil)

(diredp-toggle-find-file-reuse-dir 1)

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
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (go-guru-hl-identifier-mode)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

(exec-path-from-shell-copy-env "GOPATH")

'(eval-after-load 'company
  (add-to-list 'company-backends 'company-go)
 (add-to-list 'company-backends 'company-elisp))

;;  (add-to-list 'company-backends 'company-jedi)          ; add company-jedi to the backends.

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

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

(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode 'robe-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-inf-ruby 'company-robe))

(define-key global-map (kbd "<f10>") 'toggle-frame-fullscreen)
(define-key global-map (kbd "<end>") 'org-end-of-line)
(define-key global-map (kbd "<home>") 'org-beginning-of-line)
(define-key global-map (kbd "M-d") nil)
(key-chord-define-global "xj" 'helm-mini)
(global-set-key (kbd "<f5>") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "<f6>") 'hrs/split-window-right-and-switch)
(global-set-key (kbd "<f7>") 'other-window)
(global-set-key (kbd "<f8>") 'delete-window)
(global-set-key (kbd "<f11>") 'helm-semantic-or-imenu)
(global-set-key (kbd "<f12>") 'helm-all-mark-rings)
(global-set-key (kbd "C-c r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq magit-refs-show-commit-count nil)
;(setq magit-refs-margin nil)

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
       '(helm-follow-mode-persistent t)))

(eval-after-load 'helm-semantic-or-imenu
  (custom-set-variables
   '(helm-follow-mode-persistent t)))
