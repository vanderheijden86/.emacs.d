(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-interval 5)
 '(company-backends
   (quote
    (company-go company-ghc company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files company-abbrev
                (company-dabbrev-code company-gtags company-etags company-keywords)
                company-oddmuse company-dabbrev)))
 '(custom-safe-themes
   (quote
    ("b51c2dda65e8e7e66ab1b06bc10b59e61c153b0cf928f296efab5a7574779fb6" "7a6059160b29bffb886dfaddaa4b351588090bcb74946c30a6782f50288c9892" "9ab634dcc9131f79016c96c4955298409649f6538908c743a8a9d2c6bc8321ef" "614d92e2a3af6a08bfe6ff68a59875f987271415c3bda7bec1216b80f5db741d" "c3d843722b3bf87f037a6d54cdf0042ce4b2150bd24af4e32eb7342f782bdd44" default)))
 '(dired-use-ls-dired nil)
 '(elpy-rpc-python-command "python3")
 '(elpy-syntax-check-command "/Users/avdh/emp-24.5/.emacs.d/pyflymake.py")
 '(goflymake-debug t)
 '(magit-bury-buffer-function (quote magit-restore-window-configuration))
 '(magit-commit-arguments nil)
 '(markdown-preview-style
   "https://sindresorhus.com/github-markdown-css/github-markdown.css")
 '(mm-body-charset-encoding-alist
   (quote
    ((iso-2022-jp . 7bit)
     (iso-2022-jp-2 . 7bit)
     (utf-16 . base64)
     (utf-16be . base64)
     (utf-16le . base64)
     (utf-8 . 8bit))))
 '(org-agenda-files
   (quote
    ("~/Google Drive/Projects/Flipside/flipside.org" "~/Google Drive/Documentation/planning.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pptx\\'" . default)
     ("\\.pdf\\'" . default))))
 '(org-image-actual-width 10)
 '(org-log-done (quote time))
 '(org-mobile-directory "~/Dropbox/org-share")
 '(package-selected-packages
   (quote
    (discover yafolding flycheck-pos-tip json-mode markdown-preview-mode vagrant-tramp go-playground gotest company-ghc flycheck use-package docker-tramp exec-path-from-shell company-go gorepl-mode load-dir zenburn-theme yaml-mode wrap-region websocket undo-tree ubuntu-theme typescript switch-window sphinx-doc solidity-mode solarized-theme smex restclient realgud python-mode pytest pycoverage py-autopep8 powerline persp-projectile org-bullets openwith nodejs-repl multiple-cursors markdown-mode mark-multiple mark-more-like-this magit kooten-theme key-chord json-reformat js2-mode jedi-direx imenu-anywhere ido-vertical-mode ido-ubiquitous htmlize groovy-mode go-mode git-timemachine git-gutter ghc frame-cmds flymake-solidity flymake-json flymake-jslint flymake-cursor flx-ido expand-region etags-select engine-mode elscreen-persist elpy edit-server dockerfile-mode dired-open dired+ diminish company-jedi buffer-move buffer-extension benchmark-init backup-walker avy autopair ag)))
 '(py-ipython-command "~/bin/ipython")
 '(py-ipython-command-args "--automagic --pylab")
 '(python-check-command "pyflymake.py")
 '(python-shell-interpreter "ipython")
 '(python-shell-interpreter-args "--simple-prompt -i")
 '(python-shell-interpreter-interactive-arg "--simple-prompt -i")
 '(realgud:ipdb-command-name "~/bin/ipdb" t)
 '(realgud:pdb-command-name "python2 -m pdb")
 '(recentf-arrange-rules (quote (("Go files (%d)" ".\\.go'"))))
 '(solidity-solc-path "/Users/pascal/projects/solcpy")
 '(tabbar-separator (quote (0.5))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#14151E" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "nil" :family "Monaco"))))
 '(fringe ((t (:foreground "#14151E")))))
