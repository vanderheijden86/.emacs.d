(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auto-save-interval 5)
 '(available-screen-pixel-bounds nil)
 '(company-auto-complete t)
 '(company-auto-complete-chars (quote (32 95 41 46)))
 '(company-backends
   (quote
    (company-tide company-emacs-eclim company-go company-ghc company-bbdb company-css company-semantic company-clang company-xcode company-cmake company-capf company-files company-abbrev
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-dabbrev company-ghc company-inf-ruby)))
 '(company-begin-commands (quote (self-insert-command)))
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 0)
 '(compilation-message-face (quote default))
 '(conf-assignment-space nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "a49760e39bd7d7876c94ee4bf483760e064002830a63e24c2842a536c6a52756" "2f0a552a9d14fe8ddaaacdb7b82a0eee1ea1f7f5d0850789915e5b04a1b9669f" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "b51c2dda65e8e7e66ab1b06bc10b59e61c153b0cf928f296efab5a7574779fb6" "7a6059160b29bffb886dfaddaa4b351588090bcb74946c30a6782f50288c9892" "9ab634dcc9131f79016c96c4955298409649f6538908c743a8a9d2c6bc8321ef" "614d92e2a3af6a08bfe6ff68a59875f987271415c3bda7bec1216b80f5db741d" "c3d843722b3bf87f037a6d54cdf0042ce4b2150bd24af4e32eb7342f782bdd44" default)))
 '(dired-use-ls-dired nil)
 '(eldoc-idle-delay 0.1)
 '(eldoc-minor-mode-string "Eldoc")
 '(elpy-company-post-completion-function (quote elpy-company-post-complete-parens))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(elpy-rpc-python-command "python3")
 '(elpy-syntax-check-command "")
 '(fci-rule-color "#14151E")
 '(fill-column 100)
 '(flycheck-check-syntax-automatically (quote (save idle-change new-line mode-enabled)))
 '(flycheck-typescript-tslint-config "~/tslint.json")
 '(flycheck-typescript-tslint-executable nil)
 '(flymake-no-changes-timeout 0.1)
 '(flymake-start-syntax-check-on-newline nil)
 '(global-company-mode nil)
 '(global-discover-mode nil)
 '(global-eldoc-mode t)
 '(go-mode-hook (quote (my-go-mode-hook git-gutter-mode flycheck-mode)))
 '(goflymake-debug t)
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point (quote symbol))
 '(helm-follow-mode-persistent t)
 '(helm-mode nil)
 '(helm-source-names-using-follow
   (quote
    ("Occur" "global-mark-ring" "Imenu" "mark-ring" "Search at ~/emp-24.5/.emacs.d/")))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(ivy-mode t)
 '(jdee-server-dir "/Users/avdh/Tools/jdee-server/target")
 '(jedi:tooltip-method (quote (pos-tip popup)))
 '(js-indent-level 2)
 '(mac-option-modifier nil)
 '(magit-bury-buffer-function (quote magit-restore-window-configuration))
 '(magit-commit-arguments nil)
 '(magit-diff-use-overlays nil)
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
 '(neo-theme (quote nerd))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files
   (quote
    ("~/Google Drive/Projects/Blockchain/b3i.org" "~/.emacs.d/configuration_andre.org" "~/org/index.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pptx\\'" . default)
     ("\\.pdf\\'" . default))))
 '(org-image-actual-width 750)
 '(org-log-done (quote time))
 '(org-mobile-directory "~/Dropbox/org-share")
 '(package-selected-packages
   (quote
    (ac-emacs-eclim tern neotree ng2-mode darktooth-theme jedi kotlin-mode python-mode pythonic string-utils vagrant-tramp dired-open go-playmode maxframe helm-ag ample-theme monokai-theme flatland-theme helm-themes multiple-cursors wgrep go-guru go-eldoc indium ac-js2 jquery-doc jq helm-descbinds discover yafolding robe company-inf-ruby inf-ruby ob-go flycheck-gometalinter jq-mode flycheck-pos-tip json-mode markdown-preview-mode go-playground gotest company-ghc flycheck use-package docker-tramp exec-path-from-shell company-go gorepl-mode load-dir zenburn-theme yaml-mode wrap-region websocket undo-tree ubuntu-theme typescript switch-window sphinx-doc solidity-mode solarized-theme smex restclient realgud pytest pycoverage py-autopep8 powerline persp-projectile org-bullets openwith nodejs-repl markdown-mode mark-multiple mark-more-like-this magit kooten-theme key-chord json-reformat js2-mode jedi-direx imenu-anywhere ido-vertical-mode ido-ubiquitous htmlize groovy-mode go-mode git-timemachine git-gutter ghc frame-cmds flymake-solidity flymake-json flymake-jslint flymake-cursor flx-ido expand-region etags-select engine-mode elscreen-persist elpy edit-server dockerfile-mode dired+ diminish buffer-move buffer-extension benchmark-init backup-walker avy autopair ag)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(python-check-command ".emacs.d/pyflymake.py")
 '(python-skeleton-autoinsert t)
 '(realgud:ipdb-command-name "~/bin/ipdb" t)
 '(realgud:pdb-command-name "python2 -m pdb")
 '(recentf-arrange-rules (quote (("Go files (%d)" ".\\.go'"))))
 '(set-mark-command-repeat-pop t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(solidity-solc-path "/Users/pascal/projects/solcpy")
 '(tab-stop-list (quote (0 2)))
 '(tab-width 2)
 '(tabbar-separator (quote (0.5)))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tide-allow-popup-select (quote (code-fix jump-to-implementation)))
 '(tss-ac-summary-truncate-length 50)
 '(tss-popup-help-key s-/)
 '(typescript-mode-hook
   (quote
    ((lambda nil
       (flycheck-mode 1)
       (setq flycheck-check-syntax-automatically
             (quote
              (save mode-enabled)))
       (eldoc-mode 1)
       (company-mode-on)))))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "mediumspringgreen")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "goldenrod")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "mediumspringgreen")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "goldenrod")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "mediumspringgreen"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])
 '(yas-snippet-dirs
   (quote
    ("~/emp-24.5/.emacs.d/snippets/" "/Users/avdh/emp-24.5/.emacs.d/packages/elpy-20170303.621/snippets/" "/Users/avdh/emp-24.5/.emacs.d/packages/yasnippet-20170310.1724/snippets/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#14151E" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "nil" :family "Monaco"))))
 '(fringe ((t (:foreground "#14151E"))))
 '(neo-banner-face ((t (:foreground "#528fd1" :weight bold)))))

