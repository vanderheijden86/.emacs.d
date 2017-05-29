
;;; .emacs - Pascal van Kooten <kootenpv@gmail.com>

;;Benchmarking
;;(add-to-list 'load-path "~/emp-24.5/.emacs.d/packages/benchmark-init-el/")
;;(require 'benchmark-init)
;;(benchmark-init/activate)

;;(load-file "~/emp-24.5/.emacs.d/packages/benchmark-init-el/benchmark-init-loaddefs.el")


;; DEFAULT-DIRECTORY SHOULD BE ONLY ONE NECCESSARY
(setq default-directory user-emacs-directory)

(setq emacsd (concat user-emacs-directory ".emacs.d/"))

                (setq empinit (concat emacsd "init/"))

(setq custom-file (concat empinit "emp-custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq custom-theme-directory (concat emacsd "themes/"))

;;;;;;;; LOAD-PATHS: .emacs.d and all it's first level subdirecties.
(add-to-list 'load-path emacsd)
(dolist (f (directory-files emacsd))
  (let ((name (concat emacsd "/" f)))
    (when (and (file-directory-p name)
               (not (equal f ".."))
               (not (equal f ".")))
      (add-to-list 'load-path name))))

;; load external packages
(load (concat emacsd "package-loader.el"))

(setq backup-by-copying t
      backup-directory-alist `((".*" . ,(concat default-directory "backups")))
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 10
      version-control t)

;; should be better now
(setq auto-save-file-name-transforms
      `((".*" ,(concat default-directory "auto-saves") t)))

;;; mode associations
(add-to-list 'auto-mode-alist '("\\.snippet\\'" . snippet-mode))

(add-to-list 'auto-mode-alist '("\\.gsp\\'" . html-mode))

(add-to-list 'auto-mode-alist '("\\.\\(pl\\|pro\\|lgt\\)" . prolog-mode))

(add-to-list 'auto-mode-alist '("\\.sol\\'" . solidity-mode))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

;;; ensure bashrc is loaded in emacs
(when window-system (set-exec-path-from-shell-PATH))

(require 'server)
(or (server-running-p)
    (server-start))

;; (setq explicit-bash-args '("--noediting" "--login" "-i"))



(require 'emp-external-plugins)

(require 'emp-display)

(require 'emp-keybindings)

(require 'emp-misc-functions)

(require 'emp-misc-settings)

(require 'emp-c++)

(provide 'emp-init)

(require 'emp-dired)

;; (require 'nodejs-repl-eval)
;; (setq ns-auto-hide-menu-bar t)
;; (set-frame-position nil 0 -21)
;; (tool-bar-mode 0)
;; (set-frame-size nil 206 54)

(org-babel-load-file "~/emp-24.5/.emacs.d/configuration_andre.org")
