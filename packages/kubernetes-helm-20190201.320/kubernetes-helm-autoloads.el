;;; kubernetes-helm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kubernetes-helm" "kubernetes-helm.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from kubernetes-helm.el

(autoload 'kubernetes-helm-dep-up "kubernetes-helm" "\
Run helm dep up on a directory.

DIRECTORY is the chart location.

\(fn DIRECTORY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kubernetes-helm" '("kubernetes-helm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kubernetes-helm-autoloads.el ends here
