;;; buffer-extension-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "buffer-extension" "buffer-extension.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from buffer-extension.el

(autoload 'kill-current-mode-buffers "buffer-extension" "\
Kill all buffers that major mode same with current mode.

\(fn)" t nil)

(autoload 'kill-current-mode-buffers-except-current "buffer-extension" "\
Kill all buffers that major mode same with current mode.
And don't kill current buffer.

\(fn)" t nil)

(autoload 'kill-special-mode-buffers "buffer-extension" "\
Kill all buffers that major mode that user given.

\(fn)" t nil)

(autoload 'kill-special-mode-buffers-internal "buffer-extension" "\
Kill all buffers that major MODE same with special.
If option EXCEPT-CURRENT-BUFFER is `non-nil',
kill all buffers with MODE except current buffer.

\(fn MODE &optional EXCEPT-CURRENT-BUFFER)" t nil)

(autoload 'kill-all-buffers-except-current "buffer-extension" "\
Kill all buffers except current buffer.

\(fn)" t nil)

(autoload 'kill-other-window-buffer "buffer-extension" "\
Kill the buffer in other window.

\(fn)" t nil)

(autoload 'rename-file-and-buffer "buffer-extension" "\
Renames both current buffer and file it's visiting to NEW-NAME.

\(fn NEW-NAME)" t nil)

(autoload 'move-buffer-file "buffer-extension" "\
Move both current buffer and file it's visiting to DIR.

\(fn DIR)" t nil)

(autoload 'buffer-order-next-mark "buffer-extension" "\
Jump to next mark.

\(fn ARG)" t nil)

(autoload 'buffer-order-prev-mark "buffer-extension" "\
Jump to previous mark.

\(fn ARG)" t nil)

(autoload 'copy-buffer-file-name-as-kill "buffer-extension" "\
Copy the buffer-file-name to the kill-ring

\(fn CHOICE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buffer-extension" '("try-to-switch-buffer")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; buffer-extension-autoloads.el ends here
