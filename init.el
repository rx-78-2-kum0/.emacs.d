;;; init.el --- Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;; Emacs Configuration

;;; Code:

;; (setq debug-on-error t)


(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above!"))


;; Speed up startup
(setq auto-mode-case-fold nil)

(defvar old-file-name-handler-alist file-name-handler-alist)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))


(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))


(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)


(update-load-path)


;; set my own configuration
(with-temp-message ""
  ;; Constants
  (require 'init-const)

  ;; Customization
  (require 'init-custom)

  ;; Packages
  (require 'init-package)
  (require 'init-libs)

  ;; Preferences
  (require 'init-base)
  (require 'init-funcs)
  (require 'init-ui)
  (require 'init-utils)
  (require 'init-vcs)
  (require 'init-keys)

  (require 'init-edit)
  (require 'init-highlight)
  (require 'init-reader)

  (require 'init-window)

  (require 'init-completion)
  (require 'init-yasnippet)

  (require 'init-dired)
  ;; (require 'init-flycheck)
  (require 'init-flymake)

  ;; (require 'init-projectile)
  ;; (require 'init-workspace)

  ;; polymode
  ;; (require 'init-polymode)

  ;; ;; Programming
  ;; (require 'init-lsp)
  (require 'init-eglot)
  (require 'init-prettier)

  (require 'init-prog)
  (require 'init-org)
  (require 'init-go)
  (require 'init-dart)
  (require 'init-c)
  (require 'init-swift)
  ;; (require 'init-ruby)
  (require 'init-rust)
  (require 'init-lisp)
  ;; (require 'init-codeql)

  ;; ;; Web
  ;; (require 'init-js)
  (require 'init-web)

  (require 'init-sol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
q
