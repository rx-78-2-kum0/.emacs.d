;;; init-swift.el --- Initialize swift configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; some configuration of swift.

;;; Code:


(use-package swift-mode
  ;; :ensure nil
  ;; :mode
  ;; ("\\.swift\\'" . swift-mode)
  :functions reformatter-define
  :config
  ;; Format before save
  (defvar swift-format)
  (reformatter-define swift-format
    :program "swift-format"
    :args '("format"))
  (add-hook 'swift-mode-hook 'swift-format-on-save-mode)

  ;; Flymake support
  (require 'flymake)

  ;; This name doesn't end in "function" to avoid being unconditionally marked as risky.
  (defvar-local swift-find-executable-fn 'executable-find
    "Function to find a command executable.
The function is called with one argument, the name of the executable to find.
Might be useful if you want to use a swiftc that you built instead
of the one in your PATH.")
  (put 'swift-find-executable-fn 'safe-local-variable 'functionp)

  (defvar-local swift-syntax-check-fn 'swift-syntax-check-directory
    "Function to create the swift command-line that syntax-checks the current buffer.
The function is called with two arguments, the swiftc executable, and
the name of a temporary file that will contain the contents of the
current buffer.
Set to 'swift-syntax-check-single-file to ignore other files in the current directory.")
  (put 'swift-syntax-check-fn 'safe-local-variable 'functionp)

  (defvar-local swift-syntax-check-args '("-typecheck")
    "List of arguments to be passed to swiftc for syntax checking.
Elements of this list that are strings are inserted literally
into the command line.  Elements that are S-expressions are
evaluated.  The resulting list is cached in a file-local
variable, `swift-syntax-check-evaluated-args', so if you change
this variable you should set that one to nil.")
  (put 'swift-syntax-check-args 'safe-local-variable 'listp)

  (defvar-local swift-syntax-check-evaluated-args
      "File-local cache of swift arguments used for syntax checking
variable, `swift-syntax-check-args', so if you change
that variable you should set this one to nil.")

  (defun swift-syntax-check-single-file (swiftc temp-file)
    "Return a flymake command-line list for syntax-checking the current buffer in isolation"
    `(,swiftc ("-typecheck" ,temp-file)))

  (defun swift-syntax-check-directory (swiftc temp-file)
    "Return a flymake command-line list for syntax-checking the
current buffer along with the other swift files in the same
directory."
    (let* ((sources nil))
      (dolist (x (directory-files (file-name-directory (buffer-file-name))))
        (when (and (string-equal "swift" (file-name-extension x))
                   (not (file-equal-p x (buffer-file-name))))
          (setq sources (cons x sources))))
      `(,swiftc ("-typecheck" ,temp-file ,@sources))))

  (defun flymake-swift-init ()
    (let* ((temp-file
            (flymake-init-create-temp-buffer-copy
             (lambda (x y)
               (make-temp-file
                (concat (file-name-nondirectory x) "-" y)
                (not :DIR_FLAG)
                ;; grab *all* the extensions; handles .swift.gyb files, for example
                ;; whereas using file-name-extension would only get ".gyb"
                (replace-regexp-in-string "^\\(?:.*/\\)?[^.]*" "" (buffer-file-name)))))))
      (funcall swift-syntax-check-fn
               (funcall swift-find-executable-fn "swiftc")
               temp-file)))

  (add-to-list 'flymake-allowed-file-name-masks '(".+\\.swift$" flymake-swift-init))

  (setq flymake-err-line-patterns
        (append
         (flymake-reformat-err-line-patterns-from-compile-el
          (mapcar (lambda (x) (assoc x compilation-error-regexp-alist-alist))
                  '(swift0 swift1 swift-fatal)))
         flymake-err-line-patterns)))


(use-package lsp-sourcekit
  :after lsp-mode
  :custom
  (lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))


;; (use-package flycheck-swift
;;   :after flycheck
;;   :hook
;;   (swift-mode . flycheck-swift-setup))


(provide 'init-swift)

;;; init-swift.el ends here
