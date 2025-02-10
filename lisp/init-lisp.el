;; init-elisp.el --- Initialize lisp configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; some configuration of Lisp languages.

;;; Code:


(use-package paredit
  :commands paredit-backward-delete
  :hook
  ((emacs-lisp-mode clojure-mode) . paredit-mode)
  :bind
  (:map paredit-mode-map
        ("M-[" . paredit-wrap-angled)
        ("M-{" . paredit-wrap-curly)
        ("DEL" . my-paredit-backward-delete))
  :config
  (defun my-paredit-backward-delete (n &optional arg)
    "Delete the previous N characters;"
    (interactive "p\nP")
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (paredit-backward-delete arg))))


;; emacs-lisp-mode
(use-package elisp-mode
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
        ("C-<tab>" . my-indent-whole-buffer)))


(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))


(use-package package-lint)


;; Syntax highlighting of known Elisp symbols
(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode)
  :custom
  (highlight-defined-face-use-itself t))


;;;; clojure
(use-package clojure-mode)


(use-package cider
  :hook
  (cider-mode . eldoc-mode)
  :custom
  (cider-repl-history-file "~/.emacs.d/cider-history")
  (cider-repl-history-size 3000)
  (cider-repl-result-prefix ";; => ")
  (cider-show-error-buffer nil))


(use-package clj-refactor
  :hook
  (clojure-mode . clj-refactor-mode))


(provide 'init-lisp)

;;; init-lisp.el ends here
