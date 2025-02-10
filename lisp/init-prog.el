;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; some configuration of prog.

;;; Code:


;; (use-package prog-mode
;;   :hook
;;   (prog-mode . prettify-symbols-mode)
;;   :custom
;;   (prettify-symbols-unprettify-at-point 'right-edge)
;;   :init
;;   (setq-default prettify-symbols-alist my/prettify-symbols-alist))


(use-package vimrc-mode)


(use-package dockerfile-mode)


(use-package protobuf-mode
  :mode
  ("\\.proto$" . protobuf-mode))


(use-package yaml-mode)


(use-package graphql-mode)


(use-package json-mode)


(use-package glsl-mode)


(use-package prisma-mode
  :load-path "~/.emacs.d/site-lisp/prisma-mode"
  :mode
  ("\\.prisma$" . prisma-mode))


(use-package fish-mode)


;; (use-package treesit-auto
;;   :hook
;;   (after-init . global-treesit-auto-mode)
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   (treesit-auto-langs '(typescript)))


(provide 'init-prog)

;;; init-prog.el ends here
