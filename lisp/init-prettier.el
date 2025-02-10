;; init-prettier.el --- Initialize prettier configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; some configuration of prettier code.

;;; Code:


(use-package prettier-js
  :hook
  ((web-mode typescriptreact-mode jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode ng2-mode typescript-mode scss-mode css-mode json-mode html-mode ng2-html-mode graphql-mode yaml-mode) . prettier-js-mode)
  (js-mode . (lambda ()
               (unless (member (file-name-extension buffer-file-name) '("prisma"))
                 (prettier-js-mode)))))


(use-package clang-format+
  :hook
  ((c-mode c++-mode protobuf-mode) . clang-format+-mode))


(provide 'init-prettier)

;;; init-prettier.el ends here
