;; init-rust.el --- Initialize rust configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; some configuration of rust.

;;; Code:


(use-package rust-mode
  :custom
  (rust-format-on-save         t)
  (rust-mode-treesitter-derive t)
  (rust-indent-offset          2)
  (rust-rustfmt-switches       (list "--edition" "2018" "--config-path" (concat (getenv "HOME") "/.config/rustfmt"))))


(use-package rust-playground)


(provide 'init-rust)

;;; init-rust.el ends here
