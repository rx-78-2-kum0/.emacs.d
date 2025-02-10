;; init-dart.el --- Initialize dart configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; somme configuration of dart.

;;; Code:

(eval-when-compile
  (require 'init-funcs))


;; dart lsp
;; (use-package lsp-dart
;;   :custom
;;   (lsp-dart-outline         nil)
;;   (lsp-dart-sdk-dir         (my-home-path-resolve "/.flutter/bin/cache/dart-sdk"))
;;   (lsp-dart-flutter-sdk-dir (my-home-path-resolve "/.flutter")))


(use-package dart-mode
  :defines
  (projectile-project-root-files-bottom-up)
  :config
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD")))


;; (use-package hover
;;   :after dart-mode
;;   :bind
;;   (:map dart-mode-map
;;         ("C-M-z" . #'hover-run-or-hot-reload)
;;         ("C-M-x" . #'hover-run-or-hot-restart)
;;         ("C-M-p" . #'hover-take-screenshot'))
;;   :custom
;;   (flutter-sdk-path (my-home-path-resolve "/.flutter"))
;;   (hover-command-path (concat (getenv "GOPATH") "/bin/hover"))
;;   (hover-hot-reload-on-save t)
;;   (hover-screenshot-path (concat (getenv "HOME") "/Pictures"
;;                                  hover-screenshot-prefix "my-prefix-"
;;                                  hover-observatory-uri "http://my-custom-host:50300")))


(provide 'init-dart)

;;; init-dart.el ends here
