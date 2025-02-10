;; init-flymake.el --- Initialize flymake configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of flymake.

;;; Code:


(use-package flymake
  :diminish
  :bind
  ("C-c c c" . flymake-show-buffer-diagnostics)
  :custom
  (flymake-no-changes-timeout        nil)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-suppress-zero-counters    :note)
  :config
  (setq elisp-flymake-byte-compile-load-path
        (append elisp-flymake-byte-compile-load-path load-path)))


(use-package sideline-flymake
  :diminish sideline-mode
  :hook
  (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'point)
  (sideline-backends-right '(sideline-flymake)))


(use-package flymake-eslint
  :after eglot
  :hook
  (eglot-managed-mode . my/flymake-eslint-enable)
  :custom
  (flymake-eslint-prefer-json-diagnostics t)
  :config
  (defun my/flymake-eslint-enable ()
    "Enable `flymake-eslint' based on the project configuration."
    (interactive)
    (when-let* ((root (or (locate-dominating-file (buffer-file-name) "pnpm-lock.yaml") (locate-dominating-file (buffer-file-name) "package-lock.json"))))
      (make-local-variable 'exec-path)
      (push (file-name-concat root "node_modules" ".bin") exec-path)
      (flymake-eslint-enable))))


(provide 'init-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flymake.el ends here
