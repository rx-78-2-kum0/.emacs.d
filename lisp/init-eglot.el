;; init-eglot.el --- Initialize eglot configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of eglot.

;;; Code:


(provide 'init-eglot)


(use-package eglot
  :defer 3
  :hook
  (prog-mode . (lambda ()
                 (if (apply 'derived-mode-p my/eglot-major-mode)
                     (eglot-ensure))))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown       t)
  (eglot-menu-string        "⌨")
  :config
  (use-package consult-eglot
    :bind
    (:map eglot-mode-map
          ("C-M-." . consult-eglot-symbols))))


(use-package eglot-booster
	:after eglot
	:config	(eglot-booster-mode))


(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p   nil))

(setq max-mini-window-height 1)


(use-package eldoc-box
  :after (eglot eldoc)
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . eldoc-box-help-at-point))
  :custom
  (eldoc-box-max-pixel-height 600)
  (eldoc-box-max-pixel-width  800)
  (eldoc-box-clear-with-C-g   t))


;;; init-eglot.el ends here
