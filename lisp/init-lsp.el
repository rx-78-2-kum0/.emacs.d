;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of lsp.

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Use plists for deserialization.
(setenv "LSP_USE_PLISTS" "true")

(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; Emacs client for the Language Server Protocol
(use-package lsp-mode
  :commands lsp-format-buffer
  :hook
  (prog-mode . (lambda ()
                 (when (cl-position major-mode my/lsp-major-mode :test 'eq)
                   (lsp-deferred))))
  (lsp-mode . (lambda ()
                (when (cl-position major-mode my/lsp-on-save-major-mode :test 'eq)
                  (add-hook 'before-save-hook #'lsp-format-buffer t t))))
  (lsp-completion-mode . (lambda ()
                           (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
                                 '(orderless))))
  :init
  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'(lambda (_pattern index _total)
                                             (and (eq index 0) 'orderless-flex)) nil 'local)

  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  ;; :bind
  ;; (:map lsp-mode-map
  ;;       ([remap xref-find-definitions] . lsp-find-definition)
  ;;       ([remap xref-find-references] . lsp-find-references))
  :custom
  (lsp-completion-provider        :none)
  (lsp-diagnostics-provider       :flymake)
  (lsp-diagnostics-flycheck-default-level 'info)
  (lsp-clients-angular-language-server-command
   '("node" "/opt/homebrew/lib/node_modules/@angular/language-server" "--ngProbeLocations" "/opt/homebrew/lib/node_modules" "--tsProbeLocations" "/opt/homebrew/lib/node_modules" "--stdio"))
  (lsp-auto-guess-root            nil)      ; not Detect project root
  (lsp-keep-workspace-alive       nil)
  (lsp-signature-auto-activate    nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable  nil)
  (lsp-enable-file-watchers       nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-log-io                     nil)
  (lsp-print-performance          nil)
  (lsp-response-timeout           10)
  (lsp-idle-delay                 0.500)
  (lsp-enable-on-type-formatting  nil)
  (lsp-enable-folding             nil)
  (lsp-enable-snippet             nil)
  (lsp-enable-links               nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)

  ;; (lsp-restart 'auto-restart)
  (lsp-completion-enable               t)
  (lsp-completion-show-detail          nil)
  ;; (lsp-completion-sort-initial-results t)
  ;; (lsp-completion-use-last-result      t)
  (lsp-signature-auto-activate         nil)
  (lsp-signature-doc-lines             30)
  (lsp-enable-indentation              t)
  (lsp-eldoc-render-all                nil)
  (lsp-eldoc-enable-hover              t)
  ;; deno
  ;; (lsp-clients-deno-config                "./tsconfig.json")
  ;; (lsp-clients-deno-import-map            "./import_map.json")
  ;; (lsp-clients-deno-enable-lint           nil)
  (lsp-headerline-breadcrumb-enable       nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-lens-enable                        nil)
  (lsp-disabled-clients '((web-mode . (deno-ls)) (typescript-mode . (deno-ls))))
  ;; eslint
  (lsp-eslint-run                              "onSave")
  (lsp-eslint-format                           nil)
  (lsp-eslint-package-manager                  "npm")
  (lsp-eslint-code-action-disable-rule-comment nil)

  ;; typescript/javascript
  (lsp-clients-typescript-prefer-use-project-ts-server t)
  (lsp-typescript-format-enable nil)
  (lsp-javascript-format-enable nil))


(use-package consult-lsp
  :bind
  (:map lsp-mode-map
        ("C-M-." . consult-lsp-symbols)))


(use-package lsp-ui
  :commands lsp-ui
  :after lsp-mode
  :bind
  (:map lsp-ui-mode-map
        ("s-l c a" . lsp-ui-sideline-apply-code-actions))
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable            t)
  (lsp-ui-doc-show-with-cursor  t)
  (lsp-ui-doc-header            nil)
  (lsp-ui-doc-position          'bottom) ;; top, bottom, or at-point
  (lsp-ui-doc-border            "#443355")
  (lsp-ui-doc-max-width         120)
  (lsp-ui-doc-max-height        30)
  (lsp-ui-doc-use-childframe    t)
  (lsp-ui-doc-use-webkit        nil)
  (lsp-ui-doc-delay             0.1)
  (lsp-ui-doc-show-with-mouse   nil)
  (lsp-ui-doc-include-signature t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable               nil)
  (lsp-ui-sideline-show-hover           nil)
  (lsp-ui-sideline-ignore-duplicate     t)
  (lsp-ui-sideline-show-symbol          nil)
  (lsp-ui-sideline-show-diagnostics     nil)
  (lsp-ui-sideline-show-code-actions    nil)
  (lsp-ui-sideline-code-actions-prefix  "💡 ")
  (lsp-ui-sideline-wait-for-all-symbols nil)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable        t)
  (lsp-ui-imenu-kind-position 'top)
  (lsp-ui-imenu-auto-refresh  'after-save)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable      t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width  50)
  (lsp-ui-peek-fontify     'on-demand))


;; (use-package dap-mode
;;   :defines dap-python-executable
;;   :functions dap-hydra/nil
;;   :diminish
;;   :bind
;;   (:map lsp-mode-map
;;         ("<f5>"   . dap-debug)
;;         ("M-<f5>" . dap-hydra))
;;   :hook
;;   ((after-init         . dap-auto-configure-mode)
;;    (dap-stopped        . (lambda (_args) (dap-hydra)))
;;    (dap-terminated     . (lambda (_args) (dap-hydra/nil)))
;;    (ruby-mode          . (lambda () (require 'dap-ruby)))
;;    (go-mode            . (lambda () (require 'dap-go)))
;;    ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
;;    ((js-mode js2-mode)                     . (lambda () (require 'dap-chrome)))))


(use-package lsp-treemacs)

(use-package eglot
  :defer 3
  :hook
  (prog-mode . (lambda ()
                 (if (apply 'derived-mode-p my/eglot-major-mode)
                     (eglot-ensure))))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown       t)
  :config
  (use-package consult-eglot
    :bind
    (:map eglot-mode-map
          ("C-M-." . consult-eglot-symbols))))


(provide 'init-lsp)

;;; init-lsp.el ends here
