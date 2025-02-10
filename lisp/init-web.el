;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of web.

;;; Code:


;; CSS mode
(use-package css-mode
  :custom
  (css-indent-offset 2))


;; SCSS mode
(use-package scss-mode
  :custom
  ;; Disable complilation on save
  (scss-compile-at-save nil))


;; major-mode for editing multiple web formats
(use-package web-mode
  :mode
  (".*[^\\(\\.component\\)]\\.html$" . web-mode)
  ;; ("\\.jsx$"                         . web-mode)
  ;; ("\\.tsx$"                         . web-mode)
  ("\\.ejs$"                         . web-mode)
  :bind
  (:map web-mode-map
        ("M-;" . web-mode-comment-or-uncomment))
  :custom
  (web-mode-comment-formats '(("java"       . "/*")
                              ("javascript" . "//")
                              ("typescript" . "//")
                              ("php"        . "/*")
                              ("css"        . "/*")))
  (web-mode-markup-indent-offset               2)
  (web-mode-css-indent-offset                  2)
  (web-mode-code-indent-offset                 2)
  (web-mode-enable-html-entities-fontification t)
  (web-mode-auto-close-style                   2)
  (web-mode-enable-auto-quoting                nil)
  (web-mode-enable-auto-pairing                nil)
  (web-mode-enable-current-element-highlight   t)
  (web-mode-enable-css-colorization            t)
  (web-mode-comment-style                      2)
  (web-mode-enable-comment-annotation          t)
  (web-mode-enable-comment-interpolation       t)
  (web-mode-enable-auto-indentation            nil))


(use-package jtsx
  :commands jtsx-install-treesit-language
  :mode
  ("\\.jsx?\\'" . jtsx-jsx-mode)
  ("\\.tsx\\'"  . jtsx-tsx-mode)
  ("\\.ts\\'"   . jtsx-typescript-mode)
  :hook
  (jtsx-jsx-mode        . hs-minor-mode)
  (jtsx-tsx-mode        . hs-minor-mode)
  (jtsx-typescript-mode . hs-minor-mode)
  :custom
  (js-indent-level                                            2)
  (typescript-ts-mode-indent-offset                           2)
  (jtsx-switch-indent-offset                                  0)
  (jtsx-indent-statement-block-regarding-standalone-parent    nil)
  (jtsx-jsx-element-move-allow-step-out                       t)
  (jtsx-enable-jsx-electric-closing-element                   t)
  (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  (jtsx-enable-jsx-element-tags-auto-sync                     nil)
  (jtsx-enable-all-syntax-highlighting-features               t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node)
    (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
    (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
    (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically)
    (define-key mode-map (kbd "M-.") 'embark-dwim))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))


;; emmet-mode: dynamic snippets for HTML
(use-package emmet-mode
  :hook
  ((web-mode ng2-html-mode) . emmet-mode)
  :bind
  ((:map emmet-mode-keymap
         ("C-. ["     . emmet-prev-edit-point)
         ("C-. ]"     . emmet-next-edit-point)
         ("<backtab>" . emmet-expand-yas)
         ("<C-tab>"   . my-jsx-expand)))
  :custom
  (emmet-move-cursor-between-quotes t)
  (emmet-indentation                2)
  (emmet-expand-jsx-className?      t))


(provide 'init-web)

;;; init-web.el ends here
