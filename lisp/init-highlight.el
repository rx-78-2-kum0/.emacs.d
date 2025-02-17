;; init-highlight.el --- Initialize highlighting configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of highlight.

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-funcs))


;; Highlight the current line
(global-hl-line-mode t)


(defvar-local show-paren--off-screen-overlay nil)

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook
  (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'child-frame))


;; Highlight symbols
(use-package symbol-overlay
  :bind
  (("C-. l m" . symbol-overlay-put)
   ("C-. l M" . symbol-overlay-remove-all)
   :map symbol-overlay-map
   ("M-n"   . symbol-overlay-jump-next)
   ("M-p"   . symbol-overlay-jump-prev)
   ("M-N"   . symbol-overlay-switch-forward)
   ("M-P"   . symbol-overlay-switch-backward)
   ("M-. s" . symbol-overlay-save-symbol)
   ("h"     . nil)
   ("w"     . nil))
  :hook
  (prog-mode      . symbol-overlay-mode)
  (iedit-mode     . turn-off-symbol-overlay)
  (iedit-mode-end . turn-on-symbol-overlay)
  :custom
  (symbol-overlay-idle-time 0.1)
  :config
  (with-no-warnings
    ;; Disable symbol highlighting while selecting
    (defun turn-off-symbol-overlay (&rest _)
      "Turn off symbol highlighting."
      (interactive)
      (symbol-overlay-mode -1))
    (advice-add #'set-mark :after #'turn-off-symbol-overlay)

    (defun turn-on-symbol-overlay (&rest _)
      "Turn on symbol highlighting."
      (interactive)
      (when (derived-mode-p 'prog-mode 'yaml-mode)
        (symbol-overlay-mode 1)))
    (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay)))


;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish
  :defines helpful-mode-map
  :bind
  (:map help-mode-map
        ("w" . rainbow-mode))
  :hook
  ((html-mode php-mode helpful-mode) . rainbow-mode)
  :init
  (with-eval-after-load 'helpful
    (bind-key "w" #'rainbow-mode helpful-mode-map))
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays)))


;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))


;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom-face
  (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :hook
  (after-init .  (lambda ()
                   (global-diff-hl-mode)
                   (global-diff-hl-show-hunk-mouse-mode)))
  (dired-mode . diff-hl-dired-mode)
  :custom
  (diff-hl-draw-borders nil)
  :config
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if sys/linuxp #b11111100 #b11100000))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))


;; Pulse current line
(use-package pulse
  :ensure nil
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
  :hook
  (((dumb-jump-after-jump imenu-after-jump) . my-recenter-and-pulse)
   ((bookmark-after-jump magit-diff-visit-file next-error) . my-recenter-and-pulse-line))
  :config
  (with-no-warnings
    (defun my-pulse-momentary-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

    (defun my-pulse-momentary (&rest _)
      "Pulse the region or the current line."
      (if (fboundp 'xref-pulse-momentarily)
          (xref-pulse-momentarily)
        (my-pulse-momentary-line)))

    (defun my-recenter-and-pulse(&rest _)

      (recenter)
      (my-pulse-momentary))

    (defun my-recenter-and-pulse-line (&rest _)
      "Recenter and pulse the current line."
      (recenter)
      (my-pulse-momentary-line))

    (dolist (cmd '(recenter-top-bottom
                   other-window switch-to-buffer
                   aw-select toggle-window-split
                   windmove-do-window-select
                   pager-page-down pager-page-up
                   treemacs-select-window
                   symbol-overlay-basic-jump))
      (advice-add cmd :after #'my-pulse-momentary-line))

    (dolist (cmd '(pop-to-mark-command
                   pop-global-mark
                   goto-last-change))
      (advice-add cmd :after #'my-recenter-and-pulse))))


;; Pulse modified region
(use-package goggles
  :diminish
  :hook ((prog-mode text-mode) . goggles-mode))


(provide 'init-highlight)

;;; init-highlight.el ends here
