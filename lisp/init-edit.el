;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of edit.

;;; Code:


;; prettify-symbols-mod
;; (global-prettify-symbols-mode t)


;; Miscs
(setq set-mark-command-repeat-pop     t  ; Repeating C-SPC after popping mark pops it again
      completion-ignore-case          t
      scroll-step                     1
      scroll-margin                   0
      scroll-preserve-screen-position t
      auto-window-vscroll             nil
      scroll-conservatively           100000)


;; Permanently indent with spaces, never with TABs
(setq-default tab-width        2
              indent-tabs-mode nil
              major-mode       'text-mode)


(use-package indent
  :ensure nil
  :custom
  (standard-indent 2))


(use-package so-long
  :hook
  (after-init . global-so-long-mode)
  :custom
  (so-long-threshold 400))


;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :hook
  (after-init . global-auto-revert-mode))


;; Treat undo history as a tree
;; (use-package undo-tree
;;   :hook
;;   (after-init . global-undo-tree-mode)
;;   (web-mode . undo-tree-mode) ;; web-mode
;;   :init
;;   (add-to-list 'display-buffer-alist
;;                '("^ \\*undo-tree\\*"
;;                  display-buffer-reuse-window display-buffer-in-side-window
;;                  (reusable-frames . visible)
;;                  (side . right)
;;                  (slot . 1)
;;                  (window-width . 0.5)
;;                  (window-height . 0.15))))


;; Show number of matches in mode-line while searching
(use-package anzu
  :bind
  (([remap query-replace]        . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace]        . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook
  (after-init . global-anzu-mode))


;; Increase selected region by semantic units
(use-package expand-region
  :custom
  (expand-region-fast-keys-enabled nil)
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))


;; An all-in-one comment command to rule them all
;; (use-package comment-dwim-2
;;   :bind
;;   ([remap comment-dwim] . comment-dwim-2))


;; Multiple cursors
(use-package multiple-cursors
  :hook
  (after-init . multiple-cursors-mode)
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-M->"       . mc/skip-to-next-like-this)
   ("C-M-<"       . mc/skip-to-previous-like-this)
   :map mc/keymap
   ("C-|" . mc/vertical-align-with-space)))


;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook
  (after-init . electric-pair-mode)
  :custom
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-skip-self 'electric-pair-default-skip-self)
  (electric-pair-open-newline-between-pairs t)
  (electric-pair-pairs
   '((?\" . ?\")
     (?\' . ?\')
     (?\` . ?\`)
     (?\{ . ?\})))
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook
  (after-init . smart-region-on))


;; Hungry deletion
(use-package hungry-delete
  :hook
  (after-init . global-hungry-delete-mode)
  :bind
  (:map hungry-delete-mode-map
        ("M-<backspace>" . hungry-delete-backward))
  :custom
  (hungry-delete-except-modes  '(help-mode minibuffer-inactive-mode calc-mode minibuffer-mode))
  (hungry-delete-chars-to-skip " \t\f\v"))


;; move to the beginning/end of line or code
(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
  ([remap move-end-of-line] . mwim-end-of-code-or-line))


;; moving the cursor quickly
(use-package avy
  :custom
  (avy-all-windows     nil)
  (avy-all-windows-alt t)
  (avy-background      t)
  (avy-style           'pre)
  :bind
  ("C-S-w" . avy-goto-char-timer)
  ("C-S-l" . avy-goto-char-in-line)
  ("C-S-n" . avy-goto-line-below)
  ("C-S-p" . avy-goto-line-above))


(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))


(use-package goto-chg
  :bind
  ("M-o" . 'goto-last-change)
  ("M-O" . 'goto-last-change-reverse))


;; Flexible text folding
(use-package hideshow
  :ensure nil
  :pretty-hydra
  (("Fold"
    (("t" hs-toggle-all "toggle all")
     ("a" hs-show-all "show all")
     ("i" hs-hide-all "hide all")
     ("g" hs-toggle-hiding "toggle hiding")
     ("c" hs-cycle "cycle block")
     ("s" hs-show-block "show block")
     ("h" hs-hide-block "hide block")
     ("l" hs-hide-level "hide level"))
    "Move"
    (("C-a" mwim-beginning-of-code-or-line "⭰")
     ("C-e" mwim-end-of-code-or-line "⭲")
     ("C-b" backward-char "←")
     ("C-n" next-line "↓")
     ("C-p" previous-line "↑")
     ("C-f" forward-char "→")
     ("C-v" pager-page-down "↘")
     ("M-v" pager-page-up "↖")
     ("M-<" beginning-of-buffer "⭶")
     ("M->" end-of-buffer "⭸"))))
  :bind
  (:map hs-minor-mode-map
        ("C-~" . hideshow-hydra/body))
  :hook
  (prog-mode . hs-minor-mode)
  :config
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))


;; (use-package whole-line-or-region
;;   :hook
;;   (after-init . whole-line-or-region-global-mode))


;; (use-package selectric-mode
;;   :hook
;;   (after-init . selectric-mode))


;; (use-package edit-server
;;   :hook
;;   (after-init . edit-server-start)
;;   (kill-emacs . edit-server-stop))


(use-package delsel
  :ensure nil
  :hook
  (after-init . delete-selection-mode))


;; (use-package flyspell
;;   :ensure nil
;;   :if
;;   (executable-find "aspell")
;;   :custom
;;   (flyspell-issue-message-flag nil)
;;   (ispell-program-name         "aspell")
;;   (ispell-extra-args           '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
;;   :hook
;;   ((text-mode outline-mode) . flyspell-mode)
;;   (flyspell-mode . (lambda ()
;;                      (dolist (key '("C-;" "C-," "C-."))
;;                        (unbind-key key flyspell-mode-map))))
;;   :config
;;   ;; Correcting words with flyspell via Ivy
;;   (use-package flyspell-correct-ivy
;;     :after ivy
;;     :custom
;;     (flyspell-correct-interface #'flyspell-correct-ivy)
;;     :bind
;;     (:map flyspell-mode-map
;;           ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))))


;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :hook
  (prog-mode . subword-mode)
  (minibuffer-setup . subword-mode))


(use-package sudo-edit)


(provide 'init-edit)

;;; init-edit.el ends here
