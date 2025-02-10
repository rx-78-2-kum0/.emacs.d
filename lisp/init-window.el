;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of window.

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-funcs)
  (require 'pixel-scroll))


(when (fboundp 'pixel-scroll-precision-mode)
  (setq pixel-scroll-precision-interpolate-page t)

  (defun my-pixel-scroll-interpolate-down (&optional lines)
    (interactive)
    (if lines
        (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
      (pixel-scroll-interpolate-down)))

  (defun my-pixel-scroll-interpolate-up (&optional lines)
    (interactive)
    (if lines
        (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
    (pixel-scroll-interpolate-up))

  (defalias 'scroll-up-command 'my-pixel-scroll-interpolate-down)
  (defalias 'scroll-down-command 'my-pixel-scroll-interpolate-up))


;; Fullscreen
(if (featurep 'cocoa)
    (progn
      ;; 在Mac平台, Emacs不能进入Mac原生的全屏模式,否则会导致 `make-frame' 创建时也集成原生全屏属性后造成白屏和左右滑动现象.
      ;; 所以先设置 `ns-use-native-fullscreen' 和 `ns-use-fullscreen-animation' 禁止Emacs使用Mac原生的全屏模式.
      ;; 而是采用传统的全屏模式, 传统的全屏模式, 只会在当前工作区全屏,而不是切换到Mac那种单独的全屏工作区,
      ;; 这样执行 `make-frame' 先关代码或插件时,就不会因为Mac单独工作区左右滑动产生的bug.
      ;;
      ;; Mac平台下,不能直接使用 `set-frame-parameter' 和 `fullboth' 来设置全屏,
      ;; 那样也会导致Mac窗口管理器直接把Emacs窗口扔到单独的工作区, 从而对 `make-frame' 产生同样的Bug.
      ;; 所以, 启动的时候通过 `set-frame-parameter' 和 `maximized' 先设置Emacs为最大化窗口状态, 启动5秒以后再设置成全屏状态,
      ;; Mac就不会移动Emacs窗口到单独的工作区, 最终解决Mac平台下原生全屏窗口导致 `make-frame' 左右滑动闪烁的问题.
      (setq ns-use-native-fullscreen nil
            ns-use-fullscreen-animation nil)
      ;; 默认先最大化。

      (set-frame-parameter (selected-frame) 'fullscreen 'maximized))
  ;; 非Mac平台直接最大化
  (toggle-frame-maximized))


;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands
  (winner-undo winner-redo)
  :hook
  (after-init . winner-mode)
  :custom
  ( winner-boring-buffers '("*Completions*"
                            "*Compile-Log*"
                            "*inferior-lisp*"
                            "*Fuzzy Completions*"
                            "*Apropos*"
                            "*Help*"
                            "*cvs*"
                            "*Buffer List*"
                            "*Ibuffer*"
                            "*esh command on file*")))


;; Quickly switch windows
(use-package winum
  :commands my-winum-delete-window-macro-factory
  :pretty-hydra
  (("Actions"
    (("TAB" other-window "switch")
     ("x" delete-window "delete" :exit t)
     ("m" toggle-frame-maximized "maximize")
     ("f" toggle-frame-fullscreen "fullscreen" :exit t))
    "Resize"
    (("h" shrink-window-horizontally "←")
     ("j" enlarge-window "↓")
     ("k" shrink-window "↑")
     ("l" enlarge-window-horizontally "→")
     ("n" balance-windows "balance" :exit t))
    "Split"
    (("r" split-window-right "horizontally")
     ("R" split-window-horizontally-instead "horizontally instead")
     ("v" split-window-below "vertically")
     ("V" split-window-vertically-instead "vertically instead")
     ("t" toggle-window-split "toggle"))
    "Zoom"
    (("+" text-scale-increase "in")
     ("=" text-scale-increase "in")
     ("-" text-scale-decrease "out")
     ("0" (text-scale-increase 0) "reset"))))
  :hook
  (after-init . winum-mode)
  :bind (("C-c W"   . winum-hydra/body)
         ("M-0"     . winum-select-window-0)
         ("M-1"     . winum-select-window-1)
         ("M-2"     . winum-select-window-2)
         ("M-3"     . winum-select-window-3)
         ("M-4"     . winum-select-window-4)
         ("M-5"     . winum-select-window-5)
         ("M-6"     . winum-select-window-6)
         ("M-7"     . winum-select-window-7)
         ("M-8"     . winum-select-window-8)
         ("M-9"     . winum-select-window-9)
         ("C-x M-0" . winum-delete-window-0)
         ("C-x M-1" . winum-delete-window-1)
         ("C-x M-2" . winum-delete-window-2)
         ("C-x M-3" . winum-delete-window-3)
         ("C-x M-4" . winum-delete-window-4)
         ("C-x M-5" . winum-delete-window-5)
         ("C-x M-6" . winum-delete-window-6)
         ("C-x M-7" . winum-delete-window-7)
         ("C-x M-8" . winum-delete-window-8)
         ("C-x M-9" . winum-delete-window-9))
  :custom
  (winum-auto-setup-mode-line nil)
  :config
  (defmacro my-winum-delete-window-macro-factory ()
    "Winum delete window function macro factory."
    `(progn ,@(mapcar 'my-winum-delete-window-factory (number-sequence 0 9))))

  (my-winum-delete-window-macro-factory))


(use-package window-divider-mode
  :ensure nil
  :custom
  (window-divider-default-bottom-width 0)
  (window-divider-default-places 'right-only))


(use-package zoom
  :commands zoom--window-ignored-p
  :hook
  (after-init . zoom-mode)
  :bind
  ("C-c w z" . zoom)
  :custom
  (zoom-size                 '(0.618 . 0.618))
  (zoom-ignored-major-modes  '(flymake-diagnostics-buffer-mode undo-tree-visualizer-mode achive-visual-mode treemacs-mode vterm-mode nov-mode))
  :config
  (advice-add 'balance-windows :around (lambda (func &optional window-or-frame)
                                         (unless (zoom--window-ignored-p)
                                           (funcall func window-or-frame)))))


(use-package windmove
  :ensure nil
  :init
  (windmove-default-keybindings 'super))


(use-package popper
  :functions popper-close-window-hack
  :autoload popper-group-by-projectile
  :hook
  (emacs-startup . popper-mode)
  :bind
  (("C-`"   . popper-toggle-latest)
   ("M-`"   . popper-cycle)
   ("C-M-`" . popper-toggle-type))
  :custom
  (popper-window-height (lambda (win)
                          (fit-window-to-buffer
                           win
                           (floor (frame-height) 4)
                           (floor (frame-height) 8))))
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Compile-Log\\*"
     ;; "\\*Completions\\*"
     "\\*Warnings\\*"
     "\\*Async Shell Command\\*"
     "\\*Calendar\\*"
     "\\*Finder\\*"
     "\\*Kill Ring\\*"
     "\\*Go-Translate\\*"
     bookmark-bmenu-mode
     comint-mode
     compilation-mode
     help-mode helpful-mode
     tabulated-list-mode
     Buffer-menu-mode

     special-mode

     flymake-diagnostics-buffer-mode
     flycheck-error-list-mode flycheck-verify-mode

     grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
     yd-mode

     "^\\*Process List\\*" process-menu-mode
     list-environment-mode cargo-process-mode

     "^\\*eshell.*\\*.*$"       eshell-mode
     "^\\*shell.*\\*.*$"        shell-mode
     "^\\*terminal.*\\*.*$"     term-mode
     "^\\*vterm[inal]*.*\\*.*$" vterm-mode

     "\\*DAP Templates\\*$" dap-server-log-mode
     "\\*ELP Profiling Restuls\\*" profiler-report-mode
     "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"

     "\\*ert\\*$" overseer-buffer-mode
     "\\*gud-debug\\*$"
     "\\*lsp-help\\*$" "\\*lsp session\\*$" "*Java Dependency List*" "*LSP Symbols List*" "*LSP Error List*" xref--xref-buffer-mode
     "\\*quickrun\\*$"
     "\\*tldr\\*$"
     "\\*vc-.*\\*$"
     "^\\*elfeed-entry\\*$"
     "^\\*macro expansion\\**"

     "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
     "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
     "\\*docker-.+\\*"
     "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
     "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode rustic-cargo-plain-run-mode
     rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode rustic-popup-mode))
  :config
  (popper-echo-mode 1)

  (with-eval-after-load 'projectile
    (setq popper-group-function #'popper-group-by-projectile))

  (defun popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'popper-close-window-hack))


(provide 'init-window)

;;; init-window.el ends here
