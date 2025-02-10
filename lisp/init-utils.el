;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;;; Commentary:
;; somme configuration of utils.

;;; Code:


(eval-when-compile
  (require 'cl-lib)
  (require 'init-const)
  (require 'init-funcs)
  (require 'init-custom))


(use-package docker
  :bind
  ("C-c D". docker))


(use-package vterm
  :functions
  (my-bottom-window vterm-mode)
  :bind
  (("C-c v v" . vterm)
   :map vterm-mode-map
   ("M-p" . vterm-send-prior)
   ("M-n" . vterm-send-next)
   ("M-k" . vterm-send-up)
   ("M-j" . vterm-send-down)
   ("M-p" . vterm-yank)
   ("M-u" . vterm-undo))
  :config
  (advice-add #'vterm :override (lambda ()
                                  "Toggle vterm or create a new vterm."
                                  (interactive)

                                  (let ((buffer  (catch 'break
                                                   (dolist (i (buffer-list))
                                                     (when (string-match-p "vterm" (buffer-name i))
                                                       (throw 'break i)
                                                       )))))

                                    (if buffer
                                        (let ((win (get-buffer-window buffer)))
                                          (if (window-live-p win)
                                              (delete-window win)
                                            (let ((w (catch 'break
                                                       (dolist (i (window-list))
                                                         (let ((name (buffer-name (window-buffer i))))
                                                           (when (string-match-p "vterm" name)
                                                             ;; (delete-window name)
                                                             (throw 'break i)))))))
                                              (if w
                                                  (delete-window w)
                                                (my-bottom-window buffer)))))
                                      (setq buffer (generate-new-buffer "vterm"))
                                      (with-current-buffer (buffer-name buffer)
                                        (vterm-mode))
                                      (my-bottom-window buffer)))))

  (defvar my/vterm-buffer-list nil
    "Vterm buffer list.")

  (add-hook 'kill-buffer-hook (lambda ()
                                (if (derived-mode-p 'vterm-mode)
                                    (setq my/vterm-buffer-list
	                                        (delq (current-buffer) my/vterm-buffer-list)))))

  (add-hook 'vterm-mode-hook (lambda ()
                               (add-to-list 'my/vterm-buffer-list (current-buffer))))

  (defun my-vterm-switch (direction offset)
    (if my/vterm-buffer-list
        (let ((len (length my/vterm-buffer-list))
	            (index (cl-position (current-buffer) my/vterm-buffer-list)))
	        (if index
	            (let ((target-index (if (eq direction 'previous)
				                              (mod (+ index offset) len)
				                            (mod (- index offset) len))))
	              (switch-to-buffer (nth target-index my/vterm-buffer-list) nil t))
	          (switch-to-buffer (car my/vterm-buffer-list) nil t)))
      nil))

  (defun my-vterm-previous (&optional offset)
    "Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
    (interactive "P")
    (my-vterm-switch 'previous (or offset 1)))

  (defun my-vterm-next (&optional offset)
    "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
    (interactive "P")
    (my-vterm-switch 'next (or offset 1))))


;; process view
(use-package proced
  :ensure nil
  :bind
  ("C-c p P" . 'proced)
  :custom
  (proced-auto-update-flag     t)
  (proced-auto-update-interval 3)
  (proced-format               'verbose))


;; Youdao Dictionary
(use-package yd
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/yd.el"
  :bind
  (("C-c y y" . yd-search-at-point-posframe)
   ("C-c y Y" . yd-search-at-point)
   ("C-c y I" . yd-search-from-input)
   :map yd-mode-map
   ("?" . yd-hydra/body))
  :custom
  (yd-app-key    "78762df07eff3cf2")
  (yd-secret-key "buZDTTRkFgVRPCegCzLpX255Y1Ql17F3")
  ;; 中文分词
  (yd-use-chinese-word-segmentation t)
  :init
  (setq url-automatic-caching t))


(use-package go-translate
  :custom
  (gts-translate-list '(("en" "zh") ("zh" "en")))
  :bind
  ("C-c y g" . gts-do-translate))


;; Fast search tool `ripgrep'
(use-package rg
  :defines projectile-command-map
  :hook
  (rg-mode . rg-enable-default-bindings)
  :bind
  (("C-c R m" . rg-menu)
   :map rg-mode-map
   ("m" . rg-menu))
  :custom
  ;; (rg-keymap-prefix "\C-cR")
  (rg-group-result t)
  (rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (bind-key "s R" #'rg-project projectile-command-map)))


(use-package deadgrep
  :bind
  ("C-c R r" . deadgrep))


(use-package achive
  :load-path "~/.emacs.d/site-lisp/achive"
  :bind
  ("C-c a a" . achive)
  :custom
  (achive-stock-list '("sh600036" "sh601012" "sz000625" "sz002050" "sz002013" "sh600176" "sh603993" "sh601388" "sz002557" "sh600989" "sh600887" "sz002097" "sz000731" "sh601015" "sh601985" "sz000630" "sh600875" "sz002312" "sz000876" "sz000422" "sz00097")))


(use-package wttrin
  :load-path "~/.emacs.d/site-lisp/emacs-wttrin"
  :bind
  ("C-c w W" . wttrin)
  :custom
  (wttrin-cities           '("Guanghan"))
  (wttrin-forecast-days    2)
  (wttrin-language         "zh-cn")
  (wttrin-units-wind-speed t))


(use-package screenshot
  :load-path "~/.emacs.d/site-lisp/screenshot"
  :bind
  ("C-c s" . screenshot))


(use-package focus)


(use-package list-environment)


(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :config
  (cond
   ((executable-find "ugrep")
    (grep-apply-setting
     'grep-command "ugrep --color=auto -0In -e ")
    (grep-apply-setting
     'grep-template "ugrep --color=auto -0In -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("ugrep --color=auto -0Inr -e ''" . 30))
    (grep-apply-setting
     'grep-find-template "ugrep <C> -0Inr -e <R> <D>"))
   ((executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>"))))


(use-package find-file-in-project
  :bind
  ("C-c p f" . find-file-in-project-by-selected)
  ("C-c p F" . find-file-with-similar-name)
  ("C-c p d" . find-directory-in-project-by-selected)
  :custom
  (ffip-use-rust-fd t))


(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".git" ".dir-locals.el" "package.json")))


(provide 'init-utils)

;;; init-utils.el ends here
