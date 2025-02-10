;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;; some configuration of ui.

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom)
  (require 'init-funcs))


(use-package display-line-numbers
  :ensure nil
  :hook
  ((prog-mode yaml-mode conf-mode) . global-display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t))


(setq idle-update-delay             1.0
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling  t
      redisplay-skip-fontification-on-input t
      frame-title-format nil
      frame-resize-pixelwise t)


;; Theme
(use-package lacquer
  :load-path "~/.emacs.d/site-lisp/lacquer"
  :pretty-hydra
  (("Theme"
    (("t" lacquer-current-theme "current theme")
     ("T" lacquer-theme-selector "theme selector")
     ("C" lacquer-theme-carousel "theme carousel")
     ("M" lacquer-mode-selector "mode selector")
     ("a" lacquer-start-auto-switch "start auto switch")
     ("A" lacquer-stop-auto-switch "stop auto switch"))
    "Font"
    (("f" lacquer-current-font "current font")
     ("F" lacquer-font-selector "font selector"))))
  :hook
  (after-init . lacquer-auto-mode)
  :bind
  ("C-c l" . lacquer-hydra/body)
  :custom
  ;; (lacquer-auto-switch-time '("00:01" "00:02" "10:00" "14:00" "16:00" "18:00" "20:00" "22:00"))
  (lacquer-auto-switch-mode 'random)
  (lacquer-auto-switch-time (* 60 60))
  (lacquer-theme-list       my/theme-list)
  (lacquer-font-list        my/font-list))


;; (setq holo-layer-enable-cursor-animation t)
;; (setq holo-layer-python-command "/opt/homebrew/bin/python3")
;; (setq holo-layer-enable-indent-rainbow t)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/holo-layer")
;; (require 'holo-layer)
;; (holo-layer-enable)
;; (use-package holo-layer
;;   :load-path "~/.emacs.d/site-lisp/holo-layer"
;;   :init
;;   (require 'holo-layer)
;;   (holo-layer-enable)
;;   :custom
;;   (holo-layer-python-command "/opt/homebrew/bin/python3")
;;   (holo-layer-enable-cursor-animation t)
;;   (holo-layer-enable-indent-rainbow   t))


(when (and sys/mac-ns-p sys/mac-x-p)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))


;; Font
(when sys/macp
  ;; (set-fontset-font
  ;;  t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)

  (setq ns-use-thin-smoothing t
        ns-pop-up-frames      nil))


(set-face-attribute 'default nil :weight my/font-weight)
(setq-default line-spacing 0.3
              fill-column 80)


;; Transparent background
(modify-frame-parameters nil `((alpha . 85)))


;; Menu/Tool/Scroll bars
(when (version< emacs-version "27")
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))


;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))


;; Dashboard
(use-package dashboard
  :defines dashboard-recover-layout-p
  :bind
  (("C-c d" . my-open-dashboard)
   :map dashboard-mode-map
   ("C-, g" . dashboard-refresh-buffer)
   ("q"     . my-quit-dashboard))
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :custom
  ;; (dashboard-banner-logo-title "Help poor children in Uganda!")
  (dashboard-banner-logo-title      (concat "Happy hacking, " user-login-name " - Emacs ♥ you!"))
  (dashboard-set-file-icons         t)
  (dashboard-set-heading-icons      t)
  (dashboard-startup-banner         (or my/logo 'official))
  (dashboard-image-banner-max-width 200)
  (dashboard-center-content         t)
  (dashboard-show-shortcuts         nil)
  ;; (dashboard-projects-backend       'projectile)
  (dashboard-items                  '((recents  . 10)
                                      (projects . 10)))
  ;; (dashboard-init-info (concat "Happy hacking, " user-login-name " - Emacs ♥ you!"))
  (dashboard-set-footer nil)
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :hook
  (emacs-with-setup . (lambda ()
                        (forward-line my/dashboard-position)))
  :init
  (dashboard-setup-startup-hook)
  :config
  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defun my-open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (when (length> (window-list-1) 1)
      (setq dashboard-recover-layout-p t)
      (my-save-window-configuration))
    ;; Display dashboard in maximized window
    (delete-other-windows)
    ;; Refresh dashboard buffer
    (dashboard-refresh-buffer)
    ;; Jump to the first section
    (let ((fn (local-key-binding "r")))
      (and fn (funcall fn))))

  (defun my-quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when dashboard-recover-layout-p
      (setq dashboard-recover-layout-p nil)
      (my-restore-window-configuration))))


;; modeline nyan-mode
(use-package nyan-mode
  :load-path "~/.emacs.d/site-lisp/nyan-mode"
  :hook
  (after-init . nyan-mode)
  :custom
  (nyan-cat-flavor       'jazz)
  (nyan-bar-length       40)
  (nyan-animate-nyancat  nil)
  (nyan-wavy-trail       t)
  (nyan-animation-frames 10))


;; modeline parrot-mode
;; (use-package parrot
;;   :functions my-trigger-parrot
;;   :commands parrot-start-animation
;;   :hook
;;   (after-init . parrot-mode)
;;   :custom
;;   (parrot-num-rotations 1)
;;   :config
;;   (defun my-trigger-parrot (&_rest)
;;     "Trigger parrot animation."
;;     (parrot-start-animation))
;;   (if (boundp 'window-selection-change-functions)
;;       (add-hook 'window-selection-change-functions #'my-trigger-parrot)
;;     (add-hook 'post-command-hook #'my-trigger-parrot)))


;; modeline poke-line
;; (use-package poke-line
;;   :hook
;;   (after-init . poke-line-global-mode)
;;   :custom
;;   (poke-line-bar-length 24)
;;   (poke-line-pokemon "gengar"))


(use-package fireplace
  :commands
  (fireplace-off my-restore-window-configuration my-save-window-configuration)
  :custom
  (fireplace-smoke-on t)
  ;; :hook
  ;; (after-init . my-switch-timing-fireplace)
  :config
  (defvar my/fireplace-timer nil
    "Fireplace timer.")
  (defvar my/fireplace-p nil
    "Wether recovers the layout.")

  (defun my-switch-timing-fireplace ()
    "Switch whether `fireplace be called regularly."
    (interactive)
    (if my/fireplace-timer
        (progn
          (cancel-timer my/fireplace-timer)
          (setq my/fireplace-timer nil))
      (setq my/fireplace-timer (run-with-idle-timer 500 t
						                                        (lambda ()
                                                      (fireplace)))))
    (message "The timing fireplace is %s." (if my/fireplace-timer "on" "off")))

  (advice-add #'fireplace-off :after (lambda ()
                                       (setq my/fireplace-p nil)
                                       (my-restore-window-configuration)))
  (advice-add #'fireplace :before (lambda ()
                                    (unless my/fireplace-p
                                      (setq my/fireplace-p t)
                                      (my-save-window-configuration)
                                      (delete-other-windows)))))


;; Emoji
;; (use-package emojify
;;   :if sys/linuxp
;;   :hook
;;   (after-init . (lambda ()
;;                   (if sys/linuxp
;;                       (global-emojify-mode)))))


;; Misc
;; (fset 'yes-or-no-p 'y-or-n-p) emacs28 to COMMENTS
(size-indication-mode t)
(setq inhibit-startup-screen            t
      use-file-dialog                   nil
      use-dialog-box                    nil
      inhibit-default-init              t
      inhibit-startup-echo-area-message t
      initial-scratch-message           nil)


;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)


;; Don't open a file in a new frame
(if (boundp 'ns-pop-up-frames)
    (setq ns-pop-up-frames nil))


;; Don't use GTK+ tooltip
(if (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil))


(use-package posframe
  :hook
  (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)
  (defvar posframe-border-width 2
    "Default posframe border width.")
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))


;; icons
(use-package nerd-icons
  :config
  (when (and (display-graphic-p)
             (not (my-font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts t)))


(use-package composite
  :ensure nil
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode
     conf-mode nxml-mode markdown-mode help-mode
     shell-mode eshell-mode term-mode vterm-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (let ((alist
         '((33  . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
           (35  . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
           (36  . ".\\(?:\\(>\\)>?\\)")
           (37  . ".\\(?:\\(%\\)%?\\)")
           (38  . ".\\(?:\\(&\\)&?\\)")
           (42  . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
           ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
           (43  . ".\\(?:\\([>]\\)>?\\)")
           ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
           (45  . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
           ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")

           (47  . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
           ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
           (48  . ".\\(?:x[a-zA-Z]\\)")
           (58  . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
           (59  . ".\\(?:\\(;\\);?\\)")
           (60  . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
           (61  . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
           (62  . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
           (63  . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
           (91  . ".\\(?:\\(|\\)[]|]?\\)")
           ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
           (94  . ".\\(?:\\(=\\)=?\\)")
           (95  . ".\\(?:\\(|_\\|[_]\\)_?\\)")
           (119 . ".\\(?:\\(ww\\)w?\\)")
           (123 . ".\\(?:\\(|\\)[|}]?\\)")
           (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
           (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-ligature-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
  (set-char-table-parent composition-ligature-table composition-function-table))


;; mood-line-mode
(use-package mood-line
  :load-path "~/.emacs.d/site-lisp/mood-line"
  :hook
  (after-init . mood-line-mode)
  :custom
  (mood-line-left-align '(modified workspace buffer-name anzu multiple-cursors nyan parrot theme))
  ;; (mood-line-background "#002FA7")
  ;; (mood-line-inactive-background "#5bc2e7")
  )


;; ;; Mode-line
;; (use-package doom-modeline
;;   :hook
;;   (after-init . doom-modeline-mode)
;;   :init
;;   (setq doom-modeline-icon t
;;         doom-modeline-minor-modes t)
;;   :bind (:map doom-modeline-mode-map
;;               ("C-<f6>" . doom-modeline-hydra/body))
;;   :pretty-hydra
;;   (("Icon"
;;     (("i" (setq doom-modeline-icon (not doom-modeline-icon))
;;       "display icons" :toggle doom-modeline-icon)
;;      ("u" (setq doom-modeline-unicode-fallback (not doom-modeline-unicode-fallback))
;;       "unicode fallback" :toggle doom-modeline-unicode-fallback)
;;      ("m" (setq doom-modeline-major-mode-icon (not doom-modeline-major-mode-icon))
;;       "major mode" :toggle doom-modeline-major-mode-icon)
;;      ("c" (setq doom-modeline-major-mode-color-icon (not doom-modeline-major-mode-color-icon))
;;       "colorful major mode" :toggle doom-modeline-major-mode-color-icon)
;;      ("s" (setq doom-modeline-buffer-state-icon (not doom-modeline-buffer-state-icon))
;;       "buffer state" :toggle doom-modeline-buffer-state-icon)
;;      ("o" (setq doom-modeline-buffer-modification-icon (not doom-modeline-buffer-modification-icon))
;;       "modification" :toggle doom-modeline-buffer-modification-icon)
;;      ("x" (setq doom-modeline-time-icon (not doom-modeline-time-icon))
;;       "time" :toggle doom-modeline-time-icon)
;;      ("v" (setq doom-modeline-modal-icon (not doom-modeline-modal-icon))
;;       "modal" :toggle doom-modeline-modal-icon))
;;     "Segment"
;;     (("g h" (setq doom-modeline-hud (not doom-modeline-hud))
;;       "hud" :toggle doom-modeline-hud)
;;      ("g m" (setq doom-modeline-minor-modes (not doom-modeline-minor-modes))
;;       "minor modes" :toggle doom-modeline-minor-modes)
;;      ("g w" (setq doom-modeline-enable-word-count (not doom-modeline-enable-word-count))
;;       "word count" :toggle doom-modeline-enable-word-count)
;;      ("g e" (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding))
;;       "encoding" :toggle doom-modeline-buffer-encoding)
;;      ("g i" (setq doom-modeline-indent-info (not doom-modeline-indent-info))
;;       "indent" :toggle doom-modeline-indent-info)
;;      ("g c" (setq doom-modeline-display-misc-in-all-mode-lines (not doom-modeline-display-misc-in-all-mode-lines))
;;       "misc info" :toggle doom-modeline-display-misc-in-all-mode-lines)
;;      ("g l" (setq doom-modeline-lsp (not doom-modeline-lsp))
;;       "lsp" :toggle doom-modeline-lsp)
;;      ("g k" (setq doom-modeline-workspace-name (not doom-modeline-workspace-name))
;;       "workspace" :toggle doom-modeline-workspace-name)
;;      ("g g" (setq doom-modeline-github (not doom-modeline-github))
;;       "github" :toggle doom-modeline-github)
;;      ("g n" (setq doom-modeline-gnus (not doom-modeline-gnus))
;;       "gnus" :toggle doom-modeline-gnus)
;;      ("g u" (setq doom-modeline-mu4e (not doom-modeline-mu4e))
;;       "mu4e" :toggle doom-modeline-mu4e)
;;      ("g r" (setq doom-modeline-irc (not doom-modeline-irc))
;;       "irc" :toggle doom-modeline-irc)
;;      ("g f" (setq doom-modeline-irc-buffers (not doom-modeline-irc-buffers))
;;       "irc buffers" :toggle doom-modeline-irc-buffers)
;;      ("g s" (progn
;;               (setq doom-modeline-checker-simple-format (not doom-modeline-checker-simple-format))
;;               (and (bound-and-true-p flycheck-mode) (flycheck-buffer)))
;;       "simple checker" :toggle doom-modeline-checker-simple-format)
;;      ("g t" (setq doom-modeline-time (not doom-modeline-time))
;;       "time" :toggle doom-modeline-time)
;;      ("g v" (setq doom-modeline-env-version (not doom-modeline-env-version))
;;       "version" :toggle doom-modeline-env-version))
;;     "Style"
;;     (("a" (setq doom-modeline-buffer-file-name-style 'auto)
;;       "auto"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'auto))
;;      ("b" (setq doom-modeline-buffer-file-name-style 'buffer-name)
;;       "buffer name"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'buffer-name))
;;      ("f" (setq doom-modeline-buffer-file-name-style 'file-name)
;;       "file name"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'file-name))
;;      ("t u" (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
;;       "truncate upto project"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-project))
;;      ("t f" (setq doom-modeline-buffer-file-name-style 'truncate-from-project)
;;       "truncate from project"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'truncate-from-project))
;;      ("t w" (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
;;       "truncate with project"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'truncate-with-project))
;;      ("t e" (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
;;       "truncate except project"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'truncate-except-project))
;;      ("t r" (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
;;       "truncate upto root"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-root))
;;      ("t a" (setq doom-modeline-buffer-file-name-style 'truncate-all)
;;       "truncate all"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'truncate-all))
;;      ("t n" (setq doom-modeline-buffer-file-name-style 'truncate-nil)
;;       "truncate none"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'truncate-nil))
;;      ("r f" (setq doom-modeline-buffer-file-name-style 'relative-from-project)
;;       "relative from project"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'relative-from-project))
;;      ("r t" (setq doom-modeline-buffer-file-name-style 'relative-to-project)
;;       "relative to project"
;;       :toggle (eq doom-modeline-buffer-file-name-style 'relative-to-project)))
;;     "Project Detection"
;;     (("p a" (setq doom-modeline-project-detection 'auto)
;;       "auto"
;;       :toggle (eq doom-modeline-project-detection 'auto))
;;      ("p f" (setq doom-modeline-project-detection 'ffip)
;;       "ffip"
;;       :toggle (eq doom-modeline-project-detection 'ffip))
;;      ("p i" (setq doom-modeline-project-detection 'projectile)
;;       "projectile"
;;       :toggle (eq doom-modeline-project-detection 'projectile))
;;      ("p p" (setq doom-modeline-project-detection 'project)
;;       "project"
;;       :toggle (eq doom-modeline-project-detection 'project))
;;      ("p n" (setq doom-modeline-project-detection nil)
;;       "disable"
;;       :toggle (eq doom-modeline-project-detection nil)))
;;     "Misc"
;;     (("n" (progn
;;             (message "Fetching GitHub notifications...")
;;             (run-with-timer 300 nil #'doom-modeline--github-fetch-notifications)
;;             (browse-url "https://github.com/notifications"))
;;       "github notifications" :exit t)
;;      ("e" (cond ((bound-and-true-p flycheck-mode)
;;                  (flycheck-list-errors))
;;                 ((bound-and-true-p flymake-mode)
;;                  (flymake-show-diagnostics-buffer)))
;;       "list errors" :exit t)
;;      ("w" (if (bound-and-true-p grip-mode)
;;               (grip-browse-preview)
;;             (message "Not in preview"))
;;       "browse preview" :exit t)
;;      ("z h" (read-from-minibuffer
;;              "Eval: "
;;              (format "(setq %s %s)"
;;                      'doom-modeline-height
;;                      (symbol-value 'doom-modeline-height)))
;;       "set height" :exit t)
;;      ("z w" (read-from-minibuffer
;;              "Eval: "
;;              (format "(setq %s %s)"
;;                      'doom-modeline-bar-width
;;                      (symbol-value 'doom-modeline-bar-width)))
;;       "set bar width" :exit t)
;;      ("z g" (read-from-minibuffer
;;              "Eval: "
;;              (format "(setq %s %s)"
;;                      'doom-modeline-github-interval
;;                      (symbol-value 'doom-modeline-github-interval)))
;;       "set github interval" :exit t)
;;      ("z n" (read-from-minibuffer
;;              "Eval: "
;;              (format "(setq %s %s)"
;;                      'doom-modeline-gnus-timer
;;                      (symbol-value 'doom-modeline-gnus-timer)))
;;       "set gnus interval" :exit t)))))


(use-package hide-mode-line
  :hook
  (((treemacs-mode
     eshell-mode shell-mode
     term-mode vterm-mode
     embark-collect-mode
     lsp-ui-imenu-mode
     pdf-annot-list-mode) . turn-on-hide-mode-line-mode)
   (dired-mode . (lambda()
                   (and (bound-and-true-p hide-mode-line-mode)
                        (turn-off-hide-mode-line-mode))))))


;; A minor-mode menu for mode-line
(use-package minions
  :hook
  (doom-modeline-mode . minions-mode))


(provide 'init-ui)

;;; init-ui.el ends here
