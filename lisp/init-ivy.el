;;; init-ivy --- Summary

;;; Commentary:
;; some configuration of ivy.

;;; Code:


(use-package counsel
  :defines
  (magit-completing-read-function savehist-additional-variables)
  :commands
  (ivy-format-function-line ivy-format-function-arrow my-ivy-format-function)
  :custom-face
  (ivy-minibuffer-match-face-1 ((t (:foreground "dimgray" :distant-foreground unspecified :background unspecified))))
  (ivy-minibuffer-match-face-2 ((t (:distant-foreground unspecified :background unspecified))))
  (ivy-minibuffer-match-face-3 ((t (:distant-foreground unspecified :background unspecified))))
  (ivy-minibuffer-match-face-4 ((t (:distant-foreground unspecified :background unspecified))))
  :bind
  (("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-s"   . swiper-isearch)
   ("C-S-s" . swiper-isearch-backward)
   :map counsel-mode-map
   ([remap swiper]          . counsel-grep-or-swiper)
   ([remap swiper-backward] . counsel-grep-or-swiper-backward)
   ([remap dired]           . counsel-dired)
   ([remap set-variable]    . counsel-set-variable)
   ([remap insert-char]     . counsel-unicode-char)
   ("C-r"                   . counsel-rg)
   ("C-x C-r"               . counsel-recentf)
   ("C-c c z"               . counsel-fzf)
   ("C-c c p"               . counsel-pt)
   ("C-c c m"               . counsel-imenu)

   :map ivy-minibuffer-map
   ([escape] . minibuffer-keyboard-quit)
   :map swiper-map
   ([escape] . minibuffer-keyboard-quit))
  :hook
  (after-init . ivy-mode)
  (ivy-mode   . counsel-mode)
  :custom
  (enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (ivy-use-selectable-prompt    t)
  (ivy-use-virtual-buffers      t)   ; Enable bookmarks and recentf
  (ivy-height                   12)
  (ivy-fixed-height-minibuffer  t)
  (ivy-count-format             "(%d/%d) ")
  (ivy-on-del-error-function    nil)
  (ivy-initial-inputs-alist     nil)
  (swiper-action-recenter       t)
  (counsel-find-file-at-point   t)
  (counsel-yank-pop-separator   "\n────────\n")
  (counsel-grep-base-command    "rg -S --no-heading --line-number --color never %s %s")
  (counsel-fzf-cmd              "fd --type f --hidden --follow --exclude .git --color never '%s'")
  :init
  (add-hook 'counsel-grep-post-action-hook #'recenter)
  ;; Be compatible with `gls'
  (if (and sys/macp (executable-find "gls"))
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd      "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'ivy-views))

  ;; Highlight the selected item
  (defun my-ivy-format-function (cands)
    "Transform CANDS into a string for minibuffer."
    (if (display-graphic-p)
        (ivy-format-function-line cands)
      (ivy-format-function-arrow cands)))
  (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function)


  (defconst my-ivy-fly-commands
    '(query-replace-regexp
      flush-lines keep-lines ivy-read
      swiper swiper-backward swiper-all
      swiper-isearch swiper-isearch-backward
      lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
      counsel-grep-or-swiper counsel-grep-or-swiper-backward
      counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))

  (defconst my-ivy-fly-back-commands
    '(self-insert-command
      ivy-forward-char ivy-delete-char delete-forward-char kill-word kill-sexp
      end-of-line mwim-end-of-line mwim-end-of-code-or-line mwim-end-of-line-or-code
      yank ivy-yank-word ivy-yank-char ivy-yank-symbol counsel-yank-pop))

  (defvar-local my-ivy-fly--travel nil)
  (defun my-ivy-fly-back-to-present ()
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((or (memq this-command my-ivy-fly-back-commands)
               (equal (this-command-keys-vector) (kbd "M-n")))
           (unless my-ivy-fly--travel
             (delete-region (point) (point-max))
             (when (memq this-command '(ivy-forward-char
                                        ivy-delete-char delete-forward-char
                                        kill-word kill-sexp
                                        end-of-line mwim-end-of-line
                                        mwim-end-of-code-or-line
                                        mwim-end-of-line-or-code))
               (insert (ivy-cleanup-string ivy-text))
               (when (memq this-command '(ivy-delete-char
                                          delete-forward-char
                                          kill-word kill-sexp))
                 (beginning-of-line)))
             (setq my-ivy-fly--travel t)))))

  (defun my-ivy-fly-time-travel ()
    (when (memq this-command my-ivy-fly-commands)
      (insert (propertize
               (save-excursion
		             (set-buffer (window-buffer (minibuffer-selected-window)))
		             (ivy-thing-at-point))
               'face 'shadow))
      (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)
      (beginning-of-line)))

  (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))

  ;;
  ;; Improve search experience of `swiper' and `counsel'
  ;;
  (defun my-ivy-switch-to-swiper (&rest _)
    "Switch to `swiper' with the current input."
    (ivy-quit-and-run (swiper ivy-text)))

  (defun my-ivy-switch-to-swiper-isearch (&rest _)
    "Switch to `swiper-isearch' with the current input."
    (ivy-quit-and-run (swiper-isearch ivy-text)))

  (defun my-ivy-switch-to-swiper-all (&rest _)
    "Switch to `swiper-all' with the current input."
    (ivy-quit-and-run (swiper-all ivy-text)))

  (defun my-ivy-switch-to-rg-dwim (&rest _)
    "Switch to `rg-dwim' with the current input."
    (interactive)
    (ivy-exit-with-action #'rg-dwim))

  (defun my-ivy-switch-to-counsel-rg (&rest _)
    "Switch to `counsel-rg' with the current input."
    (ivy-quit-and-run (counsel-rg ivy-text default-directory)))

  (defun my-ivy-switch-to-counsel-git-grep (&rest _)
    "Switch to `counsel-git-grep' with the current input."
    (ivy-quit-and-run (counsel-git-grep ivy-text default-directory)))

  (defun my-ivy-switch-to-counsel-find-file (&rest _)
    "Switch to `counsel-find-file' with the current input."
    (ivy-quit-and-run (counsel-find-file ivy-text)))

  (defun my-ivy-switch-to-counsel-fzf (&rest _)
    "Switch to `counsel-fzf' with the current input."
    (ivy-quit-and-run (counsel-fzf ivy-text default-directory)))

  (defun my-ivy-switch-to-counsel-git (&rest _)
    "Switch to `counsel-git' with the current input."
    (ivy-quit-and-run (counsel-git ivy-text)))

  (defun my-ivy-switch-to-list-bookmarks (&rest _)
    "Switch to `list-bookmarks'."
    (ivy-quit-and-run (call-interactively #'list-bookmarks)))

  (defun my-ivy-switch-to-list-colors (&rest _)
    "Switch to `list-colors-display'."
    (ivy-quit-and-run (list-colors-display)))

  (defun my-ivy-switch-to-list-packages (&rest _)
    "Switch to `list-packages'."
    (ivy-quit-and-run (list-packages)))

  (defun my-ivy-switch-to-list-processes (&rest _)
    "Switch to `list-processes'."
    (ivy-quit-and-run (list-processes)))

  (defun my-ivy-copy-library-path (lib)
    "Copy the full path of LIB."
    (let ((path (find-library-name lib)))
      (kill-new path)
      (message "Copied path: \"%s\"." path)))


  (defun my-swiper-toggle-counsel-rg ()
    "Toggle `counsel-rg' and `swiper'/`swiper-isearch' with the current input."
    (interactive)
    (if (memq (ivy-state-caller ivy-last) '(swiper swiper-isearch))
        (my-ivy-switch-to-counsel-rg)
      (my-ivy-switch-to-swiper-isearch)))
  (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
  (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg counsel-ag-map)

  (with-eval-after-load 'rg
    (bind-key "<M-return>" #'my-ivy-switch-to-rg-dwim swiper-map)
    (bind-key "<M-return>" #'my-ivy-switch-to-rg-dwim counsel-ag-map))

  (defun my-swiper-toggle-swiper-isearch ()
    "Toggle `swiper' and `swiper-isearch' with the current input."
    (interactive)
    (ivy-quit-and-run
      (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
          (swiper ivy-text)
        (swiper-isearch ivy-text))))
  (bind-key "<s-return>" #'my-swiper-toggle-swiper-isearch swiper-map)

  (defun my-counsel-find-file-toggle-fzf ()
    "Toggle `counsel-fzf' with the current `counsel-find-file' input."
    (interactive)
    (ivy-quit-and-run
      (counsel-fzf (or ivy-text "") default-directory)))
  (bind-key "<C-return>" #'my-counsel-find-file-toggle-fzf counsel-find-file-map)

  (defun my-counsel-toggle ()
    "Toggle `counsel' commands and original commands."
    (interactive)
    (pcase (ivy-state-caller ivy-last)
      ('counsel-bookmark (my-ivy-switch-to-list-bookmarks))
      ('counsel-colors-emacs (my-ivy-switch-to-list-colors))
      ('counsel-colors-web (my-ivy-switch-to-list-colors))
      ('counsel-list-processes (my-ivy-switch-to-list-processes))
      ('counsel-package (my-ivy-switch-to-list-packages))
      (_ (ignore))))
  (bind-key "<C-return>" #'my-counsel-toggle ivy-minibuffer-map)

  ;; More actions
  (ivy-add-actions
   #'swiper-isearch
   '(("r" my-ivy-switch-to-counsel-rg "rg")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("s" my-ivy-switch-to-swiper "swiper")
     ("a" my-ivy-switch-to-swiper-all "swiper all")))

  (ivy-add-actions
   #'swiper
   '(("r" my-ivy-switch-to-counsel-rg "rg")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
     ("a" my-ivy-switch-to-swiper-all "swiper all")))

  (ivy-add-actions
   #'swiper-all
   '(("g" my-ivy-switch-to-counsel-git-grep "git grep")
     ("r" my-ivy-switch-to-counsel-rg "rg")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("s" my-swiper-toggle-swiper-isearch "swiper isearch")
     ("S" my-ivy-switch-to-swiper "swiper")))

  (ivy-add-actions
   #'counsel-rg
   '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
     ("S" my-ivy-switch-to-swiper "swiper")
     ("a" my-ivy-switch-to-swiper-all "swiper all")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")))

  (ivy-add-actions
   #'counsel-git-grep
   '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
     ("S" my-ivy-switch-to-swiper "swiper")
     ("r" my-ivy-switch-to-rg-dwim "rg")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("a" my-ivy-switch-to-swiper-all "swiper all")))

  (ivy-add-actions
   #'counsel-find-file
   '(("g" my-ivy-switch-to-counsel-git "git")
     ("z" my-ivy-switch-to-counsel-fzf "fzf")))

  (ivy-add-actions
   #'counsel-git
   '(("f" my-ivy-switch-to-counsel-find-file "find file")
     ("z" my-ivy-switch-to-counsel-fzf "fzf")))

  (ivy-add-actions
   'counsel-fzf
   '(("f" my-ivy-switch-to-counsel-find-file "find file")
     ("g" my-ivy-switch-to-counsel-git "git")))

  (ivy-add-actions
   'counsel-find-library
   '(("p" my-ivy-copy-library-path "copy path")))

  (ivy-add-actions
   'counsel-load-library
   '(("p" my-ivy-copy-library-path "copy path")))

  (ivy-add-actions
   #'counsel-bookmark
   '(("l" my-ivy-switch-to-list-bookmarks "list")))

  (ivy-add-actions
   #'counsel-colors-emacs
   '(("l" my-ivy-switch-to-list-colors "list")))

  (ivy-add-actions
   #'counsel-colors-web
   '(("l" my-ivy-switch-to-list-colors "list")))

  (ivy-add-actions
   #'counsel-package
   '(("l" my-ivy-switch-to-list-packages "list packages")))

  (ivy-add-actions
   #'counsel-list-processes
   '(("l" my-ivy-switch-to-list-processes "list")))


  (use-package amx
    :custom
    (amx-history-length 20))


  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
	  :custom
	  (hydra-hint-display-type 'posframe)
	  (ivy-read-action-function 'ivy-hydra-read-action)))


;; Use Ivy to open recent directories
(use-package ivy-dired-history
  :demand t
  :after dired
  :defines
  (savehist-additional-variables desktop-globals-to-save)
  :bind
  (:map dired-mode-map
        ("," . dired))
  :init
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-globals-to-save 'ivy-dired-history-variable)))


;; Ivy integration for Projectile
(use-package counsel-projectile
  :hook
  (counsel-mode . counsel-projectile-mode)
  :custom
  (counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  (counsel-projectile-grep-base-command "ugrep --color=never -rnEI %s"))


;; Better experience with icons
(use-package all-the-icons-ivy-rich
  :hook
  (ivy-mode . all-the-icons-ivy-rich-mode)
  :custom
  (all-the-icons-ivy-rich-icon t)
  :config
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'centaur-load-theme
             '(:columns
               ((all-the-icons-ivy-rich-theme-icon)
                (ivy-rich-candidate))
               :delimiter "\t"))
  (all-the-icons-ivy-rich-reload))


(use-package ivy-rich
  :hook
  (counsel-projectile-mode . ivy-rich-mode)
  (ivy-rich-mode . (lambda ()
                     "Use abbreviate in `ivy-rich-mode'."
                     (setq ivy-virtual-abbreviate
                           (or (and ivy-rich-mode 'abbreviate) 'name))))
  :custom
  (ivy-rich-parse-remote-buffer nil))


(use-package ivy-posframe
  :commands
  (ivy-posframe--minibuffer-setup my-ivy-posframe--minibuffer-setup ivy-posframe--display my-ivy-posframe--prettify-buffer)
  :hook
  (ivy-mode . ivy-posframe-mode)
  :custom-face
  (ivy-posframe-border ((t (:inherit posframe-border))))
  :custom
  ;; (ivy-height                15)
  (ivy-posframe-border-width 3)
  (ivy-posframe-parameters   '((left-fringe . 8)
                               (right-fringe . 8)))
  (ivy-posframe-display-functions-alist
   '((swiper                  . ivy-posframe-display-at-frame-center)
     (swiper-isearch          . ivy-posframe-display-at-frame-center)
     (counsel-rg              . ivy-posframe-display-at-frame-center)
     (counsel-fzf             . ivy-posframe-display-at-frame-center)
     (counsel-pt              . ivy-posframe-display-at-frame-center)
     (counsel-imenu           . ivy-posframe-display-at-frame-center)
     (swiper-isearch-backward . ivy-posframe-display-at-frame-center)
     (counsel-find-file       . ivy-posframe-display-at-frame-center)
     (counsel-recentf         . ivy-posframe-display-at-frame-center)
     (ivy-switch-buffer       . ivy-posframe-display-at-frame-center)))
  :config
  ;; HACK: hide minibuffer with same colors
  (defun my-ivy-posframe--minibuffer-setup (fn &rest args)
    "Advice function of FN, `ivy--minibuffer-setup' with ARGS."
    (if (not (display-graphic-p))
        (apply fn args)
      (let ((ivy-fixed-height-minibuffer nil))
        (apply fn args))
      (when (and ivy-posframe-hide-minibuffer
                 (posframe-workable-p)
                 (string-match-p "^ivy-posframe" (symbol-name ivy--display-function)))
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'ivy-posframe t)
          (overlay-put ov 'face
                       (let* ((face (if (or (bound-and-true-p solaire-global-mode)
                                            (bound-and-true-p solaire-mode))
                                        'solaire-default-face
                                      'default))
                              (bg-color (face-background face nil t)))
                         `(:background ,bg-color :foreground ,bg-color
                                       :box nil :underline nil
                                       :overline nil :strike-through nil)))
          (setq-local cursor-type nil)))))
  (advice-add #'ivy-posframe--minibuffer-setup :override #'my-ivy-posframe--minibuffer-setup)

  ;; Prettify the buffer
  (defun my-ivy-posframe--prettify-buffer (&rest _)
    "Add top and bottom margin to the prompt."
    (with-current-buffer ivy-posframe-buffer
      (goto-char (point-min))
      (insert (propertize "\n" 'face '(:height 0.3)))
      (goto-char (point-max))
      (insert (propertize "\n" 'face '(:height 0.3)))))
  (advice-add #'ivy-posframe--display :after #'my-ivy-posframe--prettify-buffer))


(use-package counsel-osx-app
  :if sys/macp
  :bind
  (:map counsel-mode-map
        ("C-c a p" . counsel-osx-app)))


(if sys/linux-x-p
    (bind-key "C-c a p" #'counsel-linux-app counsel-mode-map))


(provide 'init-ivy)

;;; init-ivy.el ends here
