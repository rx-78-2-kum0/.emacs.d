;; init-ibuffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; some configuration of ibuffer.

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


(use-package ibuffer
  :commands
  (ibuffer-find-file
   ibuffer-current-buffer)
  :bind
  ("C-x C-b" . ibuffer)
  :hook
  (ibuffer . (lambda ()
               ;; (persp-ibuffer-set-filter-groups)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic))))
  :custom
  (ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (with-eval-after-load 'counsel
    (advice-add #'ibuffer-find-file :override (lambda ()
                                                (interactive)
                                                (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                                                           (if (buffer-live-p buf)
                                                                               (with-current-buffer buf
                                                                                 default-directory)
                                                                             default-directory))))
                                                  (counsel-find-file default-directory)))))

  (use-package all-the-icons-ibuffer
    :hook
    (ibuffer-mode . all-the-icons-ibuffer-mode))


  ;; Group ibuffer's list by project root
  ;; (use-package ibuffer-projectile
  ;;   :functions ibuffer-do-sort-by-alphabetic
  ;;   :hook
  ;;   (ibuffer . (lambda ()
  ;;                (ibuffer-projectile-set-filter-groups)
  ;;                (unless (eq ibuffer-sorting-mode 'alphabetic)
  ;;                  (ibuffer-do-sort-by-alphabetic))))
  ;;   :custom
  ;;   (ibuffer-projectile-prefix
  ;;    (concat
  ;;     (all-the-icons-octicon "file-directory"
  ;;                            :face ibuffer-filter-group-name-face
  ;;                            :v-adjust -0.05
  ;;                            :height 1.25)
  ;;     " ")))
  )


(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))


;; Display icons for buffers
(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (nerd-icons-ibuffer-icon centaur-icon))


;; Group ibuffer's list by project
(use-package ibuffer-project
  :hook
  (ibuffer . (lambda ()
               "Group ibuffer's list by project."
               (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
               (unless (eq ibuffer-sorting-mode 'project-file-relative)
                 (ibuffer-do-sort-by-project-file-relative))))
  :custom
  (ibuffer-project-use-cache t)
  :config
  (defun my-ibuffer-project-group-name (root type)
    "Return group name for project ROOT and TYPE."
    (if (and (stringp type) (> (length type) 0))
        (format "%s %s" type root)
      (format "%s" root)))

  (progn
    (advice-add #'ibuffer-project-group-name :override #'my-ibuffer-project-group-name)
    (setq ibuffer-project-root-functions
          `((ibuffer-project-project-root . ,(nerd-icons-octicon "nf-oct-repo" :height 1.2 :face ibuffer-filter-group-name-face))
            (file-remote-p . ,(nerd-icons-codicon "nf-cod-radio_tower" :height 1.2 :face ibuffer-filter-group-name-face))))))


(provide 'init-ibuffer)

;;; init-ibuffer.el ends here
