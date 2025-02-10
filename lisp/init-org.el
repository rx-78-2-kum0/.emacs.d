;; init-org.el --- Initialize Org configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of org.

;;; Code:


(eval-when-compile
  (require 'init-custom)
  (require 'init-funcs))


(use-package org
  :ensure nil
  :bind
  (("C-, o o" . org-switch)
   ("C-, o a" . org-agenda)
   :map org-mode-map
   ("C-,"   . nil)
   ("C-, l" . font-lock-mode)
   ("C-, c" . org-cycle-agenda-files)
   ("C-c C-b" . org-mark-ring-goto))
  :custom
  (org-startup-indented                t)
  (org-startup-with-inline-images      t)
  (org-log-done                        'note)
  (org-edit-timestamp-down-means-later t)
  (org-hide-emphasis-markers         t)
  (org-catch-invisible-edits         'show)
  (org-fast-tag-selection-single-key 'expert)
  (org-tags-column                   80)
  (org-image-actual-width            nil)
  (org-src-fontify-natively          t)
  (org-list-allow-alphabetical       t)
  (truncate-lines                    nil)
  (org-todo-keywords                 '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
                                       (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")))
  (org-checkbox-hierarchical-statistics  t)
  (org-todo-keyword-faces            '(("HANGUP" . warning)
                                       ("❓" . warning)))
  (org-priority-faces                '((?A . error)
                                       (?B . warning)
                                       (?C . success)))
  :config
  ;; Embed inline CSS read from a file.
  (add-hook 'org-export-before-processing-functions 'my-org-inline-css-hook)

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  (use-package org-download
    :hook
    (dired-mode . org-download-enable)
    :custom
    (org-download-image-dir "./org-imgs")
    (org-download-backend "curl \"%s\" -o \"%s\""))

  (use-package org-superstar
    :commands my-chars-displayable-p
    :if
    (and (display-graphic-p) (my-chars-displayable-p my/org-headline-bullets-list))
    :hook
    (org-mode . org-superstar-mode)
    :custom
    (org-superstar-headline-bullets-list my/org-headline-bullets-list)))


(use-package ob-go)


(use-package ob-typescript)


(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (shell      . t)
                               (css        . t)
                               (sass       . t)
                               (go         . t)
                               (typescript . t)))


;; hugo
(use-package easy-hugo
  :commands easy-hugo
  :bind
  (("C-c H" . easy-hugo)
   :map easy-hugo-mode-map
   ("G" . my-easy-hugo-github-deploy))
  :custom
  (easy-hugo-org-header           t)
  (easy-hugo-basedir              my/easy-hugo-basedir)
  (easy-hugo-postdir              my/easy-hugo-postdir)
  (easy-hugo-url                  my/easy-hugo-url)
  (easy-hugo-preview-url          my/easy-hugo-preview-url)
  (easy-hugo-github-deploy-script my/easy-hugo-github-deploy-script)
  (easy-hugo-default-ext          ".org"))


(use-package ox-hugo
  :after ox
  :custom
  (org-hugo-base-dir my/easy-hugo-basedir))


(provide 'init-org)

;;; init-org.el ends here
