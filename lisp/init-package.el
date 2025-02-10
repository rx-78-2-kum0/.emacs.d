;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; some configuration of package.

;;; Code:

(eval-when-compile
  (require 'init-custom)
  (require 'init-funcs))

;; (if (or (executable-find "curl") (executable-find "wget"))
;;     (progn
;;       ;; Get and select the fastest package archives automatically
;;       (message "Testing connection... Please wait a moment.")
;;       (my-set-package-archives
;;        (my-test-package-archives 'no-chart)))
;;   ;; Select package archives manually
;;   ;; Use `ido-completing-read' for better experience since
;;   ;; `ivy-mode' is not available at this moment.
;;   (my-set-package-archives
;;    (intern
;;     (ido-completing-read
;;      "Select package archives: "
;;      (mapcar #'symbol-name
;;              (mapcar #'car my/package-archives-alist))))))


(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)


;; Set ELPA packages
(my-set-package-archives my/package-archives nil nil)


;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice

  (package-initialize))


;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t
        use-package-enable-imenu-support t))


(eval-when-compile
  (require 'use-package))


;; (use-package comp
;;   :ensure nil
;;   :config
;;   (defun my-package-native-compile-async (package &optional all)
;;     "Compile PACKAGE natively, or with prefix ALL, all packages."
;;     (interactive (list (unless current-prefix-arg
;; 			                   (completing-read "Package: " (mapcar #'car package-alist)))))
;;     (let* ((directory (if package
;; 			                    (file-name-directory (locate-library package))
;; 			                  package-user-dir)))
;;       (native-compile-async directory my/native-compile-async-jobs t))))


;; Required by `use-package'
(use-package diminish
  :ensure t)
(use-package bind-key)
(use-package pretty-hydra
  :demand t)


;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))


;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)


;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)


;; Auto update packages
(use-package auto-package-update
  ;; :ensure nil
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-interval            7)
  (auto-package-update-hide-results        t))

;; (native-compile-async (concat user-emacs-directory "site-lisp") 'recursively)
;; (native-compile-async (concat user-emacs-directory "lisp") 'recursively)


(provide 'init-package)

;;; init-package.el ends here
