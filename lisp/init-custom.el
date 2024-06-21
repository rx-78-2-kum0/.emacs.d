;; init-custom.el --- Define custom.	-*- lexical-binding: t -*-

;;; Commentary:
;; somme customizable variables.

;;; Code:


(eval-when-compile
  (require 'init-const))


(defcustom my/package-archives-alist
  '((melpa    . (("gnu"    . "http://elpa.gnu.org/packages/")
                 ("nongnu" . "http://elpa.nongnu.org/nongnu/")
                 ("melpa"  . "http://melpa.org/packages/")))
    (emacs-cn . (("gnu"    . "http://1.15.88.122/gnu/")
                 ("nongnu" . "http://1.15.88.122/nongnu/")
                 ("melpa"  . "http://1.15.88.122/melpa/")))
    (bfsu     . (("gnu"    . "http://mirrors.bfsu.edu.cn/elpa/gnu/")
                 ("nongnu" . "http://mirrors.bfsu.edu.cn/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.bfsu.edu.cn/elpa/melpa/")))
    (netease  . (("gnu"    . "http://mirrors.163.com/elpa/gnu/")
                 ("nongnu" . "http://mirrors.163.com/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.163.com/elpa/melpa/")))
    (sjtu     . (("gnu"    . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
                 ("nongnu" . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
                 ("melpa"  . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")))
    (tuna     . (("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                 ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
    (ustc     . (("gnu"    . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                 ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.ustc.edu.cn/elpa/melpa/"))))
  "A list of the package archives."
  :group 'centaur
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))


(defcustom my/package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'centaur
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value my/package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    my/package-archives-alist)))


;; (defconst my/package-archives 'tuna
;;   "Set package archives from which to fetch.
;; (choice
;;   (const :tag \"Melpa\" melpa)
;;   (const :tag \"Emacs-China\" emacs-china)
;;   (const :tag \"Tuna\" tuna)
;;   (const :tag \"Netease\" netease)
;;   (const :tag \"Melpa-Mirror\" melpa-mirror))")


(defconst my/logo (expand-file-name "logo.png" user-emacs-directory)
  "Path of logo file.")


(defconst my/default-theme 'monokai
  "Default theme.")


(defconst my/theme-list '((monokai-theme monokai)
                          (monokai-pro-theme monokai-pro)
                          (dracula-theme dracula)
                          (doom-themes doom-one-light)
                          (doom-themes doom-vibrant)
                          (doom-themes doom-nord)
                          (doom-themes doom-nord-light)
                          (doom-themes doom-palenight)
                          (doom-themes doom-tomorrow-night)
                          (doom-themes doom-spacegrey)
                          (doom-themes doom-fairy-floss)
                          (doom-themes doom-challenger-deep)
                          (doom-themes doom-sourcerer)
                          (doom-themes doom-solarized-dark)
                          (doom-themes doom-solarized-light)
                          (doom-themes doom-molokai)
                          (doom-themes doom-gruvbox)
                          (doom-themes doom-horizon)
                          (doom-themes doom-wilmersdorf)
                          (doom-themes doom-acario-dark)
                          (cyberpunk-theme cyberpunk)
                          (snazzy-theme snazzy)
                          (atom-one-dark-theme atom-one-dark)
                          (spacemacs-theme spacemacs-light)
                          (spacemacs-theme spacemacs-dark)
                          (srcery-theme srcery)
                          (kaolin-themes kaolin-dark)
                          (kaolin-themes kaolin-aurora)
                          (kaolin-themes kaolin-bubblegum)
                          (kaolin-themes kaolin-light)
                          (kaolin-themes kaolin-ocean)
                          (kaolin-themes kaolin-galaxy)
                          (kaolin-themes kaolin-eclipse)
                          (kaolin-themes kaolin-valley-dark)
                          (kaolin-themes kaolin-temple)
                          (leuven-theme leuven light (setq leuven-scale-outline-headlines nil))
                          (leuven-theme leuven-dark (setq leuven-scale-outline-headlines nil))
                          (ample-theme ample)
                          (ample-theme ample-flat)
                          (ample-theme ample-light)
                          (sublime-themes brin)
                          (sublime-themes granger)
                          (sublime-themes spolsky)
                          (sublime-themes fogus)
                          (sublime-themes junio)
                          (jazz-theme jazz)
                          (grandshell-theme grandshell)
                          (tron-legacy-theme tron-legacy)
                          (afternoon-theme afternoon)
                          (night-owl-theme night-owl)
                          (ef-themes ef-winter)
                          (ef-themes ef-summer)
                          (ef-themes ef-duo-light)
                          (ef-themes ef-day)
                          (ef-themes ef-frost)
                          (ef-themes ef-spring)
                          (ef-themes ef-trio-light)
                          (ef-themes ef-tritanopia-light)
                          (ef-themes ef-dark)
                          (ef-themes ef-duo-dark)
                          (ef-themes ef-bio)
                          (ef-themes ef-night)
                          (ef-themes ef-cherie)
                          (ef-themes ef-trio-dark)
                          (ef-themes ef-deuteranopia-dark)
                          (color-theme-sanityinc-tomorrow color-theme-sanityinc-tomorrow-day)
                          (color-theme-sanityinc-tomorrow color-theme-sanityinc-tomorrow-night)
                          (color-theme-sanityinc-tomorrow color-theme-sanityinc-tomorrow-blue)
                          (color-theme-sanityinc-tomorrow color-theme-sanityinc-tomorrow-bright)
                          (color-theme-sanityinc-tomorrow color-theme-sanityinc-tomorrow-eighties)
                          (modus-themes modus-operandi)
                          (modus-themes modus-operandi-tinted)
                          (modus-themes modus-operandi-deuteranopia)
                          (modus-themes modus-operandi-tritanopia)
                          (modus-themes modus-vivendi)
                          (modus-themes modus-vivendi-tinted)
                          (modus-themes modus-vivendi-deuteranopia)
                          (modus-themes modus-vivendi-tritanopia)
                          (darktooth-theme darktooth)
                          (darktooth-theme darktooth-dark)
                          (darktooth-theme darktooth-darker)
                          (kanagawa-theme kanagawa)
                          (almost-mono-themes almost-mono-white)
                          (almost-mono-themes almost-mono-black)
                          (almost-mono-themes almost-mono-gray)
                          (almost-mono-themes almost-mono-cream))
  "Theme list.")


(defconst my/font-list
  '(Menlo
    SF\ Mono
    Monaco
    Roboto\ Mono
    Ubuntu\ Mono
    Anonymous\ Pro
    FantasqueSansMono
    FiraMono
    Fira\ Code
    Operator\ Mono
    Inconsolata
    Iosevka
    Cochin
    Optima
    Victor\ Mono
    JetBrains\ Mono
    Pes\ Mono
    Fairfax\ Hax\ HD
    Unifont
    IBM\ Plex\ Mono
    Holic)
  "Font list.")


(defconst my/default-font-size
  (if sys/macp 135 100)
  "Font size.")


(defconst my/font-weight
  'normal
  "Font weight.")


(defconst my/ccls-initialization-options
  (if sys/macp
      '(:clang
        (:extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                     "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                     "-isystem/usr/local/include"]
                    :resourceDir "/Library/Developer/CommandLineTools/usr/lib/clang/11.0.3"))
    nil)
  "CCLS initialization options.")


(defconst my/easy-hugo-basedir "~/WWW-BUILDER/"
  "Hugo basedir.")


(defconst my/easy-hugo-postdir "notebook-org"
  "Hugo postdir.")


(defconst my/easy-hugo-url "https://zakudriver.github.io/"
  "Hugo site url.")


(defconst my/easy-hugo-preview-url "http://localhost:1313/"
  "Hugo preview url.")


(defconst my/easy-hugo-github-deploy-script "publish.sh"
  "Hugo github deploy script.")


(defconst my/easy-hugo-github-deploy-buffer-name "*Hugo Github Deploy*"
  "Hugo github deploy buffer name.")


(defconst my/lsp-major-mode '(go-mode web-mode ng2-mode ng2-html-mode ng2-ts-mode sh-mode ruby-mode css-mode scss-mode sass-mode clojure-mode dart-mode rust-mode swift-mode js-mode graphql-mode html-mode typescript-mode typescriptreact-mode)
  "Lsp Supported major mode.")


(defconst my/eglot-major-mode '(js-mode typescript-mode typescriptreact-mode markdown-mode yaml-mode)
  "Eglot Supported major mode.")


(defconst my/lsp-on-save-major-mode '(ruby-mode clojure-mode dart-mode)
  "Lsp Supported major mode.")


(defconst my/org-mode-export-html-css "/org/style.css"
  "Insert inline css file when org export html.")


;; (defconst my/evil-local-mode '(web-mode typescript-mode js-mode js-mode go-mode ruby-mode css-mode scss-mode)
;;   "Start evil-local-mode list.")


(defconst my/native-compile-async-jobs (or (ignore-errors
	                                           (string-to-number (shell-command-to-string "nproc")))
	                                         4)
  "How many jobs to use.")


(defconst my/org-headline-bullets-list '("🌞" "🌤" "🌦️" "🌧️" "🌈")
  "List of org headline bullets.")


;; (defconst my/prettify-symbols-alist
;;   '(("lambda" . ?λ)
;;     ("<-" . ?←)
;;     ("->" . ?→)
;;     ("->>" . ?↠)
;;     ("=>" . ?⇒)
;;     ("map" . ?↦)
;;     ("/=" . ?≠)
;;     ("!=" . ?≠)
;;     ("==" . ?≡)
;;     ("<=" . ?≤)
;;     (">=" . ?≥)
;;     ("<=<" . ?↢)
;;     (">=>" . ?↣))
;;   "Alist of symbol prettifications.")


(provide 'init-custom)

;;; init-custom.el ends here
