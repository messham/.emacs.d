;; messham emacs init file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REQUIRES EMACS >= 24.4

;; turn off all of the bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; emacs backup settings
(setq backup-directory-alist `((".*" . "~/.saves"))
      backup-by-copying t
      delete-old-versions t
      version-control t)

;; load package.el and use-package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; add file path to non-package programs
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; set individual package configs
(require 'use-package-configs)

;; load workspace specific settings
(when (eq system-type 'darwin)
  (require 'macbook-settings))
