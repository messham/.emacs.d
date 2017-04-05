;; messham emacs init file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REQUIRES EMACS >= 24.4

;; turn off all of the bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; add file path to non-package programs
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; load package.el and use-package.el
;; TERMINAL-MODE: may need to uncomment all package stuff on first startup
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
(require 'use-package)

;; allow copy with mouse in emacs terminal mode
(setq x-select-enable-clipboard t)

;; indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; set individual package configs
(setq load-prefer-newer t)
(require 'use-package-configs)

