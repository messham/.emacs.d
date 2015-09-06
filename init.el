;; messham emacs init file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; turn off all of the bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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

;; load os specific settings
(when (eq system-type 'darwin)
  (require 'macbook-settings))

;; set individual package configs
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (progn
    (load-theme 'sanityinc-tomorrow-night :no-confirm)
    (setf frame-background-mode 'dark)
    (global-hl-line-mode 1)
    (custom-set-faces
     '(cursor               ((t :background "#eebb28")))
     '(diff-added           ((t :foreground "green" :underline nil)))
     '(diff-removed         ((t :foreground "red" :underline nil)))
     '(highlight            ((t :background "black" :underline nil)))
     '(magit-item-highlight ((t :background "black")))
     '(hl-line              ((t :background "gray10"))))))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
t
(use-package helm
  :ensure t
  :init
  (progn
    (require 'helm-config)
    (helm-mode)
    (helm-adaptive-mode 1))
  :config
  (progn
    (setf helm-move-to-line-cycle-in-source t
	  helm-split-window-in-side-p t
          helm-recentf-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-adaptive-history-file (locate-user-emacs-file "local/helm")
          recentf-save-file (locate-user-emacs-file "local/recentf"))
    (global-set-key (kbd "C-x b") #'helm-mini)
    (global-set-key (kbd "C-x C-b") #'helm-buffers-list)
    (global-set-key (kbd "C-x C-f") #'helm-find-files)
    (global-set-key (kbd "C-x C-r") #'helm-recentf)
    (global-set-key (kbd "C-h w") #'helm-man-woman)
    (global-set-key (kbd "M-x") #'helm-M-x)
    (global-set-key (kbd "M-y") #'helm-show-kill-ring)
    (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") #'helm-select-action)))

(use-package helm-swoop
  :ensure t
  :config
  (progn
    (setf helm-swoop-split-with-multiple-windows t)
    (global-set-key (kbd "C-s") #'helm-swoop)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package org
  :ensure t
  :config
  (progn
    (require 'org-agenda-add-overlays)
    (add-hook 'org-agenda-finalize-hook 'org-agenda-add-overlays)
    (setf org-log-done t)
    (global-set-key (kbd "C-c a") #'org-agenda)     ;; org-agenda-files specified in os setting
    (if (file-exists-p "~/Dropbox/Apps/MobileOrg")  
	(progn
	  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
	  (setq org-mobile-inbox-for-pull "~/org/flagged.org"))))) 

(use-package virtualenvwrapper
  :ensure t
  :config
  (progn
    (venv-initialize-eshell)
    (setf venv-location "~/.virtualenvs")))

(use-package yasnippet
  :ensure t)
