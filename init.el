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

;; load os specific settings
(when (eq system-type 'darwin)
  (require 'macbook-settings))

;; set individual package configs
(use-package auto-complete
  :ensure t
  :config
  (progn
    (ac-config-default)
    (add-to-list 'ac-modes 'c++-mode)))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (progn
    (load-theme 'sanityinc-tomorrow-night :no-confirm)
    (setf frame-background-mode 'dark)
    (global-hl-line-mode 1)
    (custom-set-faces
     '(cursor               ((t :background "#eebb28")))
     '(linum                ((t :foreground "gray55")))
     '(diff-added           ((t :foreground "green" :underline nil)))
     '(diff-removed         ((t :foreground "red" :underline nil)))
     '(highlight            ((t :background "black" :underline nil)))
     '(magit-item-highlight ((t :background "black")))
     '(hl-line              ((t :background "gray10"))))))

(use-package dired
  :config
  (define-key dired-mode-map (kbd "C-l") #'dired-up-directory))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

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
    (require 'org-agenda-add-overlays)            ;; agenda + capture settings
    (add-hook 'org-agenda-finalize-hook 'org-agenda-add-overlays)
    (setf org-directory "~/org")
    (setf org-agenda-files (list org-directory))
    (setf org-default-notes-file (concat org-directory "/notes.org"))
    (setf org-agenda-start-on-weekday nil)
    (setf org-log-done t)
    (setf org-reverse-note-order t)
    (setq org-capture-templates
	  '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Tasks")
	    "* TODO %?\n  %u")
	   ))
    (global-set-key (kbd "C-c c") #'org-capture)
    (global-set-key (kbd "C-c a") #'org-agenda)
    (org-agenda-list)
    (if (file-exists-p "~/Dropbox/Apps/MobileOrg")  ;; MobileOrg settings
    	(progn
    	  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    	  (setq org-mobile-inbox-for-pull "~/org/flagged.org"))))) 

 (use-package smart-compile
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c C") #'smart-compile)))

;; !!! config smart compile for python

(use-package virtualenvwrapper
  :ensure t
  :config
  (progn
    (venv-initialize-eshell)
    (setf venv-location "~/.virtualenvs")))

(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-reload-all)
    (add-hook 'c++-mode-hook #'yas-minor-mode)))
