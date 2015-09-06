;; macbook-settings.el --- startup config for jmm macbook

;; use cmd key for meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; use s-direction to move frames
(windmove-default-keybindings)

;; startup layout config
(setf inhibit-startup-screen t)
(setq initial-frame-alist '((fullscreen . maximized)))
(if (package-installed-p 'org)
    (progn
      (setq org-agenda-files (list "~/Documents/ubc"
				   "~/Documents/ubc/cpsc110"
				   "~/Documents/ubc/cogs200"
				   "~/Documents/ubc/stat200"
				   "~/Documents/southerncross"
				   "~/Documents/other"
				   "~/Documents/finances"))
     )) ;;(org-agenda-list)))
     ;; (add-hook 'after-init-hook 'org-agenda-list)
     ;; (setq initial-buffer-choice '(lambda () (get-buffer org-agenda-buffer-name)))))

;;(next-multiframe-window)
;;(eshell)
;;(split-window-horizontally)

(provide 'macbook-settings)

;; macbook-settings.el ends here
