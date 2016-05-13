;; macbook-settings.el --- startup config for jmm macbook

;; use cmd key for meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; use s-direction to move frames
(windmove-default-keybindings)

;; startup layout config
(setf inhibit-startup-screen t)
(setq initial-frame-alist '((fullscreen . maximized)))
(if (package-installed-p 'org) (org-agenda-list))
(next-multiframe-window)
(eshell)

(provide 'macbook-settings)

;; macbook-settings.el ends here
