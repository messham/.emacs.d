;; debug.el --- functions for debugging

;; octave
(defun octave-debug-move-next()
  "Inserts dbnext string"
  (interactive)
  (insert "dbnext")
  (comint-send-input))

(defun octave-debug-cont()
  "Inserts dbcont string"
  (interactive)
  (insert "dbcont")
  (comint-send-input))

(provide 'debug)

;; debug.el ends here
