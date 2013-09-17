;;; knuth-python.el --- Emacs knuth: python.el configuration.
;;; Code:

(require 'knuth-programming)

(defun knuth-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  (electric-indent-mode -1))

(setq knuth-python-mode-hook 'knuth-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'knuth-python-mode-hook)))
(provide 'knuth-python)

;;; knuth-python.el ends here
