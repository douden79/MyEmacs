;;; knuth-css.el --- Emacs Prelude: css support
;;
;;; Code:

(eval-after-load 'css-mode
  '(progn
     (knuth-ensure-module-deps '(rainbow-mode))

     (defun knuth-css-mode-defaults ()
       (setq css-indent-offset 2)
       (rainbow-mode +1))

     (setq knuth-css-mode-hook 'knuth-css-mode-defaults)

     (add-hook 'css-mode-hook (lambda ()
                                (run-hooks 'knuth-css-mode-hook)))))

(provide 'knuth-css)
;;; knuth-css.el ends here
