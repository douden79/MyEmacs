;;; knuth-ui.el --- Emacs Knuth:UI optimizations and tweaks.

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; monokai theme load.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme 'monokai t)
;(load-theme 'solarized-light t)

;; powerline-evil
(require 'powerline-evil)
(powerline-default-theme)

;;(custom-set-faces
;;  '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
;;   '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

(provide 'knuth-ui)
;;; knuth-ui.el ends here

