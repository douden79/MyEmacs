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

;; powerline visual mode
(add-to-list 'load-path "~/.emacs.d/elpa/emacs-powerline")
(require 'powerline)

(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

(setq powerline-arrow-shape 'arrow)   ;; the default
(setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
(setq powerline-arrow-shape 'arrow14) ;; best for small fonts

(custom-set-faces
  '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
   '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;; start pre enable default setting
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default)))
 '(ecb-windows-width 0.2 )
 '(sr-speedbar-width-x 25 )
 '(current-language-environment "Korean"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Envy Code R VS" :foundry "unknown" :slant normal :weight normal :height 96 :width normal)))))

(provide 'knuth-ui)
;;; knuth-ui.el ends here

