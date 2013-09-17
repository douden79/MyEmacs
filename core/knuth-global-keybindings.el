 ;;; prelude-global-keybindings.el --- Emacs Prelude: some useful keybindings.
 ;;; copy
 
 ;Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

(require 'sr-speedbar)
 ;; sr-speedbar key mapping
(global-set-key (kbd "C-x t") 'sr-speedbar-toggle)
 
;; ecb mode
(require 'ecb)
;; custom-set-variables
(define-key ecb-mode-map (kbd "M-1") 'ecb-goto-window-directories)
(define-key ecb-mode-map (kbd "M-2") 'ecb-goto-window-sources)
(define-key ecb-mode-map (kbd "M-3") 'ecb-goto-window-methods)
(define-key ecb-mode-map (kbd "M-4") 'ecb-goto-window-history)
(define-key ecb-mode-map (kbd "M-5") 'ecb-goto-window-compilation)
(define-key ecb-mode-map (kbd "M-0") 'ecb-goto-window-edit1)

;; move emacs windows
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down) 

(provide 'knuth-global-keybindings)
(require 'helm-config)
(require 'helm-gtags)

;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(setq helm-gtags-path-style 'relative)
(setq helm-gtags-ignore-case t)
(setq helm-gtags-read-only t)
(setq helm-gtags-auto-update t)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
			  (local-set-key (kbd "M-t") 'helm-gtags-pop-stack)
			  (local-set-key (kbd "M-]") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-[") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)))

(provide 'knuth-global-keybindings)
;; end knuth-global-keybindings
