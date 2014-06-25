;;; init.el ----- knuth's configuration entry point.
;;; Code:

(when (version< emacs-version "24.3")
	(error "knuth requires at least GNU Emacs 24.3"))

(defvar knuth-dir (file-name-directory load-file-name)
	"The root dir of the Emacs knuth distribution.")

(defvar knuth-core-dir (expand-file-name "core" knuth-dir)
  "The home of knuth's core functionality.")
(defvar knuth-modules-dir (expand-file-name  "modules" knuth-dir)
  "This directory houses all of the built-in knuth modules.")
(defvar knuth-personal-dir (expand-file-name "personal" knuth-dir)
  "This directory is for your personal configuration.

Uers of Emacs knuth are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by knuth.")

(defvar knuth-savefile-dir (expand-file-name "savefile" knuth-dir)
  "This folder stores all the automatically generated save/history-files.")
;(defvar knuth-modules-file (expand-file-name "knuth-modules.el" knuth-dir)
;  "This files contains a list of modules that will be loaded by knuth.")

;; add knuth's directories to Emacs's `load-path'
(add-to-list 'load-path knuth-core-dir)
(add-to-list 'load-path knuth-modules-dir)

;; the core stuff
(require 'knuth-packages)
(require 'knuth-ui)
(require 'knuth-core)
;(require 'knuth-mode)
(require 'knuth-editor)
(require 'knuth-global-keybindings)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(current-language-environment "Korean")
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" default)))
 '(ecb-windows-width 0.2)
 '(size-indication-mode t)
 '(sr-speedbar-width-x 25)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Envy Code R VS" :foundry "unknown" :slant normal :weight bold :height 96 :width normal))))
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil))))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "dark red"))))
 '(tabbar-button-highlight ((t (:inherit tabbar-default))))
 '(tabbar-default ((t (:inherit variable-pitch :background "#959A79" :foreground "black" :weight bold))))
 '(tabbar-highlight ((t (:underline t))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#95CA59"))))
 '(tabbar-separator ((t (:inherit tabbar-default :background "#95CA59"))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))
(put 'downcase-region 'disabled nil)
