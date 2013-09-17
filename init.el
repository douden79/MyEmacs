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
