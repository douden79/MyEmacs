;;; knuth-packages.el --- Emacs knuth: default package selection.
;;
;;; Code:

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; set package-user-dir to be relative to knuth install path
(setq package-user-dir (expand-file-name "elpa" knuth-dir))
(package-initialize)

(defvar knuth-packages
  '( auto-complete auto-yasnippet cl flymake-python-pyflakes flymake-cursor highlight-symbol markdown-mode+ expand-region ecb helm-gtags diminish minimap rainbow-mode smartparens auctex xcscope powerline-evil volatile-highlights monokai-theme solarized-theme xterm-color mouse+ multi-term metaweblog org2blog xml-rpc sr-speedbar multiple-cursors mark-multiple )
  "A list of packages to ensure are installed at launch.")

(defun knuth-packages-installed-p ()
  "Check if all packages in `knuth-packages' are installed."
  (every #'package-installed-p knuth-packages))

(defun knuth-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun knuth-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'knuth-require-package packages))

(defalias 'knuth-ensure-module-deps 'knuth-require-packages)

(defun knuth-install-packages ()
  "Install all packages listed in `knuth-packages'."
  (unless (knuth-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs knuth is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (knuth-require-packages knuth-packages)))

(knuth-install-packages)

(defmacro knuth-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar knuth-auto-install-alist
    '(("\\.css\\'" css-mode css-mode)
     ("\\.markdown\\'" markdown-mode markdown-mode)
     ("\\.md\\'" markdown-mode markdown-mode)
     ("\\.php\\'" php-mode php-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (knuth-auto-install extension package mode))))
 knuth-auto-install-alist)

(provide 'knuth-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; knuth-packages.el ends here
