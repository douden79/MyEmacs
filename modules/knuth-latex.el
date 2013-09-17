;;; knuth-latex.el --- Emacs knuth: Sane setup for LaTeX writers.
;;; Code:

(knuth-ensure-module-deps '(auctex))
(require 'smartparens-latex)

;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

;; use pdflatex
(setq TeX-PDF-mode t)

(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "PDF Viewer")
        (output-html "HTML Viewer")))

;; this section is good for OS X only
;; TODO add sensible defaults for Linux/Windows
(setq TeX-view-program-list
      '(("DVI Viewer" "open %o")
        ("PDF Viewer" "open %o")
        ("HTML Viewer" "open %o")))

(defun knuth-latex-mode-defaults ()
  (turn-on-auto-fill)
  (abbrev-mode +1))

(setq knuth-latex-mode-hook 'knuth-latex-mode-defaults)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (run-hooks 'knuth-latex-mode-hook)))

(provide 'knuth-latex)

;;; knuth-latex.el ends here
