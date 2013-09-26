;; knuth envirment
;; Code

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; load korean font set.
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                   '("Gulim" . "iso10646-1"))
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                   '("Gulim" . "iso10646-1"))

(setq face-font-rescale-alist
       '((".*hiragino.*" . 1.2)
		           (".*Gulim.*" . 1.0)))

 ;; coding system read file
(modify-coding-system-alist 'file "\\.c\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.py\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.S\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.h\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.txt\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.cpp\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.txt\\'" 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

 ;; global auto-complete mode
 
 ;; org2blog mode
(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
	  '(("wordpress"
		 :url "http://192.168.1.110/wordpress/xmlrpc.php"
		 :username "douden"
		 :default-title "Hello World"
		 :default-categories ("emacs")
		 :tags-as-categories nil)
		("my-blog"
		 :url "http://192.168.1.110/wordpress/xmlprc.php"
		 :username "douden")))


;; Auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

(when (require 'auto-complete nil t)
   (global-auto-complete-mode t)
   (define-key ac-complete-mode-map "\t" 'ac-expand)
   (define-key ac-complete-mode-map "\r" 'ac-complete)
   (define-key ac-complete-mode-map "\C-\M-n" 'ac-next)
   (define-key ac-complete-mode-map "\C-\M-p" 'ac-previous)
   (setq ac-auto-start t)
   (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))
 
   (add-to-list 'ac-modes 'eshell-mode)
 
   (add-hook 'emacs-lisp-mode-hook
             (lambda ()
               (make-local-variable 'ac-sources)
               (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))
 
   (add-hook 'eshell-mode-hook
             (lambda ()
               (make-local-variable 'ac-sources)
               (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer)))))
 
(defconst c++-keywords 
      (sort 
       (list "and" "bool" "compl" "do" "export" "goto" "namespace" "or_eq" "return"
             "struct" "try" "using" "xor" "and_eq" "break" "const" "double" "extern"
             "if" "new" "private" "short" "switch" "typedef" "virtual" "xor_eq" "asm"
             "case" "const_cast" "dynamic_cast" "false" "inline" "not" "protected" 
             "signed" "template" "typeid" "void" "auto" "catch" "continue" "else" 
             "float" "int" "not_eq" "public" "sizeof" "this" "typename" "volatile"
             "bitand" "char" "default" "enum" "for" "long" "operator" "register"
             "static" "throw" "union" "wchar_t" "bitor" "class" "delete" "explicit"
             "friend" "mutable" "or" "reinterpret_cast" "static_cast" "true" 
             "unsigned" "while" ) #'(lambda (a b) (> (length a) (length b)))))
(defvar ac-source-c++
      '((candidates
         . (lambda ()
             (all-completions ac-target c++-keywords))))
      "Source for c++ keywords.")
(add-hook 'c++-mode-hook
              (lambda ()
                (make-local-variable 'ac-sources)
				(setq ac-sources '(ac-source-c++))))

; autoload gtags mode.
(autoload 'helm-gtags-mode "gtags" "" t)
(add-hook 'c-mode-hook
	'(lambda ()
		(helm-gtags-mode t)))
(add-hook 'c++-mode-hook
	'(lambda ()
		(helm-gtags-mode t)))
(add-hook 'python-mode-hook
	'(lambda ()
		(helm-gtags-mode t)))

; set background color
(set-background-color "#1B1B1B")

; set cusor color
(set-cursor-color "#268BD2")

; markdown
(require 'knuth-markdown)

(provide 'knuth-editor)
;;; knuth-editor.el ends here

