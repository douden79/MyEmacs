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
;(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance
(scroll-bar-mode -1)
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
;@@@
; python tab hook
;@@@

(add-hook 'python-mode-hook
		(function (lambda ()
				   (setq indent-tabs-mode t)
				   (setq tab-width 4)
				   (setq python-indent 4))))

; default coding system.
;;(set-default-coding-systems 'utf-8)
;(add-hook 'find-file-hook 'set-default-coding-systems)
;(set-terminal-coding-system 'utf-8)
;(set-default-coding-systems 'utf-8)

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;(set-default default-buffer-file-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . linux-c-mode))

;; browse kill ring
;; requirement : https://github.com/browse-kill-ring/browse-kill-ring
(require 'browse-kill-ring)

;; (code from browse-kill-ring+.el)
(defun browse-kill-ring-do-insert (buf pt)
  (let ((str (browse-kill-ring-current-string buf pt)))
    (with-current-buffer browse-kill-ring-original-buffer
      (when browse-kill-ring-this-buffer-replace-yanked-text
        (delete-region (mark) (point)))
      ;; delete-selection-mode
      (when (and delete-selection-mode (not buffer-read-only) transient-mark-mode mark-active)
        (delete-active-region))
      (browse-kill-ring-insert-and-highlight str))))

;; (code from browse-kill-ring+.el)
(defun browse-kill-ring-delete ()       ; Bound to `d' in selection-ring buffer.
  "Remove all occurrences of selection at point from current selection ring."
  (interactive)
  (forward-line 0)
  (unwind-protect
       (let* ((ov      (browse-kill-ring-target-overlay-at (point)))
              (target  (overlay-get ov 'browse-kill-ring-target)))
         (setq buffer-read-only  nil)
         (delete-region (overlay-start ov) (1+ (overlay-end ov)))
         (setq kill-ring (delete target kill-ring))
         (when (get-text-property (point) 'browse-kill-ring-extra)
           (let ((prev  (previous-single-property-change (point) 'browse-kill-ring-extra))
                 (next  (next-single-property-change (point) 'browse-kill-ring-extra)))
             (when prev (incf prev))
             (when next (incf next))
             (delete-region (or prev (point-min)) (or next (point-max))))))
    (setq buffer-read-only  t))
  (browse-kill-ring-resize-window)
  (browse-kill-ring-forward 0))

;; (code from browse-kill-ring+.el)
(defun browse-kill-ring-target-overlay-at (position)
  "Return overlay at POSITION that has property `browse-kill-ring-target'.
If no such overlay, raise an error."
  (let ((ovs  (overlays-at (point))))
    (catch 'browse-kill-ring-target-overlay-at
      (dolist (ov  ovs)
        (when (overlay-get ov 'browse-kill-ring-target)
          (throw 'browse-kill-ring-target-overlay-at ov)))
      (error "No selection-ring item here"))))

(setq browse-kill-ring-display-style 'one-line)

(setq browse-kill-ring-no-duplicates t)

; browse-kill-ring-display-duplicates
(setq browse-kill-ring-display-duplicates nil)
;; browse-kill-ring-maximum-display-length size 
(setq browse-kill-ring-maximum-display-length 60)
;; browse-kill-ring-show-preview 
(setq browse-kill-ring-show-preview t)
;; browse-kill-ring-quit-action
(setq browse-kill-ring-quit-action 'kill-and-delete-window)
;; browse-kill-max 
(setq kill-ring-max 100)

;; browse-kill-ring-depropertize 
(setq browse-kill-ring-depropertize t)

(add-hook 'browse-kill-ring-mode-hook
  (lambda ()
    ;; -------------------------------------------------------------------------
    ;; linum-mode 
;;    (linum-mode -1)
    ;; -------------------------------------------------------------------------
    (hl-line-mode)
    ;; -------------------------------------------------------------------------
    ;; (define-key browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-move-and-quit)
    ))

;; default disable make-backup-files
(setq make-backup-files nil)

;; automatic indentation
(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

;; highlight the current line
(global-hl-line-mode +1)

;; yasnippet
(require 'yasnippet)

(yas-global-mode 1)
 
(defun ac-yasnippet-candidates ()
  (with-no-warnings
    (cond
      (;; 0.8 onwards
        (fboundp 'yas-active-keys)
        (all-completions ac-prefix (yas-active-keys)))
      (;; >0.6.0
        (fboundp 'yas/get-snippet-tables)
        (apply 'append (mapcar 'ac-yasnippet-candidate-1
          (condition-case nil
            (yas/get-snippet-tables major-mode)
            (wrong-number-of-arguments
              (yas/get-snippet-tables))))))
      (t
        (let
          (
            (table
              (if (fboundp 'yas/snippet-table)
                ;; <0.6.0
                (yas/snippet-table major-mode)
                ;; 0.6.0
                (yas/current-snippet-table))))
        (if table
          (ac-yasnippet-candidate-1 table)))))))
 
(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate."
  :group 'auto-complete)
 
(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate.")
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;@@@
; load korean font set.
;@@@

(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                   '("Gulim" . "iso10646-1"))
(set-fontset-font "fontset-default" '(#xe0bc . #xf66e)
                   '("Gulim" . "iso10646-1"))

(setq face-font-rescale-alist
       '((".*hiragino.*" . 1.0)
		           (".*Gulim.*" . 1.0)))

 ;; coding system read file
(modify-coding-system-alist 'file "\\.c\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.py\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.S\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.h\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.txt\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.cpp\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.txt\\'" 'utf-8)


; xcscope preference
(require 'xcscope)
(cscope-setup)

; ansi-term mode disable yasnnipet
(add-hook 'term-mode-hook (lambda()
						   (yas-minor-mode -1)))
 ;; org2blog mode
(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
	  '(("wordpress"
		 :url "http://192.168.1.138/word/xmlrpc.php"
		 :username "douden"
		 :default-title "Hello World"
		 :default-categories ("emacs")
		 :tags-as-categories nil)
		 ))

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

; linux kernel c style and mode
; under line fix path for your linux kernel directory.
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

(setq auto-mode-alist (cons '("~/000.ST/linux.*/.*\\.[ch]$" . linux-c-mode)
                        auto-mode-alist))
; tabbar preference
(setq tabbar-background-color "#959A79") ;; the color of the tabbar background
(custom-set-faces
  '(tabbar-default ((t (:inherit variable-pitch :background "#959A79" :foreground "black" :weight bold))))
  '(tabbar-button ((t (:inherit tabbar-default :foreground "dark red"))))
  '(tabbar-button-highlight ((t (:inherit tabbar-default))))
  '(tabbar-highlight ((t (:underline t))))
  '(tabbar-selected ((t (:inherit tabbar-default :background "#95CA59"))))
  '(tabbar-separator ((t (:inherit tabbar-default :background "#95CA59"))))
  '(tabbar-unselected ((t (:inherit tabbar-default)))))

; set background color
(set-background-color "#1B1B1B")

; multi term
(require 'multi-term)
(multi-term-keystroke-setup)
(setq multi-term-program "/bin/bash")

;@@@
; set cusor color
;@@@

(set-cursor-color "#268BD2")

; markdown
(require 'knuth-markdown)

;@@@
; set multiple cusor
; really very good ( multi line edit )
;@@@

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-words-like-this)

(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))
(provide 'knuth-editor)
;;; knuth-editor.el ends here
