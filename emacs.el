
(let ((default-directory "~/dot-emacs/"))
      (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/dot-emacs/")

(load "exec-path-from-shell.el")
(exec-path-from-shell-initialize)

  (setenv "PATH" (concat "/usr/texbin" ":" (getenv "PATH")))

(defun make-backup-file-name (file)
  (concat "~/.emacs_backups/" (file-name-nondirectory file) "~"))

(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))

(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

(setq inhibit-splash-screen t)
(set-scroll-bar-mode nil)
(tool-bar-mode 0)

(add-to-list 'custom-theme-load-path "~/dot-emacs/vendor/solarized")
(load-theme 'solarized-light t)

(when (member "Inconsolata" (font-family-list))
	    (set-default-font "Inconsolata")
	    (set-face-attribute 'default nil :font "Inconsolata" :height 130 ))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'expand-region)
(global-set-key (kbd "C-.") 'er/expand-region)

(require 'yasnippet)
(setq yas/root-directory "~/dot-emacs/vendor/yasnippet/snippets")
(yas-global-mode 1)

(require 'ess-site)
(ess-toggle-underscore nil)

(load "htmlize.el")

(setq org-src-preserve-indentation t)

(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (ruby . t)
   (sh . t)
   (python .t)
   (scheme . t)
   ))

(setq org-src-fontify-natively t)
(setq org-src-window-setup (quote current-window))
(setq org-confirm-babel-evaluate nil)

(defun open-url-in-chrome (url)
  "Open URL in Google Chrome.  I use AppleScript to do several things:
  1. I tell Chrome to come to the front. If Chrome wasn't launched, this will also launch it.
  2. If Chrome has no windows open, I tell it to create one.
  3. If Chrome has a tab showing URL, I tell it to reload the tab, make that tab the active tab in its window, and bring its window to the front.
  4. If Chrome has no tab showing URL, I tell Chrome to make a new tab (in the front window) showing URL."
  (when (symbolp url)
    ; User passed a symbol instead of a string.  Use the symbol name.
    (setq url (symbol-name url)))
  (do-applescript (format "
tell application \"Google Chrome\"
        activate
        set theUrl to %S
        if character 1 of theUrl is \"/\" then
                set theUrl to \"file://\" & theUrl
        end if

        if (count every window) = 0 then
                make new window
                set URL of active tab of window 1 to theURL
        end if


        set found to false
        set theTabIndex to -1
        repeat with theWindow in every window
                set theTabIndex to 0
                repeat with theTab in every tab of theWindow
                        set theTabIndex to theTabIndex + 1
                        if theTab's URL = theUrl then
                                set found to true
                                exit
                        end if
                end repeat

                if found then
                        exit repeat
                end if
        end repeat

        if found then
                tell theTab to reload
                set theWindow's active tab index to theTabIndex
                set index of theWindow to 1
        else
               set theTab to make new tab at end of tabs of window 1
               set URL of theTab to theURL
        end if
end tell" url)))

(defun replace-org-export-as-html-and-open ()
  (defun org-export-as-html-and-open (arg)
    "long"
    (interactive "P")
    (org-export-as-html arg 'hidden)
    (message buffer-file-name)
    (open-url-in-chrome buffer-file-name)
    (when org-export-kill-product-buffer-when-displayed
      (kill-buffer (current-buffer))))
)

(add-hook 'org-mode-hook 'replace-org-export-as-html-and-open)

(require 'org-latex)
(setq org-export-latex-listings t)

(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
   pdflatex, or xelatex as appropriate, using latemxk."
  (let ((texcmd)))
    ;; default command: oldstyle latex via dvi
    (setq texcmd "latexmk -dvi -pdfps %f")
    ;; pdflatex -> .pdf
    (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
	(setq texcmd "latexmk -pdf %f"))
    ;; xelatex -> .pdf
    (if (string-match "LATEX_CMD: xelatex" (buffer-string))
	(setq texcmd "latexmk -pdflatex=xelatex -pdf %f"))
    (setq org-latex-to-pdf-process (list texcmd)))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)

;; Default packages included in every tex file, pdflatex or xelatex
(setq org-export-latex-packages-alist
      '(("" "graphicx" t)
	("" "longtable" nil)
	("" "float" nil)))


(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; Enable source-specials for Control-click forward/reverse search.
            (TeX-PDF-mode 1)
            (TeX-source-correlate-mode 1)
            (setq TeX-source-correlate-method 'synctex)

            (setq TeX-view-program-list
                  '(("Skim"
                     "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b"))
                  TeX-view-program-selection
                  '((output-pdf "Skim")))))

(defun my-auto-tex-parameters ()
  "Automatically select the tex packages to include."
  ;; default packages for ordinary latex or pdflatex export
  (setq org-export-latex-default-packages-alist
	'(("AUTO" "inputenc" t)
	  ("T1"   "fontenc"   t)
	  (""     "fixltx2e"  nil)
	  (""     "wrapfig"   nil)
	  (""     "soul"      t)
	  (""     "textcomp"  t)
	  (""     "marvosym"  t)
	  ("nointegrals" "wasysym"   t)
	  (""     "latexsym"  t)
	  (""     "amssymb"   t)
	  (""     "amsmath"   t)
	  (""     "hyperref"  nil)))
  
  ;; Packages to include when xelatex is used
  ;; (see https://github.com/kjhealy/latex-custom-kjh for the 
  ;; non-standard ones.)
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-export-latex-default-packages-alist
	    '(("" "fontspec" t)
	      ("" "xunicode" t)
	      ("" "url" t)
	      ("" "rotating" t)
;;	      ("" "memoir-article-styles" t)
;;	      ("american" "babel" t)
	      ("babel" "csquotes" t)
	      ("" "listings" nil)
	      (""     "amssymb"   t)
	      (""     "amsmath"   t)
;;	      ("" "listings-sweave-xelatex" nil)
	      ("svgnames" "xcolor" t)
	      ("" "soul" t)
	      ("xetex, colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels, bookmarksnumbered" "hyperref" nil)
	      )))
  
  ;; (if (string-match "LATEX_CMD: xelatex" (buffer-string))
  ;;     (setq org-export-latex-classes
  ;; 	    (cons '("article"
  ;; 		    "\\documentclass[letterpaper]{article}
  ;; \\usepackage[style=authoryear-comp-ajs, abbreviate=true]{biblatex}
  ;; \\bibliography{refs}"
  ;; 		    ("\\section{%s}" . "\\section*{%s}")
  ;; 		    ("\\subsection{%s}" . "\\subsection*{%s}")
  ;; 		    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;; 		    ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;; 		    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  ;; 		  org-export-latex-classes)))
)

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)

(require 'church)
(setq quack-fontify-style nil)
(setq quack-programs (quote ("o" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi" "ikarus" "ssh -t alonzo@nospoon.mit.edu ikarus")))

(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;; append-tuareg.el - Tuareg quick installation: Append this file to .emacs.
(setq auto-mode-alist (cons '("\\.ocaml\\w?" . tuareg-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(setq LaTeX-command "latex -synctex=1")

  (defun tex-git-friendly ()
    (interactive)
    (replace-string ". " ".\n " nil (region-beginning) (region-end))
  )
  
  (defun LaTeX-mode-keys ()
    "Modify keymaps used by `html-mode'."
    (local-set-key (kbd "M-n") 'tex-git-friendly)
    )
  
  (add-hook 'LaTeX-mode-hook 'LaTeX-mode-keys)

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
(add-hook 'js2-post-parse-callbacks
	  (lambda ()
	    (when (> (buffer-size) 0)
	      (let ((btext (replace-regexp-in-string
			    ": *true" " "
			    (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
		(mapc (apply-partially 'add-to-list 'js2-additional-externs)
		      (split-string
		       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
		       " *, *" t))
		))))

(setq js2-missing-semi-one-line-override t)

(setq js2-mode-hook
  '(lambda () (progn
    (setq indent-tabs-mode nil)
    (setq js2-basic-offset 2))))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

   (add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

     (autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
     (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
     (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
     (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(custom-set-variables '(markdown-command "~/Library/Haskell/bin/pandoc"))

(custom-set-variables '(markdown-css-path "//localhost/Users/longouyang/dot-emacs/markdown.css"))

     ;; (defun track-shell-directory/procfs ()
     ;;   (shell-dirtrack-mode 0)
     ;;   (add-hook 'comint-preoutput-filter-functions
     ;;             (lambda (str)
     ;;               (prog1 str
     ;;                 (when (string-match comint-prompt-regexp str)
     ;;                   (cd (file-symlink-p
     ;;                        (format "/proc/%s/cwd" (process-id
     ;;                                                (get-buffer-process
     ;;                                                 (current-buffer)))))))))
     ;;             nil t))
     
     ;; (add-hook 'shell-mode-hook 'track-shell-directory/procfs)
     

(require 'dired-details)
(dired-details-install)
(set-variable 'dired-details-hidden-string "- ")

   (setq dired-listing-switches "-alXGh --group-directories-first")

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)

(setq ace-jump-mode-scope 'frame)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(require 'magit)

(global-set-key (kbd "M-g") 'magit-status)

  (autoload 'paredit-mode "paredit"
    "Minor mode for pseudo-structurally editing Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
  (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
  (add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))

  ;; HT http://stackoverflow.com/a/913823/351392
  ;; HT http://stackoverflow.com/questions/5298907/emacs-php-mode-and-paredit#comment12995031_5324458
  
  (electric-pair-mode)

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun make-enter-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(add-hook 'js2-mode-hook 'make-enter-indent)
(add-hook 'python-mode-hook 'make-enter-indent)
(add-hook 'html-mode-hook 'make-enter-indent)
(add-hook 'org-mode-hook 'make-enter-indent)

;; emacs 23.1 and later joins the system clipboard with the
;; emacs killring. get rid of this.
;; taken from: http://emacswiki.org/emacs/CopyAndPaste#toc10
;; (setq interprogram-cut-function 'x-select-text)
;; (setq interprogram-paste-function x-cut-buffer-or-selection-value)
(setq interprogram-cut-function nil)
(setq interprogram-paste-function nil)
(defun paste-from-pasteboard ()
  (interactive)
  (and mark-active (filter-buffer-substring (region-beginning) (region-end) t))
  (insert (ns-get-pasteboard))
  )
(defun copy-to-pasteboard (p1 p2)
  (interactive "r*")
  (ns-set-pasteboard (buffer-substring p1 p2))
  (message "Copied selection to pasteboard")
  )
(defun cut-to-pasteboard (p1 p2) (interactive "r*") (ns-set-pasteboard (filter-buffer-substring p1 p2 t)) )
(global-set-key (kbd "s-v") 'paste-from-pasteboard)
(global-set-key (kbd "s-c") 'copy-to-pasteboard)
(global-set-key (kbd "s-x") 'cut-to-pasteboard)

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode
						     clojure-mode    scheme-mode
						     haskell-mode    ruby-mode
						     rspec-mode      python-mode
						     c-mode          c++-mode
						     objc-mode       latex-mode
						     plain-tex-mode  js2-mode
						     html-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))

(global-set-key (kbd "C-c C-=") 'comment-region)
(global-set-key (kbd "C-c C--") 'uncomment-region)

(defun prev-window ()
  (interactive)
  (other-window -1))

(defun longs-next-window ()
  (interactive)
  (other-window 1))

(global-set-key (kbd "C-x p") 'prev-window)
(global-set-key (kbd "s-}") 'longs-next-window)
(global-set-key (kbd "s-{") 'prev-window)

(global-set-key (kbd "<s-left>") 'windmove-left) 
(global-set-key (kbd "<s-right>") 'windmove-right) 
(global-set-key (kbd "<s-up>") 'windmove-up) 
(global-set-key (kbd "<s-down>") 'windmove-down)

   (global-set-key (kbd "s-0") 'delete-window)

(define-key global-map (kbd "M-i") 'back-to-indentation)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jk" 'ace-jump-mode)

(global-set-key (kbd "M-m") 'replace-string)

    (defun tdo ()
      (interactive)
    
    ;; HT http://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Buffer.html
      (switch-to-buffer-other-frame "todo"))
    

  (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit shadow :background "selectedMenuItemTextColor" :foreground "Black")))))

  ;; (setq magit-diff-options (list "--color" "--word-diff=color"))
  (setq magit-diff-options nil)
