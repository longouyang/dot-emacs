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

;; open URL in chrome
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

;; cribbed a little bit from http://kieranhealy.org/blog/archives/2011/01/21/exporting-org-mode-to-pdf-via-xelatex/
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
