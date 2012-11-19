;; path and env issues: make sure we have ikarus available
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:~/bin:/usr/texbin"))

;; (setq exec-path (append exec-path '("/usr/local/bin" "~/bin")))
;; (setenv "IKARUS_LIBRARY_PATH" 
;;         (concat "/path/to/mit-church:"
;;                 (getenv "IKARUS_LIBRARY_PATH")))

(add-to-list 'load-path "~/dot-emacs")
(add-to-list 'load-path "~/dot-emacs/vendor/ess/lisp")
;;(add-to-list 'load-path "~/dot-emacs/vendor/org-mode/lisp")

(load "~/dot-emacs/ui.el")
(load "~/dot-emacs/emacs-backend.el")
(load "~/dot-emacs/my-org.el")
(load "~/dot-emacs/ess.el")

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))

;; haskell-mode (http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs)
(load "~/dot-emacs/vendor/haskell-mode/haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; church (loads quack)
(require 'church)

(add-to-list 'load-path "~/dot-emacs/vendor/magit")
(require 'magit)

;;; append-tuareg.el - Tuareg quick installation: Append this file to .emacs.
(setq auto-mode-alist (cons '("\\.ocaml\\w?" . tuareg-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(quack-fontify-style nil)
 '(quack-programs (quote ("o" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi" "ikarus" "ssh -t alonzo@nospoon.mit.edu ikarus")))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
