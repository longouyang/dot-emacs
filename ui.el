;; -------- look ----------

;; hide splash screen
(setq inhibit-splash-screen t)

;; use solarized theme
(add-to-list 'custom-theme-load-path "~/dot-emacs/vendor/solarized")
(load-theme 'solarized-light t)

;; switch font to inconsolata
(when (member "Inconsolata" (font-family-list))
	    (set-default-font "Inconsolata")
	    (set-face-attribute 'default nil :font "Inconsolata" :height 130 ))

;; add colors to shell mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; -------- keyboard ----------

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

;; add shortcut to see kill-ring
(global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))

;; comments
(global-set-key (kbd "C-c C-=") 'comment-region)
(global-set-key (kbd "C-c C--") 'uncomment-region)

;; clear shell window
(defun clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(global-set-key (kbd "M-0") 'clear-shell)


;; just as C-x o goes to the next window, make
;; C-x p go to the previous window

(defun prev-window ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x p") 'prev-window)


