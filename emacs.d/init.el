;;; init.el --- Summary
;;; Commentary:
(require 'cask "~/.cask/cask.el")

;;; Code:
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(add-to-list 'load-path "~/.emacs.d/custom")
;(load "00common-setup.el")
(load "01-smartparens.el")

;(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))

(setq make-backup-files nil)

(setq debug-on-error t)

(load-theme 'monokai t)

;; Font
;; https://www.mozilla.org/en-US/styleguide/products/firefox-os/typeface/#download-primary
;(set-frame-font "Fira Mono OT-14" nil t)

(if (eq system-type 'gnu/linux)
  (set-frame-font "DroidSans Mono-13" nil t)
  (set-frame-font "DroidSans Mono-13" nil t))

(menu-bar-mode 1)

(scroll-bar-mode -1)

;; Show keystrokes
(setq echo-keystrokes 0.02)

;; Show cursor line
(global-hl-line-mode 1)

(global-linum-mode t) ; display line numbers in margin.

;(require 'linum-relative) ; relative line numbers

(cua-mode 1) ; Standard Copy/Paste Keys

(electric-pair-mode 1) ; Closing brackets

(show-paren-mode 1) ; turn on paren match highlighting

(setq show-paren-style 'expression) ; highlight entire bracket expression

(desktop-save-mode 1) ; save/restore opened files

(toggle-truncate-lines 1) ; no line wrapping

(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

(fset 'yes-or-no-p 'y-or-n-p) ; y or n instead or yes/no

(add-hook 'after-init-hook 'global-company-mode)

(require 'undo-tree)
(global-undo-tree-mode 1)

(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo) ; 【Ctrl+z】
(global-set-key (kbd "C-S-z") 'redo) ; 【Ctrl+Shift+z】;  Mac style

;; CIDER

(add-hook 'cider-mode-hook #'eldoc-mode)
;;(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "<em>nrepl</em>")

(require 'yasnippet)
(yas-global-mode 1)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert keybinding setup here
                               (cljr-add-keybindings-with-prefix "C-c C-r")
                               ))

;; Replace return key with newline-and-indent when in cider mode.
;(add-hook ‘cider-mode-hook ‘(lambda () (local-set-key (kbd “RET”) ‘newline-and-indent)))

;; General Auto-Complete
;;(require 'auto-complete-config)
;; (global-auto-complete-mode t)
;; (setq ac-delay 0.0)
;; (setq ac-quick-help-delay 0.5)
;; (ac-config-default)

;; ac-nrepl (Auto-complete for the nREPL)
;; (require 'ac-cider)
;; (add-hook 'cider-mode-hook 'ac-cider-setup)
;; (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;; (add-to-list 'ac-modes 'cider-mode)
;; (add-to-list 'ac-modes 'cider-repl-mode)

;; Poping-up contextual documentation
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-popup-doc))

;;(add-hook 'clojure-mode-hook 'paredit-mode)

;; require or autoload smartparens
;(add-hook 'clojure-mode-hook 'smartparens-strict-mode)


(setq nrepl-hide-special-buffers t)
(add-hook 'clojure-mode-hook 'cider-mode)

(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)

(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; rainbow delimiters
;(global-rainbow-delimiters-mode)

(global-set-key [f8] 'other-frame)
;(global-set-key [f7] 'paredit-mode)
(global-set-key [f9] 'cider-jack-in)
(global-set-key [f11] 'sr-speedbar-toggle)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 2) ;; keyboard scroll one line at a time

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(defun my-coding-hook ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (if window-system (hl-line-mode t))
  (idle-highlight-mode t))

(add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
(add-hook 'clojure-mode-hook 'my-coding-hook)
(add-hook 'js2-mode-hook 'my-coding-hook)

;; Recent files

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Projectile
(projectile-global-mode)

(require 'helm-config)
(require 'helm-projectile)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Web Mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq-default ac-sources '(ac-source-filename))

(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;; FlyCheck
;(add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-mode 1)


;; J2 Mode
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)
;(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
;(add-hook 'js2-mode-hook 'ac-js2-mode)

;(setq js2-highlight-level 3)

;(add-hook 'js-mode-hook (lambda () (tern-mode t)))
;(eval-after-load 'tern
;   '(progn
;      (require 'tern-auto-complete)
;      (tern-ac-setup)))

(put 'dired-find-alternate-file 'disabled nil)

;;; Markdown Mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(require 'sr-speedbar)
;(global-set-key (kbd "C-c b") 'sr-speedbar-toggle)

(eval-after-load 'sr-speedbar
  '(progn
     (setq speedbar-hide-button-brackets-flag t
           speedbar-show-unknown-files t
           speedbar-smart-directory-expand-flag t
           speedbar-directory-button-trim-method 'trim
           speedbar-use-images nil
           speedbar-indentation-width 2
           speedbar-use-imenu-flag t
           speedbar-file-unshown-regexp "flycheck-.*"
           sr-speedbar-width 30
           sr-speedbar-width-x 30
           sr-speedbar-auto-refresh nil
           sr-speedbar-skip-other-window-p t
           sr-speedbar-right-side nil)
     ))

(defvar graphene-font-height
  (face-attribute 'default :height)
  "Default font height.")
(defvar graphene-small-font-height
  (floor (* .847 graphene-font-height))
  "Relative size for 'small' fonts.")

;;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(js-indent-level 2)
 '(markdown-command "/usr/bin/pandoc")
 '(speedbar-obj-do-check nil)
 '(speedbar-vc-do-check nil)
 '(sr-speedbar-default-width 30)
 '(sr-speedbar-max-width 30)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:stipple nil :background "dim gray" :foreground "#A1EFE4" :inverse-video nil :weight normal))))
`(speedbar-directory-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit variable-pitch
                    :weight bold
                    :height ,graphene-small-font-height))))
 `(speedbar-file-face
   ((t (:foreground unspecified
                    :inherit speedbar-directory-face
                    :weight normal))))
`(speedbar-selected-face
   ((t (:background unspecified
                    :foreground unspecified
                    :height unspecified
                    :inherit (speedbar-file-face font-lock-function-name-face)))))
 `(speedbar-highlight-face
   ((t (:background unspecified
                    :inherit region))))
 `(speedbar-button-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit file-name-shadow))))
 `(speedbar-tag-face
   ((t (:background unspecified
                    :foreground unspecified
                    :height unspecified
                    :inherit speedbar-file-face))))
 `(speedbar-separator-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inverse-video nil
                    :inherit speedbar-directory-face
                    :overline nil
                    :weight bold))))
))

;; Web Mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;; FlyCheck
;(add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-mode 1)

;; Duplicate line

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Recalc window when speedbar opens

(when window-system
  (defadvice sr-speedbar-open (after sr-speedbar-open-resize-frame activate)
    (set-frame-width (selected-frame)
                     (+ (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-open 'after 'sr-speedbar-open-resize-frame)

  (defadvice sr-speedbar-close (after sr-speedbar-close-resize-frame activate)
    ;(sr-speedbar-recalculate-width)
    (set-frame-width (selected-frame)
                     (- (frame-width) sr-speedbar-width)))
  (ad-enable-advice 'sr-speedbar-close 'after 'sr-speedbar-close-resize-frame))



(provide 'init)

;;; init.el ends here
