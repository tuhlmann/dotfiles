(require 'cask "~/.cask/cask.el")

(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.emacs.d/custom")
;(load "00common-setup.el")
;(load "01clojure.el")

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

;(linum-mode 1) ; display line numbers in margin.

(cua-mode 1) ; Standard Copy/Paste Keys

(electric-pair-mode 1) ; Closing brackets

(show-paren-mode 1) ; turn on paren match highlighting

(setq show-paren-style 'expression) ; highlight entire bracket expression

(desktop-save-mode 1) ; save/restore opened files

(toggle-truncate-lines 1) ; no line wrapping

(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

(fset 'yes-or-no-p 'y-or-n-p) ; y or n instead or yes/no

;; (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;; (setq tabbar-ruler-global-ruler nil) ; if you want a global ruler
;; (setq tabbar-ruler-popup-menu nil) ; If you want a popup menu.
;; (setq tabbar-ruler-popup-toolbar nil) ; If you want a popup toolbar
;; (setq tabbar-ruler-popup-scrollbar nil) ; If you want to only show the
;; (require 'cl)                                        ; scroll bar when your mouse is moving.
;; (require 'tabbar-ruler)

(add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
;;(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "<em>nrepl</em>")

;; Replace return key with newline-and-indent when in cider mode.
;(add-hook ‘cider-mode-hook ‘(lambda () (local-set-key (kbd “RET”) ‘newline-and-indent)))

;; General Auto-Complete
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)

;; ac-nrepl (Auto-complete for the nREPL)
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(add-to-list 'ac-modes 'cider-mode)
(add-to-list 'ac-modes 'cider-repl-mode)

;; Poping-up contextual documentation
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-popup-doc))

(add-hook 'clojure-mode-hook 'paredit-mode)

;; require or autoload smartparens
;(add-hook 'clojure-mode-hook 'smartparens-strict-mode)

;; Enter cider mode when entering the clojure major mode
(add-hook 'clojure-mode-hook 'cider-mode)

(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; Show parenthesis mode
(show-paren-mode 1)

;; rainbow delimiters
;(global-rainbow-delimiters-mode)

(global-set-key [f8] 'other-frame)
(global-set-key [f7] 'paredit-mode)
(global-set-key [f9] 'cider-jack-in)
(global-set-key [f11] 'speedbar)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

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

;; SCALA

(require 'ensime)
;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you’re not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; If necessary, make sure "sbt" and "scala" are in the PATH environment
(setenv "PATH" (concat "/ewu/sbt:" (getenv "PATH")))
;; (setenv "PATH" (concat "/path/to/scala/bin:" (getenv "PATH")))
;; You can also customize `ensime-inf-get-project-root' and `ensime-inf-get-repl-cmd-line'
;; Duplicate line

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "C-c C-d") 'duplicate-line)

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

;;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(js-indent-level 2)
 '(sr-speedbar-default-width 30)
 '(markdown-command "/usr/bin/pandoc"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:stipple nil :background "dim gray" :foreground "#A1EFE4" :inverse-video nil :weight normal)))))


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

;; SCALA

(require 'ensime)
;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you’re not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Duplicate line

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "C-c C-d") 'duplicate-line)

(provide 'init)

;;; init.el ends here
