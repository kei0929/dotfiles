;;
;; theme
;;
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-dark-laptop))

;;
;; package.el
;;
(when (require 'package nil t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize))

;;
;; auto-complete
;;
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;;
;; auto-install
;;
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;;
;; menu/tool bar
;;
(menu-bar-mode 0)
(tool-bar-mode 0)

;;
;; keybind
;;
(global-set-key (kbd "C-m") 'newline-and-indent)
(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;;
;; locale
;;
;; (set-language-environment "Japanese")
;; (prefer-coding-system 'utf-8)
;; (when (eq window-system 'w32)
;;   (set-file-name-coding-system 'cp932)
;;   (setq locale-coding-system 'cp932))

;;
;; display
;;
;; frame
(column-number-mode t)
(size-indication-mode t)
(display-time-mode t)

;; title bar
(setq frame-title-format "%f")

;; display line number
(global-linum-mode t)

;;
;; indent
;;
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;
;; highlight
;;
(setq show-paren-delay 0)
(show-paren-mode t)

;;
;; backup and autosave directory
;;
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;;
;; redo+
;; http://www.emacswiki.org/emacs/download/redo+.el
;;
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-.") 'redo)
  (global-set-key (kbd "C-\\") 'undo))

;;
;; anything
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3             ;default 0.5
   anything-input-idle-delay 0.2       ;default 0.1
   anything-candidate-number-limit 100 ;default 50
   anything-quick-update t
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything0migemo nil t))

  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install)))

;;
;; wgrep
;;
(require 'wgrep nil t)

;;
;; zenkaku space
;;
(when (and (>= emacs-major-version 23)
           (require 'whitespace nil t))
  (setq whitespace-style
        '(face
          tabs spaces space-mark tab-mark))
  (let ((dark (eq 'dark (frame-parameter nil 'background-mode))))
    (set-face-attribute 'whitespace-space nil
                        :foreground (if dark "pink4" "azure3")
                        :background 'unspecified)
    (set-face-attribute 'whitespace-tab nil
                        :foreground (if dark "gray20" "gray80")
                        :background 'unspecified
                        :strike-through t)
    (set-face-attribute 'whitespace-newline nil
                        :foreground (if dark "darkcyan" "darkseagreen")))
  (setq whitespace-space-regexp "\\(　+\\)")
  (setq whitespace-display-mappings
        '((space-mark   ?\xA0  [?\xA4]  [?_])  ; hard space - currency
          (space-mark   ?\x8A0 [?\x8A4] [?_])  ; hard space - currency
          (space-mark   ?\x920 [?\x924] [?_])  ; hard space - currency
          (space-mark   ?\xE20 [?\xE24] [?_])  ; hard space - currency
          (space-mark   ?\xF20 [?\xF24] [?_])  ; hard space - currency
          (space-mark   ?　    [?□]    [?＿]) ; full-width space - square
          ))
  (setq whitespace-global-modes '(not dired-mode tar-mode))
  (global-whitespace-mode 1))

;;
;; cua-mode
;;
(cua-mode t)
(setq cua-enable-cua-keys nil)

;;
;; ruby-mode
;;
(require 'ruby-electric nil t)
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)

;; Flymake for Ruby
;; (defun flymake-ruby-init ()
;;   (list "ruby" (list "-c" (flymake-init-create-temp-buffer-copy
;;                            'flymake-create-temp-inplace))))

;; (add-to-list 'flymake-allowed-file-name-masks
;;              '("\\.rb\\'" flymake-ruby-init))

;; (add-to-list 'flymake-err-line-patterns
;;              '("\\(.*\\):(\\([0-9]+\\)): \\(.*\\)" 1 2 nil 3))

