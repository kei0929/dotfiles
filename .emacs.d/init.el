(require 'cl)
(setq inhibit-startup-screen t)

;; function (add load-path)
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; add directories to load-path
(add-to-load-path "elisp" "conf" "public_repos")

;; init-loader.el
;(require 'init-loader)
;(init-loader-load "~/.emacs.d/conf")


;;
;; keybind
;;
(global-set-key (kbd "C-m") 'newline-and-indent)
(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-t") 'other-window)


;;
;; locale
;;
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))


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
;; theme
;;
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-dark-laptop))

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
;; auto-install
;;
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;;
;; redo+
;; http://www.emacswiki.org/emacs/download/redo+.el
;;
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-.") 'redo)
  (global-set-key (kbd "C-\\") 'undo))

