;; Allow us to import custom modules.
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "/google/src/files/head/depot/eng/elisp/")
(add-to-list 'load-path "/home/build/public/eng/elisp")

;; Dive through ~/.emacs.d/elpa packages to add subdirs.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; Theme Settings
(require 'color-theme)
(require 'color-theme-buffer-local)
(color-theme-initialize)

;; GNU Global Tags Settings
(require 'ggtags)
(defun gtags-root-dir ()
    "Returns GTAGS root directory or nil if doesn't exist."
    (with-temp-buffer
      (if (zerop (call-process "global" nil t nil "-pr"))
          (buffer-substring (point-min) (1- (point-max)))
        nil)))
(defun gtags-update ()
    "Make GTAGS incremental update"
    (call-process "global" nil nil nil "-u"))
(defun gtags-update-hook ()
    (when (gtags-root-dir)
      (gtags-update)))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (add-hook 'after-save-hook #'gtags-update-hook)
              (ggtags-mode 1))))

;; General Editing Settings
(setq inhibit-splash-screen t)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Bury the current buffer.
(global-set-key [f9] 'bury-buffer)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t)
  (message "Buffer %s reverted" (buffer-name)))
(global-set-key [f11] 'revert-buffer-no-confirm)

;; Allow function keys to set dedicated windows for a buffer.
;; http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))
(global-set-key [f12] 'toggle-current-window-dedication)

;; Keybindings for navigating windows.
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;; Shell Settings
; Make the prompt read-only, but let C-w still delete it.
(set 'comint-prompt-read-only t)
(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map "\C-w" 'comint-kill-region)
            (define-key comint-mode-map [C-S-backspace]
              'comint-kill-whole-line)))
; Enable colour.
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-mode-hook (lambda nil (color-theme-buffer-local 'color-theme-jedit-grey
                                                                  (current-buffer))))
(add-hook 'compilation-mode-hook
          (lambda nil
            (color-theme-buffer-local 'color-theme-aalto-light (current-buffer))))
(add-hook 'compilation-filter-hook 'ansi-color-for-comint-mode-filter)

; Limit the max buffer size.
(setq comint-buffer-maximum-size 1024)
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

;; Git Settings
(setenv "GIT_PAGER" "")
(setenv "GIT_EDITOR" "emacs")

;; Project Management
(load "eproject")

;; General Programming Settings
(defun my-c-mode-common-hook ()
  ;(semantic-mode 1)
  (column-number-mode 1)
  (show-paren-mode 1)
  (setq show-trailing-whitespace t)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;; Java Programming Language Settings
;(require 'malabar-mode)
;(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))
(add-hook 'java-mode-hook
          (lambda ()
            (column-marker-1 100)))
(add-hook 'java-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'google-imports-organize-imports nil t)))

;; JavaScript Programming Language Settings
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; D Programming Language Settings
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

;; Web Mode - http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.soy\\'" . web-mode))
(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(set-face-attribute 'web-mode-html-tag-face nil :foreground "Blue")

;; Show the directory in the mode-line.
(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (add-to-list 'mode-line-buffer-identification
               '(:eval (substring default-directory
                                  (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))
(add-hook 'find-file-hook 'add-mode-line-dirtrack)

;; Easy file lookup.
(ffap-bindings)

;; W3M Web-Browsing Settings
;(require 'w3m-load)

;; Mac Keyboard Compatibility Fixes
(setq x-alt-keysym 'meta)  ;; Interpret "Alt" as "Meta"

;; Window Navigation Keybindings
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'other-frame)
; Buffer selection in the same window
(global-set-key "\C-x\C-b" 'buffer-menu)

;; TODO - Make less heavyweight and figure out good usage pattern.
;; ECB - Emacs Code Browser
;(setq ecb-examples-bufferinfo-buffer-name nil)
;(require 'ecb-autoloads)

;; TODO - Determine why tag lookup fails so often.
;; OpenGrok Tag Searching
;(require 'opengrok)

;; Org-mode settings
(setq org-src-fontify-natively t)


;; Personal preferences and customizations.
(setq-default indicate-empty-lines t)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-save-flag 1)
 '(column-number-mode t)
 '(delete-trailing-lines nil)
 '(ecb-options-version "2.40")
 '(even-window-heights t)
 '(fill-column 100)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-init-hook (quote (my-c-mode-common-hook)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 ;'(w3m-enable-google-feeling-lucky nil)
 ;'(w3m-home-page "about:blank")
 ;'(w3m-session-crash-recovery nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Ubuntu Mono")))))
