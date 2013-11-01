;; Allow us to import custom modules.
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; General Editing Settings
(setq inhibit-splash-screen t)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

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
; Limit the max buffer size.
(setq comint-buffer-maximum-size 1024)
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

;; Git Settings
(setenv "GIT_PAGER" "")
(setenv "GIT_EDITOR" "emacs")

;; General Programming Settings
(defun my-c-mode-common-hook ()
  (column-number-mode 1)
  (show-paren-mode 1)
  (setq show-trailing-whitespace t)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; D Programming Language Settings
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

;; Show the directory in the mode-line.
(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (add-to-list 'mode-line-buffer-identification
               '(:eval (substring default-directory
                                  (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))
(add-hook 'find-file-hook 'add-mode-line-dirtrack)

;; Easy file lookup.
(ffap-bindings)

;; Window Navigation Keybindings
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'other-frame)
; Buffer selection in the same window
(global-set-key "\C-x\C-b" 'buffer-menu)

(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))
