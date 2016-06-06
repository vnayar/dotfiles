;; Allow us to import custom modules.
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

;; Dive through ~/.emacs.d/elpa packages to add subdirs.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  (defun ensure-package-installed (&rest packages)
    "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
    (mapcar
     (lambda (package)
       (if (package-installed-p package)
       nil
     (if (y-or-n-p (format "Package %s is missing. Install it? " package))
         (package-install package)
       package)))
     packages))

  ;; make sure to have downloaded archive description.
  ;; Or use package-archive-contents as suggested by Nicolas Dudebout
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  (ensure-package-installed
   'auctex
   'bash-completion
   'd-mode
   'ggtags
   'markdown-mode
   'plantuml-mode
   'polymode
   'top-mode
   'web-mode
   'w3m)
  )

;; Theme Settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'wheatgrass)

;; General Editing Settings
(setq inhibit-splash-screen t)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Prevent C-z from Freezing GUI
(defun my-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-z") 'my-suspend-frame)

;; Bury the current buffer.
(global-set-key (kbd "<f9>") 'bury-buffer)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t)
  (message "Buffer %s reverted" (buffer-name)))
(global-set-key (kbd "<f11>") 'revert-buffer-no-confirm)

(global-set-key (kbd "M-s h m") 'toggle-hl-line-when-idle)

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
(global-set-key (kbd "<f12>") 'toggle-current-window-dedication)

;; Utility function to copy the buffer's file name.
(defun copy-file-name ()
  (interactive)
  (let ((fileName (buffer-file-name)))
    (progn
      (kill-new fileName)
      (message (concat "Copy file name to kill-ring: " fileName)))))

;; Initialize windows the way I prefer them at start-up.
(defun init-windows ()
  "Split a window into quarters and display the four most recently used buffers."
  (interactive)
  (delete-other-windows)
  (let* ((window_tl (selected-window))
         (window_tr (split-window-horizontally))
         (window_bl (split-window-vertically))
         (window_br (progn (select-window window_tr) (split-window-vertically))))
    (select-window window_tl)
    (switch-to-buffer nil)
    (select-window window_bl)
    (switch-to-buffer nil)
    (select-window window_tr)
    (switch-to-buffer nil)
    (select-window window_br)
    (switch-to-buffer nil)
    (select-window window_tl)))
(global-set-key (kbd "C-c w") 'init-windows)

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
;(add-hook 'comint-mode-hook
;          (lambda nil (color-theme-buffer-local 'color-theme-arjen
;                                                (current-buffer))))

;; BUCK Support
(add-to-list 'auto-mode-alist '("BUCK\\'" . python-mode))

;; Markdown Mode Support
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; Polymode Support
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown-mode))


;; Compilation Mode Settings
(add-to-list 'auto-mode-alist '("/test\\.log\\'" . compilation-minor-mode))
;(add-hook 'compilation-mode-hook
;          (lambda nil
;            (color-theme-buffer-local 'color-theme-sitaramv-solaris
;                                      (current-buffer))))
;(add-hook 'compilation-filter-hook 'ansi-color-for-comint-mode-filter)

; Limit the max buffer size.
(setq comint-buffer-maximum-size 1024)
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

;; Git Settings
(setenv "GIT_PAGER" "cat")
(setenv "GIT_EDITOR" "emacsclient")

;; PlantUML Mode
(setq plantuml-jar-path "~/bin/plantuml.jar")
(require 'plantuml-mode)
(defun my-plantuml-mode-hook ()
  (column-number-mode 1)
  (show-paren-mode 1)
  (setq-local fill-column 100)
  (setq-local show-trailing-whitespace t)
  (setq plantuml-indent-offset 2))
(add-hook 'plantuml-mode-hook 'my-plantuml-mode-hook)
(defun plantuml-compile ()
  "Compile a PlantUML file into images."
  (interactive)
  (compile (concat plantuml-jar-path " " (buffer-file-name))))


;; AUCTeX Settings
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Project Management
(load "eproject")

;; General Programming Settings
(defun my-c-mode-common-hook ()
  ;(semantic-mode 1)
  (column-number-mode 1)
  (show-paren-mode 1)
  (setq-local fill-column 100)
  (setq-local show-trailing-whitespace t)
  (if (string-match "/google3\\(/\\|$\\)" (pwd))
      (progn
        (local-set-key (kbd "M-.") 'gtags-find-tag)
        (local-set-key (kbd "M-*") 'gtags-pop-tag)
        (local-set-key (kbd "M-g n") 'gtags-next-tag)
        (local-set-key (kbd "M-g p") 'gtags-previous-tag))
    (progn
      ;(gtags-update-after-save-hook)
      ;(local-set-key (kbd "M-*") 'pop-tag-mark)
      (local-set-key (kbd "M-*") 'ggtags-navigation-mode-abort)
      (ggtags-mode)))
  (local-set-key (kbd "C-c C-r") 'gud-cont)
  (local-set-key (kbd "<f5>") 'gud-cont)
  (local-set-key (kbd "C-c C-s") 'gud-step)
  (local-set-key (kbd "<f7>") 'gud-next)
  (local-set-key (kbd "C-c C-n") 'gud-next)
  (local-set-key (kbd "<f8>") 'gud-next)
  (local-set-key (kbd "C-c C-u") 'gud-up)
  (local-set-key (kbd "C-c C-d") 'gud-down)
  (local-set-key (kbd "C-c C-b") 'gud-break))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;; Java Debugging JSwat Settings
(remove-hook 'jdb-mode-hook 'google-jdb-fix-comint-prompt)
(defun jswat ()
  (interactive)
  (setq google-jdb-jswat-command "~/scripts/jswat-launcher -jdb")
  (google-jswat))


;; Java Programming Language Settings
;(require 'jdee)
(require 'google-java-format)
;(require 'malabar-mode)
;(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
(defun my-java-mode-hook ()
  (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
  (setq-local c-basic-offset 2)
  (setq-local fill-column 100)
  (column-marker-1 100)
  ;(local-set-key (kbd "C-c i") 'google-imports-add-import-from-tag)
  ;(local-set-key (kbd "C-c i") 'google-imports-jade)
  ;(local-set-key (kbd "C-c f") #'google-java-format-region)
  ;(google-set-java-style)
  (add-hook 'before-save-hook 'google-imports-organize-imports nil t))
(add-hook 'java-mode-hook 'my-java-mode-hook)


;; JavaScript Programming Language Settings
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(defun my-js2-mode-hook ()
  (if (string-match "/google3\\(/\\|$\\)" (pwd))
      (progn
        (local-set-key (kbd "M-.") 'gtags-find-tag)
        (local-set-key (kbd "M-*") 'gtags-pop-tag)
        (local-set-key (kbd "M-g n") 'gtags-next-tag)
        (local-set-key (kbd "M-g p") 'gtags-previous-tag))
    (progn
      ;(gtags-update-after-save-hook)
      (local-set-key (kbd "M-*") 'pop-tag-mark)
      (ggtags-mode)))
  (setq-local fill-column 80))
(add-hook 'js2-mode-hook 'my-js2-mode-hook)


;; JSON Pretty Print Function
(defun json-pretty-print (start end)
  "Pretty-print the selected region of text."
  (interactive "r")
  (shell-command-on-region start end "python -m json.tool" :replace=true)
)

;; EBNF Settings
(autoload 'ebnf-mode "ebnf-mode" "Major mode for editing EBNF grammars." t)
(add-to-list 'auto-mode-alist '("\\.ebnf\\'" . ebnf-mode))


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
 '(ansi-color-names-vector ["dim gray" "red" "green" "yellow" "steel blue" "magenta" "cyan" "white"])
 '(blink-cursor-mode t)
 '(bookmark-save-flag 1)
 '(column-number-mode t)
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "aecea99f23d116cd33dabe34b9df808816c374328c064fcf12d15cecc3735237" default)))
 '(delete-trailing-lines nil)
 '(ecb-options-version "2.40")
 '(even-window-heights t)
 '(fill-column 100)
 '(gcomplete-jump-to-def-binding [ignore])
 '(jdee-server-dir "~/.emacs.d/jdee-server/")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-init-hook (quote (my-c-mode-common-hook)))
 '(org-todo-keywords (quote ((sequence "TODO" "WAIT" "DONE"))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 ;'(w3m-enable-google-feeling-lucky nil)
 ;'(w3m-home-page "about:blank")
 ;'(w3m-session-crash-recovery nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-html-tag-face ((t (:foreground "steel blue")))))
