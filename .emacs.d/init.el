;; Modify 'message to include a timestamp.
; (defun sh/current-time-microseconds ()
;   "Return the current time formatted to include microseconds."
;   (let* ((nowtime (current-time))
;          (now-ms (nth 2 nowtime)))
;     (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))
; 
; (defun sh/ad-timestamp-message (FORMAT-STRING &rest args)
;   "Advice to run before `message' that prepends a timestamp to each message.
; 
; Activate this advice with:
; (advice-add 'message :before 'sh/ad-timestamp-message)"
;   (unless (string-equal FORMAT-STRING "%s%s")
;     (let ((deactivate-mark nil)
;           (inhibit-read-only t))
;       (with-current-buffer "*Messages*"
;         (goto-char (point-max))
;         (if (not (bolp))
;           (newline))
;         (insert (sh/current-time-microseconds) " ")))))
; 
; (advice-add 'message :before 'sh/ad-timestamp-message)
;; END TIMING

;; Allow us to import custom modules.
; Workaround for a bug in Emacs 26.1
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'load-path "~/.emacs.d/lisp/")
;(add-to-list 'load-path "~/.emacs.d/org-9.0.5/lisp")
;(add-to-list 'load-path "~/.emacs.d/org-9.0.5/contrib/lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

;; Dive through ~/.emacs.d/elpa packages to add subdirs.
(when (>= emacs-major-version 24)
  (require 'package)
  ;; Org ELPA is closing before Org 9.6.
  ;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

;; A mode for code completion, auto-complete, and other features.
(require 'eglot)

(message "Checkpoint 1")
; Sets the :ensure t property for all use-package calls.
(setq use-package-always-ensure t)

;; Debugging Emacs startup time.
;(use-package esup
;  :ensure t
;  ;; To use MELPA Stable use ":pin melpa-stable",
;  :pin melpa)

;; JavaScript Programming Language Settings
;(use-package ac-js2 :defer t)
;(use-package js2-mode
;  :mode "\\.js\\'"
;  :after (ac-js2)
;  :bind (("M-*" . pop-tag-mark))
;  :config
(defun my-js2-mode-hook ()
  (company-mode)
  (eglot-ensure)
  (local-set-key (kbd "C-c .") 'xref-find-definitions)
  (setq-local fill-column 100))
(add-hook 'js2-mode-hook 'my-js2-mode-hook)
; See https://github.com/typescript-language-server/typescript-language-server
(add-to-list 'eglot-server-programs `(js2-mode . ("/usr/bin/typescript-language-server" "--stdio")))

; Quickly toggle whether automatic word-wrapping is performed while typing.
(global-set-key (kbd "C-c f") 'auto-fill-mode)

(use-package auto-complete)
(use-package tex :ensure auctex)
(use-package bash-completion)
(use-package column-marker)
(use-package company)

;; LDAP Settings
(use-package ldap
  :mode "\\.ldif")

;; D Programming Language Settings
(use-package company-dcd)
(use-package d-mode
  :mode "\\.d[i]?\\'"
  :config
  (defun my-d-mode-hook ()
    "My settings for d-mode."
    (setq-local show-trailing-whitespace t)
    (setq-local fill-column 100)
    (column-marker-1 100)
    (c-set-offset 'func-decl-cont 'nil)
    (c-set-offset 'arglist-intro '++)
    (c-set-offset 'arglist-cont 'nil)
    (c-set-offset 'arglist-cont-nonempty '++)
    (c-set-offset 'case-label '+)
    (c-set-offset 'statement-cont '++)
    (setq tab-width 2)
    (company-mode)
    ;(company-dcd-mode)
    ;(global-set-key (kbd "C-c TAB") 'company-complete))
    ;(local-set-key (kbd "C-c .") 'lsp-find-definition)
    (local-set-key (kbd "C-c .") 'xref-find-definitions)
    (local-set-key (kbd "C-c i") 'imenu)
    ;(local-set-key (kbd "C-c r") 'lsp-find-references)
    (local-set-key (kbd "C-c /") 'company-complete)
    (yas-minor-mode)
    )
  (add-hook 'd-mode-hook 'my-d-mode-hook))

(use-package sdlang-mode
  :mode "\\.sdl\\'"
  :config
  (defun my-sdlang-mode-hook ()
    "My settings for sdlang-mode."
    (setq-local show-trailing-whitespace t)
    (setq-local fill-column 100)
    (column-marker-1 100)
    (setq tab-width 2)
    (company-mode)
    )
  (add-hook 'sdlang-mode-hook 'my-sdlang-mode-hook))

;(with-eval-after-load 'lsp-mode
;  (lsp-register-client
;   (make-lsp-client
;    :new-connection (lsp-stdio-connection '("/usr/local/bin/serve-d"))
;    :major-modes '(d-mode)
;    :server-id 'serve-d)))

(add-hook 'd-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs `(d-mode . ("/usr/local/bin/serve-d")))

(defun org-babel-execute:d 'org-babel-execute:D)
; Vibe.d uses Diet templates, which are compatible with pug-mode.
(use-package pug-mode
  :mode "\\.dt\\'")

(use-package web-mode
  :mode "\\.ftl\\'")

(message "Checkpoint 2")
(use-package ess :mode "\\.R\\'")
(use-package ggtags)
(use-package google-c-style)
(use-package leuven-theme)
(use-package markdown-mode :mode "\\.md\\'")
(use-package neotree)
(require 'ob-plantuml)

;; circe IRC Client configuration
(autoload 'enable-circe-notifications "circe-notifications" nil t)
;(eval-after-load "circe-notifications"
;  '(setq circe-notifications-watch-strings
;      '("people" "you" "like" "to" "hear" "from")))
;(add-hook 'circe-server-connected-hook 'enable-circe-notifications)

(message "Checkpoint 3")
;; PlantUML Mode
(use-package plantuml-mode
  :mode "\\.puml\\'"
  :init
  (setq plantuml-jar-path "~/bin/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  :config
  (defun my-plantuml-mode-hook ()
    (column-number-mode 1)
    (show-paren-mode 1)
    (setq-local fill-column 100)
    (setq-local show-trailing-whitespace t))
  (defun plantuml-compile ()
    "Compile a PlantUML file into images."
    (interactive)
    (compile (concat "java -jar " plantuml-jar-path " " (buffer-file-name))))
  (add-hook 'plantuml-mode-hook 'my-plantuml-mode-hook))

(message "Checkpoint 4")
(use-package polymode :mode "\\.R\\'")
(use-package projectile)
;(use-package top-mode)
;(use-package web-mode
;  :mode "\\.\\(js\\|html?\\)\\'"
;  :init
;  (setq create-lockfiles nil)
;  (setq web-mode-style-padding 2)
;  (setq web-mode-script-padding 2)
;  (setq web-mode-markup-indent-offset 2)
;  (setq web-mode-css-indent-offset 2)
;  (setq web-mode-code-indent-offset 2))
(use-package rjsx-mode
  :mode "\\.js\\'"
  :init
  (setq create-lockfiles nil))
(defun my-rjsx-mode-hook ()
  (flymake-eslint-enable)
  (local-set-key (kbd "C-c /") 'company-complete)
  (yas-minor-mode)
  )
(add-hook 'rjsx-mode-hook 'my-rjsx-mode-hook)
;(add-hook 'rjsx-mode-hook 'eglot-ensure)
;(add-hook 'rjsx-mode-hook 'lsp)

;(use-package w3m)

(desktop-save-mode 't)

;; Various very small generic modes.
(require 'generic-x)
(add-to-list 'auto-mode-alist '("credentials\\'" . ini-generic-mode))

;; Theme Settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; General Editing Settings
(message "Checkpoint 5")
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

(global-set-key (kbd "C-c L") 'org-store-link)

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

(message "Checkpoint 6")
;; auto-complete settings
(require 'auto-complete)
(define-key ac-completing-map [return] nil)
(define-key ac-completing-map "\r" nil)
;(ac-config-default)

;; Text Mode Settings
(defun my-text-mode-hook ()
  (setq-local show-trailing-whitespace t))
(add-hook 'text-mode-hook 'my-text-mode-hook)

;; Shell Settings
; Make the prompt read-only, but let C-w still delete it.
(set 'comint-prompt-read-only t)
(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map "\C-w" 'comint-kill-region)
            (define-key comint-mode-map [C-S-backspace]
              'comint-kill-whole-line)))
(defun my-sh-mode-hook ()
  "Commands to run for shell-mode."
  ;(auto-complete-mode)
  (setq sh-basic-offset 2)
  ;(setq ac-delay 0.5)
  ;(setq ac-sources
  ;      '(ac-source-files-in-current-dir
  ;        ac-source-filename))
  )
(add-hook 'sh-mode-hook 'my-sh-mode-hook)

(message "Checkpoint 7")
;; Mode settings for editing shell scripts.
(defun my-sh-mode-hook ()
  "Customizations for editing shell script files."
  (setq-local fill-column 100)
  (setq-local show-trailing-whitespace t)
  (column-marker-1 100)
  (setq tab-width 2)
  )
(add-hook 'sh-mode-hook 'my-sh-mode-hook)

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
;(require 'poly-R)
;(require 'poly-markdown)
;(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown-mode))


(message "Checkpoint 8")
;; Parsing Expression Grammar files.
(require 'peg-mode)
(defun my-peg-mode-hook ()
  (modify-syntax-entry ?\[  "<"   peg-mode-syntax-table)
  (modify-syntax-entry ?\]  ">"   peg-mode-syntax-table))
(add-to-list 'auto-mode-alist '("\\.peg\\'" . peg-mode))

(message "Checkpoint 9")
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

;; AUCTeX Settings
; (load "auctex.el" nil t t)
; (setq TeX-auto-save t)
; (setq TeX-parse-self t)
; (setq-default TeX-master nil)

;; Project Management
;(load "eproject")

;; Project management, prefix key is "C-c p"
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;(require 'neotree)
(global-set-key [f8] 'treemacs)

;; General Programming Settings
(require 'google-c-style)
(defun my-c-mode-common-hook ()
  ;(semantic-mode 1)
  ;(google-set-c-style)
  (google-make-newline-indent)
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
  ;(local-set-key (kbd "C-c C-r") 'gud-cont)
  ;(local-set-key (kbd "<f5>") 'gud-cont)
  ;(local-set-key (kbd "C-c C-s") 'gud-step)
  ;(local-set-key (kbd "<f7>") 'gud-step)
  ;(local-set-key (kbd "C-c C-n") 'gud-next)
  ;(local-set-key (kbd "<f8>") 'gud-next)
  ;(local-set-key (kbd "C-c C-u") 'gud-up)
  ;(local-set-key (kbd "C-c C-d") 'gud-down)
  ;(local-set-key (kbd "C-c C-b") 'gud-break)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(message "Checkpoint 10")
; (require 'flycheck-rtags)
; (defun my-rtags-hook ()
;   "Set up rtags for use in C/C++/Objective-C."
;   (rtags-start-process-unless-running)
;   (rtags-diagnostics)
;   (rtags-enable-standard-keybindings)
;   (flycheck-select-checker 'rtags)
;   (setq-local flycheck-highlighting-mode nil)  ;; RTags creates more accurate overlays.
;   (setq-local flycheck-check-syntax-automatically nil))
; (add-hook 'c-mode-hook 'my-rtags-hook)
; (add-hook 'c++-mode-hook 'my-rtags-hook)
; (add-hook 'objc-mode-hook 'my-rtags-hook)

(message "Checkpoint 11")
;; LanguageTool for grammar checks
(setq langtool-language-tool-jar "~/opt/LanguageTool-4.7/languagetool-commandline.jar")
;(setq langtool-language-tool-server-jar "~/opt/LanguageTool-4.7/languagetool-server.jar")
;(use-package 'langtool)

;; LSP Mode
(use-package lsp-mode
  :hook
  ((java-mode) . lsp)
  :config (setq lsp-completion-enable-additional-text-edit nil))
(use-package company-lsp)
(use-package lsp-ui :after lsp)
(use-package lsp-java :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

;; Python Programming Language Settings
(defun my/python-mode-hook ()
  (company-mode)
  ;(eglot-ensure)
  ;(local-set-key (kbd "C-c .") 'xref-find-definitions)
  (local-set-key (kbd "C-c i") 'lsp-java-add-import)
  (local-set-key (kbd "C-c .") 'lsp-find-definition)
  (local-set-key (kbd "C-c r") 'lsp-find-references)
  (local-set-key (kbd "C-c /") 'company-complete)
  (yas-minor-mode)
  (setq-local fill-column 100))
(add-hook 'python-mode-hook 'my/python-mode-hook)
(add-hook 'python-mode-hook 'lsp)
; See https://github.com/python-lsp/python-lsp-server
(add-to-list 'eglot-server-programs `(python-mode . ("~/.local/bin/pylsp")))

;; Java Programming Language Settings
; Settings copied from https://github.com/emacs-lsp/lsp-java
(require 'cc-mode)
(message "Checkpoint 12")
(use-package yasnippet
  :after lsp-mode
  :config
  (yas-minor-mode)
  (yas-load-directory (car (yas-snippet-dirs))))
(use-package hydra)
(message "Checkpoint 13")
(message "Checkpoint 14")
(use-package dap-mode :after lsp-mode :config (dap-mode t) (dap-ui-mode t))
(use-package dap-java :ensure nil :after lsp-java)
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
(defun my-java-mode-hook ()
  (google-set-c-style)
  (google-make-newline-indent)
  (setq c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
  (modify-syntax-entry ?@ "< b" java-mode-syntax-table)
  (c-set-offset 'func-decl-cont 'nil)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'arglist-cont 'nil)
  (c-set-offset 'arglist-cont-nonempty '++)
  (setq-local fill-column 100)
  (setq-local mode-require-final-newline 't)
  (column-marker-1 100)
  ;(auto-complete-mode)
  (setq tab-width 2)
  ;(setq ac-sources
  ;      '(ac-source-gtags
  ;        ac-source-words-in-same-mode-buffers
  ;        ac-source-abbrev
  ;        ac-source-dictionary))
  (local-set-key (kbd "C-c i") 'lsp-java-add-import)
  (local-set-key (kbd "C-c .") 'lsp-find-definition)
  (local-set-key (kbd "C-c r") 'lsp-find-references)
  (local-set-key (kbd "C-c /") 'company-complete)
  ;(add-hook 'before-save-hook 'google-imports-organize-imports nil t))
  )
(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'java-mode-hook 'lsp)

;; Dart configuration
; See https://emacs-lsp.github.io/lsp-dart/
(defun my-dart-mode-hook ()
  (local-set-key (kbd "C-c .") 'lsp-find-definition)
  (local-set-key (kbd "C-c r") 'lsp-find-references)
  (local-set-key (kbd "C-c /") 'company-complete)
  )
(add-hook 'dart-mode-hook 'my-dart-mode-hook)
(add-hook 'dart-mode-hook 'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil)
(setenv "FLUTTER_ROOT" "/home/vnayar/opt/flutter")

;; Drools mode rules: https://docs.jboss.org/drools/release/5.2.0.Final/drools-expert-docs/html/ch05.html
; (autoload 'drools-mode "drools-mode")
; 
; (defun set-extension-mode (extension mode)
;   (setq auto-mode-alist
; 	(cons (cons (concat "\\" extension "\\'") mode)
; 	      auto-mode-alist) ) )
; 
; (set-extension-mode ".drl" 'drools-mode)
; (set-extension-mode ".dslr" 'drools-mode)
;
;(add-hook 'drools-mode-hook 'my-drools-hook)
; 
; (defun drools-return-and-indent()
;   (interactive)
;   (newline) (indent-for-tab-command) )
; 
; (defun my-drools-hook ()
;   (setq indent-tabs-mode nil)
;   (local-set-key [?\C-m] 'drools-return-and-indent) )

;; JSON Pretty Print Function
;(defun json-pretty-print (start end)
;  "Pretty-print the selected region of text."
;  (interactive "r")
;  (shell-command-on-region start end "python -m json.tool" :replace=true)
;)

(message "Checkpoint 15")
;; EBNF Settings
(autoload 'ebnf-mode "ebnf-mode" "Major mode for editing EBNF grammars." t)
(add-to-list 'auto-mode-alist '("\\.ebnf\\'" . ebnf-mode))
(message "Checkpoint 16")

;; Web Mode - http://web-mode.org/
;(require 'web-mode)
;(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.soy\\'" . web-mode))
;(setq web-mode-style-padding 2)
;(setq web-mode-script-padding 2)
;(setq web-mode-markup-indent-offset 2)
;(setq web-mode-css-indent-offset 2)
;(setq web-mode-code-indent-offset 2)
;(set-face-attribute 'web-mode-html-tag-face nil :foreground "Blue")

;; Show the directory in the mode-line.
(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (add-to-list 'mode-line-buffer-identification
               '(:eval (substring default-directory
                                  (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))
(add-hook 'find-file-hook 'add-mode-line-dirtrack)

;; Easy file lookup.
(ffap-bindings)

;; Docker
(message "Checkpoint 17")
;(require 'dockerfile-mode)
;(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Latex and Beamer configuration
; See https://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-packages-alist '("" "svg"))
(setq org-latex-listings 'minted)
(require 'ob-latex)

(message "Checkpoint 18")

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

(message "Checkpoint 19")
;; Org-mode settings
(defun my-org-mode-hook ()
  "My settings for org-mode."
  (require 'ox-gfm)
  (require 'org-ac)
  (org-ac/config-default)
  (setq-local fill-column 100)
  (setq-local show-trailing-whitespace t)
  (auto-complete-mode 0)
  (electric-indent-mode 0))
(add-hook 'org-mode-hook 'my-org-mode-hook)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
; Avoid spellchecks on code snippets.
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(message "Checkpoint 20")

;; W3M Settings
(defun w3m-copy-url-at-point ()
  (interactive)
  (let ((url (w3m-anchor)))
    (if (w3m-url-valid url)
  	(kill-new (w3m-anchor))
      (message "No URL at point!"))))

(add-hook 'w3m-mode-hook
  (lambda ()
    (local-set-key "\M-W" 'w3m-copy-url-at-point)))

;; A useful timer utility.
(use-package egg-timer)

;; Personal preferences and customizations.
(setq-default indicate-empty-lines t)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.5)
 '(ac-non-trigger-commands
   '(*table--cell-self-insert-command electric-buffer-list org-cycle))
 '(ac-use-fuzzy t)
 '(ag-highlight-search t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(auth-source-save-behavior nil)
 '(bookmark-save-flag 1)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(company-idle-delay 1.0)
 '(company-minimum-prefix-length 0)
 '(compilation-auto-jump-to-first-error nil)
 '(css-indent-offset 2)
 '(custom-enabled-themes '(leuven))
 '(custom-safe-themes
   '("f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "a1b6cc0a437a11d1d2b40cd60b32d248c1c94b0ba2e0934dfc89ea7d3248e782" "aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "cf9414f229f6df728eb2a5a9420d760673cca404fee9910551caf9c91cff3bfa" "039c01abb72985a21f4423dd480ddb998c57d665687786abd4e16c71128ef6ad" "89885317e7136d4e86fb842605d47d8329320f0326b62efa236e63ed4be23c58" "7922b14d8971cce37ddb5e487dbc18da5444c47f766178e5a4e72f90437c0711" "3cd4f09a44fe31e6dd65af9eb1f10dc00d5c2f1db31a427713a1784d7db7fdfc" "68d8ceaedfb6bdd2909f34b8b51ceb96d7a43f25310a55c701811f427e9de3a3" "672bb062b9c92e62d7c370897b131729c3f7fd8e8de71fc00d70c5081c80048c" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3" "fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10" "028de01489a683696c64dcc2a01eaa663670d04202de3fce48ec3a5542bc2da5" "28bf1b0a72e3a1e08242d776c5befc44ba67a36ced0e55df27cfc7ae6be6c24d" "d70c11f5a2b69a77f9d56eff42090138721d4c51d9d39ce986680786d694f492" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "0c71e4d0b5ad79a7cb155f180adcc93f2fe5ae3d3a863de7d3a8c898087d890c" "2e1e2657303116350fe764484e8300ca2e4cf45a73cdbd879bc0ca29cb337147" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "ad9747dc51ca23d1c1382fa9bd5d76e958a5bfe179784989a6a666fe801aadf2" "b04d091b3315afedc67e4e2e9950c272789804cf0cb7e93750d70475a47b935b" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "aecea99f23d116cd33dabe34b9df808816c374328c064fcf12d15cecc3735237" default))
 '(delete-trailing-lines nil)
 '(ecb-options-version "2.40")
 '(eldoc-echo-area-prefer-doc-buffer t)
 '(even-window-sizes t)
 '(exec-path
   '("/home/vnayar/.cargo/bin" "/home/vnayar/.local/bin" "/home/vnayar/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/local/games" "/snap/bin" "/usr/local/libexec/emacs/28.1/x86_64-pc-linux-gnu"))
 '(fci-rule-color "#37474f")
 '(fill-column 100)
 '(flycheck-idle-change-delay 2.5)
 '(gcomplete-jump-to-def-binding [ignore])
 '(gdb-many-windows t)
 '(generic-extras-enable-list
   '(alias-generic-mode apache-conf-generic-mode apache-log-generic-mode etc-fstab-generic-mode etc-modules-conf-generic-mode etc-passwd-generic-mode etc-services-generic-mode etc-sudoers-generic-mode fvwm-generic-mode hosts-generic-mode inetd-conf-generic-mode ini-generic-mode java-manifest-generic-mode java-properties-generic-mode javascript-generic-mode mailagent-rules-generic-mode mailrc-generic-mode named-boot-generic-mode named-database-generic-mode prototype-generic-mode resolve-conf-generic-mode samba-generic-mode show-tabs-generic-mode vrml-generic-mode x-resource-generic-mode xmodmap-generic-mode))
 '(global-auto-revert-mode t)
 '(grep-command "grep --color -nHE -e ")
 '(grep-find-command '("find . -type f -exec grep --color -nHE -e  {} +" . 43))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "#*#" "GPATH" "GRTAGS" "GSYMS" "GTAGS" "*.jar" "*.iml"))
 '(grep-find-template
   "find . <X> -type f <F> -exec grep <C> --color -nHE -e <R> {} +")
 '(groovy-indent-offset 2)
 '(gud-tooltip-modes '(gud-mode c-mode c++-mode fortran-mode python-mode d-mode))
 '(hl-sexp-background-color "#efebe9")
 '(indent-tabs-mode nil)
 '(jdee-server-dir "~/bin")
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(js2-global-externs nil)
 '(js2-include-node-externs t)
 '(js2-init-hook '(my-c-mode-common-hook))
 '(lsp-dart-flutter-executable "flutter")
 '(lsp-enable-on-type-formatting nil)
 '(lsp-java-autobuild-enabled nil)
 '(lsp-java-completion-guess-method-arguments t)
 '(lsp-java-completion-import-order [])
 '(lsp-java-vmargs
   '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/home/vnayar/.m2/repository/org/projectlombok/lombok/1.18.24/lombok-1.18.24.jar"))
 '(lsp-ui-doc-delay 2.0)
 '(lsp-ui-doc-show-with-cursor nil)
 '(lsp-ui-sideline-enable nil)
 '(org-ac/ac-trigger-command-keys nil)
 '(org-adapt-indentation 'headline-data)
 '(org-babel-load-languages
   '((d . t)
     (emacs-lisp . t)
     (plantuml . t)
     (shell . t)
     (js . t)
     (R . t)
     (latex . t)))
 '(org-clock-clocked-in-display 'frame-title)
 '(org-clock-sound t)
 '(org-duration-format 'h:mm)
 '(org-edit-src-content-indentation 0)
 '(org-export-backends '(ascii beamer html latex md odt))
 '(org-export-with-sub-superscripts '{})
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.4 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-indent-indentation-per-level 2)
 '(org-latex-image-default-width "1.2\\linewidth")
 '(org-latex-pdf-process
   '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f" "%latex -shell-escape -interaction nonstopmode -output-directory %o %f" "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
 '(org-link-file-path-type 'relative)
 '(org-list-description-max-indent 4)
 '(org-odt-create-custom-styles-for-srcblocks t)
 '(org-odt-fontify-srcblocks t)
 '(org-plantuml-jar-path "/home/vnayar/bin/plantuml.jar")
 '(org-time-clocksum-format
   '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
 '(org-todo-keywords '((sequence "TODO" "WAIT" "DONE")))
 '(package-selected-packages
   '(org bm tikz circe circe-notifications flymake-eslint bnf-mode treemacs lsp-treemacs js2-mode react-snippets swift-mode web-mode pug-mode go-mode d-mode bazel eglot jedi yaml-mode egg-timer tea-time bison-mode rust-mode atom-one-dark-theme tron-legacy-theme lsp-dart dart-mode adoc-mode lsp-java protobuf-mode csv csv-mode lsp-mode dap-mode lsp-ui posframe ag w3m rjsx-mode typescript-mode groovy-mode magit flycheck-plantuml terraform-doc terraform-mode flycheck-d-unittest gnu-elpa-keyring-update company-lsp abyss-theme gh-md material-theme unicode-math-input sdlang-mode gnuplot-mode w3 top-mode sr-speedbar peg ox-mediawiki ox-gfm org-ac nyx-theme neotree java-snippets grizzl gradle-mode google-c-style git ggtags fuzzy company-dcd column-marker color-theme-buffer-local apache-mode ac-rtags ac-js2 ac-alchemist))
 '(peg-mode-hook '(my-peg-mode-hook))
 '(plantuml-indent-level 2)
 '(preview-TeX-style-dir "/home/vnayar/.emacs.d/elpa/auctex-13.0.16/latex" t)
 '(preview-scale-function 1.4)
 '(projectile-completion-system 'ido)
 '(rtags-install-path nil)
 '(rtags-path "~/src/rtags/bin")
 '(rtags-reindex-on-save t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(treemacs-width 30)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '((use-package) (lsp-mode)))
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "SRC" :family "Noto Sans Mono"))))
 '(circe-server-face ((t (:foreground "dodger blue"))))
 '(lsp-lsp-flycheck-warning-unnecessary-face ((t (:underline (:color "#F4A939" :style wave) :weight bold))) t)
 '(web-mode-html-tag-face ((t (:foreground "steel blue")))))
(message "Checkpoint 21")
