;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
(ignore-errors (load-file "~/cedet-1.1/common/cedet.el"))

;; Enable EDE (Project Management) features
(global-ede-mode 'nil)
(ede-enable-generic-projects)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")


;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;; (semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode,
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
(semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberant ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
(semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languages only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)

;; Additional features for names completion and tag/class info.
(require 'semantic-ia)
(require 'semantic-java)

(global-set-key (kbd "<C-return>") 'semantic-ia-complete-symbol-menu)

;; Tell emacs where is your personal elisp lib dir
;; this is the dir you place all your extra packages
(add-to-list 'load-path "~/.emacs.d/")

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
(global-set-key [pause] 'toggle-current-window-dedication)

;; General Editing Settings
(setq inhibit-splash-screen t)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
;(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

;; Show the directory in the mode-line.
(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (add-to-list 'mode-line-buffer-identification
               '(:eval (substring default-directory
                                  (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))
(add-hook 'find-file-hook 'add-mode-line-dirtrack)

;; Easy file lookup.
(ffap-bindings)

;; Shell settings
(setenv "GIT_PAGER" "")
(setenv "GIT_EDITOR" "emacs")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
; Place git output into the *git* buffer.
;(defun eshell/git (&rest args)
;  (apply 'eshell-exec-visual (cons "git" args)))

;; Calendar settings
(setq mark-diary-entries-in-calendar t)
(setq mark-holidays-in-calendar nil)

;; Window navigation with shift-arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'other-frame)
; Buffer selection in the same window
(global-set-key "\C-x\C-b" 'buffer-menu)

;; Color theme
;(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-goodies-el")
;(require 'color-theme)
;(eval-after-load color-theme
;  '(progn
;     (color-theme-initialize)
;     (color-theme-hober)))

;; Git support
;(require 'git)
(require 'egit)

;; load the packaged named xyz.
(load "cc-mode") ;; best not to include the ending “.el” or “.elc”
(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'my-indent-setup)

;; D-Programming Language Support
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
; Semantic paths for D.
(semantic-add-system-include "/usr/include/d/dmd/phobos" 'd-mode)

;; FTL Support
(load "ftl")
(add-hook 'html-mode-hook 'turn-on-ftl-mode t t)
(add-hook 'xml-mode-hook 'turn-on-ftl-mode t t)
(add-hook 'text-mode-hook 'turn-on-ftl-mode t t)
(setq auto-mode-alist (cons (cons "\\.ftl$" 'html-mode) auto-mode-alist))

;; SCSS Support
(setq auto-mode-alist (cons (cons "\\.scss$" 'css-mode) auto-mode-alist))

;; PL-SQL Support
(load "plsql")
(setq auto-mode-alist (cons (cons "\\.sql$" 'plsql-mode) auto-mode-alist))

;; Auto-Complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; Jabber Configuration
;(require 'jabber)
;(setq jabber-account-list '(("vnayar@wgim01.wgenhq.net")))
;(setq jabber-username "vnayar")
;(setq jabber-server "wgim01.wgenhq.net")
;(setq jabber-nickname "Vijay Nayar")
;(set jabber-alert-presence-message-function (lambda (who oldstatus newstatus statustext) nil))
;(add-to-list 'jabber-muc-autojoin "skylight@conference.wgim01.wgenhq.net")
;(add-to-list 'jabber-muc-autojoin "web-assmt@conference.wgim01.wgenhq.net")
;(add-to-list 'jabber-muc-autojoin "rti@conference.wgim01.wgenhq.net")

; Message alert hooks
;(define-jabber-alert echo "Show a message in the echo area"
;  (lambda (msg)
;    (unless (minibuffer-prompt)
;      (message "%s" msg))))


;; EMMS Emacs MultiMedia System
;(require 'emms-player-simple)
;(define-emms-simple-player mikmod '(file)
;  (regexp-opt '(".669" ".AMF" ".DSM" ".FAR" ".GDM" ".IT" ".IMF"
;		".MED" ".MTM" ".OKT" ".S3M" ".STM" ".STX" ".ULT"
;		".APUN" ".XM" ".MOD" ".amf" ".dsm" ".far" ".gdm"
;		".it" ".imf" ".mod" ".med" ".mtm" ".okt" ".s3m"
;		".stm" ".stx" ".ult" ".apun" ".xm" ".mod" ".MOD")
;	      ) "mikmod" "-q" "-p" "1" "-X")
;(add-to-list 'emms-player-list 'emms-player-mikmod)

;; Web Browsing
;(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;; General java paths.
;(semantic-add-system-include "~/java/src" 'java-mode)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(verilog-auto-newline nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
