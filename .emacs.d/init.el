;; Allow us to import custom modules.
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/org-9.0.5/lisp")
(add-to-list 'load-path "~/.emacs.d/org-9.0.5/contrib/lisp")
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
   'ac-dcd
   'ac-js2
   'auto-complete
   'auctex
   'bash-completion
   'bm
   'company
   'company-dcd
   'fill-column-indicator
   'd-mode
   'flycheck-rtags
   'ggtags
   'google-c-style
   'groovy-mode
   'js2-mode
   'leuven-theme
   'markdown-mode
   'neotree
   'org-ac
   'ox-gfm
   'pcmpl-args
   'pcmpl-git
   'plantuml-mode
   'polymode
   'projectile
   'protobuf-mode
   'terraform-mode
   'web-mode
   'w3m)
  )

;; Various very small generic modes.
(require 'generic-x)
(add-to-list 'auto-mode-alist '("credentials\\'" . ini-generic-mode))

;; Load a custom version of cedet, if available.
; (when (file-exists-p "~/src/cedet/cedet-devel-load.el")
;   (load "~/src/cedet/cedet-devel-load.el"))
; (setq semantic-default-submodes
;       '(;; Perform semantic actions during idle time
;         global-semantic-idle-scheduler-mode
;         ;; Use a database of parsed tags
;         global-semanticdb-minor-mode
;         ;; Decorate buffers with additional semantic information
;         global-semantic-decoration-mode
;         ;; Highlight the name of the function you're currently in
;         global-semantic-highlight-func-mode
;         ;; show the name of the function at the top in a sticky
;         global-semantic-stickyfunc-mode
;         ;; Generate a summary of the current tag when idle
;         global-semantic-idle-summary-mode
;         ;; Show a breadcrumb of location during idle time
;         global-semantic-idle-breadcrumbs-mode
;         ;; Switch to recently changed tags with `semantic-mrub-switch-tags',
;         ;; or `C-x B'
;         global-semantic-mru-bookmark-mode))
; 
; (add-hook 'emacs-lisp-mode-hook 'semantic-mode)
; (add-hook 'python-mode-hook 'semantic-mode)
; (add-hook 'java-mode-hook 'semantic-mode)
; 
; (require 'semantic/db-javap)
 
;; Theme Settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; General Editing Settings
(setq inhibit-splash-screen t)
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Visual Bookmark Settings
(require 'bm)
(global-set-key (kbd "C-c b") 'bm-toggle)

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

(require 'fill-column-indicator)

;; Keybindings for navigating windows.
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;; auto-complete settings
(require 'auto-complete)
(define-key ac-completing-map [return] nil)
(define-key ac-completing-map "\r" nil)
(ac-config-default)

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
(defun my-shell-mode-hook ()
  "Commands to run for shell-mode."
  ;(auto-complete-mode)
  ;(setq ac-delay 0.5)
  ;(setq ac-sources
  ;      '(ac-source-files-in-current-dir
  ;        ac-source-filename))
  )
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

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

;; Jenkins files are in Groovy.
(add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode))

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


;; Parsing Expression Grammar files.
(require 'peg-mode)
(defun my-peg-mode-hook ()
  (modify-syntax-entry ?[  "<"   peg-mode-syntax-table)
  (modify-syntax-entry ?]  ">"   peg-mode-syntax-table))
(add-to-list 'auto-mode-alist '("\\.peg\\'" . peg-mode))


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

;; Image Manipulation
(eval-after-load 'image '(require 'image+))
(eval-after-load 'image+ '(imagex-global-sticky-mode 1))

;; PlantUML Mode
(setq plantuml-jar-path "~/bin/plantuml.jar")
(require 'plantuml-mode)
(defun my-plantuml-mode-hook ()
  (column-number-mode 1)
  (show-paren-mode 1)
  (setq-local fill-column 100)
  (fci-mode)
  (setq-local show-trailing-whitespace t)
  (setq plantuml-indent-offset 2))
(add-hook 'plantuml-mode-hook 'my-plantuml-mode-hook)
(defun plantuml-compile ()
  "Compile a PlantUML file into images."
  (interactive)
  (compile (concat "java -jar " plantuml-jar-path " " (buffer-file-name))))


;; AUCTeX Settings
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Project management, prefix key is "C-c p"
(require 'projectile)
(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

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
  ;(local-set-key (kbd "<f7>") 'gud-next)
  ;(local-set-key (kbd "C-c C-n") 'gud-next)
  ;(local-set-key (kbd "<f8>") 'gud-next)
  ;(local-set-key (kbd "C-c C-u") 'gud-up)
  ;(local-set-key (kbd "C-c C-d") 'gud-down)
  ;(local-set-key (kbd "C-c C-b") 'gud-break)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(require 'flycheck-rtags)
(defun my-rtags-hook ()
  "Set up rtags for use in C/C++/Objective-C."
  (rtags-start-process-unless-running)
  (rtags-diagnostics)
  (rtags-enable-standard-keybindings)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)  ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-hook 'my-rtags-hook)
(add-hook 'c++-mode-hook 'my-rtags-hook)
(add-hook 'objc-mode-hook 'my-rtags-hook)

;; Java Debugging JSwat Settings
;(require 'yasnippet)
(remove-hook 'jdb-mode-hook 'google-jdb-fix-comint-prompt)
(defun jswat ()
  (interactive)
  (setq google-jdb-jswat-command "~/scripts/jswat-launcher -jdb")
  (google-jswat))

;; Java Programming Language Settings
;(require 'semantic/db-javap)
;(require 'jdee)
;(require 'jdee-flycheck)
;(require 'google-java-format)
;(require 'malabar-mode)
;(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
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
  (fci-mode)
  (auto-complete-mode)
  (setq ac-sources
        '(ac-source-gtags
          ac-source-words-in-same-mode-buffers
          ac-source-abbrev
          ac-source-dictionary))
  ;(local-set-key (kbd "C-c i") 'google-imports-add-import-from-tag)
  ;(local-set-key (kbd "C-c i") 'google-imports-jade)
  ;(local-set-key (kbd "C-c f") #'google-java-format-region)
  ;(google-set-java-style)
  ;(add-hook 'before-save-hook 'google-imports-organize-imports nil t))
  )
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; JavaScript Programming Language Settings
(require 'ac-js2)
(require 'js2-mode)
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
  (ac-js2-mode)
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
(require 'ac-dcd)
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
(add-hook 'd-mode-hook 'ac-dcd-setup)
(defun my-d-mode-hook ()
  "My settings for d-mode."
  (setq-local show-trailing-whitespace t)
  (setq-local fill-column 100)
  (fci-mode)
  (c-set-offset 'func-decl-cont 'nil)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'arglist-cont 'nil)
  (c-set-offset 'arglist-cont-nonempty '++)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-cont '++)
  (company-dcd-mode)
  )
(add-hook 'd-mode-hook 'my-d-mode-hook)


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
(defun my-org-mode-hook ()
  "My settings for org-mode."
  (setq-local fill-column 100)
  (fci-mode)
  (setq-local show-trailing-whitespace t))
(add-hook 'org-mode-hook 'my-org-mode-hook)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
; Export to Github Flavored Markdown.
(require 'ox-gfm)
; Set up org autocomplete sources.
(require 'org-ac)
(org-ac/config-default)
; PlantUML Support in org-mode.
(require 'ob-plantuml)


;; Personal preferences and customizations.
(setq-default indicate-empty-lines t)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.2)
 '(ac-non-trigger-commands
   (quote
    (*table--cell-self-insert-command electric-buffer-list org-cycle)))
 '(ac-use-fuzzy t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["dim gray" "red" "green" "yellow" "steel blue" "magenta" "cyan" "white"])
 '(bookmark-save-flag 1)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("2e1e2657303116350fe764484e8300ca2e4cf45a73cdbd879bc0ca29cb337147" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "ad9747dc51ca23d1c1382fa9bd5d76e958a5bfe179784989a6a666fe801aadf2" "b04d091b3315afedc67e4e2e9950c272789804cf0cb7e93750d70475a47b935b" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "aecea99f23d116cd33dabe34b9df808816c374328c064fcf12d15cecc3735237" default)))
 '(delete-trailing-lines nil)
 '(ecb-options-version "2.40")
 '(even-window-heights t)
 '(fill-column 100)
 '(gcomplete-jump-to-def-binding [ignore])
 '(generic-extras-enable-list
   (quote
    (alias-generic-mode apache-conf-generic-mode apache-log-generic-mode etc-fstab-generic-mode etc-modules-conf-generic-mode etc-passwd-generic-mode etc-services-generic-mode etc-sudoers-generic-mode fvwm-generic-mode hosts-generic-mode inetd-conf-generic-mode ini-generic-mode java-manifest-generic-mode java-properties-generic-mode javascript-generic-mode mailagent-rules-generic-mode mailrc-generic-mode named-boot-generic-mode named-database-generic-mode prototype-generic-mode resolve-conf-generic-mode samba-generic-mode show-tabs-generic-mode vrml-generic-mode x-resource-generic-mode xmodmap-generic-mode)))
 '(grep-command "grep --color -nHE -e ")
 '(grep-find-command
   (quote
    ("find . -type f -exec grep --color -nHE -e  {} +" . 43)))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "#*#" "GPATH" "GRTAGS" "GSYMS" "GTAGS" "*.jar" "*.iml")))
 '(grep-find-template
   "find . <X> -type f <F> -exec grep <C> --color -nHE -e <R> {} +")
 '(hl-sexp-background-color "#efebe9")
 '(jdee-server-dir "~/bin")
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-global-externs nil)
 '(js2-include-node-externs t)
 '(js2-init-hook (quote (my-c-mode-common-hook)))
 '(org-ac/ac-trigger-command-keys (quote ("\\" "SPC" ":" "[" "+")))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (plantuml . t)
     (gnuplot . t)
     (shell . t)
     (R . t))))
 '(org-export-with-toc 4)
 '(org-link-file-path-type (quote relative))
 '(org-plantuml-jar-path "/home/vnayar/bin/plantuml.jar")
 '(org-todo-keywords (quote ((sequence "TODO" "WAIT" "DONE"))))
 '(peg-mode-hook (quote (my-peg-mode-hook)))
 '(rtags-install-path nil)
 '(rtags-path "~/src/rtags/bin")
 '(rtags-reindex-on-save t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "WenQuanYi Micro Hei Mono" :foundry "WQYF" :slant normal :weight normal :height 98 :width normal))))
 '(web-mode-html-tag-face ((t (:foreground "steel blue")))))
