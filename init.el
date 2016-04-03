
(require 'cl)

(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; stop splash screen
(setq inhibit-splash-screen t)

;; line spacing
(setq-default line-spacing 3)

;; font size
(set-face-attribute 'default nil :family "monaco" :height 125)
;; windows
;; (set-face-attribute 'default nil :family "consolas" :height 119)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; display line number
(global-linum-mode t)
(setq linum-format "%d ")

;; initialize package.el
(require 'package)
(package-initialize)

;; add marmalade repos
(require 'package)
;;(add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; load packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(paredit idle-highlight-mode magit smex auto-complete org less-css-mode
                                  helm projectile helm-projectile direx popwin markdown-mode markdown-mode+ jsx-mode
                                  js2-mode js2-refactor web-beautify ac-js2 expand-region ace-jump-mode smooth-scrolling ido-ubiquitous
				  material-theme
				  go-autocomplete go-complete go-direx go-dlv go-eldoc go-errcheck go-mode go-projectile go-rename go-stacktracer gotest
				  editorconfig)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; auto complete setup
(add-to-list 'load-path "~/.emacs.d/autocomplete")
(require 'auto-complete-config)
(ac-config-default)

;; remove ugly scrollbar
(scroll-bar-mode -1)
;; smex init
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido
(ido-mode 1)
(ido-everywhere 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(setq org-completion-use-ido t)
(setq magit-completing-read-function 'magit-ido-completing-read)

;; projectile
;; find file in project C-c p f
;; list projects C-c p p
;; search for symbol at the cursor within the project C-c p s g
(projectile-global-mode)
(setq projectile-require-project-root nil)

;; helm
(global-set-key (kbd "C-c h") 'helm-projectile)

;; neoTree
(require 'neotree)
(global-set-key (kbd "C-c o") 'neotree-toggle)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-theme 'ascii)
(custom-set-faces
 '(neo-banner-face ((t . (:inherit shadow))) t)
 '(neo-header-face ((t . (:inherit shadow))) t)
 '(neo-root-dir-face ((t . (:inherit link-visited :underline nil))) t)
 '(neo-dir-link-face ((t . (:inherit dired-directory))) t)
 '(neo-file-link-face ((t . (:inherit default))) t)
 '(neo-button-face ((t . (:inherit dired-directory))) t)
 '(neo-expand-btn-face ((t . (:inherit button))) t))

;; jsx
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;(autoload 'js2-mode "js2 mode" nil t)
(setq js2-bounce-indent-p t)

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook
             (lambda ()
               (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

(add-hook 'js2-mode-hook 'ac-js2-mode)

;; expand region - helps select text easily
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(pending-delete-mode t)

;; ace jump mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; I don't want backup files
(setq make-backup-files nil)
 
;; list symbols
(global-set-key (kbd "C-c f") 'helm-imenu)

;; set theme
;;(load-theme 'wombat t)
;;(load-theme 'material t)
(set-frame-parameter nil 'background-mode 'dark)
      (when (not (display-graphic-p))
      (set-terminal-parameter nil 'background-mode 'dark))
(load-theme 'solarized t)

;; go lang
(require 'go-mode-autoloads)

;;editorconfig
(editorconfig-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "667e296942c561382fe0a8584c26be0fe7a80416270c3beede8c6d69f2f77ccc" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "316d29f8cd6ca980bf2e3f1c44d3a64c1a20ac5f825a167f76e5c619b4e92ff4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
