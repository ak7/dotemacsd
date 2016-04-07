
(require 'cl)

(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(menu-bar-mode -1)


;; stop splash screen
(setq visible-bell t
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

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
				  go-autocomplete go-complete go-direx go-dlv go-eldoc go-errcheck go-mode go-projectile go-rename go-stacktracer gotest
				  editorconfig web-mode multiple-cursors flycheck json-mode exec-path-from-shell)
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
;;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;;(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; jsx - flycheck
;; npm install -g eslint babel-eslint eslint-plugin-react
;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))




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
;;(set-frame-parameter nil 'background-mode 'dark)
;;      (when (not (display-graphic-p))
;;      (set-terminal-parameter nil 'background-mode 'dark))
;;(load-theme 'solarized t)
(load-theme 'adwaita t)

;; go lang
(require 'go-mode-autoloads)

;;editorconfig
(editorconfig-mode 1)

;; web-mode http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

;; multiple cursor
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
