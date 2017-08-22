
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
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "monaco" :height 126)
)
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "consolas" :height 119)
)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; display line number
(global-linum-mode t)
(setq linum-format "%4d ")

;; auto indent and brakets
(electric-indent-mode t)
(electric-pair-mode t)

;; hightlight matching parens
(show-paren-mode t)

;; auto save to a temp dir
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
	`((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
	emacs-tmp-dir)


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

(defvar my-packages '(paredit idle-highlight-mode magit smex auto-complete org less-css-mode sublimity
                                  helm projectile helm-projectile direx popwin markdown-mode markdown-mode+ jsx-mode
                                  js2-mode js2-refactor web-beautify ac-js2 expand-region ace-jump-mode ido-ubiquitous
				  go-autocomplete go-complete go-direx go-dlv go-eldoc go-errcheck go-mode go-projectile go-rename go-stacktracer gotest
				  editorconfig web-mode multiple-cursors flycheck json-mode js2-refactor discover-js2-refactor flycheck-flow flycheck-pos-tip
				  exec-path-from-shell neotree twilight-bright-theme)
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

;; sublimity
(require 'sublimity)
(require 'sublimity-scroll)
;;(require 'sublimity-map)
;;(require 'sublimity-attractive)
(sublimity-mode 1)

;; neoTree
(require 'neotree)
(global-set-key (kbd "C-c o") 'neotree-toggle)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-theme 'ascii)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-banner-face ((t :inherit shadow)))
 '(neo-button-face ((t :inherit dired-directory)))
 '(neo-dir-link-face ((t :inherit dired-directory)))
 '(neo-expand-btn-face ((t :inherit button)))
 '(neo-file-link-face ((t :inherit default)))
 '(neo-header-face ((t :inherit shadow)))
 '(neo-root-dir-face ((t :inherit link-visited :underline nil))))

;; jsx
;;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;;(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; jsx - flycheck
;; npm install -g eslint babel-eslint eslint-plugin-react
;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; tooltip sort of..
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

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
(add-hook 'js2-mode-hook #'js2-refactor-mode)
;; C-c C-r

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
(load-theme 'twilight-bright t)


;; go lang
(require 'go-mode)
(defun my-go-mode-hook ()
;; gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; prerequisite step
  ;;       go get github.com/rogpeppe/godef
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

;; before auto complete works pre requisite step -
;;     go get -u github.com/nsf/gocode
(require 'auto-complete-config)
(define-key ac-mode-map (kbd "C-.") 'auto-complete)


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

;; omnisharp
;;(add-hook 'csharp-mode-hook 'omnisharp-mode)
;;(global-set-key (kbd "C-.") 'omnisharp-auto-complete)






