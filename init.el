
(require 'cl)

(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; stop splash screen
(setq inhibit-splash-screen t)

;; line spacing
(setq-default line-spacing 5)

;; font size
(set-face-attribute 'default nil :height 140)

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
				  material-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; auto complete setup
(add-to-list 'load-path "~/.emacs.d/autocomplete")
(require 'auto-complete-config)
(ac-config-default)

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
;; switch project C-c p s
;; find file in project C-c p f
;; list projects C-c p p
(projectile-global-mode)
(setq projectile-require-project-root nil)

;; helm
(global-set-key (kbd "C-c h") 'helm-projectile)

;; popwin, need for some direx stuff
(require 'popwin)
(popwin-mode 1)

;; direx
(require 'direx)

;; the project browser will show in a popwin.
(push '(direx:direx-mode :position right :width 50 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-g") 'direx-project:jump-to-project-root-other-window)

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

;; idle highlight mode
(defun my-coding-hook ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (if window-system (hl-line-mode t))
  (idle-highlight-mode t))

;; set theme
;;(load-theme 'wombat t)
(load-theme 'material-light t)

