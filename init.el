;; borrowed from https://github.com/credmp/emacs-config/blob/master/loader.org
;; prior to init - manual steps
;;     python -m pip install --user certifi
;;     brew install gnutls

(require 'cl)

;; GUI
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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

;; ensure downloads are over tls.
(setq tls-checktrust t)

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; initialize package
(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Add marmalade to package repos
(setq package-archives nil) ;;Because the default setting for package-archives is to use the HTTP access to the GNU archive
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(package-initialize)

(unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable"))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  (message "running packages-install")
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

;; Install extensions if they're missing
(defun init--install-packages ()
  (message "Lets install some packages")
  (packages-install
   ;; Since use-package this is the only entry here
   ;; ALWAYS try to use use-package!
   (cons 'use-package melpa)
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; set theme
(use-package twilight-bright-theme
  :ensure t
  :config
  (load-theme 'twilight-bright t))

;; discoverablitiy
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; enviorment
(if (or
     (eq system-type 'darwin)
     (eq system-type 'berkeley-unix))
    (setq system-name (car (split-string system-name "\\."))))

;; Backup settings
(defvar --backup-directory (concat user-emacs-directory "backups"))

(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))

(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat user-emacs-directory "backups")))))


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

;;Don’t open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; window management
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-c p") 'ace-window))

(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
  (bind-key "C-c SPC" 'ace-jump-mode))

;; Custom binding for magit-status
(use-package magit
  :config
  (global-set-key (kbd "C-c m") 'magit-status))

(setq inhibit-startup-message t)
(global-linum-mode)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "C-c n") 'iwb)

(electric-pair-mode t)

;; git find file
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (defun couns-git ()
    "Find file in the current Git repository."
    (interactive)
    (let* ((default-directory (locate-dominating-file
                               default-directory ".git"))
           (cands (split-string
                   (shell-command-to-string
                    "git ls-files --full-name --")
                   "\n"))
           (file (ivy-read "Find file: " cands)))
      (when file
        (find-file file))))
  :bind ("M-o" . couns-git)
  )

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

;; projects

;; find file in project C-c p f
;; list projects C-c p p
;; search for symbol at the cursor within the project C-c p s g
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-require-project-root nil)
  )

(use-package helm
  :ensure t
  :config

  ;; helm
  (global-set-key (kbd "C-c h") 'helm-projectile)

  ;; list symbols
  (global-set-key (kbd "C-c f") 'helm-imenu)
  )


;; word wrapping
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1)
                   (visual-line-mode 1)
                   )))
;; markdown
(use-package markdown-mode
  :ensure t)

;; html
(use-package htmlize
  :ensure t)

(use-package hydra
  :ensure t)

;;Code Folding
(use-package hideshow
  :ensure t
  :bind (("C->" . my-toggle-hideshow-all)
         ("C-<" . hs-hide-level)
         ("C-;" . hs-toggle-hiding))
  :config
  ;; Hide the comments too when you do a 'hs-hide-all'
  (setq hs-hide-comments nil)
  ;; Set whether isearch opens folded comments, code, or both
  ;; where x is code, comments, t (both), or nil (neither)
  (setq hs-isearch-open 'x)
  ;; Add more here
  (setq hs-set-up-overlay
        (defun my-display-code-line-counts (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put ov 'display
                         (propertize
                          (format " ... <%d>"
                                  (count-lines (overlay-start ov)
                                               (overlay-end ov)))
                          'face 'font-lock-type-face)))))

  (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
     ;;;###autoload
  (defun my-toggle-hideshow-all () "Toggle hideshow all."
         (interactive)
         (setq my-hs-hide (not my-hs-hide))
         (if my-hs-hide
             (hs-hide-all)
           (hs-show-all)))

  (hs-minor-mode 1))

;; Auto completion
(use-package company
  :ensure t
  :bind (("C-c /". company-complete))
  :config
  (global-company-mode)
  )

;;Version Control Magit is the only thing you need when it comes to Version Control (Git)
(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status)))

;;REST support
(use-package restclient
  :ensure t)

;; web editing The web-mode is particularily good for editing HTML and JS files.
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xhtml?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2))

  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package less-css-mode
  :ensure t)

(use-package emmet-mode
  :ensure t)

;; go
(use-package go-mode
  :ensure t
  :config
  (defun my-go-mode-hook ()

    ;; Use goimports instead of go-fmt
    ;; go get golang.org/x/tools/cmd/goimports
    (setq gofmt-command "goimports")
    ;; gofmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save)
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go generate && go build -v && go test -v && go vet"))

    ;; Go oracle
    ;; go get golang.org/x/tools/cmd/guru
    ;; C-c C-o ?
    ;; < show callers to a function
    ;; > show callees to a function
    ;; s show call stack (== possible paths from main())
    ;; i show implements (== what interfaces does this thing implement?)
    ;; d describe expression (== show function params or type of identifier)
    ;; r referrers
    (go-guru-hl-identifier-mode)

    ;; prerequisite step
    ;;       go get github.com/rogpeppe/godef
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-*") 'pop-tag-mark)

    (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
    (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
    (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
    (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg
    )
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  )

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(use-package go-autocomplete
  :ensure t)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

;; before auto complete works pre requisite step -
;;     go get -u github.com/nsf/gocode
(use-package auto-complete-config
  :ensure t
  :config
  (define-key ac-mode-map (kbd "C-.") 'auto-complete)
  (ac-config-default))

;; navigation, editing ...
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (pending-delete-mode t))

