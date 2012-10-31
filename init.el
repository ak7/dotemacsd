;; MANUAL SETUP STEPS
;; ------------------
;; python IDE setup - https://github.com/jorgenschaefer/pyde
;; install steps
;; 1. easy_install --user rope ropemode ropemacs
;; 2. git clone https://github.com/pinard/Pymacs.git
;;    cd Pymacs
;;    make
;;    python setup.py install --user


(require 'cl)

(defalias 'yes-or-no-p 'y-or-n-p)

;; seems to be needed for starter-kit
(require 'hippie-exp)

;; stop splash screen
(setq inhibit-splash-screen t)

;; set theme
(load-theme 'wombat t)

;; initialize package.el
(require 'package)
(package-initialize)

;; add marmalade repos
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; load packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-bindings starter-kit-js starter-kit-lisp starter-kit-ruby auto-complete python-mode pymacs)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; auto complete setup
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(ac-config-default)



