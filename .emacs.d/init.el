;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(set-language-environment 'utf-8)

(unless package-archive-contents (package-refresh-contents))

(defvar my/favorite-packages
  '(
    ag
    atom-one-dark-theme
    auto-complete
    elixir-mode
    flycheck
    flycheck-pos-tip
    git-gutter
    haml-mode
    haskell-mode
    helm
    helm-projectile
    js2-mode
    markdown-mode
    multiple-cursors
    powerline
    projectile
    rubocop
    ruby-mode
    web-mode
    yaml-mode
    ))

(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Theme
(load-theme 'atom-one-dark t)

;; Line numbers
(global-display-line-numbers-mode)

(require 'ag)

(require 'auto-complete)
(ac-config-default)

(require 'flycheck)
(global-flycheck-mode)
(flycheck-pos-tip-mode)

(require 'git-gutter)
(global-git-gutter-mode t)

(require 'haml-mode)

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(require 'helm-config)
(helm-mode 1)

(require 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'powerline)
(powerline-center-theme)

(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

(require 'ruby-mode)
(setq ruby-insert-encoding-magic-comment nil)

(require 'web-mode)

(require 'whitespace)
(setq whitespace-style '(face spaces space-mark tabs tab-mark trailing))
(global-whitespace-mode t)

(set-face-attribute 'whitespace-space nil
                    :background "#282C34"
                    :foreground "#3E4451"
                    :weight 'bold)
(set-face-attribute 'whitespace-tab nil
                    :background "#282C34"
                    :foreground "#3E4451"
                    :underline nil)
(set-face-attribute 'whitespace-trailing nil
                    :background "#282C34"
                    :foreground "#3E4451"
                    :underline t)

(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (font-spec :family "ヒラギノ角ゴシック" :size 12))

(require 'yaml-mode)

(projectile-mode)
(helm-projectile-on)

(setq helm-mini-default-sources
      '(helm-source-projectile-files-list
        helm-source-projectile-projects
        helm-source-recentf))

(setq make-backup-files nil)

(setenv "PATH" (format "%s:%s" (getenv "PATH") "/usr/local/bin"))
(setenv "PATH" (format "%s:%s" (getenv "PATH") "~/.rbenv/shims"))
(setq exec-path (split-string (getenv "PATH") ":"))

;; Keybindings
(define-key global-map (kbd "C-;")     'helm-mini)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "s-p")     'helm-projectile)
(define-key global-map (kbd "s-F")     'helm-projectile-ag)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-t") 'other-window-or-split)

;; Startup Message
(progn
  (setq inhibit-startup-message t)
  (setq initial-scratch-message "")
  (setq inhibit-splash-screen t))

;; Toolbar
(tool-bar-mode 0)

;; Scrollbar / Scrolling
(scroll-bar-mode 0)
(setq scroll-conservatively 1)
(setq
 mouse-wheel-scroll-amount '(1 ((shift) . 2) ((control)))
 mouse-wheel-progressive-speed nil)

;; Fullscreen
(set-frame-parameter nil 'fullscreen 'maximized)

;; Sound
(setq ring-bell-function 'ignore)

;; Column
(column-number-mode t)

;; Line Spacing
(setq-default line-spacing 3)

;; Cursor
(setq-default cursor-type 'bar)

;; Tabs
(setq-default tab-width 2 indent-tabs-mode nil)

;; Highlight (Current Line)
(global-hl-line-mode t)

;; Highlight (Active Region)
(transient-mark-mode t)

;; Background Color (Active Region)
(set-face-background 'region "#555")

;; Path
(setq frame-title-format "%f")

;; Reverting
(global-auto-revert-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
