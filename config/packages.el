;;; packages.el --- Load packages
;;; Commentary:

;; Copyright (C) 2021 Andreas Füglistaler
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages for Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; Use-package for ease of configuration
(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

;; Needed to remove minor modes from bar
(use-package delight
  :straight t
  :config
  (delight '((outline-minor-mode nil "outline")
             (abbrev-mode nil "abbrev")
             (eldoc-mode nil "ElDoc")
             (counsel-mode nil "counsel"))))

;; Needed for key bindings
(use-package general
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vim imitation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :straight t

  :custom
  (evil-want-C-u-scroll t)

  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  (evil-mode 1)

  :config
  ;; Search result in center
  (defun my-center-line (&rest _)
    (evil-scroll-line-to-center nil))
  (advice-add 'evil-search-next :after #'my-center-line)
  (advice-add 'evil-search-previous :after #'my-center-line)

  ;; leader-key
  (evil-set-leader 'insert (kbd "C-,"))
  (evil-set-leader 'normal (kbd ","))

  :general ;; General emacs- and evil- keybindings
  ("<leader>v" 'evil-window-vsplit
   "<leader>h" 'evil-window-split
   "<leader><tab>" 'evil-window-next
   "<leader><backtab>" 'evil-window-prev

   "<leader>w" 'save-buffer
   "<leader>q" 'evil-quit
   "<leader>k" 'kill-this-buffer
   "<leader>K" ;; kill all buffers
   (lambda () (interactive)
     (mapc 'kill-buffer (buffer-list)) (delete-other-windows))

   "<leader>i" 'imenu
   "<leader>f" 'ff-find-other-file
   "<leader>c" 'recompile
   "<leader>t" 'toggle-truncate-lines)
  (:states 'motion
   "gb" 'pop-global-mark))

;; Some needed stuff for vim imitation
(use-package evil-collection
  :after evil
  :straight t
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-unimpaired-p nil)
  :init
  (evil-collection-init))

;; Jump from opening keyword to closing keyword
(use-package evil-matchit
  :after evil
  :straight t
  :delight
  :init (global-evil-matchit-mode 1))

;; Do redu correctly
(use-package undo-fu
  :straight t
  :general
  (:states 'normal
   "u" 'undo-fu-only-undo
   "C-r" 'undo-fu-only-redo
   "U" 'undo-fu-only-redo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Color theme
(use-package eclipse-theme
  :straight t
  :init
  (load-theme 'eclipse t)
  (set-face-italic 'font-lock-comment-face t))

;; Show indentation with line
(use-package highlight-indent-guides
  :straight t
  :delight
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-auto-character-face-perc 30
        highlight-indent-guides-method 'character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto-complete
(use-package company
  :straight t
  :delight
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends (delete 'company-semantic company-backends))
  (setq-default company-dabbrev-downcase 0        ;; Case sensitive
                company-idle-delay 0              ;; No delay
                company-minimum-prefix-length 2)  ;; Start after 2 characters

  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-latex-commands)

  :general
  (company-active-map
   "<tab>" 'company-complete-common-or-cycle ;; cycle with tab
   "<backtab>" 'company-select-previous))

;; Minibuffer completion
(use-package ivy
  :straight t
  :delight

  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")

  :init
  (ivy-mode 1)
  
  :general
  ("<leader>b" 'ivy-switch-buffer))

(use-package swiper
  :straight t
  :general
  (:states 'motion
   "/" 'swiper))

;; Ivy-enhanced versions of common Emacs
(use-package counsel
  :straight t
  :delight
  :init
  (counsel-mode 1)
  :general
  ("<leader>e" 'counsel-find-file
   "<leader>g" 'counsel-git
   "<leader>r" 'counsel-git-grep
   "<leader>z" (lambda () (interactive) (counsel-fzf nil "~"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax checking
(use-package flycheck
  :straight t
  :delight
  :init
  (global-flycheck-mode)
  :general
  ("<leader>n" 'flycheck-next-error
   "<leader>p" 'flycheck-previous-error))

;; Format source code
(use-package clang-format
  :straight t
  :hook c-mode-common-hook
  :general
  (:states 'motion
   :keymaps 'c-mode-base-map
   "=" 'clang-format-region))


;; Dlang
(use-package d-mode
  :straight t)

;; Dlang run unittests
(use-package flycheck-d-unittest
  :after flycheck
  :after d-mode
  :straight t
  :init
  (setup-flycheck-d-unittest))

;; Lua
(use-package lua-mode
  :straight t)

;; Lsp mode
(use-package lsp-mode
  :straight t
  :config
  (add-hook 'c-mode-common-hook #'lsp)
  (add-hook 'd-mode-hook  #'lsp)
  (add-hook 'lua-mode-hook  #'lsp)
  (add-hook 'sh-mode-hook   #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  :init
  (setq lsp-modeline-diagnostics-enable nil))


(provide 'packages)
;;; packages.el ends here
