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
  :straight t)

;; Needed for key bindings
(use-package general
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vim imitation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :straight t
  :delight undo-tree-mode

  :custom
  (evil-want-C-u-scroll t)

  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)

  :config
  (evil-mode 1)
  ;; Search result in center
  (defun my-center-line (&rest _)
    (evil-scroll-line-to-center nil))
  (advice-add 'evil-search-next :after #'my-center-line)
  (advice-add 'evil-search-previous :after #'my-center-line))

;; Some needed stuff for vim imitation
(use-package evil-collection
  :after evil
  :straight t
  :custom (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages without keybindings
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
;;; Packages without keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dlang
(use-package d-mode
  :straight t)


(provide 'packages)
;;; packages.el ends here
