;;; settings.el --- Global Settings for emacs
;;; Commentary:

;; Copyright (C) 2021 Andreas Füglistaler
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Code:

;; (Un)set some modes
(blink-cursor-mode 0)                ;; Pease don't blink!
(tool-bar-mode -1)                   ;; No toolbar
(global-hl-line-mode t)              ;; Highlight current line
(show-paren-mode t)                  ;; Show corresponding paren
(electric-pair-mode 1)               ;; Insert closing paren when opening
(which-function-mode 1)              ;; Show name of function
(global-display-line-numbers-mode 1) ;; Show line numbers

;; Set some variables
(setq column-number-indicator-zero-based nil ;; Start columns at 1
      inhibit-startup-screen t               ;; No startup screen
      visible-bell t                         ;; blink when unhappy
      enable-local-variables nil             ;; no emacs-setting from source code
      column-number-mode t                   ;; see column numbers
      display-line-numbers-type 'relative    ;; relative line numbers
      font-use-system-font t                 ;; System font
      backup-directory-alist                 ;; Backup to here
      `(("." . "~/.emacs.d/.saves"))

      auto-save-file-name-transforms         ;; Transform backup-file-names
      `((".*" , "~/.emacs.d/.saves/" t))

      custom-file "~/.emacs.d/custom.el")   ;; Don't overwrite this init file

(put 'narrow-to-region 'disabled nil)       ;; Enable narrowing
(load custom-file 'noerror)                 ;; load custom-file as defined above

;; Set some defaults
(setq-default tab-width 8
              indent-tabs-mode t       ;; Use tabs to indent
              c-basic-offset 8         ;; 8 spaces (1 tab)
              c-default-style "linux"  ;; Better than GNU

              display-line-numbers 'visual ;;
              display-line-numbers-widen t
              display-line-numbers-current-absolute t)

;; Hooks
(add-hook 'prog-mode-hook #'outline-minor-mode)     ;; Folding
(add-hook 'prog-mode-hook 'imenu-add-menubar-index) ;; Jump to function names
(add-hook 'lisp-mode-hook
          (lambda () (setq indent-tabs-mode nil)))  ;; Spaces for lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq indent-tabs-mode nil)))  ;; Spaces for elisp
(add-hook 'python-mode-hook
          (lambda () (setq indent-tabs-mode nil     ;; Spaces for python
                           python-indent-offset 4)))
(add-hook 'c-mode-common-hook
          #'(lambda () (modify-syntax-entry ?_ "w"))) ;; Underscore does not end identifier

;; Set font to Fantsaque Mono
(set-face-attribute 'default nil
                    :family "Fantasque Sans Mono"
                    :foundry "PfEd"
                    :slant 'normal
                    :weight 'normal
                    :height 130
                    :width 'normal)

(provide 'settings)
;;; init.el ends here
