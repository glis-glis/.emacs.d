;;; init.el --- Initialization file for Emacs

;;; Commentary:

;; Copyright (C) 2021 Andreas Füglistaler
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

;;; Code:

(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory))

(require 'settings)
(require 'packages)

(provide 'init)
;;; init.el ends here
