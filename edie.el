;;; edie.el --- A desktop environment -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 David Leal

;; Author: David Leal <dleal@mojotech.com>
;; Maintainer: David Leal <dleal@mojotech.com>
;; Created: 2022
;; Version: 0.1.0
;; Homepage: https://github.com/dleal-mojotech/edie
;; Package-Requires: ((emacs "28.1") (xelb "0.18"))

;; This file is part of Edie.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Edie is an Emacs-based desktop environment.

;;; Code:

(require 'edie-bar-vertico)
(require 'edie-debug)
(require 'edie-keys)
(require 'edie-redshift)
(require 'edie-run)
(require 'edie-wallpaper)
(require 'edie-wm)

(defgroup edie nil
  "Settings related to Edie and its components."
  :group 'x)

(defcustom edie-after-init-hook nil
  "Hook run when Emacs' `after-init-hook' runs."
  :type 'hook
  :group 'edie)

(defcustom edie-startup-hook nil
  "Hook run when Emacs' `emacs-startup-hook' runs."
  :type 'hook
  :group 'edie)

;;;###autoload
(define-minor-mode edie-mode
  nil
  :global t
  (when edie-mode
      (progn
        (add-hook 'after-init-hook #'edie-bar-set-bar)
        (add-hook 'after-init-hook #'edie-wm-mode 0)
        (add-hook 'after-init-hook #'edie-wallpaper-mode 0)
        (add-hook 'emacs-startup-hook #'edie-keys-mode 0)
        (add-hook 'emacs-startup-hook #'edie-redshift-mode 90)
        (add-hook 'emacs-startup-hook #'edie-run-mode 90)
        (add-hook 'emacs-startup-hook #'edie-bar-mode 90))))

(defun edie--after-init ()
  (run-hooks 'edie-after-init-hook))

(defun edie--startup ()
  (run-hooks 'edie-startup-hook))

(provide 'edie)
;;; edie.el ends here
