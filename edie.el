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

(defgroup edie nil
  "Settings related to Edie and its components."
  :group 'x)

(require 'edie-bar)
(require 'edie-wm-hyprland)

(defun edie-setup ()
  (add-hook 'before-init-hook
	    (lambda ()
	      (edie-bar-setup)
	      (edie-wm-mode))))

(provide 'edie)
;;; edie.el ends here
