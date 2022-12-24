;;; edie-wm-openbox.el --- Openbox backend for edie-wm -*- lexical-binding: t -*-

;; Copyright (C) 2022 David Leal

;; Author: David Leal <dleal@mojotech.com>
;; Maintainer: David Leal <dleal@mojotech.com>
;; Created: 2022
;; Version: 0.0.1

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

;;

;;; Code:

(require 'edie-wm-x11)

(defvar edie-wm-openbox--process nil)

(defun edie-wm-openbox-start (&rest args)
  (setq edie-wm-openbox--process (start-process "edie-wm-wm" "*edie-wm-wm*" "openbox"))

  (edie-wm-x11-mode +1)

  (apply #'edie-wm-configure args))

(defun edie-wm-openbox-reconfigure ()
  (interactive)
  (call-process "openbox" nil 0 nil "--reconfigure"))

(cl-defun edie-wm-openbox-configure (&key default-desktop-list)
  (when default-desktop-list
    (edie-wm-x11-wm-set-desktops default-desktop-list)))

(provide 'edie-wm-openbox)
;;; edie-wm-openbox.el ends here
