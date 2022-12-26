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
(require 'xdg)

(defcustom edie-wm-openbox-theme-filename
  (file-name-concat (xdg-data-home) "themes" "Edie" "openbox-3" "themerc")
  "Path to Openbox's theme file."
  :type 'file)

(defcustom edie-wm-openbox-theme-alist
  '(("window.handle.width" . 0)
    ("border.width" . edie-wm-window-border-width)
    ("window.active.border.color" . edie-wm-window-active-border-color)
    ("window.inactive.border.color" . edie-wm-window-inactive-border-color))
  "Alist with option names and where to get their values."
  :type '(alist :key-type string :value-type (choice number symbol)))

(defvar edie-wm-openbox--process nil)

(defun edie-wm-openbox-start ()
  (edie-wm-openbox--write-configuration)

  (setq edie-wm-openbox--process (start-process "edie-wm-wm" "*edie-wm-wm*" "openbox"))


  (edie-wm-x11-mode +1)

  (when edie-wm-default-desktop-list
    (edie-wm-x11-wm-set-desktops edie-wm-default-desktop-list)))

(defun edie-wm-openbox--normalize-color (color)
  (apply #'color-rgb-to-hex (append (color-name-to-rgb color) (list 2))))

(defun edie-wm-openbox--write-theme ()
  "Write Openbox's theme configuration."
  (with-temp-buffer
    (pcase-dolist (`(,str . ,thing) edie-wm-openbox-theme-alist)
      (insert str ": "
              (cond
               ((and (symbolp thing) (eq (get thing 'custom-type) 'color))
                (edie-wm-openbox--normalize-color (symbol-value thing)))
               ((and (symbolp thing) (eq (get thing 'custom-type) 'edie-wm-unit))
                (format "%d" (symbol-value thing)))
               ((numberp thing)
                (format "%d" thing))
               (t (format "%s" thing)))
              "\n"))
    (set-visited-file-name edie-wm-openbox-theme-filename)
    (set-buffer-modified-p t)
    (save-buffer)
    (kill-buffer)))

(defun edie-wm-openbox--write-configuration ()
  ""
  (edie-wm-openbox--write-theme))

(defun edie-wm-openbox-reconfigure ()
  ""
  (interactive)
  (edie-wm-openbox--write-configuration)
  (call-process "openbox" nil 0 nil "--reconfigure"))

(provide 'edie-wm-openbox)
;;; edie-wm-openbox.el ends here
