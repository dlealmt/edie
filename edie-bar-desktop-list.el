;;; edie-bar-desktop-list.el --- Desktop list widget for Edie bar -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 David Leal

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

(require 'edie-wm)

(defcustom edie-bar-desktop-list-icon-color-active "#f97316"
  "The color of an icon symbolizing the current desktop."
  :type 'color)

(defcustom edie-bar-desktop-list-icon-color-used "#fdba74"
  "The color of an icon symbolizing a desktop containing windows."
  :type 'color)

(defcustom edie-bar-desktop-list-icon 'circle
  "Name of the icon used to symbolize a desktop."
  :type 'symbol)

(edie-widget-define desktop-list
  :hook edie-wm-desktop-focus-changed-hook

  :state
  (lambda (_ _)
    (let ((monitors (edie-wm-monitor-list))
          (windows (edie-wm-window-list)))
      (mapcar (lambda (desktop)
                (or
                 (and (seq-find (lambda (mon)
                              (equal (edie-wm-property mon 'focused-desktop)
                                     (edie-wm-property desktop 'id)))
                                monitors)
                      1)
                 (and (seq-find (lambda (wnd)
                              (equal (edie-wm-property wnd 'desktop)
                                     (edie-wm-property desktop 'id)))
                                windows)
                      t)))
              (edie-wm-desktop-list))))

  :render
  (pcase-lambda ((map spacing icon icon-size) desktops)
    `(box ((spacing . ,(or spacing 8)))
          ,@(mapcar (lambda (dsk)
                      `(icon ((name . ,icon)
                              (size . ,icon-size)
                              (color . ,(cond
                                         ((eq dsk t)
                                          edie-bar-desktop-list-icon-color-used)
                                         ((numberp dsk)
                                          edie-bar-desktop-list-icon-color-active))))))
                    desktops))))

(provide 'edie-bar-desktop-list)
;;; edie-bar-desktop-list.el ends here
