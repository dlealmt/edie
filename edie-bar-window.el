;;; edie-bar-window.el --- Window widget for Edie bar -*- lexical-binding: t -*-

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

(cl-defmethod edie-widget-render ((_ (head window)))
  ""
  (declare (edie-log nil))
  (edie-widget-add-update-hook 'edie-wm-window-focus-changed-hook
                               'edie-wm-window-updated-hook
                               'edie-wm-window-closed-hook)

  `(box nil
     (text nil ,(if-let ((window (edie-wm-current-window)))
                    (edie-wm-property window 'title)
                  ""))))

(provide 'edie-bar-window)
;;; edie-bar-window.el ends here
