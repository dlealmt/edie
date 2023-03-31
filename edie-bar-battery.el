;;; edie-bar-battery.el --- Battery widget for edie-bar -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 David Leal

;; Author: David Leal <dleal@mojotech.com>
;; Maintainer: David Leal <dleal@mojotech.com>
;; Created: 2022
;; Package-Requires: ((emacs "28.1"))

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

(require 'edie-widget)
(require 'battery)

(defcustom edie-bar-battery-format "%t"
  "Format for the battery widget."
  :group 'edie-bar
  :type 'string)

(edie-widget-define battery
  :hook battery-update-functions

  :init
  (lambda ()
    (display-battery-mode +1))

  :state
  (pcase-lambda (_ _)
    (funcall battery-status-function))

  :render
  (lambda (attributes state)
    (pcase-let* (((map format) attributes)
                 ((map (?p (app string-to-number charge))
                       (?B (or (and "fully-charged" full)
                               (and "charging" charging)
                               (and "discharging" discharging))))
                  state)
                 (icon (cond
                        (full
                         "battery-charging")
                        ((or charging discharging)
                         (format "battery-%d" (- charge (% charge 10)))))))
      `(box ,attributes
         (icon ((name . ,icon)))
         ,(unless full
            `(text nil ,(format-spec (or format edie-bar-battery-format) state)))))))

(provide 'edie-bar-battery)
;;; edie-bar-battery.el ends here
