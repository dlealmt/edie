;;; edie-bar-clock.el --- Clock widget for edie-bar -*- lexical-binding: t -*-

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

(require 'dom)

(eval-when-compile
  (require 'edie-widget))

(defconst edie-bar-clock--numbers
  ["twelve" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven"])

(edie-widget-define clock
  :every 1

  :state
  (pcase-lambda ((map format) _)
    (format-time-string format))

  :render
  (pcase-lambda ((and properties (map icon format)) _)
    (let* ((icon-name (cond
                       ((eq icon 'moving-clock)
                        (let ((pos (% (decoded-time-hour (decode-time)) 12)))
                          (format "clock-time-%s" (aref edie-bar-clock--numbers pos))))
                       (icon
                        (symbol-name icon)) )))
      `(box ,properties
         ,(when icon-name
            `(icon ((name . ,icon-name))))
         (text nil ,(format-time-string format))))))

(provide 'edie-bar-clock)
;;; edie-bar-clock.el ends here
