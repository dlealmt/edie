;;; edie-redshift.el --- Control display color temperature -*- lexical-binding: t -*-

;; Copyright (C) 2022 David Leal

;; Author: David Leal <dleal@mojotech.com>
;; Maintainer: David Leal <dleal@mojotech.com>
;; Created: 2022

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

;; Edie-Redshift is a very thin wrapper for the `redie-redshift' program, and as such
;; requires `redie-redshift' to be installed.

;; Start by customizing `edie-redshift-latitude' and `edie-redshift-longitude'. Then
;; start `edie-redshift-mode'.

;;; Code:

(defgroup edie-redshift nil
  "Color temperature changer settings."
  :group 'edie
  :prefix "edie-redshift-")

(defcustom edie-redshift-latitude 0.0
  "The latitude used to detemine when to change the color temperature."
  :type 'float)

(defcustom edie-redshift-longitude 0.0
  "The longitude used to detemine when to change the color temperature."
  :type 'float)

(defcustom edie-redshift-program "redie-redshift"
  "The path to redie-redshift."
  :type 'string)

(defvar edie-redshift--process nil)

;;;###autoload
(define-minor-mode edie-redshift-mode
  nil
  :global t
  (if edie-redshift-mode
      (unless (process-live-p edie-redshift--process)
        (setq edie-redshift--process (edie-redshift--run)))
    (kill-process edie-redshift--process)))

(defun edie-redshift--run ()
  (start-process "edie-redshift" "*edie-redshift*"
                 edie-redshift-program
                 "-l" (format "%f:%f" edie-redshift-latitude edie-redshift-longitude)))

(provide 'edie-redshift)
;;; edie-redshift.el ends here
