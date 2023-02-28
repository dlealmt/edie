;;; edie-wallpaper.el --- Wallpaper slideshow thing -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 David Leal

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

;; Edie Wallpaper changes the current wallpaper at random, after a certain
;; user-defined interval, using images found in a user-defined
;; directory.  It also provides a command to delete wallpaper images
;; you no longer want.

;; By default, Edie-Wallpaper requires `nitrogen' to be installed, but you can
;; use other background setters as long as you customize `edie-wallpaper-program'
;; and `edie-wallpaper-program-args' correctly.

;; To start using Edie Wallpaper, be sure to at least set the right path to
;; your wallpaper images directory.  Then run `edie-wallpaper-mode'.

;;; Code:

(require 'edie-wallpaper)

(defcustom edie-wp-swaybg-program "swaybg"
  "Path to the swaybg program."
  :type 'file)

(defcustom edie-wp-swaybg-args '("--mode" "fill")
  "Arguments to pass to `edie-wp-swaybg-program'."
  :type '(repeat string))

(defvar edie-wp-swaybg--current-process nil
  "Current process running `edie-wp-swaybg-program'.")

(defvar edie-wp-swaybg--last-process nil
  "Previous process running `edie-wp-swaybg-program'.")

(defun edie-wallpaper-next-image-swaybg ()
  (interactive)
  (setq edie-wp-swaybg--last-process edie-wp-swaybg--current-process)
  (setq edie-wp-swaybg--current-process
        (apply #'start-process
               "edie-wp-swaybg"
               edie-wallpaper-process-buffer-name
               edie-wp-swaybg-program
               (append
                edie-wp-swaybg-args
                (list "--image" edie-wallpaper-current-image-path))))

  (run-with-idle-timer 5 nil #'edie-wp-swaybg-kill-last-process))

(defun edie-wp-swaybg-kill-last-process ()
  "Kill the last process running `edie-wp-swaybg-program'."
  (when (process-live-p edie-wp-swaybg--last-process)
    (kill-process edie-wp-swaybg--last-process)))

(provide 'edie-wp-swaybg)
;;; edie-wallpaper.el ends here
