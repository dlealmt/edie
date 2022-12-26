;;; edie-wallpaper.el --- Wallpaper slideshow thing -*- lexical-binding: t -*-

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

(require 'edie)

(defgroup edie-wallpaper nil
  "Wallpaper changer settings."
  :group 'edie
  :prefix "edie-wallpaper-")

(defcustom edie-wallpaper-wallpaper-change-interval 300
  "The time between wallpaper changes, in seconds."
  :type 'natnum)

(defcustom edie-wallpaper-program "nitrogen"
  "The name by which to invoke the background setter program."
  :type 'string)

(defcustom edie-wallpaper-program-args '("--set-zoom-fill")
  "Command line switches to pass to `edie-wallpaper-program'."
  :type '(list string))

(defcustom edie-wallpaper-image-path "~/wallpapers"
  "Path to the directory where the images are stored."
  :type 'directory)

(defvar edie-wallpaper--current-image-path nil)
(defvar edie-wallpaper--timer nil)

;;;###autoload
(define-minor-mode edie-wallpaper-mode
  "Toggle wallpaper image slideshow."
  :global t
  (when edie-wallpaper--timer
    (cancel-timer edie-wallpaper--timer))
  (when edie-wallpaper-mode
    (edie-wallpaper-next-image)))

(defun edie-wallpaper-next-image ()
  "Show a different wallpaper, picked at random.

Reset the wallpaper change timer, so that the wallpaper will be
displayed for the time set in `edie-wallpaper-wallpaper-change-interval'."
  (interactive)
  (when edie-wallpaper--timer
    (cancel-timer edie-wallpaper--timer))

  (setq edie-wallpaper--timer (run-at-time edie-wallpaper-wallpaper-change-interval nil #'edie-wallpaper-next-image))
  (setq edie-wallpaper--current-image-path (seq-random-elt (edie-wallpaper--image-list)))

  (apply #'start-process
         edie-wallpaper-program
         "*edie-wallpaper*"
         edie-wallpaper-program
         (append edie-wallpaper-program-args (list edie-wallpaper--current-image-path))))

(defun edie-wallpaper-delete-current-image ()
  "Delete the current wallpaper image.

Either delete the current wallpaper image, or move it to the
trash, depending on the setting of `delete-by-moving-to-trash'.

If the image is to be deleted, ask for confirmation first.

Show a new image afterwards."
  (interactive)
  (let ((victim edie-wallpaper--current-image-path))
    (when (or delete-by-moving-to-trash
              (yes-or-no-p "Are you sure you want to delete this wallpaper image?"))
      (delete-file victim)
      (edie-wallpaper-next-image))))

(defun edie-wallpaper--image-list ()
  "Build the the list of images in `edie-wallpaper-image-path'."
  (directory-files edie-wallpaper-image-path t "[[:alnum:]]$"))

(provide 'edie-wallpaper)
;;; edie-wallpaper.el ends here
