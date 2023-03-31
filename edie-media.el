;; edie-media-wp.el --- Media controls for Edie -*- lexical-binding: t -*-

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

(defcustom edie-media-mode-map (let ((map (make-sparse-keymap)))
                                 (keymap-set map "<AudioMute>" 'edie-media-volume-mute)
                                 (keymap-set map "<AudioRaiseVolume>" 'edie-media-volume-up)
                                 (keymap-set map "<AudioLowerVolume>" 'edie-media-volume-down)
                                 (keymap-set map "<AudioMicMute>" 'edie-media-microphone-mute)
                                 (keymap-set map "<MonBrightnessUp>" 'edie-media-brightness-up)
                                 (keymap-set map "<MonBrightnessDown>" 'edie-media-brightness-down)
                                 map)
  nil
  :type 'keymap)

(define-minor-mode edie-media-mode
  nil
  :global t
  :keymap edie-media-mode-map)

(defun edie-media-volume-mute ()
  ""
  (interactive)
  (call-process "amixer" nil "*edie-media*" nil "sset" "Master" "toggle"))

(defun edie-media-volume-up ()
  ""
  (interactive)
  (call-process "amixer" nil "*edie-media*" nil "sset" "Master" "5%+"))

(defun edie-media-volume-down ()
  ""
  (interactive)
  (call-process "amixer" nil "*edie-media*" nil "sset" "Master" "5%-"))

(defun edie-media-microphone-mute ()
  ""
  (interactive)
  (call-process "amixer" nil "*edie-media*" nil "sset" "Capture" "toggle"))

(defun edie-media-brightness-up ()
  ""
  (interactive)
  (call-process "brightnessctl" nil "*edie-media*" nil "set" "5%+"))

(defun edie-media-brightness-down ()
  ""
  (interactive)
  (call-process "brightnessctl" nil "*edie-media*" nil "set" "5%-"))

(provide 'edie-media)
;;; edie-media-wp.el ends here
