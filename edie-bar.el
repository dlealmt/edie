;;; edie-bar.el --- A desktop bar for Edie -*- lexical-binding: t -*-

;; Copyright (C) 2022 David Leal

;; Author: David Leal <dleal@mojotech.com>
;; Maintainer: David Leal <dleal@mojotech.com>
;; Created: 2022
;; Package-Requires: ((emacs "28.2"))

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

(eval-when-compile
  (require 'pcase))

(defgroup edie-bar nil
  "Settings for Edie bar."
  :group 'edie)

(defcustom edie-bar-frame-spec
  '((minibuffer . (:frame-parameters ((left . 0)
                                      (top . 0)
                                      (width . 1.0)
                                      (height . (text-pixels . 48))))))
  "Configuration for Edie bars."
  :type '(alist :key-type symbol :value-type (plist :key-type keyword))
  :group 'edie-bar)

(defcustom edie-bar-default-frame-alist
  '((horizontal-scroll-bars . nil)
    (internal-border-width . 0)
    (left-fringe . 0)
    (line-spacing . 0)
    (mode-line . nil)
    (menu-bar-lines . 0)
    (no-accept-focus . t)
    (right-fringe . 0)
    (tool-bar-lines . 0)
    (undecorated . t)
    (unsplittable . t)
    (vertical-scroll-bars . nil)
    (z-group . above)
    (sticky . t))
  "Default settings applied to all edie-bar frames."
  :type '(alist :key-type symbol)
  :group 'edie-bar)

(defvar edie-bar-frame-list nil)

(define-minor-mode edie-bar-mode
  nil
  :global t
  (when edie-bar-mode
    (let ((frames nil))
      (pcase-dolist (`(,frame-type . ,(map :frame-parameters)) edie-bar-frame-spec)
        (when (eq frame-type 'minibuffer)
          (push (make-frame-on-display x-display-name
                                       (map-merge 'alist
                                                  edie-bar-default-frame-alist
                                                  frame-parameters
                                                  '((minibuffer . only))))
                frames)))
      (setq edie-bar-frame-list (nreverse frames)))))

(defun edie-bar-resize (frame)
  ""
  (pcase-let* ((window (minibuffer-window frame))
               (buffer (window-buffer window))
               (cur-height (frame-pixel-height frame))
               (min-height (or (frame-parameter frame 'min-height-px) 0))
               (`(,_ . ,buf-height) (buffer-text-pixel-size buffer window))
               (max-height (or (frame-parameter frame 'max-height-px) (max min-height buf-height)))
               (next-height (thread-first buf-height (max min-height) (min max-height))))
    (when (/= cur-height next-height)
      (set-frame-height frame next-height nil t))))

(provide 'edie-bar)
;;; edie-bar.el ends here
