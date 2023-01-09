;;; edie-bar.el --- A desktop bar for Edie -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 David Leal

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

(require 'svg)

(defgroup edie-bar nil
  "Settings for Edie bar."
  :group 'edie)

(defcustom edie-bar-default-frame-alist
  '((border-width . 0)
    (dedicated . t)
    (horizontal-scroll-bars . nil)
    (internal-border-width . 0)
    (left-fringe . 0)
    (line-spacing . 0)
    (mode-line . nil)
    (menu-bar-lines . 0)
    (no-accept-focus . t)
    (right-fringe . 0)
    (scroll-bar-height . 0)
    (scroll-bar-width . 0)
    (skip-pager . t)
    (skip-taskbar . t)
    (tool-bar-lines . 0)
    (undecorated . t)
    (unsplittable . t)
    (vertical-scroll-bars . nil)
    (z-group . above)
    (sticky . t))
  "Default settings applied to all edie-bar frames."
  :type '(alist :key-type symbol)
  :group 'edie-bar)

(defvar edie-bar-frame nil)

;;;###autoload
(define-minor-mode edie-bar-mode
  nil
  :global t
  (if edie-bar-mode
      (progn
        (add-to-list 'set-message-functions #'edie-bar-set-message t)
        (setq command-error-function #'edie-bar-command-error)
        (add-function :filter-args completing-read-function #'edie-bar-svg-prompt))
    (setq set-message-functions (delq 'edie-bar-set-message set-message-functions))
    (setq command-error-function #'command-error-default-function)
    (remove-function completing-read-function #'edie-bar-svg-prompt)))

(defun edie-bar-setup ()
  (setq minibuffer-frame-alist
        (map-merge 'alist minibuffer-frame-alist edie-bar-default-frame-alist)))

(defun edie-bar--set-bar ()
  ""
  (setq edie-bar-frame default-minibuffer-frame))

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

(defun edie-bar-set-message (message)
  ""
  (with-selected-frame edie-bar-frame
    (propertize (substring-no-properties message) 'display (edie-ml-render nil `(text ,message)))))

(defun edie-bar-command-error (data _ _)
  ""
  (message "%s" (error-message-string data)))

(cl-defun edie-bar-svg-prompt ((str &rest args))
  (with-selected-frame edie-bar-frame
    (append
     (list (propertize (substring-no-properties str)
                       'display (edie-ml-render `(:width ,(length str)) `(text ,str))))
     args)))

(provide 'edie-bar)
;;; edie-bar.el ends here
