;;; edie-bar.el --- A desktop bar for Edie -*- lexical-binding: t -*-

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

;; Desktop bar for Edie, an Emacs-based desktop environment.

;;; Code:

(eval-when-compile
  (require 'pcase)
  (require 'subr-x))

(require 'edie-widget)
(require 'map)
(require 'svg)

(defgroup edie-bar nil
  "Settings for Edie Bar."
  :group 'edie)

(defcustom edie-bar-spec nil
  ""
  :type 'sexp
  :group 'edie-bar)

(defcustom edie-bar-default-frame-alist
  '((border-width . 0)
    (dedicated . t)
    (horizontal-scroll-bars . nil)
    (internal-border-width . 0)
    (left-fringe . 0)
    (line-spacing . 0)
    (menu-bar-lines . 0)
    (minibuffer . only)
    (right-fringe . 0)
    (scroll-bar-height . 0)
    (scroll-bar-width . 0)
    (tool-bar-lines . 0)
    (vertical-scroll-bars . nil)
    (window-system . pgtk))
  "Default settings applied to all edie-bar frames."
  :type '(alist :key-type symbol)
  :group 'edie-bar)

(defcustom edie-bar-tick-hook nil
  "Hook run after each tick."
  :type 'hook
  :group 'edie-bar)

(defvar edie-bar--tick-timer nil)

;;;###autoload
(define-minor-mode edie-bar-mode
  nil
  :global t
  (when edie-bar--tick-timer
    (cancel-timer edie-bar--tick-timer)
    (setq edie-bar--tick-timer nil))

  (when edie-bar-mode
    (with-selected-frame default-minibuffer-frame
      (with-current-buffer (window-buffer (frame-root-window))
        (edie-widget-render-to (selected-frame) edie-bar-spec)))

    (setq edie-bar--tick-timer (run-with-timer 0 1 #'edie-bar--tick))))

(defun edie-bar--tick ()
  "Run the tick hook."
  (run-hooks 'edie-bar-tick-hook))

(cl-defun edie-bar-setup ()
  ""
  (pcase-let* ((frame-alist (append edie-bar-default-frame-alist minibuffer-frame-alist))
               (frame (make-frame frame-alist))
               ((map left top) frame-alist)
               ((map ('width (or `(text-pixels . ,width) width))) frame-alist)
               ((map ('height (or `(text-pixels . ,height) height))) frame-alist))
    (setq default-minibuffer-frame frame)

    (add-hook 'edie-wm-mode-hook
              (lambda ()
                (when (frame-visible-p frame)
                  (edie-wm-update-window
                   (edie-wm-window `((title . ,(frame-parameter frame 'name))))
                   (edie-wm-geometry `((left . ,(or left 0))
			               (top . ,(or top 0))
			               (width . ,(or width 1.0))
			               (height . ,(or height 48))
			               (workarea . screen)))))))))

(defsubst edie-bar-frame ()
  default-minibuffer-frame)

(cl-defmethod edie-widget-render (((_ attributes &rest children) (head bar)))
  ""
  `(box ,attributes ,@children))

(provide 'edie-bar)
;;; edie-bar.el ends here
