;;; edie-run.el --- Extensions for launching apps -*- lexical-binding: t -*-

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

(eval-when-compile
  (require 'cl-lib))

(require 'edie-wm)

(defcustom edie-run-proxy-frame-name "_edie-run_"
  nil
  :type 'string)

(defvar edie-run-filter-list nil)
(defvar edie-run--driver-frame nil)

;;;###autoload
(define-minor-mode edie-run-mode
  nil
  :global t
  :group 'edie
  (if edie-run-mode
      (progn
        (unless (frame-live-p edie-run--driver-frame)
          (edie-run--ensure-driver-frame))
        (add-hook 'edie-wm-window-close-functions #'edie-run-window-close))
    (when (frame-live-p edie-run--driver-frame)
      (delete-frame edie-run--driver-frame))
    (remove-hook 'edie-wm-window-close-functions #'edie-run--on-window-close)))

(defvar edie-run-frame-alist
  '((alpha . 20)
    (height . 0)
    (horizontal-scroll-bars . nil)
    (internal-border-width . 0)
    (left-fringe . 0)
    (left . 0)
    (min-height . 0)
    (min-width . 0)
    (no-accept-focus . t)
    (no-other-frame . t)
    (right-fringe . 0)
    (skip-taskbar . t)
    (top . 0)
    (undecorated . t)
    (unsplittable . t)
    (vertical-scroll-bars . nil)
    (width . 0)
    (z-group . below)))

(defmacro edie-run-with-global-context (&rest body)
  ""
  (declare (indent defun))
  (let ((dir (make-symbol "dir"))
        (prev-window (make-symbol "prev-window")))
    `(let* ((,prev-window (edie-wm-current-window))
            (,dir (with-current-buffer (current-buffer)
                    default-directory)))
       (edie-run--ensure-driver-frame)
       (edie-wm-focus-window (edie-wm-window `((title . ,edie-run-proxy-frame-name))))

       (with-selected-frame edie-run--driver-frame
         (with-current-buffer (window-buffer (frame-root-window))
           (setq-local default-directory ,dir)

           (unwind-protect
               (progn
                 (hack-dir-local-variables-non-file-buffer)
                 ,@body)
             (when ,prev-window
               (edie-wm-focus-window ,prev-window))))))))

(defun edie-run-once (filters command &rest command-args)
  ""
  (declare (edie-log t))
  (cl-pushnew filters edie-run-filter-list :test #'equal)

  (if-let ((window (edie-wm-window filters)))
      (edie-wm-update-window window `((hidden . nil)
                                      (focus . t)
                                      (desktop . ,(edie-wm-property (edie-wm-current-desktop) 'id))))
    (apply command command-args)))

(defun edie-run--ensure-driver-frame ()
  "Make frame showing the input buffer."
  (if (frame-live-p edie-run--driver-frame)
      edie-run--driver-frame
    (setq edie-run--driver-frame
          (window-frame
           (display-buffer
            (edie-run--input-buffer)
            `((display-buffer-reuse-window display-buffer-pop-up-frame)
              (pop-up-frame-parameters
               . ,(append edie-run-frame-alist `((name . ,edie-run-proxy-frame-name))))
              (dedicated . t)
              (reusable-frames . ,edie-run--driver-frame)
              (window-parameters . ((dedicated . t)))))))))

(defun edie-run--input-buffer ()
  "Return the input buffer, creating it if it doesn't exist."
  (let* ((buffer (get-buffer-create "*edie-run*")))
    (with-current-buffer buffer
      (read-only-mode t)
      (setq-local window-min-width 0)
      (setq-local window-min-height 0)
      (setq-local frame-alpha-lower-limit 0)
      buffer)))

(defun edie-run-window-close (window)
  (when (seq-find (lambda (filter)
                    (edie-wm-window-filter-match-p filter window))
                  edie-run-filter-list)
    (prog1 t (edie-wm-update-window window `(:hidden t)))))

(provide 'edie-run)
;;; edie-run.el ends here
