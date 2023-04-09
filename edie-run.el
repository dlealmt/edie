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
        (add-hook 'edie-wm-window-added-hook #'edie-run-try-advise-command -90)
        (add-hook 'edie-wm-window-updated-hook #'edie-run-try-advise-command -90)
        (add-hook 'edie-wm-window-focus-changed-hook #'edie-run-try-advise-command -90)
        (add-hook 'edie-wm-window-close-functions #'edie-run-window-close -90))
    (remove-hook 'edie-wm-window-added-hook #'edie-run-try-advise-command)
    (remove-hook 'edie-wm-window-updated-hook #'edie-run-try-advise-command)
    (remove-hook 'edie-wm-window-focus-changed-hook #'edie-run-try-advise-command)
    (remove-hook 'edie-wm-window-close-functions #'edie-run-window-close)))

(defun edie-run-try-advise-command ()
  (declare (edie-log nil))
  (pcase (edie-wm-window-find-rule (edie-wm-current-window))
    (`(,filters . ,(map ('single-instance (and command (pred commandp)))))
     (edie-debug 'advising-command command)
     (let ((advisor (intern (format "edie-run-advisor--%s" command))))
       (advice-remove command advisor)
       (advice-add command
                   :before-until
                   (lambda (&rest _) (edie-run-try-bring-window filters))
                   `((name . ,advisor)))))))

(defun edie-run-try-bring-window (filters)
  (when-let (window (edie-wm-window filters))
    (edie-wm-window-move-to-desktop (edie-wm-desktop-current) window)
    (edie-wm-focus-window window)))

(defun edie-run-window-single-instance-p (window)
  (let ((rule (edie-wm-window-find-rule window)))
    (map-elt (cdr rule) 'single-instance)))

(defvar-keymap edie-run-frame-mode-map
 :doc "Keymap for `edie-run-frame-mode'."
  :parent special-mode-map
  "f" '+edie-run)

(define-derived-mode edie-run-frame-mode special-mode "Edie frame"
  "Major mode for the edie-run driver frame."
  :interactive nil
  (setq buffer-read-only t)

  (setq-local window-min-width 0)
  (setq-local window-min-height 0)
  (setq-local frame-alpha-lower-limit 0)

  (hack-dir-local-variables-non-file-buffer))

(defvar edie-run-frame-alist
  '((alpha . 20)
    (height . 0)
    (horizontal-scroll-bars . nil)
    (internal-border-width . 0)
    (left-fringe . 0)
    (left . 0)
    (min-height . 0)
    (min-width . 0)
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

(defun edie-run-menu ()
  (interactive)
  (edie-run-with-global-context
    (execute-kbd-macro (kbd "SPC"))))

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
        (unless (eq major-mode 'edie-run-frame-mode)
          (edie-run-frame-mode)))
      buffer))

(defun edie-run-window-close (window)
  (declare (edie-log t))
  (when-let ((rule (edie-wm-window-find-rule window))
             ((map-elt (cdr rule) 'single-instance)))
    (prog1 t (edie-wm-update-window window `((hidden t))))))

(provide 'edie-run)
;;; edie-run.el ends here
