;;; edie-bar-minibuffer.el --- A desktop bar for Edie -*- lexical-binding: t -*-

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

;; Render minibuffer contents using SVG, for more control.

;;; Code:

(require 'edie-widget)

(defgroup edie-bar-minibuffer nil
  "Settings for Edie bar."
  :group 'edie-bar)

(defcustom edie-bar-minibuffer-message-spec (lambda (s) `(text nil ,s))
  "The function used to render a minibuffer/echo area message."
  :type 'sexp)

(defcustom edie-bar-minibuffer-prompt-spec (lambda (s) `(text nil ,s))
  "The function used to render a minibuffer prompt."
  :type 'sexp)

;;;###autoload
(define-minor-mode edie-bar-minibuffer-mode
  "Render minibuffer contents using SVG."
  :global t
  (if edie-bar-minibuffer-mode
      (progn
        (add-to-list 'set-message-functions #'edie-bar-minibuffer-set-message t)
        (setq command-error-function #'edie-bar-minibuffer-command-error)
        (advice-add #'read-string :filter-args #'edie-bar-minibuffer-svg-prompt)
        (advice-add #'read-string :around #'edie-bar-minibuffer--svg-input-advice)
        (advice-add #'read-from-minibuffer :filter-args #'edie-bar-minibuffer-svg-prompt)
        (advice-add #'read-from-minibuffer :around #'edie-bar-minibuffer--svg-input-advice))
    (setq set-message-functions (delq 'edie-bar-minibuffer-set-message set-message-functions))
    (setq command-error-function #'command-error-default-function)
    (advice-remove #'read-string #'edie-bar-minibuffer--svg-input-advice)
    (advice-remove #'read-string #'edie-bar-minibuffer-svg-prompt)
    (advice-add #'read-from-minibuffer :around #'edie-bar-minibuffer--svg-input-advice)
    (advice-remove #'read-from-minibuffer #'edie-bar-minibuffer-svg-prompt)))

(defun edie-bar-minibuffer-set-message (message)
  "Render MESSAGE.

See `set-message-functions.'"
  (with-selected-frame default-minibuffer-frame
    (edie-widget-propertize message (funcall edie-bar-minibuffer-message-spec message))))

(defun edie-bar-minibuffer-command-error (data _ _)
  "Render DATA as an error message.

See `command-error-function.'"
  (message "%s" (error-message-string data)))

(cl-defun edie-bar-minibuffer-svg-prompt ((str &rest args))
  "Transform STR into an SVG image and pass it on, along with ARGS."
  (with-selected-frame default-minibuffer-frame
    (nconc (list (edie-widget-propertize str (funcall edie-bar-minibuffer-prompt-spec str))) args)))

(defun edie-bar-minibuffer--svg-input-advice (&rest args)
  "Advice for read functions.

Setup minibuffer and forward ARGS."
  (minibuffer-with-setup-hook
      (lambda ()
        (add-hook 'post-command-hook #'edie-bar-minibuffer--svg-input nil 'local))
    (apply args)))

(defun edie-bar-minibuffer--svg-input ()
  "Replace input with the corresponding SVG representation."
  (let* ((begin (minibuffer-prompt-end))
         (count (1+ (- (buffer-size) begin)))
         (end (+ begin count)))
    (dotimes (i count)
      (let ((from (+ i begin))
            (to (+ 1 i begin)))
        (put-text-property from to 'invisible t)
        (edie-widget-put-image `(text nil ,(buffer-substring from to)) from to)))))

(provide 'edie-bar-minibuffer)
;;; edie-bar-minibuffer.el ends here
