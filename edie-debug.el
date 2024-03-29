;;; edie-debug.el --- Debug tools for edwin -*- lexical-binding: t -*-

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

;; Contract macros and others.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'map)
(require 'pcase)

(defgroup edie-debug nil
  "Settings to help with debugging."
  :group 'edie)

(defcustom edie-debug nil
  "If non-nil, enable debugging features."
  :type 'boolean
  :group 'edie-debug)

(defconst edie-debug-declare-form '(edie-log edie-debug--log-setup)
  "Form to be used in `declare' to enable edie debugging.")

(defvar edie-debug--log-buffer-marker nil)
(defvar edie-debug--log-depth nil)
(defvar edie-debug--log-inhibit nil)

(defun edie-debug-clear-buffer ()
  "Clear the log buffer."
  (interactive)
  (with-current-buffer (edie-debug--log-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun edie-debug--log-buffer ()
  (if-let ((buffer (get-buffer "*edie-debug*")))
      buffer
    (with-current-buffer (get-buffer-create "*edie-debug*")
      (lisp-data-mode)
      (read-only-mode +1)
      (current-buffer))))

(defun edie-debug--log-buffer-marker ()
  (with-current-buffer (edie-debug--log-buffer)
    (let ((marker edie-debug--log-buffer-marker) )
      (when (or (not (markerp marker))
                (not (eq (current-buffer) (marker-buffer marker))))
        (setq marker (point-min-marker)
              edie-debug--log-buffer-marker marker)
        (set-marker-insertion-type marker t))
      marker)))

(defun edie-debug (tag sexp)
  (let* ((marker (edie-debug--log-buffer-marker))
         (time (format-time-string "\"[%H:%M:%S:%3N]\""))
         (edie-debug--log-depth (or (and edie-debug--log-depth (1+ edie-debug--log-depth)) 0)))
    (with-current-buffer (edie-debug--log-buffer)
      (goto-char marker)
      (let ((inhibit-read-only t))
        (unwind-protect
            (progn
              (insert
               (format "%s%s"
                       (make-string (* 2 edie-debug--log-depth) ?\ )
                       (append (list tag time) sexp)))
              (newline))
          (set-marker marker (point)))))))

(defun edie-debug--log (fname arglist fn args)
  (let* ((lst (if-let ((lst (seq-mapn #'list arglist args)))
                  lst
                (list "()")))
         result)
    (edie-debug fname lst)
    (unwind-protect
        (setq result (apply fn args))
      (edie-debug fname (append lst (list '=> result))))
    result))

(defun edie-debug--log-function (fname arglist fn args)
  (edie-debug--log fname arglist fn args))

(defun edie-debug--log-setup (f arglist enable)
  (let ((sym (intern (format "edie-debug--log--instance--%s" f))))
    (if enable
        `(advice-add
          (quote ,f)
          :around #'(lambda (fn &rest args)
                      (edie-debug--log-function
                       (quote ,f) (quote ,arglist) fn args))
          '((name . ,sym)))
      `(advice-remove (quote ,f) (quote ,sym)))))

(defmacro edie-match (expr expval)
  "Checke if EXPR match the `pcase' pattern EXPVAL."
  (if edie-debug
      `(pcase ,expr
         (,expval ,expr)
         (_ (signal 'assertion-failed (list ',expr ,expr ',expval))))
    expr))

(defmacro edie-check (&rest body)
  "Check if BODY fulfills any of the optional assertions."
  (declare (indent defun))

  (pcase-let* (((map :assert-before :assert-after :before :after) body)
               (body (thread-first
                       body
                       (map-delete :before)
                       (map-delete :assert-before)
                       (map-delete :assert-after)
                       (map-delete :after)))
               (retval (make-symbol "retval")))
    (if edie-debug
        `(progn
           ,before
           ,(and assert-before `(edie-match ,assert-before (pred identity)))
           (let ((,retval (progn ,@body)))
             ,(and assert-after `(edie-match ,assert-after (pred identity)))
             ,after
             ,retval))
      `(progn ,@body))))

(cl-pushnew edie-debug-declare-form defun-declarations-alist)

(provide 'edie-debug)
;;; edie-debug.el ends here
