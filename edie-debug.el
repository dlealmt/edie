;;; edie-debug.el --- Debug tools for edwin -*- lexical-binding: t -*-

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

;; Contract macros and others.

;;; Code:

(require 'map)
(require 'pcase)

(defgroup edie-debug nil
  "Settings to help with debugging."
  :group 'edie)

(defcustom edie-debug nil
  "Whether to enable contract checks."
  :type 'boolean)

(defcustom edie-debug-xephyr-arguments '("-br" "-ac" "-noreset")
  "Arguments to be passed to Xephyr."
  :type '(repeat string))

(defcustom edie-debug-xephyr-window-size '(1600 . 1200)
  "Size of the Xephyr window, in pixels."
  :type '(cons natnum natnum))

(defcustom edie-debug-xephyr-display-number 1
  "The display Xephyr will use when launched."
  :type 'natnum)

(defun edie-debug-try-config ()
  (interactive)
  (edie-debug-start-xephyr)
  (sleep-for 1)
  (edie-debug-start-emacs))

(defun edie-debug-start-xephyr ()
  (interactive)
  (apply #'start-process "edie-debug-xephyr" "*edie-debug*"
         "Xephyr" (append edie-debug-xephyr-arguments
                          (list "-screen"
                                (format "%dx%d"
                                        (car edie-debug-xephyr-window-size)
                                        (cdr edie-debug-xephyr-window-size))
                                (format ":%d" edie-debug-xephyr-display-number)))))

(defun edie-debug-start-emacs ()
  (interactive)
  (with-environment-variables (("DISPLAY" (format ":%d" edie-debug-xephyr-display-number)))
    (start-process "edie-debug-emacs" "*edie-debug*" "emacs" "--debug-init")))

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

(provide 'edie-debug)
;;; edie-debug.el ends here
