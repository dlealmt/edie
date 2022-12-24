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
