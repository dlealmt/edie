;;; edie-keys.el --- Global key binding management. -*- lexical-binding: t -*-

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

;;

;;; Code:

(defgroup edie-keys nil
  ""
  :group 'edie)

(defcustom edie-keys-mode-map (make-sparse-keymap)
  ""
  :type 'keymap)

(define-minor-mode edie-keys-mode
  "Handle global key shortcuts."
  :global t
  :keymap edie-keys-mode-map)

(defun edie-keys-dispatch (keyseq)
  "Queue an external key sequence KEYSEQ to be processed by Emacs."
  (let* ((window-system initial-window-system))
    (setq unread-command-events (listify-key-sequence (kbd keyseq)))))

(provide 'edie-keys)
;;; edie-keys.el ends here
