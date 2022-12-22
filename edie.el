;;; edie.el --- An Emacs-based desktop environment -*- lexical-binding: t -*-

;; Copyright (C) 2022 David Leal

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

;; Edie is an Emacs-based desktop environment.

;;; Code:

(defgroup edie nil
  "Settings related to Edie and its components.")

(defun edie-dispatch (keyseq)
  (let* ((window-system initial-window-system))
    (setq unread-command-events (listify-key-sequence (kbd keyseq)))))

(provide 'edie)
;;; edie.el ends here
