;;; edie-bar-vertico.el --- SVG-based extension for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 David Leal

;; Author: David Leal <dleal@mojotech.com>
;; Maintainer: David Leal <dleal@mojotech.com>
;; Created: 2022
;; Homepage: https://github.com/dleal-mojotech/edie
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

;;

;;; Code:

(require 'edie-ml)

(defcustom edie-bar-vertico-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap left-char] #'vertico-previous)
    (define-key map [remap right-char] #'vertico-next)
    map)
  ""
  :type 'keymap)

(defun edie-bar-vertico--display-candidates (candidates)
  ""
  (with-selected-frame edie-bar-frame
    (let* ((char-height (frame-char-height))
           (width (edie-bar-vertico--candidates-width))
           (height (/ (frame-pixel-height) (float char-height)))
           (candidates-string (string-join candidates))
           (rendered (edie-ml-render `(:width ,width) `(text ,candidates-string))))
      (move-overlay vertico--candidates-ov (point-max) (point-max))
      (overlay-put vertico--candidates-ov 'after-string
                   (concat #(" " 0 1 (cursor t))
                           (propertize (substring-no-properties candidates-string)
                                       'display rendered))))))

(defun edie-bar-vertico--arrange-candidates ()
  ""
  (with-selected-frame edie-bar-frame
    (let* ((collection (nthcdr vertico--index vertico--candidates))
           (width (edie-bar-vertico--candidates-width))
           (unit (frame-char-width))
           (candidates nil)
           candidate)
      (while (and (setq candidate (pop collection)) (> width 0))
        (when (text-property-any 0 (length candidate) 'invisible t candidate)
          (setq candidate (substring candidate 0 (1- (length candidate)))))
        (setq candidate (format " %s " candidate))
        (setq width (- width (string-width candidate)))
        (push candidate candidates))
      (prog1 (setq candidates (nreverse candidates))
        (when-let ((c (car candidates)))
          (add-face-text-property 0 (length c) 'vertico-current 'append c))))))

(defun edie-bar-vertico-format-count (count)
  ""
  (with-selected-frame edie-bar-frame
    (propertize (substring-no-properties count)
                'display (edie-ml-render `(:width ,(length count)) `(text ,count)))))

(defun edie-bar-vertico--candidates-width ()
  (- (frame-width) (car (posn-col-row (posn-at-point (1- (point-max)))))))

(define-minor-mode edie-bar-vertico-mode
  nil
  :global t
  (if edie-bar-vertico-mode
      (progn
        (add-to-list 'minor-mode-map-alist `(vertico--input . ,edie-bar-vertico-map))
        (advice-add #'vertico--arrange-candidates :override #'edie-bar-vertico--arrange-candidates)
        (advice-add #'vertico--display-candidates :override #'edie-bar-vertico--display-candidates)
        (advice-add #'vertico--format-count :filter-return #'edie-bar-vertico-format-count))
    (setq minor-mode-map-alist (delete `(vertico--input . ,edie-bar-vertico-map)
                                       minor-mode-map-alist))
    (advice-remove #'vertico--arrange-candidates #'edie-bar-vertico--arrange-candidates)
    (advice-remove #'vertico--display-candidates #'edie-bar-vertico--display-candidates)
    (advice-remove #'vertico--format-count #'edie-bar-vertico-format-count)))

(provide 'edie-bar-vertico)
;;; edie-bar-vertico.el ends here
