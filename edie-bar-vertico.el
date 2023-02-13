;;; edie-bar-vertico.el --- SVG-based extension for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 David Leal

;; Author: David Leal <dleal@mojotech.com>
;; Maintainer: David Leal <dleal@mojotech.com>
;; Created: 2022
;; Homepage: https://github.com/dleal-mojotech/edie
;; Package-Requires: ((emacs "29.1"))

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

;; This is an SVG-based extension for Vertico, meant to emulate the look
;; of dmenu.  Being SVG-based should (I hope) make it easier to control
;; the position of the elements, as well as achieve other effects that I
;; don't think are possible without the use of SVG.

;; I plan to implement a vertical version eventually, and I will likely
;; remove the dependency on Vertico in the near future.

;; Beware: this is still very incomplete.  Use only if you're willing to
;; put up with all the bugs, inconsistencies, etc.

;; How to use:

;; (require 'edie-bar-vertico)
;;
;; (add-hook 'vertico-mode-hook 'edie-bar-vertico-mode)

;;; Code:

(require 'edie-widget)
(require 'vertico)

(defcustom edie-bar-vertico-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap left-char] #'vertico-previous)
    (define-key map [remap right-char] #'vertico-next)
    map)
  "Additional bindings to help navigate the horizontal list."
  :type 'keymap)

(defcustom edie-bar-vertico-spec (lambda (s) `(text ((pad-x . 8)) ,s))
  nil
  :type 'function)

(defun edie-bar-vertico--display-candidates (candidates)
  "Display CANDIDATES horizontally."
  (with-selected-window (active-minibuffer-window)
    (let* ((char-height (frame-char-height))
           (height (/ (frame-pixel-height) (float char-height)))
           (candidates-string (string-join candidates)))
      (move-overlay vertico--candidates-ov (point-max) (point-max))
      (overlay-put vertico--candidates-ov 'after-string
                   (concat #(" " 0 1 (cursor t))
                           (edie-widget-propertize
                            (substring-no-properties candidates-string)
                            `(box nil
                              ,@(let ((elts nil))
                                  (dolist (c candidates (nreverse elts))
                                    (push (funcall edie-bar-vertico-spec c) elts))))))))))

(defun edie-bar-vertico--arrange-candidates ()
  "Arrange candidates."
  (with-selected-window (active-minibuffer-window)
    (let* ((collection (nthcdr vertico--index vertico--candidates))
           (width (edie-bar-vertico--candidates-width))
           (unit (frame-char-width))
           (candidates nil)
           (index (max 0 vertico--index))
           candidate)
      (while (and (setq candidate (pop collection)) (> width 0))
        (when (text-property-any 0 (length candidate) 'invisible t candidate)
          (setq candidate (substring candidate 0 (1- (length candidate)))))
        (push
         (string-trim
          (vertico--format-candidate
           (thread-last
             (list candidate) (funcall vertico--highlight) (car)
             (replace-regexp-in-string "[[:space:]]+" " "))
           "" "" index vertico--index))
         candidates)
        (setq index (1+ index))
        (setq width (- width (string-width candidate))))
      (nreverse candidates))))

(defun edie-bar-vertico-format-count (count)
  "Render vertico's COUNT using SVG."
  (with-selected-frame (edie-bar-frame)
    (edie-widget-propertize count `(text nil ,count))))

(defun edie-bar-vertico--candidates-width ()
  "Calculate the approximate width of candidates.  Buggy."
  (- (frame-width) (car (posn-col-row (posn-at-point (1- (point-max)))))))

(define-minor-mode edie-bar-vertico-mode
  "A global mode that tries to mimick the look of dmenu using SVG."
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
