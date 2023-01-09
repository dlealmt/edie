;;; edie-ml.el --- Widget markup language for Edie. -*- lexical-binding: t -*-

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

;;

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'map)
  (require 'pcase))

(require 'xml)

(defvar edie-ml-icon-directory "~/.cache/material-design/svg")

(defvar edie-ml-unit-x 10.5)
(defvar edie-ml-unit-y nil)

(cl-defun edie-ml-normalize (spec)
  (pcase spec
    ((pred stringp) spec)
    ((seq tag (and attrs (guard (keywordp (car-safe attrs)))) &rest children)
     `(,tag ,attrs ,(mapcar #'edie-ml-normalize children)))
    ((seq tag &rest children)
     `(,tag nil ,(mapcar #'edie-ml-normalize children)))))

(cl-defun edie-ml ((&key (width nil) (height nil)) spec)
  ""
  (pcase-let* ((edie-ml-unit-x (or edie-ml-unit-x (frame-char-width)))
               (edie-ml-unit-y (or edie-ml-unit-y (frame-char-height)))
               (height (or (and height (* height edie-ml-unit-y)) (frame-pixel-height)))
               (width (or (and width (* width edie-ml-unit-x)) (frame-pixel-width))))
    `(svg
      ((width . ,width)
       (height . ,height)
       (version . "1.1")
       (xmlns . "http://www.w3.org/2000/svg")
       (xmlns:xlink . "http://www.w3.org/1999/xlink"))
      ,(edie-ml-parse (edie-ml-normalize spec)))))

(defun edie-ml-render (attrs spec)
  (let ((svg (edie-ml attrs spec)))
    (create-image
     (with-temp-buffer
       (insert (edie-ml--render svg))
       (buffer-string))
     'svg
     t
     :scale 1)))

(cl-defun edie-ml--render (spec)
  (pcase spec
    ((pred stringp) spec)
    ((seq tag attrs &rest children)
     (format "<%s%s>%s</%s>"
             tag
             (string-join (map-apply (lambda (k v) (format " %s=\"%s\"" k v)) attrs))
             (string-join (mapcar #'edie-ml--render children))
             tag))
    (_ (error "Don't know how to convert `%S' to string" spec))))

(cl-defgeneric edie-ml-parse (node))

;; text

(defun edie-ml--specified-face-attributes (face attribute-filter)
  ""
  (let ((all (face-all-attributes face (selected-frame)))
        (filtered nil))
    (pcase-dolist (`(,attr . ,val) all filtered)
      (when (and (not (eq val 'unspecified)) (memq attr attribute-filter))
        (setf (alist-get attr filtered) val)))))

(defun edie-ml--face-attributes-at (point str attribute-filter)
  "Get a subset of face attributes at POINT in STR.

ATTRIBUTE-FILTER is the list of face attributes that interest us.

Only returns attributes that are specified (i.e., their value is
something other than `unspecified') for the faces found at point"
  (when-let ((props (text-properties-at point str))
             (face-prop (plist-get props 'face)))
    (let ((attrs nil))
      (dolist (face (if (listp face-prop) face-prop (list face-prop)) attrs)
        (setq attrs (map-merge 'alist
                               (edie-ml--specified-face-attributes face attribute-filter)
                               attrs))))))

(defun edie-ml--face-attributes-to-svg (face-attributes)
  "Convert FACE-ATTRIBUTES to SVG presentation attributes.

The `:foreground' and `:background' attributes both map to `fill'
so if both are in FACE-ATTRIBUTES, `fill' will be overwritten."
  (let ((alist nil))
    (pcase-dolist (`(,attr . ,val) face-attributes alist)
      (cond
       ((eq attr :family) (push (cons 'font-family val) alist))
       ((eq attr :foreground) (push (cons 'fill val) alist))
       ((eq attr :height) (push (cons 'font-size (format "%fpt" (/ val 10.0))) alist))
       ((eq attr :background) (push (cons 'fill val) alist))))))

(defun edie-ml--text (tspans backgrounds)
  ""
  (let ((default-attrs (edie-ml--face-attributes-to-svg
                        (face-all-attributes 'default (selected-frame)))))
    `(g
      ((width . "100%")
       (height . "100%")
       (x . 0)
       (y . 0))
      ,@backgrounds
      (text ,(map-merge
              'alist
              default-attrs
              '((width . "100%")
                (height . "100%")
                (x . 0)
                (y . "50%")
                (dominant-baseline . "middle")
                ("xml:space" . "preserve")))
            ,@tspans))))

(defun edie-ml--text-span (string &optional attributes)
  ""
  (let* ((base-attrs (thread-last
                       '(:family :foreground :height)
                       (edie-ml--face-attributes-at 0 string)
                       (edie-ml--face-attributes-to-svg)))
         (svg-attrs (map-merge 'alist
                               `((y . "50%")
                                 (heigth . "100%")
                                 (alignment-baseline . "central"))
                               base-attrs
                               attributes)))
    (append (list 'tspan svg-attrs) (list (xml-escape-string (substring-no-properties string))))))

(defun edie-ml--text-background (string attributes)
  ""
  (let* ((default-attrs (edie-ml--face-attributes-to-svg
                         (edie-ml--specified-face-attributes 'default '(:background))))
         (base-attrs (thread-last
                       '(:background)
                       (edie-ml--face-attributes-at 0 string)
                       (edie-ml--face-attributes-to-svg)))
         (svg-attrs (map-merge 'alist
                               `((x . ,(* (alist-get 'x attributes) edie-ml-unit-x))
                                 (width . ,(* (length string) edie-ml-unit-x))
                                 (height . "100%"))
                               default-attrs
                               base-attrs
                               attributes)))
    (list 'rect svg-attrs)))

(defun edie-ml--string-to-text (string)
  ""
  (let ((point 0)
        (tspans nil)
        (backgrounds nil))
    (while point
      (let* ((next-point (next-single-property-change point 'face string))
             (string (substring string point next-point))
             (this-text (edie-ml--text-span string))
             (this-text-attrs (nth 1 this-text))
             (this-text-content (nth 2 this-text))
             (prev-text (car tspans))
             (prev-text-attrs (nth 1 prev-text))
             (prev-text-content (nth 2 prev-text))
             (this-bg (edie-ml--text-background string `((x . ,point))))
             (this-bg-attrs (nth 1 this-bg))
             (this-bg-fill (alist-get 'fill this-bg-attrs))
             (this-bg-width (alist-get 'width this-bg-attrs))
             (prev-bg (car backgrounds))
             (prev-bg-attrs (nth 1 prev-bg))
             (prev-bg-fill (alist-get 'fill prev-bg-attrs))
             (prev-bg-width (alist-get 'width prev-bg-attrs)))
        (cond
         ((not this-text)
          (error "`this-text' should always be set"))
         ((equal this-text-attrs prev-text-attrs)
          (setf (nth 2 prev-text) (concat prev-text-content this-text-content)))
         (t
          (push this-text tspans)))
        (cond
         ((not prev-bg)
          (push this-bg backgrounds))
         ((equal this-bg-fill prev-bg-fill)
          (setf prev-bg-attrs (map-put! prev-bg-attrs 'width (+ prev-bg-width this-bg-width))))
         (t
          (push this-bg backgrounds)))
        (setq point next-point)))
    (edie-ml--text (nreverse tspans) backgrounds)))

(cl-defmethod edie-ml-parse (((_ _ (body)) (head text)))
  (edie-ml--string-to-text body))

(provide 'edie-ml)
