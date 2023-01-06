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

(require 'pcase)
(require 'map)

(defvar edie-ml-icon-directory "~/.cache/material-design/svg")

(defvar edie-ml-unit-x 10.5)
(defvar edie-ml-unit-y nil)

(cl-defun edie-ml-normalize (spec)
  (pcase spec
    ((pred stringp) spec)
    ((seq tag (and attrs (guard (keywordp (car-safe attrs)))) &rest children)
     `(,tag ,attrs ,@(mapcar #'edie-ml-normalize children)))
    ((seq tag &rest children)
     `(,tag nil ,@(mapcar #'edie-ml-normalize children)))))

(cl-defun edie-ml ((&key width height) spec)
  ""
  (cl-assert (and (numberp width) (numberp height)))

  (pcase-let* ((edie-ml-unit-x (or edie-ml-unit-x (frame-char-width)))
               (edie-ml-unit-y (or edie-ml-unit-y (frame-char-height)))
               ((seq tag attrs &rest children) (edie-ml-normalize spec))
               (merged-attrs (map-merge 'plist `(:width ,width :height ,height) attrs)))
    `(svg
      ((width . ,(* width edie-ml-unit-x))
       (height . ,(* height edie-ml-unit-y))
       (version . "1.1")
       (xmlns . "http://www.w3.org/2000/svg")
       (xmlns:xlink . "http://www.w3.org/1999/xlink"))
      ,@(let ((parsed (edie-ml-parse (append (list tag merged-attrs) children))))
          (if (symbolp (car-safe parsed))
              (list (car parsed))
            parsed)))))

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

(defconst pt-to-pixel-ratio 1.3333343412075)

(defun edie-ml--face-attributes (faces attribute-list &optional fallback-face)
  ""
  (let ((faces (if (listp faces) faces (list faces)))
        (attrs nil))
    (dolist (face faces)
      (dolist (attr attribute-list)
        (when (not (alist-get attr attrs))
          (setf (alist-get attr attrs) (face-attribute-specified-or
                                        (face-attribute face attr) nil)))))
    (when fallback-face
      (dolist (attr attribute-list)
        (unless (alist-get attr attrs)
          (setf (alist-get attr attrs) (face-attribute fallback-face attr)))))
    attrs))

(defun edie-ml--tspan-attributes (face)
  ""
  (map-let (:family :foreground :height)
      (edie-ml--face-attributes face '(:family :foreground :height) 'default)
    (append
     (when family `((font-family . ,family)))
     (when foreground `((fill . ,foreground)))
     (when height `((font-size . ,(format "%fpt" (/ height 10.0))))))))

(defun edie-ml--rect-attributes (face)
  ""
  (map-let (:background) (edie-ml--face-attributes face '(:background))
    (when background `((fill . ,background)))))

(cl-defmethod edie-ml-parse (((_ (&key width height) body) (head text)))
  (let ((tspans nil)
        (rects nil))
    (if-let ((intervals (object-intervals body)))
        (pcase-let* ((`(,from ,to ,(map face)) (car intervals))
                     (text-attrs (edie-ml--tspan-attributes face))
                     (text (substring-no-properties body from to))
                     (rect-attrs (edie-ml--rect-attributes face))
                     (rect-from from)
                     (str nil))
          (pcase-dolist (`(,from ,to ,(map face)) (cdr intervals))
            (setq str (substring-no-properties body from to))
            (let ((attrs (edie-ml--tspan-attributes face)))
              (if (equal text-attrs attrs)
                  (setq text (concat text str))
                (push `(tspan ,text-attrs ,(xml-escape-string text)) tspans)
                (setq text-attrs attrs)
                (setq text str)))
            (let ((attrs (edie-ml--rect-attributes face)))
              (when (not (equal rect-attrs attrs))
                (when (alist-get 'fill rect-attrs)
                  (push `(rect
                          ,(map-merge
                            'alist
                            `((x . ,(* rect-from edie-ml-unit-x))
                              (y . 0)
                              (width . ,(format "%dpx" (* (- from rect-from) edie-ml-unit-x)))
                              (height . "100%"))
                            rect-attrs))
                        rects))
                (setq rect-attrs attrs)
                (setq rect-from to))))
          (push `(tspan ,text-attrs ,(xml-escape-string text)) tspans))
      (push `(tspan nil ,body) tspans))
    (append
     (nreverse rects)
     (list `(text ((width . ,(format "%dpx" width))
                   (height . ,(format "%dpx" height))
                   (x . 0)
                   (y . "50%")
                   ("xml:space" . "preserve"))
                  ,@(nreverse tspans))))))

(provide 'edie-ml)
