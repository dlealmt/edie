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

(cl-defun edie-ml ((&key width height unit) spec)
  ""
  (cl-assert (and (numberp width) (numberp height)))
  (cl-assert (consp unit))

  (pcase-let* ((edie-ml-unit-x (or edie-ml-unit-x (car unit)))
               (edie-ml-unit-y (or edie-ml-unit-y (cdr unit)))
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

(defun edie-ml--face-attributes (faces attribute-list)
  ""
  (let ((faces (if (listp faces) faces (list faces)))
        (attrs nil))
    (dolist (face faces)
      (dolist (attr attribute-list)
        (when-let ((val (face-attribute-specified-or (face-attribute face attr) nil))
                   ((not (alist-get attr attrs))))
          (setf (alist-get attr attrs) val))))
    attrs))

(cl-defmethod edie-ml-parse (((_ (&key width height) body) (head text)))
  (let ((tspans nil)
        (rects nil))
    (if-let ((intervals (object-intervals body)))
        (pcase-dolist (`(,from ,to ,(map face)) intervals)
          (let* ((text-face-attrs (edie-ml--face-attributes face '(:family :foreground :height)))
                 (rect-face-attrs (edie-ml--face-attributes face '(:background)))
                 (str (substring-no-properties body from to))
                 (text-attrs nil)
                 (rect-attrs nil))
            (map-let (fill font-family font-size) text-attrs
              (when-let (((not fill))
                         (fg (alist-get :foreground text-face-attrs
                                        (face-attribute 'default :foreground))))
                (push `(fill . ,fg) text-attrs))
              (when-let (((not font-family))
                         (fam (alist-get :family text-face-attrs
                                         (face-attribute 'default :family))))
                (push `(font-family . ,fam) text-attrs))
              (when-let (((not font-size))
                         (size (alist-get :height text-face-attrs
                                          (face-attribute 'default :height))))
                (push `(font-size . ,(format "%fpt" (/ size 10.0))) text-attrs)))
            (map-let (fill) rect-attrs
              (when-let (((not fill))
                         (fg (alist-get :background rect-face-attrs
                                        (face-attribute 'default :background))))
                (push `(fill . ,fg) rect-attrs)))
            (push `(tspan ,text-attrs ,(xml-escape-string str)) tspans)
            (when rect-attrs
              (push `(rect ,(map-merge 'alist
                                       `((x . ,(* from edie-ml-unit-x))
                                         (y . 0)
                                         (width . ,(format "%dpx" (* (- to from) edie-ml-unit-x)))
                                         (height . "100%"))
                                       rect-attrs))
                    rects))))
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