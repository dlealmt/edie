;;; edie-widget.el --- Base library for edie widgets -*- lexical-binding: t -*-

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

(require 'dom)
(require 'map)
(require 'svg)
(require 'xml)

(eval-when-compile
  (require 'cl-lib)
  (require 'dom)
  (require 'map)
  (require 'pcase)
  (require 'subr-x))

(defcustom edie-widget-icon-padding-top 0
  nil
  :type 'natnum)

(defcustom edie-widget-icon-size 24
  nil
  :type 'natnum)

(defcustom edie-widget-text-padding-top 0
  nil
  :type 'natnum)

(defvar edie-widget--dom nil)

(defvar edie-widget-icon-directory "~/.cache/material-design/svg")

(defun edie-widget-render-to (frame &optional spec)
  ""
  (let ((spec (or spec (frame-parameter frame 'edie-bar:spec))))
    (with-selected-frame frame
      (with-current-buffer (window-buffer (frame-root-window))
        (when (not (minibuffer-prompt))
          (let* ((update (lambda () (edie-widget-render-to frame)))
                 (state (edie-widget--render-tree spec update)))
            (unless (equal state (frame-parameter frame 'edie-bar:state))
              (delete-region (point-min) (point-max))
              (let ((svg (edie-widget--render-svg state)))
                (modify-frame-parameters frame `((edie-bar:state . ,state)
                                                 (edie-bar:svg . ,svg)
                                                 (edie-bar:spec . ,spec)))
                (edie-widget--insert-image svg)
                svg))))))))

(defun edie-widget-propertize (string spec)
  (edie-widget-put-image spec 0 (length string) string))

(defun edie-widget-put-image (spec from to &optional where)
  (let* ((s (edie-widget--render-tree spec nil))
         (svg (edie-widget--render-svg s)))
    (put-text-property from to 'display (edie-widget--create-image svg) where)
    (put-text-property from to 'edie:svg svg where)
    where))

(cl-defgeneric edie-widget-render (widget _)
  ""
  widget)

(cl-defgeneric edie-widget-svg (node))

(cl-defgeneric edie-widget-width (node))

(cl-defgeneric edie-widget-height (node))

(cl-defgeneric edie-widget-x (node))

(cl-defgeneric edie-widget-y (node))

(cl-defgeneric edie-widget-child-x (parent child))

(cl-defgeneric edie-widget-child-y (parent child ))

(defun edie-widget-inner-width (node)
  (let-alist (dom-attributes node)
    (- .width (* (or .pad-x 0) 2))))

;; frame
(cl-defmethod edie-widget-width ((frame (head frame)))
  (frame-pixel-width (dom-attr frame 'frame)))

(cl-defmethod edie-widget-height ((frame (head frame)))
  (frame-pixel-height (dom-attr frame 'frame)))

(cl-defmethod edie-widget-svg ((frame (head frame)))
  (edie-widget-svg (car (edie-widget--children frame))))

(cl-defmethod edie-widget-child-x ((frame (head frame)) child)
  0)

;; box
(cl-defmethod edie-widget-width ((box (head box)))
  (if-let ((w (dom-attr box 'width)))
      w
    (let ((total 0)
          (spacing (or (dom-attr box 'spacing) 0))
          (pad-x (or (dom-attr box 'pad-x) 0))
          (pad-left (or (dom-attr box 'pad-left) 0)))
      (dolist (c (edie-widget--children box))
        (when-let ((w (edie-widget-width c)))
          (setq total (+ total w))))
      (+ total (* (1- (length (dom-children box))) spacing) (* pad-x 2) pad-left))))

(cl-defmethod edie-widget-height ((box (head box)))
  (edie-widget-height (edie-widget--parent box)))

(cl-defmethod edie-widget-x ((box (head box)))
  (edie-widget-child-x (edie-widget--parent box) box))

(cl-defmethod edie-widget-y ((_ (head box)))
  0)

(cl-defmethod edie-widget-child-x ((box (head box)) child)
  (+ (or (dom-attr box 'pad-x) 0)
     (or (dom-attr box 'pad-left) 0)
     (cond
      ((eq (dom-attr child 'align) 'right)
       (edie-widget-inner-width box))
      ((eq (dom-attr child 'align) 'center)
       (/ (edie-widget-inner-width box) 2))
      (t
       (pcase-let* ((x 0)
                    (spacing (or (dom-attr box 'spacing) 0))
                    ((seq head &rest rest) (edie-widget--children box)))
         (while (and head (not (eq head child)))
           (setq x (+ x (edie-widget-width head) spacing))
           (setq head (car rest)
                 rest (cdr rest)))
         x)))))

(cl-defmethod edie-widget-transform (node)
  (cond
   ((eq (dom-attr node 'align) 'right)
    (format "translate(-%d)" (edie-widget-width node)))
   ((eq (dom-attr node 'align) 'center)
    (format "translate(-%d)" (/ (edie-widget-width node) 2)))
   (t
    "none")))

(cl-defmethod edie-widget-svg ((node (head box)))
  ""
  (let ((width (edie-widget-width node))
        x transform)
    (edie-widget--make-svg-node
     (map-merge
      'alist
      `((width . ,width)
        (height . ,(edie-widget-height node))
        (x . ,(edie-widget-x node))
        (y . ,(edie-widget-y node))
        (transform . ,(edie-widget-transform node))))
     (edie-widget--svg-list (edie-widget--children node)))))

;; icon
(defconst edie-widget--original-icon-size 24)

(cl-defmethod edie-widget-width ((node (head icon)))
  (or (dom-attr node 'size) edie-widget-icon-size))

(cl-defmethod edie-widget-height ((node (head icon)))
  (or (dom-attr node 'size) edie-widget-icon-size))

(cl-defmethod edie-widget-x ((icon (head icon)))
  (edie-widget-child-x (edie-widget--parent icon) icon))

(cl-defmethod edie-widget-y ((icon (head icon)))
  (+ (/ (- (edie-widget-height (edie-widget--parent icon)) (edie-widget-height icon)) 2)
     edie-widget-icon-padding-top))

(cl-defmethod edie-widget-svg ((node (head icon)))
  ""
  (let ((svg (thread-last
               (format "%s.svg" (dom-attr node 'name))
               (file-name-concat edie-widget-icon-directory)
               (xml-parse-file)
               (car))))
    (dom-set-attribute svg 'width (edie-widget-width node))
    (dom-set-attribute svg 'height (edie-widget-height node))
    (dom-set-attribute svg 'x (edie-widget-x node))
    (dom-set-attribute svg 'y (edie-widget-y node))
    (when-let ((fill (dom-attr node 'color))
               (paths (dom-by-tag svg 'path)))
      (dolist (p paths)
        (dom-set-attribute p 'fill fill)))
    (dom-set-attribute
     svg 'viewBox
     (format "0 0 %d %d" edie-widget--original-icon-size edie-widget--original-icon-size))
    svg))

;; text
(cl-defmethod edie-widget-width ((text (head text)))
  (+ (* (or (dom-attr text 'pad-x) 0) 2) (* (frame-char-width) (length (dom-text text)))))

(cl-defmethod edie-widget-height ((text (head text)))
  (edie-widget-height (edie-widget--parent text)))

(cl-defmethod edie-widget-x ((text (head text)))
  (edie-widget-child-x (edie-widget--parent text) text))

(cl-defmethod edie-widget-y ((text (head text)))
  (let* ((pheight (edie-widget-height (edie-widget--parent text))))
    (+ edie-widget-text-padding-top (/ pheight 2))))

(cl-defmethod edie-widget-svg ((node (head text)))
  ""
  (let* ((string (car (edie-widget--children node)))
         (cwidth (frame-char-width))
         (from 0)
         (pad-x (or (dom-attr node 'pad-x) 0))
         (svg (edie-widget--make-svg-node `((width . ,(edie-widget-width node))
                                        (height . ,(edie-widget-height node))
                                        (x . ,(edie-widget-x node)))
                                      nil)))
    (while from
      (let* ((to (next-single-property-change from 'face string))
             (face (plist-get (text-properties-at from string) 'face))
             (fg (edie-widget--color-hex (edie-widget--face-attribute face :foreground)))
             (bg (edie-widget--color-hex (edie-widget--face-attribute face :background)))
             (family (edie-widget--face-attribute face :family))
             (substr (substring-no-properties string from to))
             (svg-substr (edie-widget--make-svg-node
                          `((x . ,(+ (* from cwidth) pad-x))
                            (width . ,(* cwidth (length substr)))
                            (height . ,(edie-widget-height node)))
                          (list
                           (dom-node 'rect `((width . "100%") (height . "100%") (fill . ,bg)))
                           (dom-node 'text `((fill . ,fg)
                                             (y . ,(edie-widget-y node))
                                             (font-family . ,family))
                                     (xml-escape-string substr))))))
        (dom-append-child svg svg-substr)
        (setq from to)))
    (cl-flet ((pad-rect (children width)
                (let ((fill (thread-first
                              children
                              (car)
                              (dom-children)
                              (car)
                              (dom-attr 'fill))))
                  (dom-node 'rect `((width . ,width) (height . "100%") (fill . ,fill))))))
      (dom-add-child-before svg (pad-rect (dom-children svg) (* pad-x 2)))
      (dom-add-child-before svg (pad-rect (reverse (dom-children svg)) "100%")))
    svg))

(defun edie-widget--face-attribute (faces attribute)
  (let ((faces (append (ensure-list faces) '(default)))
        value)
    (while (not value)
      (when-let ((face (pop faces)))
        (setq value (face-attribute-specified-or (face-attribute face attribute) nil))))
    value))

(cl-defun edie-widget--stringify (spec)
  ""
  (pcase spec
    ((pred stringp) spec)
    ((seq tag attrs &rest children)
     (format "<%s%s>%s</%s>"
             tag
             (string-join (map-apply (lambda (k v) (format " %s=\"%s\"" k v)) attrs))
             (string-join (mapcar #'edie-widget--stringify children))
             tag))
    (_ (error "Don't know how to convert `%S' to string" spec))))

(defun edie-widget--parent (node)
  (dom-parent edie-widget--dom node))

(defalias 'edie-widget--children #'dom-children)

(defun edie-widget--color-hex (name)
  (if (string-prefix-p "#" name)
      name
    (apply #'color-rgb-to-hex (nconc (color-name-to-rgb name) (list 2)))))

(defun edie-widget--create-image (svg)
  ""
  (create-image (edie-widget--stringify svg) 'svg t :scale 1))

(defun edie-widget--insert-image (svg)
  (let* ((marker (point-marker))
         (image (edie-widget--create-image svg)))
    (insert-image image)
    (dom-set-attribute svg 'image marker)))

(defun edie-widget--make-svg-node (attributes children)
  (edie-widget--make-node
   'svg
   (map-merge
    'alist
    attributes
    '((xmlns . "http://www.w3.org/2000/svg")
      (xmlns:edie . "http://github.com/dleal-mojotech/edie")))
   children))

(defun edie-widget--make-node (tag attributes children)
  (apply #'dom-node tag attributes children))

(defun edie-widget--render-tree (spec update)
  ""
  (pcase spec
    ((pred stringp) spec)
    (_
     (pcase-let (((and next (seq tag attributes &rest children)) (edie-widget-render spec update))
                 (nchildren))
       `(,tag
         ,attributes
         ,@(dolist (c children (nreverse nchildren))
             (push (edie-widget--render-tree c update) nchildren)))))))

(defun edie-widget--svg-list (nodes)
  (let (lst)
    (dolist (n nodes (nreverse lst))
      (push (edie-widget-svg n) lst))))

(defun edie-widget--render-svg (spec)
  ""
  (let ((edie-widget--dom `(frame ((frame . ,(selected-frame))) ,(copy-tree spec))))
    (edie-widget-svg edie-widget--dom)))

(provide 'edie-widget)
;;; edie-widget.el ends here
