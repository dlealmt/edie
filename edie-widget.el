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
(require 'edie-desktop)

(eval-when-compile
  (require 'cl-lib)
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

(defvar edie-widget--setup-hooks nil)
(defvar edie-widget--install nil)

(defun edie-widget-render-to (frame spec)
  ""
  (declare (edie-log nil))

  (with-selected-frame frame
    (when spec
      (let ((widget (edie-widget-make spec)))
        (set-frame-parameter frame 'edie-bar:spec widget)
        (edie-widget-init widget frame)))

    (edie-widget--refresh-now frame)))

(defcustom edie-widget-refresh-debounce-time 0.1
  "Time in seconds before the widgets are refreshed."
  :type 'float)

(defvar edie-widget--refresh-timer nil)

(defun edie-widget--refresh (frame)
  (when (timerp edie-widget--refresh-timer)
    (cancel-timer edie-widget--refresh-timer))

  (setq edie-widget--refresh-timer
        (run-with-idle-timer edie-widget-refresh-debounce-time nil
                             #'edie-widget--refresh-now frame)))

(defun edie-widget--refresh-now (frame)
  ""
  (declare (edie-log nil))
  (with-selected-frame frame
    (with-current-buffer (window-buffer (frame-root-window))
      (when (not (minibuffer-prompt)) ;; TODO We're assuming frame is a minibuffer frame
        (let* ((state (edie-widget-render (frame-parameter frame 'edie-bar:spec))))
          (delete-region (point-min) (point-max))
          (let ((svg (edie-widget--render-svg state)))
            (modify-frame-parameters frame `((edie-bar:state . ,state) (edie-bar:svg . ,svg)))
            (edie-widget--insert-image svg)
            svg))))))

(defun edie-widget-propertize (string spec)
  (declare (edie-log nil))
  (edie-widget-put-image spec 0 (length string) string))

(defun edie-widget-put-image (spec from to &optional where)
  (let* ((widget (edie-widget-make spec))
         svg)
    (edie-widget-compute-state widget)

    (setq svg (edie-widget--render-svg (edie-widget-render widget)))

    (put-text-property from to 'display (edie-widget--create-image svg) where)
    (put-text-property from to 'edie:svg svg where)

    where))

(cl-defmacro edie-widget-define (name &key render (state nil) (every nil) (hook nil) (init nil))
  "Define widget NAME.

NAME is the symbol that names the widget.

STATE is the function used to compute the arguments for the
widget's render function.  It should accept an alist of
attributes as its single argument and return a list containing
the computed state.  This value will be compared against the
previous state of the widget, and if it is different, the new
state will be stored, and the widget will be re-rendered.

EVERY is an optional interval of time in seconds, after which the
widget state will be recomputed.

HOOK is either `nil', a hook or a list of hooks that will cause
the widget state to be recomputed, and possibly lead to
re-rendering the widget.

INIT is the function that will be called during widget
initialization.  It will be called only once per widget instance,
but since their may be multiple instances of the same widget, it
may be called more than once.

RENDER is the function that renders the widget.  It should accept
the widget attributes as its first argument, and the widget state
as its second argument, and return an EDML node that will be used
to render the widget."
  (declare (indent defun))

  (let* ((state-var (intern (format "edie-widget--%s--state" name)))
         (hook-function (intern (format "edie-widget--%s--hook" name)))
         (timer-var (intern (format "edie-widget--%s--timer" name)))
         (init-function (intern (format "edie-widget--%s--init" name)))
         (render-function (intern (format "edie-widget--%s--render" name)))
         (state-function (intern (format "edie-widget--%s--compute-state" name)))
         (timer-function (intern (format "edie-widget--%s--timer-function" name))))
    `(progn
       (defvar ,state-var nil)
       (defvar ,timer-var nil)

       (defalias ',init-function ,(or init (list 'quote 'ignore)))
       (defalias ',render-function ,render)
       (defalias ',state-function ,state)

       (cl-defmethod edie-widget-set-state ((edml (head ,name)) state)
         (let* ((widget-id (dom-attr edml 'id))
                (elt (assoc widget-id ,state-var)))
           (delq elt ,state-var)
           (push (cons widget-id state) ,state-var)))

       (cl-defmethod edie-widget-state ((edml (head ,name)))
         (let ((widget-id (dom-attr edml 'id)))
           (cdr (assoc widget-id ,state-var))))

       (cl-defmethod edie-widget-render ((edml (head ,name)))
         (edie-widget--render edml #',render-function))

       (cl-defmethod edie-widget-compute-state ((edml (head ,name)))
         (edie-widget--compute-state edml #',state-function))

       (cl-defmethod edie-widget-init ((edml (head ,name)) frame)
         (edie-widget--init edml frame #',init-function
                            ,every ',timer-function
                            ,(and hook (list 'quote hook)) ',hook-function)))))

(defun edie-widget--init (edml frame init-function every timer-function hook hook-function)
  (when init-function
    (funcall init-function))

  (edie-widget-compute-state edml)

  (defalias hook-function
    (lambda (&rest _)
      (when (edie-widget-compute-state edml)
        (edie-widget--refresh frame))))

  (dolist (h (ensure-list hook))
    (add-hook h hook-function))

  (when every
    (let ((counter 0))
      (defalias timer-function
        (lambda (&rest _)
          (if (/= counter every)
              (cl-incf counter)
            (funcall hook-function)
            (setq counter 0))))

      (add-hook 'edie-bar-tick-hook timer-function)))

  (dolist (child (dom-children edml))
    (edie-widget-init child frame)))

(defun edie-widget--compute-state (edml state-function)
  (let* ((old-state (edie-widget-state edml))
         (new-state (funcall state-function (dom-attributes edml) old-state)))
    (when (not (equal old-state new-state))
      (edie-widget-set-state edml new-state)
      t)))

(defun edie-widget--render (edml render-function)
  (let ((attrs (cons (cons 'children (dom-children edml)) (dom-attributes edml))))
    (edie-widget-render (funcall render-function attrs (edie-widget-state edml)))))

;;; Widgets
(cl-defgeneric edie-widget-render (widget)
  widget)

(cl-defgeneric edie-widget-set-state (widget state))

(cl-defgeneric edie-widget-state (widget))

(cl-defgeneric edie-widget-compute-state (_edml)
  t)

(cl-defgeneric edie-widget-make (edml)
  (pcase-let (((seq tag attributes &rest children) edml))
    (apply #'list
           tag
           (edie-widget--with-uuid attributes)
           (mapcar #'edie-widget-make children))))

(cl-defgeneric edie-widget-init (_edml _frame)
  nil)

(cl-defgeneric edie-widget-svg (node))

(cl-defgeneric edie-widget-width (node))

(cl-defgeneric edie-widget-height (node))

(cl-defgeneric edie-widget-x (node))

(cl-defgeneric edie-widget-y (node))

(cl-defgeneric edie-widget-child-x (parent child))

(cl-defgeneric edie-widget-child-y (parent child ))

(defun edie-widget-inner-width (node)
  (let-alist (dom-attributes node)
    (- (edie-widget-width node) (* (or .pad-x 0) 2))))

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
  (let ((width (dom-attr box 'width)))
    (cond
     ((floatp width)
      (round (* width (edie-widget-inner-width (dom-parent edie-widget--dom box)))))
     ((integerp width)
      width)
     ((null width)
      (let ((total 0)
            (spacing (or (dom-attr box 'spacing) 0))
            (pad-x (or (dom-attr box 'pad-x) 0))
            (pad-left (or (dom-attr box 'pad-left) 0)))
        (dolist (c (edie-widget--children box))
          (when-let ((w (edie-widget-width c)))
            (setq total (+ total w))))
        (+ total (* (1- (length (dom-children box))) spacing) (* pad-x 2) pad-left)))
     (t
      (error "Invalid width: %s" width)))))

(cl-defmethod edie-widget-height ((box (head box)))
  (or (dom-attr box 'height) (edie-widget-height (edie-widget--parent box))))

(cl-defmethod edie-widget-x ((box (head box)))
  (edie-widget-child-x (edie-widget--parent box) box))

(cl-defmethod edie-widget-y ((box (head box)))
  (or (dom-attr box 'y) 0))

(cl-defmethod edie-widget-child-x ((box (head box)) child)
  (+ (or (dom-attr box 'pad-x) 0)
     (or (dom-attr box 'pad-left) 0)
     (if (or (not (dom-attr box 'direction)) (eq (dom-attr box 'direction) 'row))
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
             x)))
       0)))

(cl-defmethod edie-widget-child-y ((box (head box)) child)
  (if (eq (dom-attr box 'direction) 'column)
      (pcase-let* ((y 0)
                   ((seq head &rest rest) (edie-widget--children box)))
        (while (and head (not (eq head child)))
          (setq y (+ y (edie-widget-height head)))
          (setq head (car rest)
                rest (cdr rest)))
        y)
    ;; TODO This needs to change when we support vertical alignment, padding, etc
    0))

(cl-defmethod edie-widget-transform (node)
  (cond
   ((eq (dom-attr node 'align) 'right)
    (format "translate(-%d)" (edie-widget-width node)))
   ((eq (dom-attr node 'align) 'center)
    (format "translate(-%d)" (/ (edie-widget-width node) 2)))
   (t
    "none")))

(cl-defmethod edie-widget-render (((_ attributes &rest children) (head box)))
  (let (chlist)
    (nconc (list 'box attributes)
           (dolist (c children (nreverse chlist))
             (when c
               (push (edie-widget-render c) chlist))))))

(cl-defmethod edie-widget-init ((box (head box)) frame)
  (dolist (child (dom-children box))
    (edie-widget-init child frame)))

(cl-defmethod edie-widget-svg ((node (head box)))
  ""
  (let ((width (edie-widget-width node)))
    (cl-assert (>= width 0))
    (edie-widget--make-svg-node
     `((width . ,width)
       (height . ,(edie-widget-height node))
       (x . ,(edie-widget-x node))
       (y . ,(edie-widget-y node))
       (transform . ,(edie-widget-transform node)))
     (edie-widget--svg-list (edie-widget--children node)))))

;; icon
(cl-defmethod edie-widget-width ((node (head icon)))
  (or (dom-attr node 'size) edie-desktop-icon-theme-scale))

(cl-defmethod edie-widget-height ((node (head icon)))
  (or (dom-attr node 'size) edie-desktop-icon-theme-scale))

(cl-defmethod edie-widget-x ((icon (head icon)))
  (edie-widget-child-x (edie-widget--parent icon) icon))

(cl-defmethod edie-widget-y ((icon (head icon)))
  (+ (/ (- (edie-widget-height (edie-widget--parent icon)) (edie-widget-height icon)) 2)
     edie-widget-icon-padding-top))

(cl-defmethod edie-widget-svg ((node (head icon)))
  ""
  (when-let ((path (edie-desktop-icon-theme-file (dom-attr node 'name)))
             (svg (car (xml-parse-file path))))

    (dom-set-attribute svg 'width (edie-widget-width node))
    (dom-set-attribute svg 'height (edie-widget-height node))
    (dom-set-attribute svg 'x (edie-widget-x node))
    (dom-set-attribute svg 'y (edie-widget-y node))

    (when-let ((fill (dom-attr node 'color))
               (paths (dom-by-tag svg 'path)))
      (dolist (p paths)
        (dom-set-attribute p 'fill fill)))
    svg))

;; text
(cl-defmethod edie-widget-width ((text (head text)))
  (+ (* (or (dom-attr text 'pad-x) 0) 2) (* (frame-char-width) (length (dom-text text)))))

(cl-defmethod edie-widget-height ((text (head text)))
  (or (dom-attr text 'height) (edie-widget-height (edie-widget--parent text))))

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
             (face-fg (edie-widget--face-attribute face :foreground))
             (face-bg (edie-widget--face-attribute face :background))
             (fg (and face-fg (edie-widget--color-hex face-fg)))
             (bg (and face-bg (edie-widget--color-hex face-bg)))
             (family (edie-widget--face-attribute face :family))
             (weight (edie-widget--face-attribute face :weight))
             (substr (substring-no-properties string from to))
             (svg-substr (edie-widget--make-svg-node
                          `((x . ,(+ (* from cwidth) pad-x))
                            (width . ,(* cwidth (length substr)))
                            (height . ,(edie-widget-height node)))
                          (list
                           (when bg
                             (dom-node 'rect `((width . "100%") (height . "100%") (fill . ,bg))))
                           (dom-node 'text
                                     (delq
                                      nil
                                      `(("xml:space" . "preserve")
                                        ,(when fg `(fill . ,fg))
                                        (y . ,(edie-widget-y node))
                                        ,(when family `(font-family . ,family))
                                        ,(when weight `(font-weight . ,weight))))
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
                  (when fill
                    (dom-node 'rect `((width . ,width) (height . "100%") (fill . ,fill)))))))
      (when-let ((node (pad-rect (reverse (dom-children svg)) "100%")))
        (dom-add-child-before svg node))
      (when-let ((node (pad-rect (dom-children svg) (* pad-x 2))))
        (dom-add-child-before svg node)))
    svg))

(defun edie-widget--face-attribute (faces attribute)
  (declare (edie-log nil))
  (seq-let (face &rest ancestors) (append (ensure-list faces) '(default))
    (if face
        (when-let ((fa (face-attribute face attribute nil ancestors))
                   ((not (equal fa (face-attribute 'default attribute nil)))))
            fa)
      (edie-widget--face-attribute ancestors attribute))))

;; string
(cl-defmethod edie-widget-make ((str string))
  str)

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
  (create-image (edie-widget--stringify svg) 'svg t :scale 1
                :mask 'heuristic :ascent 'center))

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

(defun edie-widget--svg-list (nodes)
  (let (lst)
    (dolist (n nodes (nreverse lst))
      (push (edie-widget-svg n) lst))))

(defun edie-widget--render-svg (spec)
  ""
  (declare (edie-log nil))
  (let ((edie-widget--dom `(frame ((frame . ,default-minibuffer-frame))
                                  ,(copy-tree spec))))
    (edie-widget-svg edie-widget--dom)))

(defun edie-widget--with-uuid (attributes)
  (if (alist-get 'id attributes)
      attributes
    (cons (cons 'id (edie-widget--uuid)) attributes)))

(defun edie-widget--uuid ()
  "Return string with random (version 4) UUID.

(Shamelessly stolen from `org-id-uuid'.)"
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
			  (random)
			  (current-time)
			  (user-uid)
			  (emacs-pid)
			  (user-full-name)
			  user-mail-address
			  (recent-keys)))))
    (intern (format "%s-%s-4%s-%s%s-%s"
	            (substring rnd 0 8)
	            (substring rnd 8 12)
	            (substring rnd 13 16)
	            (format "%x"
		            (logior
		             #b10000000
		             (logand
		              #b10111111
		              (string-to-number
		               (substring rnd 16 18) 16))))
	            (substring rnd 18 20)
	            (substring rnd 20 32)))))

(provide 'edie-widget)
;;; edie-widget.el ends here
