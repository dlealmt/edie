;;; edie-wm.el --- Tools for managing windows -*- lexical-binding: t -*-
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

(eval-when-compile
  (require 'subr-x)
  (require 'map))

(defvar edie-wm-window-close-functions nil)

(defvar edie-wm--window-list nil)

(defvar edie-wm--current-window-id nil)

(defgroup edie-wm nil
  "Window manager settings."
  :group 'edie)

(define-widget 'edie-wm-unit 'lazy
  "Zero or more pixels."
  :type '(choice natnum (const 0)))

(define-widget 'edie-wm-geometry 'lazy
  "A property list describing the size and position of a rectangle."
  :type '(alist :keyword-type (choice (const left) (const top) (const width) (const right))
                :value-type edie-wm-unit))

(defcustom edie-wm-monitor-padding-alist
  '((nil . ((left . 0) (top . 0) (bottom . 0) (right . 0))))
  "The amount of whitespace, in pixels, reserved at each edge of the desktop."
  :type '(repeat (alist :key-type symbol :value-type sexp)))

(defcustom edie-wm-window-margins 0
  "The amount of whitespace, in pixels, surrounding each window."
  :type 'edie-wm-unit)

(defcustom edie-wm-window-border-width 0
  "The width of the border surrounding every window."
  :type 'edie-wm-unit)

(defcustom edie-wm-window-active-border-color "#fe8019"
  "The color of active window borders."
  :type 'color)

(defcustom edie-wm-window-inactive-border-color "#282828"
  "The color of inactive window borders."
  :type 'color)

(defcustom edie-wm-monitor-added-hook nil
  "Normal hook run when a monitor is added."
  :type 'hook)

(defcustom edie-wm-monitor-focus-changed-hook nil
  "Normal hook run after switching active monitors."
  :type 'hook)

(defcustom edie-wm-desktop-focus-changed-hook nil
  "Normal hook run after switching virtual desktops."
  :type 'hook)

(defcustom edie-wm-window-focus-changed-hook nil
  "Normal hook run after a window takes focus."
  :type 'hook)

(defcustom edie-wm-window-added-hook nil
  "Normal hook run after a window is created and mapped."
  :type 'hook)

(defcustom edie-wm-window-updated-hook nil
  "Normal hook run after some significant window property changes."
  :type 'hook)

(defcustom edie-wm-window-rules-functions nil
  "Functions run to determine window properties."
  :type 'hook)

(defcustom edie-wm-window-closed-hook nil
  "Normal hook run after a window is closed."
  :type 'hook)

(defcustom edie-wm-rules-alist nil
  "Alist of window rules.

A rule is a cons cell of the form (FILTERS . ATTRIBUTES).

FILTERS is an alist, where each keyword corresponds to a window
attribute which will be used to match against windows.

ATTRIBUTES is an alist, where each keyword is an attribute that
will be applied to windows matched by FILTERS."
  :type '(choice (const nil)
                 (alist :key-type (alist :key-type symbol :value-type sexp)
                        :value-type (alist :key-type symbol :value-type sexp))))

(defcustom edie-wm-backend 'hyprland
  nil
  :type '(choice (const hyprland)))

(defcustom edie-wm-max-desktops-per-monitor 1024
  "The maximum number of virtual desktops per monitor."
  :type 'natnum)

(defcustom edie-wm-monitors 2
  "The number of monitors for which to generate switch/move commands."
  :type 'natnum)

(defcustom edie-wm-desktops 8
  "The number of desktops, per monitor, for which to generate switch/move commands."
  :type 'natnum)

;;;###autoload
(define-minor-mode edie-wm-mode
  nil
  :global t
  (if edie-wm-mode
      (progn
        (require (intern (format "edie-wm-%s" edie-wm-backend)))

        (edie-wm-backend-start)

        (add-hook 'edie-wm-window-added-hook #'edie-wm-apply-rules)
        (add-hook 'edie-wm-window-updated-hook #'edie-wm-apply-rules)
        (add-hook 'edie-wm-window-focus-changed-hook #'edie-wm-window-raise)
        (add-hook 'edie-wm-window-rules-functions #'edie-wm-tile-maybe-tile)
        (add-hook 'edie-wm-window-close-functions #'edie-wm-backend-window-close 95))

    (edie-wm-backend-stop)

    (remove-hook 'edie-wm-window-close-functions #'edie-wm-backend-window-close)

    (remove-hook 'edie-wm-window-focus-changed-hook #'edie-wm-window-raise)
    (remove-hook 'edie-wm-window-added-hook #'edie-wm-apply-rules)
    (remove-hook 'edie-wm-window-updated-hook #'edie-wm-apply-rules)
    (remove-hook 'edie-wm-window-rules-functions #'edie-wm-tile-maybe-tile)))

(defun edie-wm-set-properties (obj props)
  "Set PROPS on OBJ."
  (dolist (prop props obj)
    (edie-wm-set-property obj (car prop) (cdr prop))))

(defun edie-wm-set-property (obj property value)
  "Replace the value of PROPERTY on OBJ with VALUE."
  (declare (edie-log nil))
  (let ((obj-props (cdr obj)))
    (setf (alist-get property obj-props) value)
    (setcdr obj obj-props))
  obj)

(defun edie-wm-property (obj property)
  "Return the value of OBJ's PROPERTY."
  (cdr (assq property (cdr obj))))

(defun edie-wm-properties (obj &optional properties)
  "Return the values of OBJ's PROPERTIES."
  (if properties
      (let ((props nil))
        (dolist (prop properties (nreverse props))
          (push (cons prop (edie-wm-property obj prop)) props)))
    (cdr obj)))

(defun edie-wm-id (obj)
  (edie-wm-property obj 'id))

(defun edie-wm-title (obj)
  (edie-wm-property obj 'title))

(defun edie-wm-left (obj)
  (edie-wm-property obj 'left))

(defun edie-wm-right (obj)
  (+ (edie-wm-property obj 'left) (edie-wm-property obj 'width)))

(defun edie-wm-top (obj)
  (edie-wm-property obj 'top))

(defun edie-wm-bottom (obj)
  (+ (edie-wm-property obj 'height) (edie-wm-property obj 'top)))

(defun edie-wm-on-desktop-focus-change (_)
  (declare (edie-log t))
  (run-hooks 'edie-wm-desktop-focus-changed-hook))

(defun edie-wm-desktop-make (properties)
  "Create a new desktop."
  (cons 'desktop properties))

(defun edie-wm-desktop (filters)
  "Return the desktop named NAME."
  (declare (edie-log nil))
  (car (edie-wm-desktop-list filters)))

(defun edie-wm-current-desktop (&optional monitor)
  "The desktop we are currently working in."
  (declare (edie-log nil))
  (let* ((monitor (or monitor (edie-wm-current-monitor)))
        (id (edie-wm-property monitor 'focused-desktop)))
    (edie-wm-desktop id)))

(defun edie-wm-desktop-switch (desktop)
  "Switch to desktop DESKTOP.

When called interactively, prompt for the desktop we want to
switch to."
  (declare (edie-log t))
  (when (natnump desktop)
    (setq desktop (edie-wm-desktop desktop)))
  (edie-wm-backend-desktop-focus (edie-wm-property desktop 'id)
                                 (edie-wm-property desktop 'monitor))
  desktop)

(defun edie-wm-desktop-create-switch (direction)
  "Switch to desktop DESKTOP.

When called interactively, prompt for the desktop we want to
switch to."
  (declare (edie-log t))
  (let ((desktop (edie-wm-current-desktop)))
    (edie-wm-backend-desktop-focus 'crete (edie-wm-property desktop 'monitor))
    (edie-wm-current-desktop)))

(defun edie-wm-desktop-list (&optional filters)
  "The list of virtual desktops."
  (declare (edie-log nil))
  (let ((desktops (edie-wm-backend-desktop-list)))
    (pcase filters
      ((pred natnump)
       (setq filters `((id . ,filters))))
      ((seq 'monitor &rest (map id))
       (setq filters `((monitor . ,(edie-wm-property filters 'id))))))
    (if filters
        (seq-filter
         (lambda (dsk)
           (map-every-p
            (lambda (prop val)
              (equal val (edie-wm-property dsk prop)))
            filters))
         desktops)
      desktops)))

(defun edie-wm-desktop-in-monitor-p (desktop monitor)
  (= (edie-wm-property desktop 'monitor) (edie-wm-property monitor 'id)))

(defun edie-wm-select-desktop ()
  "Prompt for a desktop."
  (pcase-let* ((desktop (edie-wm-current-desktop))
               (desktops (thread-last
                           (edie-wm-desktop-list)
                           (seq-remove (lambda (dsk) (eq dsk desktop)))
                           (mapcar (lambda (dsk)
                                     (cons (edie-wm-property dsk 'name) dsk)))))
               (choice (completing-read "Desktop: " desktops)))
    (cdr (assq (intern choice) desktops))))

(defvar edie-wm--current-window nil)

(defmacro edie-wm-with-current-window (&rest body)
  "Execute BODY with WINDOW as the current window."
  (declare (indent defun))
  `(let ((edie-wm--current-window (edie-wm-backend-current-window)))
     ,@body))

(defun edie-wm-on-window-focus (_)
  (declare (edie-log t))
  (edie-wm-with-current-window
    (run-hooks 'edie-wm-window-focus-changed-hook)))

(defun edie-wm-on-window-add (_)
  (declare (edie-log t))
  (edie-wm-with-current-window
    (run-hooks 'edie-wm-window-added-hook)))

 (defun edie-wm-on-window-remove (_)
  (declare (edie-log t))
  (run-hooks 'edie-wm-window-closed-hook))

(defun edie-wm-on-window-update (_ _)
  (declare (edie-log t))
  (edie-wm-with-current-window
    (run-hooks 'edie-wm-window-updated-hook)))

(defun edie-wm-focus-window (window)
  "Focus WINDOW."
  (declare (edie-log nil))
  (edie-wm-backend-window-focus window)
  window)

(defalias 'edie-wm-window-focus 'edie-wm-focus-window)

(defun edie-wm-focus-left ()
  (interactive)
  (edie-wm-focus-direction 'left))

(defun edie-wm-focus-right ()
  (interactive)
  (edie-wm-focus-direction 'right))

(defun edie-wm-focus-up ()
  (interactive)
  (edie-wm-focus-direction 'up))

(defun edie-wm-focus-down ()
  (interactive)
  (edie-wm-focus-direction 'down))

(defun edie-wm-window-desktop (window)
  (let* ((desktop-id (edie-wm-property window 'desktop)))
    (edie-wm-desktop desktop-id)))

(defun edie-wm-window-in-direction (direction window)
  (let* ((desktop (edie-wm-window-desktop window))
         (windows (edie-wm-window-list desktop)))
    (seq-find (lambda (w)
                (pcase direction
                  ('left
                   (< (edie-wm-left w) (edie-wm-left window)))
                  ('right
                   (< (edie-wm-right window) (edie-wm-right w)))
                  ('up
                   (< (edie-wm-top window) (edie-wm-top w)))
                  ('down
                   (< (edie-wm-bottom w) (edie-wm-bottom window)))))
              windows)))

(defun edie-wm-focus-direction (direction &optional thing)
  (let ((thing (or thing (edie-wm-window-current))))
    (pcase thing
      ((seq 'window)
       (or (edie-wm-window-focus-direction direction thing)
           (edie-wm-focus-direction (edie-wm-window-desktop thing))))
      ((seq 'monitor)
       (edie-wm-monitor-focus-direction direction thing))
      ((or (pred null) (seq 'desktop))
       (and (memq direction '(left right))
            (edie-wm-desktop-focus-direction direction thing))))))

(defun edie-wm-desktop-focus-direction (direction &optional desktop)
  (if-let ((desktop (or desktop (edie-wm-current-desktop)))
           (new-desktop (edie-wm-desktop-in-direction direction desktop)))
      (edie-wm-desktop-switch new-desktop)
    (when (not (edie-wm-monitor-focus-direction direction))
      (edie-wm-desktop-create-switch direction))))

(defun edie-wm-window-focus-direction (direction &optional window)
  (if-let ((window (or window (edie-wm-current-window)))
           (new-window (edie-wm-window-in-direction direction window)))
      (edie-wm-window-focus new-window)
    (edie-wm-desktop-focus-direction direction)))

(defun edie-wm-monitor-focus-direction (direction &optional monitor)
  (when-let ((monitor (or monitor (edie-wm-current-monitor)))
             (new-monitor (edie-wm-monitor-in-direction direction monitor)))
    (edie-wm-monitor-focus new-monitor)))

(defun edie-wm-monitor-focus (monitor)
  (edie-wm-backend-monitor-focus monitor)
  monitor)

(defun edie-wm-desktop-in-direction (direction &optional desktop)
  (let* ((desktop (or desktop (edie-wm-current-desktop)))
         (monitor (edie-wm-monitor (edie-wm-property desktop 'monitor)))
         (desktops (edie-wm-desktop-list monitor))
         (desktop-pos (seq-position desktops desktop)))
    (pcase direction
      ('left
       (nth (1- desktop-pos) desktops ))
      ('right
       (nth (1+ desktop-pos) desktops )))))

(defun edie-wm-select-window ()
  "Prompt for a window."
  (let ((indexed (seq-map (pcase-lambda ((map id title))
                            (cl-assert id)
                            (cons (format "%s%s"
                                          (propertize (number-to-string id) 'invisible t)
                                          title)
                                  id))
                          (edie-wm-window-list))))
    (map-elt indexed (completing-read "Window: " indexed))))

(defun edie-wm-window-raise (&optional window)
  "Raise WINDOW."
  (declare (edie-log t))
  (edie-wm-backend-window-raise window))

(defun edie-wm-window-close (&optional window)
  "Close the active window or WINDOW."
  (declare (edie-log t))
  (interactive)
  (when-let ((window (or window (edie-wm-current-window))))
    (run-hook-with-args-until-success 'edie-wm-window-close-functions window)))

(defun edie-wm-window-to-desktop (desktop &optional window)
  "Send WINDOW to DESKTOP."
  (declare (edie-log t))
  (interactive (list (edie-wm-select-desktop)))
  (when-let ((window (or window (edie-wm-current-window))))
    (edie-wm-update-window window `((desktop . ,(edie-wm-property desktop 'id))))))

(defun edie-wm-window-to-monitor (monitor &optional window)
  "Move WINDOW to MONITOR."
  (declare (edie-log t))
  (let ((window (or window (edie-wm-current-window))))
    (edie-wm-update-window window `((monitor . (edie-wm-property monitor 'id))))))

(defun edie-wm-current-window ()
  "Return the window that is currently focused."
  (declare (edie-log nil))
  (or edie-wm--current-window (edie-wm-backend-current-window)))

(defalias 'edie-wm-window-current #'edie-wm-current-window)

(defun edie-wm-window (filters)
  "Return the first window that matches FILTERS."
  (declare (edie-log nil))
  (car (edie-wm-window-list (or (and (listp filters) filters) (list (cons 'id filters))))))

(defun edie-wm-window-list (&optional filters)
  "The list of windows across all desktops."
  (declare (edie-log nil))
  (let* ((all-windows (edie-wm-backend-window-list))
         (windows (pcase filters
                    ((seq 'desktop &rest (map id))
                     (edie-wm-window-filter-list `((desktop . ,id)) all-windows))
                    ((and (pred listp) (seq (pred consp)))
                     (edie-wm-window-filter-list filters all-windows))
                    (_
                     all-windows))))
    ;; TODO This needs to be moved to a hook
    (seq-remove (lambda (w) (equal (edie-wm-title w) "_main-bar_")) windows)))

(defun edie-wm-window-filter-list (filters &optional windows)
  "Return all windows matching FILTERS.

If WINDOWS is given, filter only that list.

For a description of supported filters and their format, see
`edie-wm-window-filter-match-p'.

Return nil or the list of windows that match the filters."
  (declare (edie-log nil))
  (seq-filter (lambda (window)
                (edie-wm-window-filter-match-p filters window))
              (or windows (edie-wm-window-list))))

(defun edie-wm-window-filter-match-p (filters &optional window)
  "Check whether WINDOW matches the given FILTERS."
  (declare (edie-log nil))
  (pcase-let (((map skip-window-list class desktop height instance left title top width) filters)
              (window (or window (edie-wm-current-window))))
    (and window
         (or (not class) (string-match-p class (edie-wm-property window 'class)))
         (or (not desktop) (equal desktop (edie-wm-property window 'desktop)))
         (or (not height) (equal height (edie-wm-property window 'height)))
         (or (not instance) (string-match-p instance (edie-wm-property window 'instance)))
         (or (not left) (equal left (edie-wm-property window 'left)))
         (or (not title) (string-match-p title (edie-wm-property window 'title)))
         (or (not top) (equal top (edie-wm-property window 'top)))
         (or (not width) (equal width (edie-wm-property window 'width))))))

(defun edie-wm-update-window (window alist)
  ""
  (declare (edie-log t))
  (when-let ((changes (edie-wm-window-merge-changes window alist)))
    (edie-wm-backend-window-update window changes)))

(defun edie-wm-window-merge-changes (window alist)
  (declare (edie-log nil))
  (let ((changes nil))
    (dolist (elt alist)
      (pcase elt
        ((and `(left . ,val) (guard (not (equal val (edie-wm-property window 'left)))))
         (setf (alist-get (car elt) changes) val)
         (unless (alist-get 'top changes)
           (setf (alist-get 'top changes) (edie-wm-property window 'top))))
        ((and `(top . ,val) (guard (not (equal val (edie-wm-property window 'top)))))
         (setf (alist-get (car elt) changes) val)
         (unless (alist-get 'left changes)
           (setf (alist-get 'left changes) (edie-wm-property window 'left))))
        ((and `(width . ,val) (guard (not (equal val (edie-wm-property window 'width)))))
         (setf (alist-get (car elt) changes) val)
         (unless (alist-get 'height changes)
           (setf (alist-get 'height changes) (edie-wm-property window 'height))))
        ((and `(height . ,val) (guard (not (equal val (edie-wm-property window 'height)))))
         (setf (alist-get (car elt) changes) val)
         (unless (alist-get 'width changes)
           (setf (alist-get 'width changes) (edie-wm-property window 'width))))
        ((and `(class . ,val) (guard (not (equal val (edie-wm-property window 'class)))))
         (setf (alist-get (car elt) changes) val))
        ((and `(instance . ,val) (guard (not (equal val (edie-wm-property window 'instance)))))
         (setf (alist-get (car elt) changes) val))
        ((and `(desktop . ,val) (guard (not (equal val (edie-wm-property window 'desktop)))))
         (setf (alist-get (car elt) changes) val))
        ((and `(title . ,val) (guard (not (equal val (edie-wm-property window 'title)))))
         (setf (alist-get (car elt) changes) val))))
    changes))

(defun edie-wm-monitor-make ()
  "Create a new monitor."
  (list 'monitor))

(defun edie-wm-monitor-focused-p (monitor)
  "Return t if MONITOR is focused."
  (edie-wm-property monitor 'focused))

(defun edie-wm-monitor-padding (monitor)
  "Return the padding of MONITOR."
  (or (cdr (assoc (edie-wm-property monitor 'name) edie-wm-monitor-padding-alist))
      (cdr (assq nil edie-wm-monitor-padding-alist))))

(defun edie-wm-current-monitor ()
  (declare (edie-log nil))
  (seq-find #'edie-wm-monitor-focused-p (edie-wm-monitor-list)))

(defun edie-wm-monitor (filters)
  "Return the first monitor that matches FILTERS."
  (declare (edie-log nil))
  (car (edie-wm-monitor-list filters)))

(defun edie-wm-monitor-list (&optional filters)
  (declare (edie-log nil))
  (let ((monitors (edie-wm-backend-monitor-list)))
    (pcase filters
      ((pred natnump)
       (setq filters `((id . ,filters)))))
    (seq-filter (lambda (mon)
                  (map-every-p (lambda (prop val)
                                 (equal val (edie-wm-property mon prop)))
                               filters))
                monitors)))

(defun edie-wm-monitor-in-direction (direction &optional monitor)
  (let* ((monitor (or monitor (edie-wm-current-monitor)))
         (monitors (edie-wm-monitor-list)))
    (seq-find (lambda (mon)
                (pcase direction
                  ('left
                   (< (edie-wm-left mon) (edie-wm-left monitor)))
                  ('right
                   (< (edie-wm-right monitor) (edie-wm-right mon)))
                  ('up
                   (< (edie-wm-top monitor) (edie-wm-top mon)))
                 ('down
                   (< (edie-wm-bottom monitor) (edie-wm-bottom mon))) ))
              monitors)))

(defun edie-wm-on-monitor-add (_)
  (declare (edie-log t))
  (run-hooks 'edie-wm-monitor-added-hook))

(defun edie-wm-on-monitor-focus-change (_)
  (declare (edie-log t))
  (run-hooks 'edie-wm-monitor-focus-changed-hook))

(defun edie-wm-on-monitor-remove (_)
  (declare (edie-log t))
  (run-hooks 'edie-wm-monitor-removed-hook))

(defun edie-wm-workarea ()
  (pcase-let* ((mon (edie-wm-current-monitor))
               ((map ('left pad-left)
                     ('top pad-top)
                     ('right pad-right)
                     ('bottom pad-bot))
                (edie-wm-monitor-padding mon))
               ((map ('left mon-left)
                     ('top mon-top)
                     ('width mon-width)
                     ('height mon-height))
                (edie-wm-screenarea)))
    `((left . ,(+ mon-left pad-left))
      (top . ,(+ mon-top pad-top))
      (width . ,(- mon-width pad-left pad-right))
      (height . ,(- mon-height pad-top pad-bot)))))

(defun edie-wm-screenarea (&optional monitor)
  (let ((mon (or monitor (edie-wm-current-monitor))))
    (edie-wm-properties mon '(left top width height))))

(defun edie-wm-geometry (alist)
  (declare (edie-log nil))
  (pcase-let* (((map ('left wa-left) ('top wa-top) ('width wa-width) ('height wa-height))
                (if (eq (cdr (assq 'workarea alist)) 'screen)
                    (edie-wm-screenarea)
                  (edie-wm-workarea)))
               ((map left top width height) alist))
    (thread-last
      `((left . ,(and left (+ wa-left (edie-wm-unit-pixel left wa-width))))
        (top . ,(and top (+ wa-top (edie-wm-unit-pixel top wa-height))))
        (width . ,(and width (edie-wm-unit-pixel width wa-width)))
        (height . ,(and height (edie-wm-unit-pixel height wa-height))))
      (map-filter (lambda (_ v) v))
      (map-merge 'alist alist)
      (edie-wm--adjust-margins))))

(defun edie-wm-unit-pixel (fraction total)
  (cl-assert (and fraction total))
  (let ((size (pcase fraction
                ((and (pred integerp) (guard (<= fraction total)))
                 fraction)
                ((and (pred floatp) (guard (<= 0.0 fraction 1.0)))
                 (* fraction total))
                ((and `(/ ,(and slice (pred numberp)) ,(and whole (pred numberp)))
                      (guard (<= slice whole)))
                 (* (/ slice (float whole)) total))
                ((seq '+ &rest operands)
                 (apply #'+ (mapcar (lambda (n) (edie-wm-unit-pixel n total)) operands))))))
    (cl-assert (numberp size))
    (truncate size)))

(defun edie-wm--adjust-margins (alist)
  (if (eq (cdr (assq 'workarea alist)) 'screen)
      alist
    (pcase-let* (((map ('left wnd-left)
                       ('top wnd-top)
                       ('width wnd-width)
                       ('height wnd-height)
                       ('margins wnd-margins))
                  alist)
                 (`(,wnd-m-left ,wnd-m-right ,wnd-m-top ,wnd-m-bot)
                  (make-list 4 (or wnd-margins edie-wm-window-margins))))
      (if (and wnd-left wnd-top wnd-width wnd-height)
          (map-merge 'alist
                     alist
                     `((left . ,(+ wnd-left wnd-m-left))
                       (top . ,(+ wnd-top wnd-m-top))
                       (width . ,(- wnd-width wnd-m-right wnd-m-left))
                       (height . ,(- wnd-height wnd-m-top wnd-m-bot))))
        alist))))

(defun edie-wm-apply-rules ()
  (declare (edie-log t))
  (when-let ((window (edie-wm-current-window)))
    (let* ((new-props nil)
           (rules (cdr (seq-find (pcase-lambda (`(,filter . ,_))
                                   (edie-wm-window-filter-match-p filter window))
                                 edie-wm-rules-alist)))
           result)
      (dolist (fun edie-wm-window-rules-functions)
        (setq result (funcall fun rules window))
        (dolist (elt result)
          (setf (alist-get (car elt) new-props) (cdr elt))))

      (when new-props
        (edie-wm-update-window window new-props)))))

(defun edie-wm-tile--gen-commands (tiles)
  (dolist (tile tiles)
    (defalias (intern (format "edie-wm-tile-fit-%s" tile))
      (lambda ()
        (interactive)
        (edie-wm-tile-fit tile)))
    (defalias (intern (format "edie-wm-tile-focus-cycle-%s" tile))
      (lambda ()
        (interactive)
        (edie-wm-tile-focus-cycle tile)))))

(defcustom edie-wm-tile-alist nil
  "Alist of tiles."
  :type '(choice (const nil) (alist :key-type symbol :value-type sexp))
  :set (lambda (symbol tiles)
         (set-default symbol tiles)
         (edie-wm-tile--gen-commands (mapcar #'car tiles))))

(defun edie-wm-tile-maybe-tile (rules window)
  "Tile WINDOW if it matches RULES."
  (declare (edie-log t))
  (let* ((elt (assq 'tile rules))
         (tiles (ensure-list (cdr elt)))
         (desktop (edie-wm-current-desktop))
         tile)
    (cond
     ((and elt (not tiles))
      nil)
     ((setq tile (edie-wm-tile-window-tile window))
      (edie-wm-geometry (edie-wm-tile--spec tile)))
     ((setq tile (seq-find (lambda (tl) (null (edie-wm-tile-window-list desktop tl))) tiles))
      (edie-wm-geometry (edie-wm-tile--spec tile)))
     ((memq (setq tile (edie-wm-tile-current-tile)) tiles)
      (edie-wm-geometry (edie-wm-tile--spec tile)))
     (tiles
      (edie-wm-geometry (edie-wm-tile--spec (car tiles)))))))

(defun edie-wm-tile-focus-cycle (tile)
  "Cycle focus between windows in TILE."
  (declare (edie-log t))
  (if (eq (edie-wm-tile-current-tile) tile)
      (if-let ((windows (edie-wm-tile-window-list (edie-wm-current-desktop) tile))
               (window (car windows)))
          (edie-wm-focus-window window))
    (edie-wm-tile-focus-tile tile)))

(defun edie-wm-tile-cycle (&optional tile)
  (interactive)
  (let ((tile (or tile (edie-wm-tile-current-tile))))
    (when-let ((windows (edie-wm-tile-window-list (edie-wm-current-desktop) tile))
               (window (car (reverse windows)))
      (edie-wm-focus-window window)))))

(defun edie-wm-tile-focus-tile (tile)
  "Focus the topmost window of TILE."
  (declare (edie-log t))
  (let* ((desktop (edie-wm-current-desktop)))
    (when-let ((window (car (edie-wm-tile-window-list desktop tile))))
      (edie-wm-focus-window window))))

(defun edie-wm-tile-current-tile ()
  "Return the tile that the currently focused window is in."
  (declare (edie-log nil))
  (when-let ((w (edie-wm-current-window)))
    (edie-wm-tile-window-tile w)))

(defun edie-wm-tile-window-tile (&optional window)
  "Return the tile that WINDOW is in."
  (declare (edie-log t))
  (pcase-let* ((window (or window (edie-wm-current-window)))
               ((map left top width height) (edie-wm-properties window)))
    (car-safe (seq-find
               (pcase-lambda (`(,_ . ,(map ('left tl) ('top tt) ('width tw) ('height th))))
                 (and (= left tl) (= top tt) (= width tw) (= height th)))
               (edie-wm-tile--geometries)))))

(defun edie-wm-tile-window-list (desktop tile)
  "Return a list of windows in tile TILE of DESKTOP."
  (declare (edie-log t))
  (let ((desktop-id (edie-wm-property desktop 'id))
        (geom (edie-wm-geometry (edie-wm-tile--spec tile))))
    (edie-wm-window-filter-list `((desktop . ,desktop-id) ,@geom))))

(defun edie-wm-tile-fit (tile &optional window)
  "Fit WINDOW to TILE's dimensions."
  (declare (edie-log t))
  (when-let ((w (or window (edie-wm-current-window))))
    (edie-wm-update-window w (edie-wm-geometry (edie-wm-tile--spec tile)))))

(defun edie-wm-tile--geometries ()
  "Return an alist of tiles and their geometries."
  (map-apply (lambda (k v) `(,k . ,(edie-wm-geometry v))) edie-wm-tile-alist))

(defun edie-wm-tile--spec (tile)
  "Return the tile specification for TILE."
  (cl-assert tile)

  (let* ((spec (map-elt edie-wm-tile-alist tile)))
    (cl-assert spec)
    spec))

(provide 'edie-wm)
;;; edie-wm.el ends here
