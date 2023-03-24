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

(defcustom edie-wm-backend 'openbox
  nil
  :type '(choice (const openbox) (const hyprland)))

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

(defun edie-wm-desktop--gen-commands (num-desktops)
  (dotimes (i num-desktops)
    (defalias (intern (format "edie-wm-switch-to-desktop-%s" i))
      (lambda ()
        (interactive)
        (edie-wm-switch-to-desktop (nth i (edie-wm-desktop-list)))))
    (defalias (intern (format "edie-wm-window-to-desktop-%s" i))
      (lambda ()
        (interactive)
        (edie-wm-window-to-desktop (nth i (edie-wm-desktop-list)))))))

(defcustom edie-wm-desktops 5
  "The number of known desktops."
  :type 'natnum
  :set (lambda (symbol value)
         (set-default symbol value)
         (edie-wm-desktop--gen-commands value)))

(defun edie-wm-on-desktop-focus-change (_)
  (declare (edie-log t))
  (run-hooks 'edie-wm-desktop-focus-changed-hook))

(defun edie-wm-desktop-make ()
  "Create a new desktop."
  (list 'desktop))

(defun edie-wm-desktop (filters)
  "Return the desktop named NAME."
  (declare (edie-log nil))
  (catch 'found
    (dolist (dsk (edie-wm-desktop-list))
      (or (catch 'not-found
            (pcase-dolist (`(,key . ,value) filters)
              (or (equal (edie-wm-property dsk key) value)
                  (throw 'not-found t))))
          (throw 'found dsk)))))

(defun edie-wm-current-desktop ()
  "The desktop we are currently working in."
  (declare (edie-log nil))
  (let ((id (edie-wm-property (edie-wm-current-monitor) 'focused-desktop)))
    (edie-wm-desktop `((id . ,id)))))

(defun edie-wm-switch-to-desktop (desktop)
  "Switch to desktop DESKTOP.

When called interactively, prompt for the desktop we want to
switch to."
  (declare (edie-log t))
  (interactive (list (edie-wm-select-desktop)))
  (edie-wm-backend-desktop-focus desktop))

(defun edie-wm-desktop-list ()
  "The list of virtual desktops."
  (declare (edie-log nil))
  (edie-wm-backend-desktop-list))

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

(defmacro edie-wm-with-current-window (window &rest body)
  "Execute BODY with WINDOW as the current window."
  (declare (indent defun) (edie-log t))
  `(let ((edie-wm--current-window (edie-wm-window ,window)))
     ,@body))

(defun edie-wm-on-window-focus (wid)
  (declare (edie-log t))
  (edie-wm-with-current-window wid
    (run-hooks 'edie-wm-window-focus-changed-hook)))

(defun edie-wm-on-window-add (wid)
  (declare (edie-log t))
  (edie-wm-with-current-window wid
    (run-hooks 'edie-wm-window-added-hook)))

 (defun edie-wm-on-window-remove (_)
  (declare (edie-log t))
  (run-hooks 'edie-wm-window-closed-hook))

(defun edie-wm-on-window-update (_ _)
  (declare (edie-log t))
  (run-hooks 'edie-wm-window-updated-hook))

(defun edie-wm-focus-window (window)
  "Focus WINDOW."
  (declare (edie-log t))
  (edie-wm-backend-window-focus window))

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
  (or edie-wm--current-window
      (edie-wm-backend-current-window)))

(defun edie-wm-window (filters)
  "Return the first window that matches FILTERS."
  (declare (edie-log nil))
  (car (edie-wm-window-list (or (and (listp filters) filters) (list (cons 'id filters))))))


(defun edie-wm-window-list (&optional filters)
  "The list of windows across all desktops."
  (declare (edie-log nil))
  (if filters
      (edie-wm-window-filter-list filters (edie-wm-backend-window-list))
    (edie-wm-backend-window-list)))

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

(defun edie-wm-monitor-list ()
  (declare (edie-log nil))
  (edie-wm-backend-monitor-list))

(defun edie-wm-on-monitor-add (_)
  (declare (edie-log t))
  (run-hooks 'edie-wm-monitor-added-hook))

(defun edie-wm-on-monitor-focus-change (name)
  (declare (edie-log t))
  (dolist (mon (edie-wm-monitor-list))
    (edie-wm-set-property
     mon 'focused (equal (edie-wm-property mon 'name) name)))
  (run-hooks 'edie-wm-monitor-focus-changed-hook))

(defun edie-wm-on-monitor-remove (_)
  (declare (edie-log t))
  (edie-wm-monitor-reset-list)
  (edie-wm-desktop-reset-list)
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
