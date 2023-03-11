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
  :type '(plist :keyword-type (choice (const :left) (const :top) (const :width) (const :right))
                :value-type edie-wm-unit))

(defcustom edie-wm-default-desktop-list '("default")
  "A list of desktop names."
  :type '(repeat string))

(defcustom edie-wm-desktop-padding-alist
  '((nil . (:left 0 :top 0 :bottom 0 :right 0)))
  "The amount of whitespace, in pixels, reserved at each edge of the desktop."
  :type '(repeat (alist :key-type symbol :value-type edie-wm-geometry)))

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

FILTERS is a plist, where each keyword corresponds to a window
attribute which will be used to match against windows.

ATTRIBUTES is a plist, where each keyword is an attribute that
will be applied to windows matched by FILTERS."
  :type '(choice (const nil)
                 (alist :key-type (plist :key-type symbol :value-type sexp)
                        :value-type (plist :key-type symbol :value-type sexp))))

(defcustom edie-wm-tile-alist nil
  "Alist of tiles."
  :type '(choice (const nil) (alist :key-type symbol :value-type sexp)))

(defcustom edie-wm-tile-commands nil
  "Tile commands to be auto-generated."
  :type '(list (set (const fit) (const focus) (const focus-cycle))
               (repeat symbol)
               (plist))
  :set (pcase-lambda (var (and val `(,actions ,tiles ,(map :prefix))))
         (custom-set-default var val)
         (dolist (a actions)
           (dolist (tile tiles)
             (let ((command (intern (format "%s%s-%s" (or prefix "") a tile)))
                   (action (intern (format "edie-wm-tile-%s" a))))
               (defalias command
                 (lambda ()
                   (interactive)
                   (funcall action tile))))))))

(defcustom edie-wm-backend 'openbox
  nil
  :type '(choice (const openbox) (const hyprland)))

;;;###autoload
(define-minor-mode edie-wm-mode
  nil
  :global t
  (if edie-wm-mode
      (let ((backend (intern (format "edie-wm-%s" edie-wm-backend))))
        (require backend)

        (edie-wm-backend-start)

        (edie-wm-reset-window-list)

        (setq edie-wm--current-window-id (edie-wm-backend-current-window-id))
        (add-hook 'edie-wm-window-rules-functions #'edie-wm-tile-maybe-tile))
    (remove-hook 'edie-wm-window-rules-functions #'edie-wm-tile-maybe-tile) ))

(defun edie-wm-set-properties (obj props)
  "Set PROPS on OBJ."
  (dolist (prop props obj)
    (edie-wm-set-property obj (car prop) (cdr prop))))

(defun edie-wm-set-property (obj property value)
  "Replace the value of PROPERTY on OBJ with VALUE."
  (setf (alist-get property obj) value)
  obj)

(defun edie-wm-property (obj property)
  "Return the value of OBJ's PROPERTY."
  (alist-get property obj))
(defun edie-wm-current-desktop ()
  "The desktop we are currently working in."
  (let ((mon-id (edie-wm-property (edie-wm-current-monitor) 'id)))
    (seq-find (lambda (dsk)
                (string= (edie-wm-desktop-property dsk :monitor) mon-id))
              (edie-wm-desktop-list))))

(defun edie-wm-switch-to-desktop (desktop)
  "Switch to desktop DESKTOP.

When called interactively, prompt for the desktop we want to
switch to."
  (interactive (list (edie-wm-select-desktop)))
  (edie-wm-backend-desktop-focus desktop))

(defun edie-wm-desktop-list (&optional reload)
  "The list of virtual desktops."
  (cl-mapcar (lambda (name id)
               (list 'desktop (list :name name) id))
             edie-wm-default-desktop-list
             (edie-wm-backend-desktop-id-list)))

(defun edie-wm-select-desktop ()
  "Prompt for a desktop."
  (pcase-let* ((desktop (edie-wm-current-desktop))
               (desktops (thread-last
                           (edie-wm-desktop-list)
                           (seq-remove (lambda (d) (equal d desktop)))
                           (seq-map (pcase-lambda ((and d `(desktop ,(map :name) ,id)))
                                      (cons name d)))))
               (choice (completing-read "Desktop: " desktops nil 'confirm))
               ((seq 'desktop _ desktop-id) (map-elt desktops choice)))
    desktop-id))

(defun edie-wm-desktop-index (desktop)
  "Return the index of DESKTOP."
  (nth 2 desktop))

(defun edie-wm-desktop-make (name id)
  "Make a desktop instance."
  `(desktop (:name ,name) ,id))

(cl-defstruct edie-wm-window
  id
  (left 0 :type integer)
  (top 0 :type integer)
  (width 0 :type integer)
  (height 0 :type integer)
  desktop class instance title)

(defun edie-wm-focus-window (window)
  "Focus WINDOW."
  (let* ((wid (edie-wm-window-id window))
         (elt (assoc wid edie-wm--window-list)))
    (setq edie-wm--window-list (delq elt edie-wm--window-list)
          edie-wm--current-window-id wid)
    (push elt edie-wm--window-list)

    (edie-wm-backend-window-focus wid)
    (run-hooks 'edie-wm-window-focus-changed-hook)))

(defun edie-wm-select-window ()
  "Prompt for a window."
  (let ((indexed (seq-map (pcase-lambda ((cl-struct edie-wm-window id title))
                            (cl-assert id)
                            (cons (format "%s%s"
                                          (propertize (number-to-string id) 'invisible t)
                                          title)
                                  id))
                          (edie-wm-window-list))))
    (map-elt indexed (completing-read "Window: " indexed))))

(defun edie-wm-window-close (&optional window)
  "Close the active window or WINDOW."
  (interactive)
  (when-let ((window (or window (edie-wm-current-window))))
    (run-hook-with-args-until-success 'edie-wm-window-close-functions window)))

(defun edie-wm-window-to-desktop (desktop &optional window)
  "Send WINDOW to DESKTOP."
  (interactive (list (edie-wm-select-desktop)))
  (when-let ((window (or window (edie-wm-current-window))))
    (edie-wm-update-window window (list :desktop desktop))))

(defun edie-wm-window-to-monitor (monitor &optional window)
  "Move WINDOW to MONITOR."
  (let ((window (or window (edie-wm-current-window))))
    (edie-wm-update-window window (list :monitor (edie-wm-property monitor 'id)))))

(defun edie-wm-current-window ()
  "Return the window that is currently focused."
  (when edie-wm--current-window-id
    (cdr (assoc edie-wm--current-window-id edie-wm--window-list))))

(defun edie-wm-window (filters)
  "Return the first window that matches FILTERS."
  (seq-find (lambda (wnd) (edie-wm-window-filter-match-p filters wnd)) (edie-wm-window-list)))

(defun edie-wm-window-alist ()
  "An alist of windows where the keys are the window ids."
  (or edie-wm--window-list (edie-wm-reset-window-list)))

(defun edie-wm-reset-window-list ()
  "Reload the window list."
  (interactive)
  (let ((windows nil))
    (dolist (w (edie-wm-backend-window-list))
      (edie-wm-update-window w (edie-wm--apply-rules w))
      (push (cons (edie-wm-window-id w) w) windows))
    (setq edie-wm--window-list windows)))

(defun edie-wm-window-list ()
  "The list of windows across all desktops."
  (map-values (edie-wm-window-alist)))

(defun edie-wm-window-filter-list (filters &optional windows)
  "Return all windows matching FILTERS.

If WINDOWS is given, filter only that list.

For a description of supported filters and their format, see
`edie-wm-window-filter-match-p'.

Return nil or the list of windows that match the filters."
  (seq-filter (lambda (window)
                (edie-wm-window-filter-match-p filters window))
              (or windows (edie-wm-window-list))))

(defun edie-wm-window-filter-match-p (filters &optional window)
  "Check whether WINDOW matches the given FILTERS."
  (pcase-let (((map :class :desktop :height :instance :left :title :top :width) filters)
              (window (or window (edie-wm-current-window))))
    (and window
         (or (not class) (string-match-p class (edie-wm-window-class window)))
         (or (not desktop) (equal desktop (edie-wm-window-desktop window)))
         (or (not height) (equal height (edie-wm-window-height window)))
         (or (not instance) (string-match-p instance (edie-wm-window-instance window)))
         (or (not left) (equal left (edie-wm-window-left window)))
         (or (not title) (string-match-p title (edie-wm-window-title window)))
         (or (not top) (equal top (edie-wm-window-top window)))
         (or (not width) (equal width (edie-wm-window-width window))))))

(defun edie-wm-update-window (window plist)
  ""
  (when-let ((changes (edie-wm-window-merge-changes window plist)))
    (edie-wm-backend-window-update (edie-wm-window-id window) changes)))

(defun edie-wm-window-merge-changes (window plist)
  (let ((changes nil))
    (while (keywordp (car plist))
      (pcase (cons (pop plist) (pop plist))
        ((and `(:left . ,val) (guard (not (equal val (edie-wm-window-left window)))))
         (setf (edie-wm-window-left window) val)
         (setq changes (plist-put changes :left val))
         (unless (plist-get changes :top)
           (setq changes (plist-put changes :top (edie-wm-window-top window)))))
        ((and `(:top . ,val) (guard (not (equal val (edie-wm-window-top window)))))
         (setf (edie-wm-window-top window) val)
         (setq changes (plist-put changes :top val))
         (unless (plist-get changes :left)
           (setq changes (plist-put changes :left (edie-wm-window-left window)))))
        ((and `(:width . ,val) (guard (not (equal val (edie-wm-window-width window)))))
         (setf (edie-wm-window-width window) val)
         (setq changes (plist-put changes :width val))
         (unless (plist-get changes :height)
           (setq changes (plist-put changes :height (edie-wm-window-height window)))))
        ((and `(:height . ,val) (guard (not (equal val (edie-wm-window-height window)))))
         (setf (edie-wm-window-height window) val)
         (setq changes (plist-put changes :height val))
         (unless (plist-get changes :width)
           (setq changes (plist-put changes :width (edie-wm-window-width window)))))
        ((and `(:class . ,val) (guard (not (equal val (edie-wm-window-class window)))))
         (setf (edie-wm-window-class window) val)
         (setq changes (plist-put changes :class val)))
        ((and `(:instance . ,val) (guard (not (equal val (edie-wm-window-instance window)))))
         (setf (edie-wm-window-instance window) val)
         (setq changes (plist-put changes :instance val)))
        ((and `(:desktop . ,val) (guard (not (equal val (edie-wm-window-desktop window)))))
         (setf (edie-wm-window-desktop window) val)
         (setq changes (plist-put changes :desktop val)))
        ((and `(:title . ,val) (guard (not (equal val (edie-wm-window-title window)))))
         (setf (edie-wm-window-title window) val)
         (setq changes (plist-put changes :title val)))))
    changes))

(defun edie-wm-monitor-focused-p (monitor)
  "Return t if MONITOR is focused."
  (edie-wm-property monitor 'focused))

(defvar edie-wm--monitor-list nil)

(defun edie-wm-current-monitor ()
  (seq-find #'edie-wm-monitor-focused-p (edie-wm-monitor-list)))

(defun edie-wm-monitor-list ()
   (or edie-wm--monitor-list (edie-wm-monitor-reset-list)))

(defun edie-wm-monitor-reset-list ()
  "Reset the list of monitors."
  (setq edie-wm--monitor-list (edie-wm-backend-monitor-list)))

(defun edie-wm-on-monitor-add (_)
  (edie-wm-monitor-reset-list)
  (run-hook 'edie-wm-monitor-added-hook))

(defun edie-wm-on-monitor-focus-change (name)
  (dolist (mon (edie-wm-monitor-list))
    (edie-wm-set-property
     mon 'focused (equal (edie-wm-property m 'name) name)))
  (run-hook 'edie-wm-monitor-focus-changed-hook))

(defun edie-wm-on-monitor-remove (_)
  (edie-wm-monitor-reset-list)
  (run-hook 'edie-wm-monitor-removed-hook))

(defun edie-wm-workarea ()
  (pcase-let* ((mon (edie-wm-current-monitor))
               ((map (:left p-left)
                     (:top p-top)
                     (:right p-right)
                     (:bottom p-bot))
                (cdr (or (assoc (edie-wm-property mon 'name) edie-wm-desktop-padding)
                         (assoc nil edie-wm-desktop-padding))))
               ((map :left :top :width :height) (edie-wm-screenarea)))
    `(:left ,(+ left p-left)
      :top ,(+ top p-top)
      :width ,(- width p-left p-right)
      :height ,(- height p-top p-bot))))

(defun edie-wm-screenarea ()
  (pcase-let (((map left top width height) (edie-wm-current-monitor)))
    `(:left ,left :top ,top :width ,width :height ,height)))

(defun edie-wm-geometry (plist)
  (pcase-let* (((map (:left wa-left) (:top wa-top) (:width wa-width) (:height wa-height))
                (if (eq (map-elt plist :workarea) 'screen)
                    (edie-wm-screenarea)
                  (edie-wm-workarea)))
               ((map :left :top :width :height) plist))
    (thread-last
      (list :left (and left (+ wa-left (edie-wm-unit-pixel left wa-width)))
            :top (and top (+ wa-top (edie-wm-unit-pixel top wa-height)))
            :width (and width (edie-wm-unit-pixel width wa-width))
            :height (and height (edie-wm-unit-pixel height wa-height)))
      (map-filter (lambda (_ v) v))
      (map-merge 'plist plist)
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

(defun edie-wm-on-window-focus (wid)
  (when (not (equal wid edie-wm--current-window-id))
    (let ((window (cdr (assoc wid edie-wm--window-list))))
      (cond
       (window
        (edie-wm-focus-window window))
       ((not window)
        (edie-wm-focus-window (cdr (assoc wid edie-wm--window-list))))
       ((not wid)
        (setq edie-wm--current-window-id nil))))))

(defun edie-wm-on-window-add (window)
  ;; TODO ensure that window still exists
  (setf (map-elt edie-wm--window-list (edie-wm-window-id window)) window)

  (let ((changes (edie-wm--apply-rules window)))
    (setq edie-wm--current-window-id (edie-wm-window-id window))
    (run-hooks 'edie-wm-window-added-hook)

    (edie-wm-focus-window window)

    (when changes
      (edie-wm-update-window window changes))))

(defun edie-wm-on-window-remove (wid)
  (when-let ((window (cdr (assoc wid edie-wm--window-list)))
             (edie-wm--current-window-id wid)
             (desktop-id (edie-wm-window-desktop window)))
    (run-hooks 'edie-wm-window-closed-hook)

    (setq edie-wm--window-list (delq (assoc wid edie-wm--window-list) edie-wm--window-list))

    (catch 'found
      (dolist (w (edie-wm-window-list))
        (when (equal (edie-wm-window-desktop w) desktop-id)
          (edie-wm-focus-window w)
          (throw 'found w))))))

(defun edie-wm-on-window-update (wid changes)
  (when-let ((window (or (and wid (cdr (assoc wid edie-wm--window-list)))
                         (edie-wm-current-window)))
             (edie-wm--current-window-id wid)
             (actual-changes (edie-wm-window-merge-changes window changes)))
    (run-hooks 'edie-wm-window-updated-hook)

    (when-let ((more-changes (edie-wm--apply-rules window)))
      (edie-wm-update-window window more-changes))))

(defun edie-wm-on-desktop-focus-change ()
  (let ((desktop-id (edie-wm-backend-current-desktop-id)))
    (setf (edie-wm-monitor-desktop (edie-wm-current-monitor)) desktop-id)
    (if-let ((window (car (edie-wm-window-filter-list (list :desktop desktop-id)))))
        (edie-wm-focus-window window)
      (setq edie-wm--current-window-id nil)))
  (run-hooks 'edie-wm-desktop-focus-changed-hook))

(defun edie-wm--adjust-margins (plist)
  (if (eq (map-elt plist :workarea) 'screen)
      plist
    (pcase-let* (((map (:left wnd-left)
                       (:top wnd-top)
                       (:width wnd-width)
                       (:height wnd-height)
                       (:margins wnd-margins))
                  plist)
                 (`(,wnd-m-left ,wnd-m-right ,wnd-m-top ,wnd-m-bot)
                  (make-list 4 (or wnd-margins edie-wm-window-margins))))
      (if (and wnd-left wnd-top wnd-width wnd-height)
          (map-merge 'plist
                     plist
                     (list :left (+ wnd-left wnd-m-left)
                           :top (+ wnd-top wnd-m-top)
                           :width (- wnd-width wnd-m-right wnd-m-left)
                           :height (- wnd-height wnd-m-top wnd-m-bot)))
        plist))))

(defun edie-wm--apply-rules (window)
  (let* ((new-props nil)
         (rules (and window (edie-wm--find-rule window)))
         result)
    (dolist (fun edie-wm-window-rules-functions)
      (setq result (funcall fun rules window))
      (while (keywordp (car result))
        (setq new-props (plist-put new-props (pop result) (pop result)))))
    new-props))

(defun edie-wm--find-rule (window)
  (cdr (seq-find (pcase-lambda (`(,filter . ,_))
                   (edie-wm-window-filter-match-p filter window))
                 edie-wm-rules-alist)))

(cl-defun edie-wm-tile-maybe-tile ((&key tile &allow-other-keys) window)
  "Tile WINDOW if it matches RULES."
  (let ((tiles (ensure-list tile))
        (desktop (edie-wm-current-desktop))
        tile)
    (cond
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
  (if (eq (edie-wm-tile-current-tile) tile)
      (let* ((windows (edie-wm-tile-window-list (edie-wm-current-desktop) tile))
             (window (car (reverse windows))))
        (edie-wm-focus-window window))
    (edie-wm-tile-focus-tile tile)))

(defun edie-wm-tile-focus-tile (tile)
  "Focus the topmost window of TILE."
  (let* ((desktop (edie-wm-current-desktop)))
    (when-let ((window (car (edie-wm-tile-window-list desktop tile))))
      (edie-wm-focus-window window))))

(defun edie-wm-tile-current-tile ()
  "Return the tile that the currently focused window is in."
  (when-let ((w (edie-wm-current-window)))
    (edie-wm-tile-window-tile w)))

(defun edie-wm-tile-window-tile (window)
  "Return the tile that WINDOW is in."
  (pcase-let (((cl-struct edie-wm-window left top width height) window))
    (car-safe (seq-find
               (pcase-lambda (`(,_ . ,(map (:left tl) (:top tt) (:width tw) (:height th))))
                 (and (= left tl) (= top tt) (= width tw) (= height th)))
               (edie-wm-tile--geometries)))))

(defun edie-wm-tile-window-list (desktop tile)
  "Return a list of windows in tile TILE of DESKTOP."
  (pcase-let* (((seq 'desktop _ desktop-id) desktop)
               (geom (edie-wm-geometry (edie-wm-tile--spec tile))))
    (edie-wm-window-filter-list `(:desktop ,desktop-id ,@geom))))

(defun edie-wm-tile-fit (tile &optional window)
  "Fit WINDOW to TILE's dimensions."
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
