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

(defvar edie-wm-current-desktop-function nil)
(defvar edie-wm-focus-window-function nil)
(defvar edie-wm-set-desktop-function nil)
(defvar edie-wm-window-make-function nil)
(defvar edie-wm-current-window-id-function nil)
(defvar edie-wm-update-window-function nil)
(defvar edie-wm-window-close-function nil)
(defvar edie-wm-window-list-function nil)

(defvar edie-wm-workarea-function #'edie-wm-screenarea)
(defvar edie-wm-geometry-function #'edie-wm-geometry-1)

(defvar edie-wm-on-window-focus-function #'edie-wm--on-window-focus-1)
(defvar edie-wm-on-window-add-function #'edie-wm--on-window-add-1)
(defvar edie-wm-on-window-remove-function #'edie-wm--on-window-remove-1)
(defvar edie-wm-on-window-update-function #'edie-wm--on-window-update-1)

(defvar edie-wm--apply-rules-function #'edie-wm--apply-rules-1)

(defvar edie-wm--window-list nil)

(defvar edie-wm--current-window-id nil)

(defconst edie-wm--window-properties-slot 2)

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

(defcustom edie-wm-desktop-padding '(:left 0 :top 0 :bottom 0 :right 0)
  "The amount of whitespace, in pixels, reserved at each edge of the desktop."
  :type 'edie-wm-geometry)

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

(defcustom edie-wm-desktop-focus-change-hook nil
  "Normal hook run after switching virtual desktops."
  :type 'hook)

(defcustom edie-wm-window-focus-change-hook nil
  "Normal hook run after a window takes focus."
  :type 'hook)

(defcustom edie-wm-window-update-hook nil
  "Normal hook run after some significant window property changes."
  :type 'hook)

(defcustom edie-wm-window-close-hook nil
  "Normal hook run after a window is closed."
  :type 'hook)

(defcustom edie-wm-rules-alist nil
  "Alist of window rules.

A rule is a cons cell of the form (FILTERS . ATTRIBUTES).

FILTERS is a plist, where each keyword corresponds to a window
attribute which will be used to match against windows.

ATTRIBUTES is a plist, where each keyword is an attribute that
will be applied to windows matched by FILTERS."
  :type '(alist
          :key-type (plist :key-type keyword :value-type string)
          :value-type (plist :key-type keyword :value-type string)))

(defcustom edie-wm-tile-alist nil
  "Alist of tiles."
  :type '(alist :key-type keyword :value-type sexp))

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
  :type '(choice (const openbox)))

;;;###autoload
(define-minor-mode edie-wm-mode
  nil
  :global t
  (when edie-wm-mode
    (let ((backend (intern (format "edie-wm-%s" edie-wm-backend)))
          (start-fn (intern (format "edie-wm-%s-start" edie-wm-backend))))
      (require backend)

      (add-function :filter-return edie-wm-geometry-function #'edie-wm--adjust-margins)
      (add-function :filter-args edie-wm-update-window-function #'edie-wm--write-borders)
      (add-function :filter-return edie-wm-update-window-function #'edie-wm--read-borders)
      (add-function :after edie-wm-on-window-add-function #'edie-wm--apply-rules)
      (add-function :filter-return edie-wm-on-window-update-function #'edie-wm--apply-rules)
      (add-function :filter-return edie-wm-workarea-function #'edie-wm--adjust-workarea)
      (add-function :filter-args edie-wm--apply-rules-function #'edie-wm-tile-maybe-tile)

      (funcall start-fn)

      (setq edie-wm--current-window-id (funcall edie-wm-current-window-id-function)))))


(defun edie-wm-current-desktop ()
  "The desktop we are currently working in."
  (pcase (funcall edie-wm-current-desktop-function)
    ((seq desktop _ id) (seq-elt (edie-wm-desktop-list) id))))

(defun edie-wm-switch-to-desktop (desktop)
  "Switch to desktop DESKTOP.

When called interactively, prompt for the desktop we want to
switch to."
  (interactive (list (edie-wm-select-desktop)))
  (funcall edie-wm-set-desktop-function desktop))

(defun edie-wm-desktop-list (&optional reload)
  "The list of virtual desktops."
  (seq-map-indexed #'edie-wm-desktop-make edie-wm-default-desktop-list))

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

(defun edie-wm-current-desktop ()
  "The desktop we are currently working in."
  (pcase (edie-wm-x11-current-desktop)
    ((seq desktop _ id) (seq-elt (edie-wm-desktop-list) id))))

(defun edie-wm-desktop-index (desktop)
  "Return the index of DESKTOP."
  (nth 2 desktop))

(defun edie-wm-desktop-make (name id)
  "Make a desktop instance."
  `(desktop (:name ,name) ,id))

(defun edie-wm-focus-window (window)
  "Focus WINDOW."
  (funcall edie-wm-focus-window-function window))

(defun edie-wm-window-id (window)
  "Return the ID of WINDOW."
  (pcase-let (((seq 'window wid) window))
    (cl-assert wid t)
    wid))

(defun edie-wm-window-properties (window)
  "Return the properties of WINDOW."
  (cl-assert window t)
  (nth edie-wm--window-properties-slot window))

(defun edie-wm-select-window ()
  "Prompt for a window."
  (let ((indexed (seq-map (pcase-lambda ((seq 'window wid (map :title)))
                            (cl-assert wid)
                            (cons (format "%s%s"
                                          (propertize (number-to-string wid) 'invisible t)
                                          title)
                                  wid))
                          (edie-wm-window-list))))
    (map-elt indexed (completing-read "Window: " indexed))))

(defun edie-wm-window-close (&optional window)
  "Close the active window or WINDOW."
  (interactive)
  (when-let ((w (or window (edie-wm-current-window))))
    (funcall edie-wm-window-close-function w)))

(defun edie-wm-window-to-desktop (desktop &optional window)
  "Send WINDOW to DESKTOP."
  (interactive (list (edie-wm-select-desktop)))
  (when-let ((window (or window (edie-wm-current-window))))
    (edie-wm-update-window window (list :desktop desktop))))

(defun edie-wm-current-window ()
  "Return the window that is currently focused."
  (map-elt edie-wm--window-list edie-wm--current-window-id))

(defun edie-wm-window-property (window propname)
  "Return the value of the property PROPNAME for WINDOW."
  (plist-get (edie-wm-window-properties window) propname))

(defun edie-wm-window (filters)
  "Return the first window that matches FILTERS."
  (seq-find (lambda (wnd) (edie-wm-window-filter-match-p filters wnd)) (edie-wm-window-list)))

(defun edie-wm-window-alist ()
  "An alist of windows where the keys are the window ids."
  (or edie-wm--window-list (edie-wm-reset-window-list)))

(defun edie-wm-reset-window-list ()
  "Reload the window list."
  (let ((windows (seq-map (pcase-lambda ((and window (seq 'window wid)))
                            (cons wid window))
                          (funcall edie-wm-window-list-function))))

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

(defun edie-wm-window-filter-match-p (filters window)
  "Check whether WINDOW matches the given FILTERS."
  (pcase-let (((map (:class fclass)
                    (:desktop fdesktop)
                    (:height fheight)
                    (:instance finstance)
                    (:left fleft)
                    (:title ftitle)
                    (:top ftop)
                    (:width fwidth))
               filters)
              ((seq 'window _ (map (:class wclass)
                                   (:desktop wdesktop)
                                   (:height wheight)
                                   (:instance winstance)
                                   (:left wleft)
                                   (:title wtitle)
                                   (:top wtop)
                                   (:width wwidth)))
               window))
    (and (or (not fclass) (string-match-p fclass wclass))
         (or (not fdesktop) (equal fdesktop wdesktop))
         (or (not fheight) (equal fheight wheight))
         (or (not finstance) (string-match-p finstance winstance))
         (or (not fleft) (equal fleft wleft))
         (or (not ftitle) (string-match-p ftitle wtitle))
         (or (not ftop) (equal ftop wtop))
         (or (not fwidth) (equal fwidth wwidth)))))

(defun edie-wm-update-window (window plist)
  (pcase-let (((seq 'window wid) window))
    (setf (map-elt edie-wm--window-list wid)
          (funcall edie-wm-update-window-function
                   window
                   (map-merge 'plist plist (edie-wm-geometry plist))))))

(defun edie-wm-workarea ()
  (funcall edie-wm-workarea-function))

(defun edie-wm-screenarea ()
  (seq-let (left top width height) (map-elt (car (display-monitor-attributes-list)) 'geometry)
    `(:left ,left :top ,top :width ,width :height ,height)))

(defun edie-wm-geometry (spec)
  (funcall edie-wm-geometry-function spec))

(defun edie-wm-geometry-1 (plist)
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
      (map-merge 'plist plist))))

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
  (funcall edie-wm-on-window-focus-function wid))

(defun edie-wm--on-window-focus-1 (wid)
  (when-let (((/= wid 0))
             ;; sometimes we get an id, but we don't have a window
             (window (alist-get wid edie-wm--window-list)))
    (setf (alist-get wid edie-wm--window-list nil 'remove) nil)
    (setf (alist-get wid edie-wm--window-list) window)
    (setq edie-wm--current-window-id wid))

  (run-hooks 'edie-wm-window-focus-change-hook))

(defun edie-wm-on-window-add (window)
  (pcase-let ((`(window ,wid) window))
    (funcall edie-wm-on-window-add-function window)))

(defun edie-wm--on-window-add-1 (window)
  (pcase-let (((seq 'window wid) window))
    (setf (map-elt edie-wm--window-list wid) window)))

(defun edie-wm-on-window-remove (wid)
  (let ((window (alist-get wid edie-wm--window-list)))
    (funcall edie-wm-on-window-remove-function wid)))

(defun edie-wm--on-window-remove-1 (wid)
  (setf (alist-get wid edie-wm--window-list nil 'remove) nil)
  (run-hooks 'edie-wm-window-close-hook))

(defun edie-wm-on-window-update (wid changes)
  (funcall edie-wm-on-window-update-function wid changes))

(defun edie-wm--on-window-update-1 (wid changes)
  (let* ((window (alist-get wid edie-wm--window-list))
         (props (edie-wm-window-properties window)))
    (map-do (lambda (k v) (setf props (plist-put props k v))) changes)

    (run-hooks 'edie-wm-window-update-hook)

    window))

(defun edie-wm-on-desktop-focus-change ()
  (run-hooks 'edie-wm-desktop-focus-change-hook))

(cl-defun edie-wm--write-borders ((window plist))
  (pcase-let* (((map (:left wnd-left)
                     (:top wnd-top)
                     (:width wnd-width)
                     (:height wnd-height)
                     (:type type))
                plist)
               (`(,b-left ,b-right ,b-top ,b-bot)
                (make-list 4 edie-wm-window-border-width)))
    (if (and (not (eq type 'dock)) wnd-left wnd-top wnd-width wnd-height)
        (list window
              (map-merge 'plist plist
                         (list :left wnd-left
                               :top wnd-top
                               :width (- wnd-width b-right b-left)
                               :height (- wnd-height b-top b-bot))))
      (list window plist))))

(defun edie-wm--read-borders (window)
  (pcase-let* ((`(window ,wid ,(and props (map :left :top :width :height))) window)
               (`(,b-left ,b-right ,b-top ,b-bot) (make-list 4 edie-wm-window-border-width)))
    (list 'window wid (map-merge 'plist props `(:width ,(+ width b-left b-right)
                                                :height ,(+ height b-top b-bot))))))

(defun edie-wm--adjust-workarea (workarea)
  (pcase-let* (((map (:left p-left)
                     (:top p-top)
                     (:right p-right)
                     (:bottom p-bot))
                edie-wm-desktop-padding)
               ((map :left :top :width :height) workarea))
    `(:left ,(+ left p-left)
      :top ,(+ top p-top)
      :width ,(- width p-left p-right)
      :height ,(- height p-top p-bot))))

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
  (let* ((match (edie-wm--find-rule window)))
    (when-let ((rules (cdr match)))
      (funcall edie-wm--apply-rules-function rules window))))

(defun edie-wm--apply-rules-1 (rules window)
  (edie-wm-update-window window rules))

(defun edie-wm--find-rule (window)
  (seq-find (pcase-lambda (`(,filter . ,_))
              (edie-wm-window-filter-match-p filter window))
            edie-wm-rules-alist))

(cl-defun edie-wm-tile-maybe-tile ((rules window))
  "Tile WINDOW if it matches RULES."
  (list
   (if-let ((tile (map-elt rules :tile)))
       (let ((tiles (ensure-list tile)))
         (map-merge
          'plist
          rules
          (edie-wm-tile--spec
           (if (memq (edie-wm-tile-current-tile) tiles)
               (edie-wm-tile-current-tile)
             (if-let ((tile (seq-find (lambda (tile)
                                        (null
                                         (edie-wm-tile-window-list
                                          (edie-wm-current-desktop) tile)))
                                      tiles)))
                 tile
               (car tiles))))))
     rules)
   window))

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
  (pcase-let (((seq 'window _ (map :left :top :width :height)) window))
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
    (edie-wm-update-window w (edie-wm-tile--spec tile))))

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
