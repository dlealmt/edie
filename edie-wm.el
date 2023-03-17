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
        (add-hook 'edie-wm-window-rules-functions #'edie-wm-tile-maybe-tile)
        (add-hook 'edie-wm-window-close-functions #'edie-wm-backend-window-close 95)

        (edie-wm-window-reset-list))

    (edie-wm-backend-stop)

    (remove-hook 'edie-wm-window-added-hook #'edie-wm-apply-rules)
    (remove-hook 'edie-wm-window-updated-hook #'edie-wm-apply-rules)
    (remove-hook 'edie-wm-window-close-functions #'edie-wm-backend-window-close)
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

(defun edie-wm-desktop--gen-commands (names)
  (dolist (name names)
    (defalias (intern (format "edie-wm-switch-to-desktop-%s" name))
      (lambda ()
        (interactive)
        (edie-wm-switch-to-desktop (edie-wm-desktop `((name . ,name))))))
    (defalias (intern (format "edie-wm-window-to-desktop-%s" name))
      (lambda ()
        (interactive)
        (edie-wm-window-to-desktop (edie-wm-desktop `((name . ,name))))))))

(defcustom edie-wm-default-desktop-list '(default)
  "A list of desktop names."
  :type '(repeat symbol)
  :set (lambda (symbol value)
         (set-default symbol value)
         (edie-wm-desktop--gen-commands value)))

(defvar edie-wm--desktop-alist nil)

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

(defvar edie-wm--current-desktop nil)

(defun edie-wm-current-desktop ()
  "The desktop we are currently working in."
  (declare (edie-log nil))
  (or edie-wm--current-desktop
      (catch 'found
        (let ((mon (edie-wm-current-monitor)))
          (dolist (desktop (edie-wm-desktop-list))
            (when (equal (edie-wm-property desktop 'id)
                         (edie-wm-property mon 'focused-desktop))
              (throw 'found desktop)))))))

(defun edie-wm-switch-to-desktop (desktop)
  "Switch to desktop DESKTOP.

When called interactively, prompt for the desktop we want to
switch to."
  (declare (edie-log t))
  (interactive (list (edie-wm-select-desktop)))

  ;; TODO: Handle the case where the desktop is already visible on another monitor
  (edie-wm-set-property (edie-wm-current-monitor)
                        'focused-desktop (edie-wm-property desktop 'id))

  (edie-wm-backend-desktop-focus desktop)

  )

(defun edie-wm-default-desktop-list ()
  "Return the default desktop list."
  (mapcar (lambda (name)
            `(desktop . ((name . ,name))))
          edie-wm-default-desktop-list))

(defun edie-wm-desktop-reset-list ()
  "Reset the desktop list."
  (interactive)
  (setq edie-wm--desktop-alist (mapcar (lambda (dsk)
                                         (cons (edie-wm-property dsk 'id) dsk))
                                       (edie-wm-backend-desktop-list))))

(defun edie-wm-desktop-list ()
  "The list of virtual desktops."
  (declare (edie-log nil))
  (mapcar #'cdr (or edie-wm--desktop-alist (edie-wm-desktop-reset-list))))

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

(cl-defstruct edie-wm-window
  id
  (left 0 :type integer)
  (top 0 :type integer)
  (width 0 :type integer)
  (height 0 :type integer)
  (skip-window-list nil :type boolean)
  desktop class instance title)

(defalias 'edie-wm-window-skip-list-p 'edie-wm-window-skip-list)

(defun edie-wm-on-window-focus (wid)
  (declare (edie-log t))
  (if (and wid (not (edie-wm-property (edie-wm-current-desktop) 'focused-window)))
      (edie-wm-set-property (edie-wm-current-desktop) 'focused-window wid))

  (let ((window (and wid (edie-wm-window `((id . ,wid))))))
    (cond
     ((null wid)

      (run-hooks 'edie-wm-window-focus-changed-hook))
     ((eq (edie-wm-current-window) window)
      (edie-wm-window-raise-current)
      (run-hooks 'edie-wm-window-focus-changed-hook))
     (window
      (edie-wm-focus-window window)))))

(defun edie-wm-on-window-add (window)
  (declare (edie-log t))
  ;; TODO ensure that window still exists
  (setf (map-elt edie-wm--window-list (edie-wm-window-id window)) window)

  (edie-wm-with-current-window window
    (run-hooks 'edie-wm-window-added-hook))

  (edie-wm-focus-window window))

(defun edie-wm-on-window-remove (wid)
  (declare (edie-log t))
  (when-let ((elt (assoc wid edie-wm--window-list))
             (window (cdr elt))
             (desktop (edie-wm-desktop `((id . ,(edie-wm-window-desktop window))))))
    (setq edie-wm--window-list (delq elt edie-wm--window-list))

    (edie-wm-with-current-window window
    (run-hooks 'edie-wm-window-closed-hook))))

(defun edie-wm-on-window-update (wid changes)
  (declare (edie-log t))
  (when-let ((window (or (and wid (cdr (assoc wid edie-wm--window-list)))
                         (edie-wm-current-window)))
             ((edie-wm-window-merge-changes window changes)))
    (edie-wm-with-current-window window
      (run-hooks 'edie-wm-window-updated-hook))))

(defmacro edie-wm-with-current-window (window &rest body)
  "Execute BODY with WINDOW as the current window."
  (declare (indent defun))
  `(let ((edie-wm--current-window-id (edie-wm-window-id ,window)))
     ,@body))

(defun edie-wm-focus-window (window)
  "Focus WINDOW."
  (declare (edie-log t))
  (let* ((wid (edie-wm-window-id window))
         (elt (assoc wid edie-wm--window-list)))
    (edie-wm-set-property (edie-wm-current-desktop) 'focused-window wid)

    (setq edie-wm--window-list (delq elt edie-wm--window-list))
    (push elt edie-wm--window-list)

    (edie-wm-backend-window-focus wid)))

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

(defun edie-wm-window-raise-current ()
  "Raise WINDOW."
  (declare (edie-log t))
  (edie-wm-backend-window-raise-current))

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
  (edie-wm-window `((id . ,(edie-wm-property (edie-wm-current-desktop) 'focused-window)))))

(defun edie-wm-window (filters)
  "Return the first window that matches FILTERS."
  (declare (edie-log nil))
  (car (edie-wm-window-list filters)))

(defun edie-wm-window-alist ()
  "An alist of windows where the keys are the window ids."
  (or edie-wm--window-list (edie-wm-reset-window-list)))

(defun edie-wm-window-reset-list ()
  "Reload the window list."
  (declare (edie-log t))
  (interactive)
  (let ((windows nil))
    (dolist (w (edie-wm-backend-window-list))
      (push (cons (edie-wm-window-id w) w) windows))
    (setq edie-wm--window-list (nreverse windows))))

(defun edie-wm-window-list (&optional filters)
  "The list of windows across all desktops."
  (declare (edie-log nil))
  (if filters
      (edie-wm-window-filter-list filters)
    (mapcar #'cdr (edie-wm-window-alist))))

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
         (or (not skip-window-list) (not (edie-wm-window-skip-list-p window)))
         (or (not class) (string-match-p class (edie-wm-window-class window)))
         (or (not desktop) (equal desktop (edie-wm-window-desktop window)))
         (or (not height) (equal height (edie-wm-window-height window)))
         (or (not instance) (string-match-p instance (edie-wm-window-instance window)))
         (or (not left) (equal left (edie-wm-window-left window)))
         (or (not title) (string-match-p title (edie-wm-window-title window)))
         (or (not top) (equal top (edie-wm-window-top window)))
         (or (not width) (equal width (edie-wm-window-width window))))))

(defun edie-wm-update-window (window alist)
  ""
  (declare (edie-log t))
  (when-let ((changes (edie-wm-window-merge-changes window alist)))
    (edie-wm-backend-window-update (edie-wm-window-id window) changes)))

(defun edie-wm-window-merge-changes (window alist)
  (declare (edie-log nil))
  (let ((changes nil))
    (dolist (elt alist)
      (pcase elt
        ((and `(left . ,val) (guard (not (equal val (edie-wm-window-left window)))))
         (setf (edie-wm-window-left window) val)
         (setf (alist-get (car elt) changes) val)
         (unless (alist-get 'top changes)
           (setf (alist-get 'top changes) (edie-wm-window-top window))))
        ((and `(top . ,val) (guard (not (equal val (edie-wm-window-top window)))))
         (setf (edie-wm-window-top window) val)
         (setf (alist-get (car elt) changes) val)
         (unless (alist-get 'left changes)
           (setf (alist-get 'left changes) (edie-wm-window-left window))))
        ((and `(width . ,val) (guard (not (equal val (edie-wm-window-width window)))))
         (setf (edie-wm-window-width window) val)
         (setf (alist-get (car elt) changes) val)
         (unless (alist-get 'height changes)
           (setf (alist-get 'height changes) (edie-wm-window-height window))))
        ((and `(height . ,val) (guard (not (equal val (edie-wm-window-height window)))))
         (setf (edie-wm-window-height window) val)
         (setf (alist-get (car elt) changes) val)
         (unless (alist-get 'width changes)
           (setf (alist-get 'width changes) (edie-wm-window-width window))))
        ((and `(class . ,val) (guard (not (equal val (edie-wm-window-class window)))))
         (setf (edie-wm-window-class window) val)
         (setf (alist-get (car elt) changes) val))
        ((and `(instance . ,val) (guard (not (equal val (edie-wm-window-instance window)))))
         (setf (edie-wm-window-instance window) val)
         (setf (alist-get (car elt) changes) val))
        ((and `(desktop . ,val) (guard (not (equal val (edie-wm-window-desktop window)))))
         (setf (edie-wm-window-desktop window) val)
         (setf (alist-get (car elt) changes) val))
        ((and `(title . ,val) (guard (not (equal val (edie-wm-window-title window)))))
         (setf (edie-wm-window-title window) val)
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

(defvar edie-wm--monitor-list nil)

(defun edie-wm-current-monitor ()
  (declare (edie-log nil))
  (seq-find #'edie-wm-monitor-focused-p (edie-wm-monitor-list)))

(defun edie-wm-monitor-list ()
  (declare (edie-log nil))
  (or edie-wm--monitor-list (edie-wm-monitor-reset-list)))

(defun edie-wm-monitor-reset-list ()
  "Reset the list of monitors."
  (declare (edie-log t))
  (interactive)
  (setq edie-wm--monitor-list (edie-wm-backend-monitor-list)))

(defun edie-wm-on-monitor-add (_)
  (declare (edie-log t))
  (edie-wm-monitor-reset-list)
  (edie-wm-desktop-reset-list)
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

(defun edie-wm-on-desktop-focus-change (desktop-id)
  (declare (edie-log t))
  (when-let ((monitor (edie-wm-current-monitor))
             ((equal (edie-wm-property monitor 'focused-desktop) desktop-id)))
    (run-hooks 'edie-wm-desktop-focus-changed-hook)

    (when-let (((not (edie-wm-current-window)))
               (window (edie-wm-window `((desktop . ,(edie-wm-property desktop 'id))))))
      (edie-wm-focus-window window))))

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
           (rules (edie-wm-find-rule window))
           result)
      (dolist (fun edie-wm-window-rules-functions)
        (setq result (funcall fun rules window))
        (dolist (elt result)
          (setf (alist-get (car elt) new-props) (cdr elt))))

      (when new-props
        (edie-wm-update-window window new-props)))))

(defun edie-wm-find-rule (window)
  (declare (edie-log t))
  (cdr (seq-find (pcase-lambda (`(,filter . ,_))
                   (edie-wm-window-filter-match-p filter window))
                 edie-wm-rules-alist)))

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
               (window (car (reverse windows))))
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
               ((cl-struct edie-wm-window left top width height) window))
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
