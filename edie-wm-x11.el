;;; edie-wm-x11.el --- X11 backend for edwin -*- lexical-binding: t -*-

;; Copyright (C) 2022 David Leal

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

(require 'edie-wm)
(require 'map)
(require 'pcase)
(require 'xcb)
(require 'xcb-ewmh)

(defconst edie-wm-x11-slot-tag 0)
(defconst edie-wm-x11-slot-id 1)
(defconst edie-wm-x11-slot-properties 2)

(defconst edie-wm-x11-sticky #xffffffff)

(defvar edie-wm-x11--window-id-list nil)

(define-error 'edie-wm-x11-no-reply "Expected a reply for this request.")

(define-minor-mode edie-wm-x11-mode
  nil
  :global t
  (if edie-wm-x11-mode
      (progn
        (setq edie-wm-current-desktop-function #'edie-wm-x11-current-desktop)
        (setq edie-wm-current-window-id-function #'edie-wm-x11-current-window-id)
        (setq edie-wm-focus-window-function #'edie-wm-x11-window-focus)
        (setq edie-wm-set-desktop-function #'edie-wm-x11-wm-set-desktop)
        (setq edie-wm-update-window-function #'edie-wm-x11-window-update)
        (setq edie-wm-window-list-function #'edie-wm-x11-window-list)
        (setq edie-wm-window-make-function #'edie-wm-x11-window-make)

        (add-hook 'edie-wm-window-close-functions #'edie-wm-x11-window-close)

        (edie-wm-x11-connect)
        (edie-wm-x11--event-listen edie-wm-x11--root-window)

        (xcb:+event edie-wm-x11--connection
                    'xcb:PropertyNotify
                    #'edie-wm-x11--event-on-property-change)

        (xcb:flush edie-wm-x11--connection))

    (setq edie-wm-current-desktop-function nil)
    (setq edie-wm-current-window-id-function nil)
    (setq edie-wm-focus-window-function nil)
    (setq edie-wm-set-desktop-function nil)
    (setq edie-wm-update-window-function nil)
    (setq edie-wm-window-list-function nil)
    (setq edie-wm-window-make-function nil)

    (remove-hook 'edie-wm-window-close-functions #'edie-wm-x11-window-close)

    (edie-wm-x11-disconnect)))

;; Desktops

(defun edie-wm-x11-wm-set-desktops (desktops)
  (let ((number-of-desktops (length desktops))
        (desktop-names (string-join
                        (seq-map (pcase-lambda ((seq 'desktop (map :name))) name) desktops)
                        "\0")))
    (edie-wm-x11-dispatch (edie-wm-x11-make-ewmh-event
                       'xcb:ewmh:_NET_NUMBER_OF_DESKTOPS
                       :window edie-wm-x11--root-window
                       :new-number-of-desktops number-of-desktops))

    (edie-wm-x11-dispatch 'xcb:ewmh:set-_NET_DESKTOP_NAMES
                      :window edie-wm-x11--root-window
                      :data desktop-names)))

(defun edie-wm-x11-wm-set-desktop (desktop-id)
  (cl-assert desktop-id)

  (edie-wm-x11-dispatch (edie-wm-x11-make-ewmh-event 'xcb:ewmh:_NET_CURRENT_DESKTOP
                                                     :timestamp xcb:Time:CurrentTime
                                                     :new-index desktop-id
                                                     :window edie-wm-x11--root-window)))

(defun edie-wm-x11-current-desktop ()
  (edie-wm-desktop-make nil (edie-wm-x11<->property '_NET_CURRENT_DESKTOP)))

(defun edie-wm-x11-current-window-id ()
  (edie-wm-x11<->property '_NET_ACTIVE_WINDOW))

(defalias 'edie-wm-x11-window-list 'edie-wm-x11-root-client-list-stacking)

(defun edie-wm-x11-desktop-make (did)
  (let ((id (if (= did edie-wm-x11-sticky) t did)))
    `(desktop (:id ,id) ,id)))

(defun edie-wm-x11->window-state (wid)
  (edie-wm-x11->property '_NET_WM_STATE wid))

(defun edie-wm-x11<-window-state (req)
  (seq-map (lambda (atom)
             (cond
              ((= atom xcb:Atom:_NET_WM_STATE_ABOVE)
               'above)
              ((= atom xcb:Atom:_NET_WM_STATE_BELOW)
               'below)
              ((= atom xcb:Atom:_NET_WM_STATE_MODAL)
               'modal)
              ((= atom xcb:Atom:_NET_WM_STATE_HIDDEN)
               'hidden)
              ((= atom xcb:Atom:_NET_WM_STATE_SHADED)
               'shaded)
              ((= atom xcb:Atom:_NET_WM_STATE_STICKY)
               'sticky)
              ((= atom xcb:Atom:_NET_WM_STATE_FOCUSED)
               'focused)
              ((= atom xcb:Atom:_NET_WM_STATE_FULLSCREEN)
               'fullscreen)
              ((= atom xcb:Atom:_NET_WM_STATE_SKIP_PAGER)
               'skip-pager)
              ((= atom xcb:Atom:_NET_WM_STATE_SKIP_TASKBAR)
               'skip-taskbar)
              ((= atom xcb:Atom:_NET_WM_STATE_MAXIMIZED_HORZ)
               'maximized-horizontally)
              ((= atom xcb:Atom:_NET_WM_STATE_MAXIMIZED_VERT)
               'maximized-vertically)
              ((= atom xcb:Atom:_OB_WM_STATE_UNDECORATED)
               'undecorated)
              (t atom)))
           (edie-wm-x11<-property req)))

(defun edie-wm-x11-window-make (wid)
  (condition-case nil
      (pcase-let* ((parent-id-req (edie-wm-x11-> 'xcb:QueryTree :window wid))
                   (title-req (edie-wm-x11->property '_NET_WM_NAME wid))
                   (desktop-req (edie-wm-x11->property '_NET_WM_DESKTOP wid))
                   (class-req (edie-wm-x11->property 'WM_CLASS wid))
                   (geometry-req (edie-wm-x11->
                                  'xcb:GetGeometry
                                  :drawable (slot-value (edie-wm-x11<- parent-id-req) :parent))))

        (list 'window wid (nconc `(:id ,wid
                                   :title ,(edie-wm-x11<-property title-req)
                                   :desktop ,(edie-wm-x11<-property desktop-req))
                                 (flatten-tree
                                  (cl-mapcar #'list
                                             '(:instance :class)
                                             (thread-first
                                               (edie-wm-x11<- class-req)
                                               (slot-value 'value)
                                               (split-string "\0"))))
                                 (with-slots (x y width height) (edie-wm-x11<- geometry-req)
                                   (list :left x :top y :width width :height height)))))
    (edie-wm-x11-no-reply nil)))

(defun edie-wm-x11-root-client-list-stacking ()
  (nreverse (seq-map #'edie-wm-x11-window-make (edie-wm-x11<->property '_NET_CLIENT_LIST_STACKING))))

(defun edie-wm-x11-window-focus (wid)
  ""
  (edie-wm-x11-dispatch (edie-wm-x11-make-ewmh-event
                         'xcb:ewmh:_NET_ACTIVE_WINDOW
                         :source-indication 2
                         :timestamp xcb:Time:CurrentTime
                         :window wid
                         :current-active-window 0)))

(defun edie-wm-x11-window-close (window)
  (pcase-let (((seq 'window wid) window))
    (cl-assert wid)

    (edie-wm-x11-dispatch
     (edie-wm-x11-make-ewmh-event 'xcb:ewmh:_NET_CLOSE_WINDOW
                              :source-indication 2
                              :timestamp xcb:Time:CurrentTime
                              :window wid))))

(defun edie-wm-x11-window-update (wid plist)
  ""
  (map-let (:left :top :width :height) plist
    (when (and left top width height)
      (edie-wm-x11-window-update-geometry wid plist)))

  (when (map-contains-key plist :hidden)
    (edie-wm-x11-window-update-visibility wid (not (map-elt plist :hidden))))

  (when (map-elt plist :focus)
    (edie-wm-x11-window-focus wid))

  (map-let (:type) plist
    (when type
      (edie-wm-x11-window-update-type wid type)))

  (pcase plist
    ((map (:desktop (and (pred integerp) desktop-id)))
     (edie-wm-x11-window-update-desktop wid desktop-id))
    ((map (:desktop (pred (eq t))))
     (edie-wm-x11-window-update-desktop wid edie-wm-x11-sticky)))

  (pcase plist
    ((map (:stacking (and stacking (pred identity))))
     (edie-wm-x11-window-update-stacking wid stacking))))

(defun edie-wm-x11-window-update-type (wid type)
  (let ((type-atom (cond
                    ((eq type 'dock) xcb:Atom:_NET_WM_WINDOW_TYPE_DOCK))))
    (cl-assert wid type)
    (edie-wm-x11-dispatch 'xcb:UnmapWindow :window wid)
    (edie-wm-x11-dispatch 'xcb:ewmh:set-_NET_WM_WINDOW_TYPE
                          :window wid
                          :data (list type-atom))
    (edie-wm-x11-dispatch 'xcb:MapWindow :window wid)))

(defun edie-wm-x11-window-update-visibility (wid visible)
  (let ((action (if visible
                    xcb:ewmh:_NET_WM_STATE_REMOVE
                  xcb:ewmh:_NET_WM_STATE_ADD))))
  (cl-assert wid)
  (edie-wm-x11-dispatch (edie-wm-x11-make-ewmh-event 'xcb:ewmh:_NET_WM_STATE
                                                     :action action
                                                     :first-property xcb:Atom:_NET_WM_STATE_HIDDEN
                                                     :second-property 0
                                                     :window wid
                                                     :source-indication 2)))

(defun edie-wm-x11-window-update-stacking (wid stacking)
  (let ((prop (cond
               ((eq stacking 'above)
                xcb:Atom:_NET_WM_STATE_ABOVE)
               ((eq stacking 'below)
                xcb:Atom:_NET_WM_STATE_BELOW))))
    (cl-assert wid)
    (edie-wm-x11-dispatch (edie-wm-x11-make-ewmh-event 'xcb:ewmh:_NET_WM_STATE
                                                       :action xcb:ewmh:_NET_WM_STATE_ADD
                                                       :first-property prop
                                                       :second-property 0
                                                       :window wid
                                                       :source-indication 2))))

(defun edie-wm-x11-window-update-geometry (wid plist)
  (pcase-let* (((map :left :top :width :height :border) plist))
    (cl-assert (and wid (numberp left) (numberp top) (numberp width) (numberp height)))

    (edie-wm-x11-dispatch 'xcb:ConfigureWindow
                          :window wid
                          :value-mask (logior xcb:ConfigWindow:X
                                              xcb:ConfigWindow:Y
                                              xcb:ConfigWindow:Width
                                              xcb:ConfigWindow:Height)
                          :x left :y top
                          :width (- width (* 2 border)) :height (- height (* 2 border)))))

(defun edie-wm-x11-window-update-desktop (wid desktop-id)
  (cl-assert wid t)
  (cl-assert desktop-id t)

  (edie-wm-x11-dispatch (make-instance 'xcb:MapWindow :window wid)
                        (edie-wm-x11-make-ewmh-event 'xcb:ewmh:_NET_WM_DESKTOP
                                                     :window wid
                                                     :new-desktop desktop-id
                                                     :source-indication 2)))

(defun edie-wm-x11--event-listen (wid)
  (xcb:+request-checked+request-check edie-wm-x11--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window wid
                     :value-mask xcb:CW:EventMask
                     :event-mask (logior xcb:EventMask:PropertyChange
                                         xcb:EventMask:SubstructureNotify)))
  (xcb:flush edie-wm-x11--connection))

(defun edie-wm-x11--event-on-property-change (data _)
  (let ((event (make-instance 'xcb:PropertyNotify)))
    (xcb:unmarshal event data)
    (with-slots (atom window) event
      (cond
       ((= atom xcb:Atom:_NET_WM_NAME)
        (edie-wm-on-window-update
         window
         (list :title (edie-wm-x11<->property '_NET_WM_NAME window))))
       ((= atom xcb:Atom:_NET_CLIENT_LIST)
        (let* ((old-list edie-wm-x11--window-id-list)
               (new-list (edie-wm-x11<->property '_NET_CLIENT_LIST))
               (added (seq-difference new-list old-list))
               (removed (seq-difference old-list new-list)))
          (setq edie-wm-x11--window-id-list new-list)
          (if added
              (dolist (wid added)
                (when-let ((wnd (edie-wm-x11-window-make wid)))
                  (edie-wm-x11--event-listen wid)
                  (edie-wm-on-window-add wnd)))
            (dolist (wid removed)
              (edie-wm-on-window-remove wid)))))
       ((= atom xcb:Atom:_NET_ACTIVE_WINDOW)
        (edie-wm-on-window-focus (edie-wm-x11-current-window-id)))
       ((= atom xcb:Atom:_NET_CURRENT_DESKTOP)
        (edie-wm-on-desktop-focus-change))))))

(defun edie-wm-x11->property (sym &optional window-id)
  (let* ((str (symbol-name sym))
         (atom-req-sym (cond
                        ((string-prefix-p "_NET_" str)
                         (intern (concat "xcb:ewmh:get-" str)))
                        ((string-prefix-p "WM_" str)
                         (intern (concat "xcb:icccm:get-" str)))
                        (t
                         (error "Not implemented: %S" sym)))))
    (edie-wm-x11-> atom-req-sym :window (or window-id edie-wm-x11--root-window))))

(defun edie-wm-x11<-property (req)
  (slot-value (edie-wm-x11<- req) 'value))

(defun edie-wm-x11<->property (&rest args)
  (edie-wm-x11<-property (apply #'edie-wm-x11->property args)))

(defun edie-wm-x11-> (sym &rest args)
  (xcb:+request edie-wm-x11--connection
      (apply #'make-instance sym args)))

(defun edie-wm-x11<- (req)
  (if-let ((reply (car (xcb:+reply edie-wm-x11--connection req))))
      reply
    (signal 'edie-wm-x11-no-reply (list req))))

(defun edie-wm-x11-make-ewmh-event (event-class &rest event-args)
  (let ((event (apply #'make-instance event-class event-args)))
    (make-instance 'xcb:ewmh:SendEvent
                   :destination edie-wm-x11--root-window
                   :event (xcb:marshal event edie-wm-x11--connection))))

(defun edie-wm-x11-send-ewmh (event &rest event-args)
  (xcb:+request-checked+request-check edie-wm-x11--connection
      (edie-wm-x11-make-ewmh-message event event-args))
  (xcb:flush edie-wm-x11--connection))

(defun edie-wm-x11-dispatch (event &rest event-args)
  (if (symbolp event)
      (xcb:+request-checked+request-check edie-wm-x11--connection
          (apply #'make-instance event event-args))
    (xcb:+request-checked+request-check edie-wm-x11--connection event)
    (dolist (e event-args)
      (xcb:+request-checked+request-check edie-wm-x11--connection e)))

  (xcb:flush edie-wm-x11--connection))

(defvar edie-wm-x11--connection nil
  "An X11 connection structure, as returned by XELB.")
(defvar edie-wm-x11--root-window nil
  "The ID of the root window.")

(defun edie-wm-x11-root-window ()
  (cl-assert edie-wm-x11--root-window)
  edie-wm-x11--root-window)

(defun edie-wm-x11-connect ()
  (when (not edie-wm-x11--connection)
    (setq edie-wm-x11--connection (xcb:connect (getenv "DISPLAY"))
          edie-wm-x11--root-window (let* ((setup (xcb:get-setup edie-wm-x11--connection))
                                      (root (car (slot-value setup 'roots))))
                                 (slot-value root 'root)))
    (xcb:ewmh:init edie-wm-x11--connection)
    (xcb:flush edie-wm-x11--connection))
  edie-wm-x11--connection)

(defun edie-wm-x11-disconnect ()
  (when edie-wm-x11--connection
    (xcb:disconnect edie-wm-x11--connection))

  (setq edie-wm-x11--connection nil
        edie-wm-x11--root-window nil))

(defclass xcb:ewmh:_NET_NUMBER_OF_DESKTOPS
  (xcb:ewmh:-ClientMessage)
  ((type :initform xcb:Atom:_NET_NUMBER_OF_DESKTOPS)
   (new-number-of-desktops :initarg :new-number-of-desktops :type xcb:CARD32)))

(provide 'edie-wm-x11)
;;; edie-wm-x11.el ends here
