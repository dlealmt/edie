;;; edie-wm-hyprland.el --- Hyprland backend for edie-wm -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 David Leal

;; Author: David Leal <dleal@mojotech.com>
;; Maintainer: David Leal <dleal@mojotech.com>
;; Created: 2022
;; Version: 0.0.1

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

(defvar edie-wm-hypr--conn-events nil)
(defvar edie-wm-hypr--current-window-id nil)

(defun edie-wm-hyprland-start ()
  (setq edie-wm-hypr--conn-events (make-network-process
                                   :service (edie-wm-hypr--socket-path "socket2")
                                   :name "edie-wm-hyprland-events"
                                   :buffer "*edie-wm-hyprland-events*"
                                   :filter #'edie-wm-hypr--conn-events-filter
                                   :family 'local))

  (setq edie-wm-set-desktop-function #'edie-wm-hypr--wm-set-desktop
        edie-wm-update-window-function #'edie-wm-hypr--window-update
        edie-wm-window-list-function #'edie-wm-hypr--window-list))

(defun edie-wm-hyprland-stop ()
  (delete-process edie-wm-hypr--conn-events)
  (setq edie-wm-hypr--conn-events nil)

  (setq edie-wm-set-desktop-function nil
        edie-wm-update-window-function nil
        edie-wm-window-list-function nil))

(defun edie-wm-backend-current-desktop-id ()
  (let ((str (edie-wm-hypr--read 'monitors)))
    (when (string-match (rx "active workspace: " (group (+ digit))) str)
      (match-string 1 str))))

(defun edie-wm-backend-desktop-id-list ()
  (let ((ids nil))
    (dotimes (i (length edie-wm-default-desktop-list))
      (push (number-to-string (1+ i)) ids))
    (nreverse ids)))

(defun edie-wm-hypr--window-list ()
  (mapcar #'edie-wm-hypr--parse-window
          (split-string (edie-wm-hypr--read 'clients) "\n\n" t "[[:space:]\n]+")))

(defun edie-wm-hypr--conn-events-filter (proc string)
  (edie-wm-hypr--print-to-process-buffer proc string)

  (let ((lines (split-string string "\n")))
    (dolist (line lines)
      (edie-wm-hypr--handle-event line))))

(defvar edie-wm-hypr--event-queue nil)
(defvar edie-wm-hypr--event-queue-timer nil)
(defvar edie-wm-hypr--event-queue-interval 0.1)

(defvar edie-wm-hypr--event-priority
  '(dsk-focus wnd-add mon-focus wnd-focus wnd-upd wnd-rm mon-add mon-rm))

(defun edie-wm-hypr--insert-event (event)
  "Push EVENT to the event queue.

EVENT is a cons cell with the event type (a symbol) as its CAR, and the
event data as its CDR.

An event is placed in the queue according to its type.

The following event types are supported (listed in order of priority):

- `dsk-focus': the desktop with the given id has received focus;
- `wnd-add': a window has been created;
- `mon-focus': the active monitor has changed;
- `wnd-focus': the window with the given id has received focus;
- `wnd-upd': the window with the given id has been updated;
- `wnd-rm': a window has been removed;
- `mon-add': a monitor has been added;
- `mon-rm': a monitor has been removed."
  (when (timerp edie-wm-hypr--event-queue-timer)
    (cancel-timer edie-wm-hypr--event-queue-timer))

  (let* ((prios edie-wm-hypr--event-priority)
         (event-prio (seq-position prios (car event)))
         (queue edie-wm-hypr--event-queue)
         (curr (car queue)))
    (while (and (setq curr (cadr queue))
                (> (seq-position prios (car curr)) event-prio))
      (setq queue (cdr queue)))
    (if queue
        (setcdr queue (cons event (cdr queue)))
        (setq edie-wm-hypr--event-queue (list event)))

    (setq edie-wm-hypr--event-queue-timer
        (run-with-timer edie-wm-hypr--event-queue-interval nil #'edie-wm-hypr--flush-events))))

(defun edie-wm-hypr--handle-event (event)
  (pcase event
    ((rx "activewindowv2>>" (let wid (+ hex)))
     (let ((window (edie-wm-hypr--current-window)))
       (when (equal wid (edie-wm-window-id window))
         (edie-wm-hypr--insert-event
          (list 'wnd-upd wid
                :class (edie-wm-window-class window)
                :title (edie-wm-window-title window)
                :left (edie-wm-window-left window)
                :top (edie-wm-window-top window)
                :width (edie-wm-window-width window)
                :height (edie-wm-window-height window)))
         (edie-wm-hypr--insert-event (cons 'wnd-focus wid)))))
    ((rx "activewindowv2>>,")
     (edie-wm-hypr--insert-event (cons 'wnd-focus nil)))
    ((rx "openwindow>>"
         (let wid (+ hex)) ","
         (let did (+ digit)) ","
         (let class (+ (not ","))) ","
         (let title (+ (not ",")))
         eos)
     (edie-wm-hypr--insert-event
      (cons 'wnd-add (make-edie-wm-window :id wid :desktop did :class class :title title))))
    ((rx "movewindow>>" (let wid (+ hex)) "," (let did (+ digit)))
     (edie-wm-hypr--insert-event (list 'wnd-upd wid :desktop did)))
    ((rx "closewindow>>" (let wid (+ hex)))
     (edie-wm-hypr--insert-event (cons 'wnd-rm wid)))
    ((rx "focusedmon>>" (let mon (+ (not ","))) "," (let did (+ any)))
     (edie-wm-hypr--insert-event (cons 'mon-focus mon))
     (edie-wm-hypr--insert-event (cons 'dsk-focus did)))
    ((rx "workspace>>" (let did (+ digit)))
     (edie-wm-hypr--insert-event (cons 'dsk-focus did)))
    ((rx "monitoradded>>" (let name (+ any)))
     (edie-wm-hypr--insert-event (cons 'mon-add name)))
    ((rx "monitorremoved>>" (let name (+ any)))
     (edie-wm-hypr--insert-event (cons 'mon-rm name)))))

(defun edie-wm-hypr--flush-events ()
  (let ((queue edie-wm-hypr--event-queue))
    (setq edie-wm-hypr--event-queue nil)

    (dolist (event (nreverse queue))
      (pcase event
        (`(wnd-add . ,wnd)
         (edie-wm-on-window-add wnd))
        (`(wnd-focus . ,wid)
         (edie-wm-on-window-focus wid))
        (`(mon-focus . ,mon)
         (edie-wm-on-monitor-focus-change mon))
        (`(dsk-focus . ,_)
         (edie-wm-on-desktop-focus-change))
        (`(wnd-rm . ,wid)
         (edie-wm-on-window-remove wid))
        (`(mon-add . ,mid)
         (edie-wm-on-monitor-add mid))
        (`(mon-rm . ,mid)
         (edie-wm-on-monitor-remove mid))
        ((seq 'wnd-upd wid &rest changes)
         (edie-wm-on-window-update wid changes))))))

(defun edie-wm-hypr--window-raise-active ()
  (edie-wm-hypr--write 'bringactivetotop))

(defun edie-wm-backend-window-focus (wid)
  (edie-wm-hypr--window-update wid '(:focus t)))

(defun edie-wm-hypr--window-update (wid props)
  (cl-assert (and wid props) t)

  (pcase-let (((map :left :top :width :height :focus :monitor) props))
    (when (and left top)
      (cl-assert (and (numberp left) (numberp top)) t)
      (edie-wm-hypr--write 'moveactive 'exact left top))

    (when (and width height)
      (cl-assert (and (numberp width) (numberp height)) t)
      (edie-wm-hypr--write 'resizeactive 'exact width height))

    (when focus
      (edie-wm-hypr--write 'focuswindow (format "address:0x%s" wid)))

    (when monitor
      (edie-wm-hypr--write 'movewindow monitor))))

(defun edie-wm-backend-current-window-id ()
  (when-let ((wnd (edie-wm-hypr--current-window)))
    (edie-wm-window-id wnd)))

(defun edie-wm-hypr--current-window ()
  (edie-wm-hypr--parse-window (edie-wm-hypr--read 'activewindow)))

(defun edie-wm-hypr--parse-window (string)
  (when (not (string= (string-trim string) "Invalid"))
    (let* ((window (make-edie-wm-window)))
      (dolist (line (split-string string "\n" t "[[:space:]]+"))
        (pcase line
          ((rx bos "Window " (let id (1+ hex)) " -> ")
           (setf (edie-wm-window-id window) id))
          ((rx bos "at: " (let x (+ digit)) "," (let y (+ digit)) eos)
           (setf (edie-wm-window-left window) (string-to-number x)
                 (edie-wm-window-top window) (string-to-number y)))
          ((rx bos "size: " (let w (+ digit)) "," (let h (+ digit)) eos)
           (setf (edie-wm-window-width window) (string-to-number w)
                 (edie-wm-window-height window) (string-to-number h)))
          ((rx bos "workspace: " (let ws (+ digit)) (+ anything) eos)
           (setf (edie-wm-window-desktop window) ws))
          ((rx bos "class: " (let class (+ (not space))) eos)
           (setf (edie-wm-window-class window) class))
          ((rx bos "title: " (let title (+ anything)) eos)
           (setf (edie-wm-window-title window) title))
          ((rx bos "pinned: 1" eos)
           (setf (edie-wm-window-desktop window) t))))
      window)))

(defun edie-wm-backend-monitor-list ()
  (mapcar #'edie-wm-hypr--parse-monitor
          (split-string (edie-wm-hypr--read 'monitors) "\n\n" t "[[:space:]\n]+")))

(defun edie-wm-hypr--parse-monitor (string)
  (let ((monitor (make-edie-wm-monitor)))
    (dolist (line (split-string string "\n" t "[[:space:]]+"))
      (pcase line
        ((rx bos "Monitor "
             (let name (+ (not space)))
             " (ID " (let id (+ digit)) "):")
         (setf (edie-wm-monitor-name monitor) name)
         (setf (edie-wm-monitor-id monitor) id))
        ((rx (let width (+ digit)) "x" (let height (+ digit))
             (+ (not space)) " at "
             (let x (+ digit)) "x" (let y (+ digit)))
         (setf (edie-wm-monitor-left monitor) (string-to-number x))
         (setf (edie-wm-monitor-top monitor) (string-to-number y))
         (setf (edie-wm-monitor-width monitor) (string-to-number width))
         (setf (edie-wm-monitor-height monitor) (string-to-number height)))
        ((rx bos "focused: yes")
         (setf (edie-wm-monitor-focused monitor) t))
        ((rx bos "active workspace: " (let ws (+ digit)))
         (setf (edie-wm-monitor-desktop monitor) ws))))
    monitor))

(defun edie-wm-hypr--read (&rest args)
  (with-output-to-string
    (let ((args (edie-wm-hypr--ctl args)))
      (apply #'call-process (car args) nil standard-output nil (cdr args)))))

(defun edie-wm-hypr--write (&rest args)
  (apply #'start-process "hyprctl" "*edie-wm-hyprland-commands*"
         (edie-wm-hypr--ctl (cons 'dispatch args))))

(defun edie-wm-hypr--ctl (args)
  (let ((args (mapcar (lambda (e)
                        (cond
                         ((numberp e)
                          (number-to-string e))
                         ((symbolp e)
                          (symbol-name e))
                         (t
                          e)))
                      args)))
    (cons "hyprctl" args)))

(defun edie-wm-hypr--socket-path (file)
  (format "/tmp/hypr/%s/.%s.sock" (getenv "HYPRLAND_INSTANCE_SIGNATURE") file))

(defun edie-wm-hypr--print-to-process-buffer (proc string)
  (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (let ((moving (= (point) (process-mark proc))))
          (save-excursion
            ;; Insert the text, advancing the process marker.
            (goto-char (process-mark proc))
            (insert string)
            (set-marker (process-mark proc) (point)))
          (when moving (goto-char (process-mark proc)))))))

(provide 'edie-wm-hyprland)
;;; edie-wm-hyprland.el ends here
