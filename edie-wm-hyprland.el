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

  (setq edie-wm-current-desktop-function #'edie-wm-hypr--current-desktop
        edie-wm-current-window-id-function #'edie-wm-hypr--current-window-id
        edie-wm-focus-window-function #'edie-wm-hypr--window-focus
        edie-wm-set-desktop-function #'edie-wm-hypr--wm-set-desktop
        edie-wm-update-window-function #'edie-wm-hypr--window-update
        edie-wm-window-list-function #'edie-wm-hypr--window-list))

(defun edie-wm-hyprland-stop ()
  (delete-process edie-wm-hypr--conn-events)
  (setq edie-wm-hypr--conn-events nil)

  (setq edie-wm-current-desktop-function
        edie-wm-current-window-id-function
        edie-wm-focus-window-function
        edie-wm-set-desktop-function
        edie-wm-update-window-function
        edie-wm-window-list-function))

(defun edie-wm-hypr--current-desktop ()
  (let ((str (edie-wm-hypr--read 'monitors)))
    (when (string-match (rx "active workspace: " (group (+ digit))) str)
      (edie-wm-desktop-make nil (1- (string-to-number (match-string 1 str)))))))

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
  '(wnd-add wnd-focus wnd-upd dsk-focus wnd-rm))

(defun edie-wm-hypr--insert-event (event)
  "Push EVENT to the event queue.

EVENT is a cons cell with the event type (a symbol) as its CAR, and the
event data as its CDR.

An event is placed in the queue according to its type.

The following event types are supported (listed in order of priority):

- `wnd-add': a window has been created;
- `wnd-focus': the window with the given id has received focus;
- `wnd-upd': the window with the given id has been updated;
- `dsk-focus': the desktop with the given id has received focus;
- `wnd-rm': a window has been removed."
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
    ((rx "activewindow>>"
         (let class (+ (not ","))) ","
         (let title (+ (not ",")))
         eos)
     (edie-wm-hypr--insert-event
      (list 'wnd-upd :class class :title title)))
    ((rx "activewindowv2>>" (let wid (+ hex)))
     (edie-wm-hypr--insert-event (cons 'wnd-focus wid)))
    ((rx "activewindowv2>>,")
     (edie-wm-hypr--insert-event (cons 'wnd-focus nil)))
    ((rx "openwindow>>"
         (let wid (+ hex)) ","
         (let did (+ digit)) ","
         (let class (+ (not ","))) ","
         (let title (+ (not ",")))
         eos)
     (edie-wm-hypr--insert-event
      (cons 'wnd-add (list 'window wid (list :desktop did :class class :title title)))))
    ((rx "closewindow>>" (let wid (+ hex)))
     (edie-wm-hypr--insert-event (cons 'wnd-rm wid)))
    ((rx "workspace>>" (let did (+ digit)))
     (edie-wm-hypr--insert-event (cons 'dsk-focus did)))))

(defun edie-wm-hypr--flush-events ()
  (let ((queue edie-wm-hypr--event-queue))
    (setq edie-wm-hypr--event-queue nil)

    (dolist (event (nreverse queue))
      (pcase event
        (`(wnd-add . ,wnd)
         (edie-wm-on-window-add wnd))
        (`(wnd-focus . ,wid)
         (edie-wm-on-window-focus wid))
        ((seq 'wnd-upd &rest changes)
         (edie-wm-on-window-update nil changes))
        (`(dsk-focus . ,_)
         (edie-wm-on-desktop-focus-change))
        (`(wnd-rm . ,wid)
         (edie-wm-on-window-remove wid))))))

(defun edie-wm-hypr--window-raise-active ()
  (edie-wm-hypr--write 'bringactivetotop))

(defun edie-wm-hypr--window-focus (wid)
  (edie-wm-hypr--window-update wid '(:focus t)))

(defun edie-wm-hypr--window-update (wid props)
  (cl-assert (and wid props) t)

  (pcase-let (((map :left :top :width :height :focus) props))
    (when (and left top)
      (cl-assert (and (numberp left) (numberp top)) t)
      (edie-wm-hypr--write 'moveactive 'exact left top))

    (when (and width height)
      (cl-assert (and (numberp width) (numberp height)) t)
      (edie-wm-hypr--write 'resizeactive 'exact width height))

    (when focus
     (edie-wm-hypr--write 'focuswindow (format "address:0x%s" wid)) )))

(defun edie-wm-hypr--current-window-id ()
  (when-let ((wnd (edie-wm-hypr--current-window)))
    (edie-wm-window-id wnd)))

(defun edie-wm-hypr--current-window ()
  (edie-wm-hypr--parse-window (edie-wm-hypr--read 'activewindow)))

(defun edie-wm-hypr--parse-window (string)
  (when (not (string= (string-trim string) "Invalid"))
    (let* ((wid nil)
           (wprops nil))
      (dolist (line (split-string string "\n" t "[[:space:]]+"))
        (pcase line
          ((rx bos "Window " (let id (1+ hex)) " -> ")
           (setq wid id))
          ((rx bos "at: " (let x (+ digit)) "," (let y (+ digit)) eos)
           (setq wprops (plist-put wprops :left (string-to-number x)))
           (setq wprops (plist-put wprops :top (string-to-number y))))
          ((rx bos "size: " (let w (+ digit)) "," (let h (+ digit)) eos)
           (setq wprops (plist-put wprops :width (string-to-number w)))
           (setq wprops (plist-put wprops :height (string-to-number h))))
          ((rx bos "workspace: " (let ws (+ digit)) (+ anything) eos)
           (setq wprops (plist-put wprops :desktop (1- (string-to-number ws)))))
          ((rx bos "floating: 1" eos)
           (setq wprops (plist-put wprops :floating t)))
          ((rx bos "monitor: " (let mon (+ digit)) eos)
           (setq wprops (plist-put wprops :monitor (string-to-number mon))))
          ((rx bos "class: " (let class (+ (not space))) eos)
           (setq wprops (plist-put wprops :class class)))
          ((rx bos "title: " (let title (+ anything)) eos)
           (setq wprops (plist-put wprops :title title)))
          ((rx bos "pid: " (let pid (+ digit)) eos)
           (setq wprops (plist-put wprops :pid (string-to-number pid))))
          ((rx bos "xwayland: 1" eos)
           (setq wprops (plist-put wprops :xwayland t)))
          ((rx bos "pinned: 1" eos)
           (setq wprops (plist-put wprops :desktop t)))
          ((rx bos "fullscreen: 1" eos)
           (setq wprops (plist-put wprops :fullscreen t)))
          ((rx bos "fullscreenmode: 1" eos)
           (setq wprops (plist-put wprops :fullscreenmode t)))
          ((rx bos "fakefullscreen: 1" eos)
           (setq wprops (plist-put wprops :fakefullscreen t)))
          ((rx bos "grouped: 1" eos)
           (setq wprops (plist-put wprops :grouped t)))
          ((rx bos "swallowing: 1" eos)
           (setq wprops (plist-put wprops :swallowing t)))))
      (list 'window wid wprops))))

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
