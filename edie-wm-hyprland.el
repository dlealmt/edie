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

(defun edie-wm-backend-start ()
  (setq edie-wm-hypr--conn-events (make-network-process
                                   :service (edie-wm-hypr--socket-path "socket2")
                                   :name "edie-wm-hyprland-events"
                                   :buffer "*edie-wm-hyprland-events*"
                                   :filter #'edie-wm-hypr--conn-events-filter
                                   :family 'local)))

(defun edie-wm-backend-stop ()
  (delete-process edie-wm-hypr--conn-events)
  (setq edie-wm-hypr--conn-events nil))

(defun edie-wm-backend-current-desktop-id ()
  (let ((str (edie-wm-hypr--read 'monitors)))
    (when (string-match (rx "active workspace: " (group (+ digit))) str)
      (match-string 1 str))))

(defun edie-wm-backend-window-list ()
  (declare (edie-log t))
  (mapcar #'edie-wm-hypr--parse-window
          (split-string (edie-wm-hypr--read 'clients) "\n\n" t "[[:space:]\n]+")))

(defun edie-wm-hypr--conn-events-filter (proc string)
  (edie-wm-hypr--print-to-process-buffer proc string)

  (let ((lines (split-string string "\n")))
    (dolist (line lines)
      (edie-wm-hypr--handle-event line))))

(defun edie-wm-backend-window-close (window)
  (edie-wm-hypr--write 'closewindow (format "address:0x%s" (edie-wm-window-id window))))

(defun edie-wm-hypr--handle-event (event)
  (pcase event
    ((rx "activewindowv2>>" (let wid (+ hex)))
     (let ((window (edie-wm-hypr--current-window)))
       (when (equal wid (edie-wm-window-id window))
         (edie-wm-on-window-update
          wid
          `((class . ,(edie-wm-window-class window))
            `(desktop . ,(edie-wm-window-desktop window))
            `(title . ,(edie-wm-window-title window))
            `(left . ,(edie-wm-window-left window))
            `(top . ,(edie-wm-window-top window))
            `(width . ,(edie-wm-window-width window))
            `(height . ,(edie-wm-window-height window))))
         (edie-wm-on-window-focus wid))))
    ((rx "activewindowv2>>,")
     (edie-wm-on-window-focus nil))
    ((rx "openwindow>>"
         (let wid (+ hex)) ","
         (let did (+ digit)) ","
         (let class (+ (not ","))) ","
         (let title (+ (not ",")))
         eos)
     (edie-wm-on-window-add (make-edie-wm-window :id wid :desktop did :class class :title title)))
    ((rx "movewindow>>" (let wid (+ hex)) "," (let did (+ digit)))
     (edie-wm-on-window-update wid `((desktop . ,did))))
    ((rx "closewindow>>" (let wid (+ hex)))
     (edie-wm-on-window-remove wid))
    ((rx "focusedmon>>" (let mon (+ (not ","))) "," (let did (+ any)))
     (edie-wm-on-monitor-focus-change mon))
    ((rx "workspace>>" (let did (+ digit)))
     (edie-wm-on-desktop-focus-change did))
    ((rx "monitoradded>>" (let name (+ any)))
     (edie-wm-on-monitor-add (cons 'mon-add name)))
    ((rx "monitorremoved>>" (let name (+ any)))
     (edie-wm-on-monitor-remove name))))

(defun edie-wm-backend-window-raise-current ()
  (declare (edie-log t))
  (edie-wm-hypr--write 'bringactivetotop))

(defun edie-wm-backend-window-focus (wid)
  (declare (edie-log t))
  (edie-wm-backend-window-update wid '((focus . t))))

(defun edie-wm-backend-window-update (wid props)
  (declare (edie-log t))
  (cl-assert (and wid props) t)

  (pcase-let (((map left top width height focus monitor) props))
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

(defun edie-wm-hypr--current-monitor ()
  (seq-filter (lambda (mon) (edie-wm-property mon 'focused)) (edie-wm-backend-monitor-list)))

(defun edie-wm-backend-monitor-list ()
  (declare (edie-log t))
  (let* ((strings (split-string (edie-wm-hypr--read 'monitors)
                                "\n\n" t "[[:space:]\n]+"))
         (monitors nil))
    (dolist (str strings (nreverse monitors))
      (push (edie-wm-hypr--parse-monitor str) monitors))))

(defun edie-wm-hypr--parse-monitor (string)
  (let ((monitor (edie-wm-monitor-make)))
    (dolist (line (split-string string "\n" t "[[:space:]]+"))
      (pcase line
        ((rx bos "Monitor "
             (let name (+ (not space)))
             " (ID " (let id (+ digit)) "):")
         (edie-wm-set-properties monitor `((id . ,id) (name . ,name))))
        ((rx (let width (+ digit)) "x" (let height (+ digit))
             (+ (not space)) " at "
             (let x (+ digit)) "x" (let y (+ digit)))
         (edie-wm-set-properties
          monitor `((left . ,(string-to-number x))
                    (top . ,(string-to-number y))
                    (width . ,(string-to-number width))
                    (height . ,(string-to-number height)))))
        ((rx bos "active workspace: " (let focused-desktop (+ digit)))
         (edie-wm-set-property monitor 'focused-desktop focused-desktop))
        ((rx bos "focused: yes")
         (edie-wm-set-property monitor 'focused t))))
    monitor))

(defun edie-wm-backend-desktop-focus (desktop)
  (edie-wm-hypr--write 'workspace (edie-wm-property desktop 'id)))

(defun edie-wm-backend-desktop-list ()
  (let ((hypr-dsks (edie-wm-hypr--desktop-list))
        desktops)
    (dotimes (i (length edie-wm-desktops))
      (let ((id (number-to-string (1+ i)))
            dsk (seq-find (lambda (hypr) (equal (edie-wm-property hypr 'id) id)) hypr-dsks))
        (when dsk
          (setq dsk (edie-wm-desktop-make))

          (edie-wm-set-property dsk 'id id)
          (edie-wm-set-property dsk 'focused-window (edie-wm-property hypr 'focused-window))
          (edie-wm-set-property dsk 'monitor (edie-wm-property (edie-wm-hypr--current-monitor) 'id)))
        (push dsk desktops)))
    (nreverse desktops)))

(defun edie-wm-hypr--desktop-list ()
  (let* ((strings (split-string (edie-wm-hypr--read 'workspaces)
                                "\n\n" t "[[:space:]\n]+"))
         (monitors (edie-wm-backend-monitor-list))
         (desktops nil))
    (dolist (str strings (nreverse desktops))
      (push (edie-wm-hypr--parse-desktop str monitors) desktops))))

(defun edie-wm-hypr--parse-desktop (string monitors)
  (let* ((desktop (edie-wm-desktop-make))
         (lines (split-string string "\n" t "[[:space:]]+"))
         (line (car lines))
         (match (string-match (rx bos "workspace ID " (group (+ digit))
                                  " (" (+ (not ")")) ")"
                                  " on monitor " (group (+ (not ":"))) ":" eos)
                              line)))
    (edie-wm-set-property desktop 'id (match-string 1 line))
    (edie-wm-set-property desktop
                          'monitor
                          (edie-wm-property
                           (seq-find (lambda (mon)
                                       (equal (edie-wm-property mon 'name) (match-string 2 line)))
                                     monitors)
                           'id))

    (dolist (line (cdr lines) desktop)
      (pcase line
        ((rx bos "lastwindow: 0x" (let wid (+ hex)) eos)
         (when (not (string= wid "0"))
           (edie-wm-set-property desktop 'focused-window wid)))))))

(defun edie-wm-hypr--read (&rest args)
  (with-output-to-string
    (let ((args (edie-wm-hypr--ctl args)))
      (apply #'call-process (car args) nil standard-output nil (cdr args)))))

(defun edie-wm-hypr--write (&rest args)
  (apply #'start-process "hyprctl" " *edie-wm-hyprland-commands*"
         (edie-wm-hypr--ctl (cons 'dispatch args))))

(defun edie-wm-hypr--ctl (args)
  (declare (edie-log nil))
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
