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

(defun edie-wm-hypr--conn-events-filter (proc string)
  (edie-wm-hypr--print-to-process-buffer proc string)

  (let ((lines (split-string string "\n")))
    (dolist (line lines)
      (edie-wm-hypr--handle-event line))))

(defun edie-wm-hypr--handle-event (event)
  (pcase event
    ((rx "activewindowv2>>,")
     (edie-wm-on-window-focus nil))
    ((rx "activewindowv2>>" (let wid (+ hex)))
     (edie-wm-on-window-focus wid))
    ((rx "openwindow>>" (let wid (+ hex)))
     (edie-wm-on-window-add wid))
    ((rx "movewindow>>")
     (edie-wm-on-window-update nil nil))
    ((rx "closewindow>>")
     (edie-wm-on-window-remove nil))
    ((rx "focusedmon>>")
     (edie-wm-on-monitor-focus-change nil))
    ((rx bos "workspace>>")
     (edie-wm-on-desktop-focus-change nil))
    ((rx "monitoradded>>")
     (edie-wm-on-monitor-add nil))
    ((rx "monitorremoved>>")
     (edie-wm-on-monitor-remove nil))))

(defun edie-wm-backend-window-list ()
  (declare (edie-log nil))
  (mapcar #'edie-wm-hypr--make-window (edie-wm-hypr--read-json 'clients)))

(defun edie-wm-backend-current-window ()
  (edie-wm-hypr--make-window (edie-wm-hypr--read-json 'activewindow)))

(defun edie-wm-hypr--make-window (props)
  (pcase-let* ((window (cons 'window props))
               ((map ('at `(,left ,top))
                     ('size `(,width ,height))
                     ('workspace (map ('id desktop))))
                props))
    (edie-wm-set-property window 'left left)
    (edie-wm-set-property window 'top top)
    (edie-wm-set-property window 'width width)
    (edie-wm-set-property window 'height height)
    (edie-wm-set-property window 'desktop desktop)

    window))

(defun edie-wm-backend-window-raise (_window)
  (declare (edie-log t))
  ;; TODO check if _window is the active window
  (edie-wm-hypr--write 'bringactivetotop))

(defun edie-wm-backend-window-close (window)
  (edie-wm-hypr--write 'closewindow (format "address:%s" (edie-wm-property window 'address))))

(defun edie-wm-backend-window-focus (window)
  (declare (edie-log t))
  (edie-wm-hypr--write 'focuswindow (format "address:%s" (edie-wm-property window 'address))))

(defun edie-wm-backend-window-update (_ props)
  (declare (edie-log t))
  (cl-assert props t)

  (pcase-let (((map left top width height monitor) props))
    (when (and left top)
      (cl-assert (and (numberp left) (numberp top)) t)
      (edie-wm-hypr--write 'moveactive 'exact left top))

    (when (and width height)
      (cl-assert (and (numberp width) (numberp height)) t)
      (edie-wm-hypr--write 'resizeactive 'exact width height))

    (when monitor
      (edie-wm-hypr--write 'movewindow monitor))))

(defun edie-wm-hypr--current-monitor ()
  (cons 'monitor
        (seq-filter (lambda (mon)
                      (edie-wm-property mon 'focused))
                    (edie-wm-backend-monitor-list))))

(defun edie-wm-backend-monitor-list ()
  (let (monitors monitor)
    (dolist (mon (edie-wm-hypr--read-json 'monitors) (nreverse monitors))
      (setq monitor (cons 'monitor mon))

      (edie-wm-set-property monitor 'focused-desktop
                            (alist-get 'id (alist-get 'activeWorkspace mon)))
      (edie-wm-set-property monitor 'left (alist-get 'x mon))
      (edie-wm-set-property monitor 'top (alist-get 'y mon))

      (push monitor monitors))))

(defun edie-wm-backend-desktop-focus (desktop)
  (edie-wm-hypr--write 'workspace (edie-wm-property desktop 'id)))

(defun edie-wm-backend-desktop-list ()
  (let ((desktops (number-sequence 1 edie-wm-desktops))
        (instances (edie-wm-hypr--read-json 'workspaces)))
    (mapcar (lambda (dsk)
              (cons 'desktop
                    (if-let ((found (seq-find (lambda (inst)
                                                (equal (alist-get 'id inst) dsk))
                                              instances)))
                        found
                      (list (cons 'id dsk)))))
            desktops)))

(defun edie-wm-hypr--read (&rest args)
  (with-output-to-string
    (let ((args (edie-wm-hypr--ctl args)))
      (apply #'call-process (car args) nil standard-output nil (cdr args)))))

(defun edie-wm-hypr--read-json (&rest args)
  (with-current-buffer (get-buffer-create " edie-wm-hyprland")
    (erase-buffer)
    (let ((args (edie-wm-hypr--ctl args)))
      (apply #'call-process (car args) nil t nil "-j" (cdr args))
      (goto-char (point-min))
      (json-parse-buffer
       :object-type 'alist :array-type 'list :false-object nil :null-object nil))))

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
