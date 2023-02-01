;;; edie-wm-openbox.el --- Openbox backend for edie-wm -*- lexical-binding: t -*-

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

(require 'color)
(require 'dom)
(require 'edie-wm-x11)
(require 'xml)
(require 'xdg)

(defcustom edie-wm-openbox-theme-filename
  (file-name-concat (xdg-data-home) "themes" "Edie" "openbox-3" "themerc")
  "Path to Openbox's theme file."
  :type 'file)

(defcustom edie-wm-openbox-rc-filename
  (file-name-concat (xdg-config-home) "openbox" "rc.xml")
  "Path to Openbox's rc file."
  :type 'file)

(defcustom edie-wm-openbox-theme-alist
  '(("window.handle.width" . 0)
    ("border.width" . edie-wm-window-border-width)
    ("window.active.border.color" . edie-wm-window-active-border-color)
    ("window.inactive.border.color" . edie-wm-window-inactive-border-color))
  "Alist with option names and where to get their values."
  :type '(alist :key-type string :value-type (choice number symbol)))

(defcustom edie-wm-openbox-base-rc
  '(openbox_config (:xmlns "http://openbox.org/3.4/rc" :xmlns:xi "http://www.w3.org/2001/XInclude")
     (focus
       (focusNew nil)
       (focusLast nil)
       (followMouse nil))
     (placement
       (policy "Smart"))
     (theme
       (name "Edie")
       (keepBorder t)
       (animateIconify nil))
     (desktops
       (number 1)
       (firstdesk 1)
       (popupTime 0))
     (resize
       (drawContents t)
       (popupShow "Never"))
     (keyboard
       (rebindOnMappingNotify nil))
     (mouse
       (screenEdgeWarpTime 0)
       (screenEdgeWarpMouse nil))
     (applications
       (application (:class "*")
         (decor nil)
         (focus t))))
  "Base Openbox config."
  :type 'sexp)

(defvar edie-wm-openbox--process nil)

(defun edie-wm-openbox-start ()
  ""
  (edie-wm-openbox--write-configuration)

  (setq edie-wm-openbox--process (start-process "edie-wm-wm" "*edie-wm-wm*" "openbox"))

  (setq edie-wm-current-desktop-function #'edie-wm-x11-current-desktop)
  (setq edie-wm-current-window-id-function #'edie-wm-x11-current-window-id)
  (setq edie-wm-focus-window-function #'edie-wm-x11-window-focus)
  (setq edie-wm-set-desktop-function #'edie-wm-x11-wm-set-desktop)
  (setq edie-wm-update-window-function #'edie-wm-x11-window-update)
  (setq edie-wm-window-close-function #'edie-wm-x11-window-close)
  (setq edie-wm-window-list-function #'edie-wm-x11-window-list)
  (setq edie-wm-window-make-function #'edie-wm-x11-window-make)

  (setq edie-wm-x11-on-desktop-focus-change-function #'edie-wm-on-desktop-focus-change)
  (setq edie-wm-x11-on-window-focus-function #'edie-wm-on-window-focus)
  (setq edie-wm-x11-on-window-add-function #'edie-wm-on-window-add)
  (setq edie-wm-x11-on-window-remove-function #'edie-wm-on-window-remove)
  (setq edie-wm-x11-on-window-update-function #'edie-wm-on-window-update)

  (edie-wm-x11-mode +1)

  (when edie-wm-default-desktop-list
    (edie-wm-x11-wm-set-desktops (edie-wm-desktop-list))))

(defun edie-wm-openbox--normalize-color (color)
  ""
  (apply #'color-rgb-to-hex (append (color-name-to-rgb color) (list 2))))

(defun edie-wm-openbox--write-theme ()
  "Write Openbox's theme configuration."
  (with-temp-buffer
    (pcase-dolist (`(,str . ,thing) edie-wm-openbox-theme-alist)
      (insert str ": "
              (cond
               ((and (symbolp thing) (eq (get thing 'custom-type) 'color))
                (edie-wm-openbox--normalize-color (symbol-value thing)))
               ((and (symbolp thing) (eq (get thing 'custom-type) 'edie-wm-unit))
                (format "%d" (symbol-value thing)))
               ((numberp thing)
                (format "%d" thing))
               (t (format "%s" thing)))
              "\n"))
    (set-visited-file-name edie-wm-openbox-theme-filename)
    (set-buffer-modified-p t)
    (save-buffer)
    (kill-buffer)))

(defun edie-wm-openbox--mod-description (mod)
  (pcase mod
    ("s" "W")
    ("M" "A")
    ("control" "C")
    ((or "S" "C") mod)
    (unknown (error "Unknown modifier: %S" mod))))

(defun edie-wm-openbox--char-description (char )
  (pcase char
    (";" "semicolon")
    ("<c-i>" "C-i")
    (_ char)))

(defun edie-wm-openbox--key-description (key)
  ""
  (rx-let ((modifier (or "control" "s" "C" "M" "S")))
    (pcase (single-key-description key)
      ((rx string-start
           (let mod1 modifier) "-"
           (? "<")
           (opt (let mod2 modifier) "-")
           (? "<")
           (opt (let mod3 modifier) "-")
           (? "<")
           (opt (let mod4 modifier) "-")
           (let char anything)
           (? ">")
           string-end)
       (concat
        (edie-wm-openbox--mod-description mod1) "-"
        (if mod2 (concat (edie-wm-openbox--mod-description mod2) "-") "")
        (if mod3 (concat (edie-wm-openbox--mod-description mod3) "-") "")
        (if mod4 (concat (edie-wm-openbox--mod-description mod4) "-") "")
        (edie-wm-openbox--char-description char)))
      (unknown (error "unknown key description: %S" unknown)))))

(defun edie-wm-openbox--rc-keybindings ()
  ""
  (let ((bindings nil))
    (map-keymap (lambda (key def)
                  (let ((desc (single-key-description key))
                        (ob-desc (edie-wm-openbox--key-description key)))
                    (push
                     `(keybind (:key ,ob-desc)
                        (action (:name "Execute")
                          (command ,(xml-escape-string
                                     (format
                                      "emacsclient --eval '(edie-keys-dispatch \"%s\")'"
                                      desc)))))
                     bindings)))
                edie-keys-mode-map)
    bindings))

(defun edie-wm-openbox--rc ()
  ""
  (pcase-let (((seq 'openbox_config attrs &rest children) edie-wm-openbox-base-rc))
    (append (list 'openbox_config attrs)
            (seq-map (lambda (child)
                       (pcase child
                         ((seq 'keyboard &rest kchildren)
                          (append '(keyboard) kchildren (edie-wm-openbox--rc-keybindings)))
                         (_ child)))
                     children))))

(defun edie-wm-openbox--to-dom (tree)
  ""
  (pcase tree
    (`(,tag nil)
     (dom-node tag nil "no"))
    (`(,tag t)
     (dom-node tag nil "yes"))
    (`(,tag ,(and text (pred stringp)))
     (dom-node tag nil text))
    (`(,tag ,(and num (pred numberp)))
     (dom-node tag nil (format "%d" num)))
    ((seq tag (and attrs (guard (keywordp (car attrs)))) &rest children)
     (apply #'dom-node
            tag
            (map-apply (lambda (k v) (cons (seq-rest (symbol-name k)) v)) attrs)
            (seq-map #'edie-wm-openbox--to-dom children)))
    ((seq tag &rest children)
     (apply #'dom-node tag nil (seq-map #'edie-wm-openbox--to-dom children)))))

(defun edie-wm-openbox--write-rcxml ()
  ""
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n")
    (dom-print (edie-wm-openbox--to-dom (edie-wm-openbox--rc)) t t)
    (set-visited-file-name edie-wm-openbox-rc-filename)
    (set-buffer-modified-p t)
    (save-buffer)))

(defun edie-wm-openbox--write-configuration ()
  ""
  (edie-wm-openbox--write-rcxml)
  (edie-wm-openbox--write-theme))

(defun edie-wm-openbox-reconfigure ()
  ""
  (interactive)
  (edie-wm-openbox--write-configuration)
  (call-process "openbox" nil 0 nil "--reconfigure"))

(provide 'edie-wm-openbox)
;;; edie-wm-openbox.el ends here
