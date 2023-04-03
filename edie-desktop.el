;; -*- lexical-binding: t; -*-

(require 'xdg)

(defgroup edie-desktop nil
  "Global desktop settings for Edie."
  :group 'edie)

(defcustom edie-desktop-icon-theme "breeze-dark"
  "The icon theme to use for Edie desktop icons."
  :type 'string
  :group 'edie-desktop)

(defcustom edie-desktop-icon-theme-scale 24
  "The icon size to use for Edie desktop icons."
  :type 'integer
  :group 'edie-desktop)

(defun edie-desktop-icon-theme-dirs ()
  (let ((dirs '()))
    (dolist (dir (append (list (xdg-data-home)) (xdg-data-dirs)) (nreverse dirs))
      (let ((path (file-name-concat (expand-file-name "icons" dir) edie-desktop-icon-theme)))
        (when (file-directory-p path)
          (push path dirs))))))

(defun edie-desktop-icon-theme-file (name)
  "Return the path to icon NAME.

Return nil if the file can't be found."
  (catch 'found
    (dolist (dir (edie-desktop-icon-theme-dirs))
      (when-let ((paths (directory-files-recursively
                         dir
                         (format "\\`%s.svg\\'" name)
                         nil
                         (lambda (d)
                           (or (string= (file-name-concat dir (file-name-nondirectory d)) d)
                               (string-match-p (format
                                                "%s/[^/]+/%d"
                                                dir edie-desktop-icon-theme-scale)
                                               d))))))
        (throw 'found (car paths))))))

(edie-desktop-icon-theme-file "battery-full")

(provide 'edie-desktop)
