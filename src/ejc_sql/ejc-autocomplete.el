;;; ejc-autocomplete.el -- SQL completitions at point (the part of ejc-sql).

;;; Copyright © 2013 - Kostafey <kostafey@gmail.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

(require 'auto-complete)
(require 'ejc-lib)

(defvar ejc-owners-list nil
  "Owners list cache.
The owners list probably should not be changed very often.")

;;;###autoload
(defun ejc--select-db-meta-script (meta-type &optional owner)
  (cond
   ;;----------
   ;; informix
   ;;----------
   ((string-match "informix" ejc-db-type)
    (cond
     ((eq :owners meta-type) nil)
     ((eq :tables meta-type)
      (concat " SELECT TRIM(t.tabname) as tablesList "
              " FROM systables AS t   "
              " WHERE t.tabtype = 'T' "
              "   AND t.tabid >= 100  "
              " ORDER BY t.tabname;   "))))
   ;;-------
   ;; mysql
   ;;-------
   ((string-match "mysql" ejc-db-type)
    (cond
     ((eq :owners meta-type) nil)
     ((eq :tables meta-type)
      (concat
       " SELECT table_name FROM INFORMATION_SCHEMA.TABLES "
       " WHERE table_schema = '" ejc-db-name "'"))))
   ;;--------
   ;; oracle
   ;;--------
   ((string-match "oracle" ejc-db-type)
    (cond
     ((eq :owners meta-type)
      (concat "select DISTINCT(owner) "
              " from ALL_OBJECTS"))
     ((eq :tables meta-type)
      (concat " SELECT table_name, owner \n"
              " FROM all_tables          \n"
              " WHERE owner = " (if owner
                                    (ejc-add-squotes owner)
                                  (ejc-add-squotes ejc-db-owner))))))))

;; "SELECT TRIM(t.tabname) || '.' || TRIM(c.colname) AS table_dot_column
;;   FROM systables AS t, syscolumns AS c
;;  WHERE t.tabid = c.tabid
;;    AND t.tabtype = 'T'
;;    AND t.tabid >= 100
;;  ORDER BY t.tabname, c.colno;"

(defun ejc-get-owners-list ()
  (ejc--eval-get-list (ejc--select-db-meta-script :owners)))

(defun ejc-get-tables-list (&optional owner)
  (ejc--eval-get-list (ejc--select-db-meta-script :tables owner)))

(defun ejc-get-prefix-word ()
  "Return the word preceding dot before the typing."
  (let ((dot (save-excursion
               (search-backward "." nil t)))
        (space (save-excursion
                 (re-search-backward "[ \n\t\r^.]+" nil t))))
    (if (and dot
             space
             (> dot space)) ; is a dot completition
        (buffer-substring (1+ space) dot)
      nil)))

(defun ejc-get-completitions-list ()
  (if (and (ejc--select-db-meta-script :qwners)
           (not ejc-owners-list))
      (setq ejc-owners-list (ejc-get-owners-list)))
  (let ((prefix-1 (ejc-get-prefix-word))
        (prefix-2 (save-excursion
                    (search-backward "." nil t)
                    (ejc-get-prefix-word)))
        (owner)
        (table))
    (if prefix-1
        (progn
          (if (member prefix-1 ejc-owners-list)
              (setq owner prefix-1)
            (setq table prefix-1))
          (if (and prefix-2
                   (member prefix-2 ejc-owners-list))
              (setq owner prefix-2))))
    (if (and (not table)
             (not owner))
        (append ejc-owners-list (ejc-get-tables-list))
      (if (not table)
          (ejc-get-tables-list owner)))))

;;;###autoload
(defun ejc-candidates ()
  (append '("select" "where" "from" "insert" "update" "delete" "drop")
           (ejc-get-completitions-list)))

(defvar ac-source-ejc-sql
  '((candidates . ejc-candidates)))

;;;###autoload
(defun ejc-ac-setup ()
  "Add the completion sources to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-ejc-sql))

(provide 'ejc-autocomplete)
