;;; ejc-direx.el

;;; Copyright © 2017 - Kostafey <kostafey@gmail.com>

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

;;; Code:


(require 'dash)
(require 'direx)
(require 'ejc-interaction)

(defgroup ejc-direx nil
  "Database structure visualisation tree."
  :group 'ejc
  :prefix "ejc-direx")


;;; Core

(defclass ejc-direx:object (direx:tree) ((cache :initarg :cache)))
(defclass ejc-direx:database (ejc-direx:object direx:node)
  ((buffer :initarg :buffer)
   (file-name :initarg :file-name
              :accessor direx:file-full-name)))
(defclass ejc-direx:schema (ejc-direx:object direx:node) ())
(defclass ejc-direx:table (ejc-direx:object direx:node) ())
(defclass ejc-direx:colomn (ejc-direx:object direx:leaf) ())

(defvar ejc-direx:type-class-map
  '(("database" . ejc-direx:database)
    ("schema" . ejc-direx:schema)
    ("table" . ejc-direx:table)
    ("colomn" . ejc-direx:colomn)))

(defun ejc-direx:node-from-cache (cache)
  (let* ((type (plist-get (car cache) :type))
         (class (or (assoc-default type ejc-direx:type-class-map)
                    'ejc-direx:colomn)))
    (make-instance class
                   :cache cache
                   :name (plist-get (car cache) :name))))

(defun ejc-direx:-filter-cache (items)
  "Filter out cache items according to configuration."
  (delq nil items))

(defmethod direx:node-children ((node ejc-direx:object))
  (mapcar 'ejc-direx:node-from-cache
          (ejc-direx:-filter-cache (cdr (oref node :cache)))))


;;; Face

(defface ejc-direx:schema
  '((t :inherit font-lock-type-face))
  "Face for schema name in direx tree"
  :group 'ejc-direx)

(defface ejc-direx:table
  '((t :inherit font-lock-function-name-face))
  "Face for table name in direx tree"
  :group 'ejc-direx)

(defface ejc-direx:colomn
  '((t :inherit font-lock-variable-name-face))
  "Face for column name in direx tree"
  :group 'ejc-direx)


;;; View

(defclass ejc-direx:item (direx:item) ())

(defmethod direx:make-item ((tree ejc-direx:object) parent)
  (make-instance 'ejc-direx:item :tree tree :parent parent))

(defmethod direx:make-item ((tree ejc-direx:colomn) parent)
  (let ((item (call-next-method)))
    (oset item :face 'ejc-direx:colomn)
    item))

(defmethod direx:make-item ((tree ejc-direx:table) parent)
  (let ((item (call-next-method)))
    (oset item :face 'ejc-direx:table)
    item))

(defmethod direx:make-item ((tree ejc-direx:schema) parent)
  (let ((item (call-next-method)))
    (oset item :face 'ejc-direx:schema)
    item))

(defun direx-ejc:-goto-item (item)
  (destructuring-bind (&key line_nr column &allow-other-keys)
      (car (oref (direx:item-tree item) :cache))
    (ejc:goto--line-column line_nr column)))

(defmethod direx:generic-find-item ((item ejc-direx:item)
                                    not-this-window)
  (let* ((root (direx:item-root item))
         (filename (direx:file-full-name (direx:item-tree root))))
    (if not-this-window
        (find-file-other-window filename)
      (find-file filename))
    (direx-ejc:-goto-item item)))

(defmethod direx:generic-display-item ((item ejc-direx:item))
  (let* ((root (direx:item-root item))
         (filename (direx:file-full-name (direx:item-tree root))))
    (with-selected-window (display-buffer (find-file-noselect filename))
      (direx-ejc:-goto-item item))))

(defvar ejc-direx:item-refresh--recurring nil)

(defun ejc-direx:get-structure ()
  "Provide data - database structure for direx tree."
  (cl-labels ((column-item (column)
                           (list :type "column" :name column))
              (table-item (table)
                          (let ((columns
                                 (cdr (ejc-get-colomns-candidates ejc-db table))))
                            (cons (list :type "table" :name table)
                                  (-map 'list
                                        (-map #'column-item columns))))))
    (let ((tables (cdr (ejc-get-tables-candidates ejc-db nil nil))))
      (-map #'table-item tables))))

(defmethod direx:item-refresh ((item ejc-direx:item) &key recursive)
  "Currently it always recursively refreshes whole tree."
  (if ejc-direx:item-refresh--recurring
      (call-next-method)
    (let* ((ejc-direx:item-refresh--recurring t)
           (root (direx:item-root item))
           (module (direx:item-tree root)))
      (if (with-current-buffer (oref module :buffer)
            (unless (eq (cdr (oref module :cache)) (ejc-direx:get-structure))
              (oset module :cache (cons nil (ejc-direx:get-structure)))))
          (call-next-method root :recursive t)
        (message "No need to refresh")))))


;;; Command

(defun ejc-direx:make-buffer ()
  (direx:ensure-buffer-for-root
   (make-instance 'ejc-direx:database
                  :name (format "*direx-ejc: %s*" ejc-connection-name)
                  :buffer (current-buffer)
                  :file-name (buffer-file-name)
                  :cache (cons nil (ejc-direx:get-structure)))))

;;;###autoload
(defun ejc-direx:pop-to-buffer ()
  (interactive)
  (pop-to-buffer (ejc-direx:make-buffer)))

;;;###autoload
(defun ejc-direx:switch-to-buffer ()
  (interactive)
  (switch-to-buffer (ejc-direx:make-buffer)))

(provide 'ejc-direx)

;;; ejc-direx.el ends here
