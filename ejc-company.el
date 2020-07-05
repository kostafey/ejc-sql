;;; ejc-company.el -- SQL completitions at point by company-mode (the part of ejc-sql).

;;; Copyright © 2020 - Kostafey <kostafey@gmail.com>

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

;;; Commentary:

;; `ejc-company' is a `company' completion backend for `ejc-sql'.
;; To use it, add `ejc-company-backend' to `company-backends':

;;     (requre 'ejc-company)
;;     (push 'ejc-company-backend company-backends)

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'company)
(require 'ejc-completion-common)

(defun ejc-company-make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun ejc-company-add-meta (meta candidates)
  (-map (lambda (k) (list k meta))
        candidates))

(defun ejc-company-candidates (prefix)
  (let (res)
    (dolist (item
             (cl-remove-if-not
              (lambda (c) (string-prefix-p prefix (car c) t))
              (append
               (ejc-append-without-duplicates
                (ejc-company-add-meta
                 "ANSI SQL" (ejc-get-ansi-sql-words))
                (ejc-company-add-meta
                 "keyword" (ejc-get-keywords))
                'car :right)
               (ejc-company-add-meta
                "owner" (ejc-owners-candidates))
               (ejc-company-add-meta
                "table" (ejc-tables-candidates))
               (ejc-company-add-meta
                "view" (ejc-views-candidates))
               (ejc-company-add-meta
                "package" (ejc-packages-candidates))
               (ejc-company-add-meta
                "colomn" (ejc-colomns-candidates)))))
      (push (ejc-company-make-candidate item) res))
    res))

(defun ejc-company-annotation (candidate)
  (format " %s" (get-text-property 0 'meta candidate)))

(defun ejc-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'ejc-company-backend))
    (prefix (and (bound-and-true-p ejc-sql-mode)
                 (company-grab-symbol)))
    (candidates (ejc-company-candidates arg))
    (annotation (ejc-company-annotation arg))))

(provide 'ejc-company)

;;; ejc-company.el ends here
