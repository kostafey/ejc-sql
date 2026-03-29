;;; ejc-capf.el -- SQL completitions at point by Completion-at-Point Function (Capf)

;;; Copyright © 2026 - Kostafey <kostafey@gmail.com>

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

(require 'ejc-completion-common)

(defun ejc-find-lists-containing (target collection)
  "Return a list of keys from COLLECTION that contain TARGET."
  (mapcar #'car
          (seq-filter (lambda (pair)
                        (seq-contains-p (cadr pair) target))
                      collection)))

(defun ejc-capf-get-annotation (cand)
  (let* ((on-point (ejc-get-prefix-word))
         (my-data (list (list "ansi sql" (ejc-get-ansi-sql-words))
                        (list "keyword" (ejc-get-keywords))
                        (list "owner" (ejc-owners-candidates))
                        (list "table" (ejc-tables-candidates))
                        (list "view" (ejc-views-candidates))
                        (when on-point
                          (list "column" (ejc-colomns-candidates)))))
         ;; Find which lists contain the candidate
         (matches (ejc-find-lists-containing cand my-data)))
    (when matches
      (format " [%s]" (mapconcat #'identity matches ", ")))))

(defun ejc-capf ()
  "SQL completion-at-point function."
  (let ((bds (bounds-of-thing-at-point 'symbol))
        (on-point (ejc-get-prefix-word)))
    (when bds
      (list (car bds) (cdr bds)
            (append (ejc-get-ansi-sql-words)
                    (ejc-get-keywords)
                    (ejc-owners-candidates)
                    (ejc-tables-candidates)
                    (ejc-views-candidates)
                    (when on-point
                      (ejc-colomns-candidates)))
            :exclusive 'yes
            :annotation-function #'ejc-capf-get-annotation))))

(provide 'ejc-capf)

;;; ejc-capf.el ends here
