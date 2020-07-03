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

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ejc-autocomplete)

(defun ejc-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (cl-case command
    (interactive (company-begin-backend 'ejc-company-backend))
    (prefix (and ejc-sql-mode
                 (company-grab-symbol)))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c t))
      (ejc-get-ansi-sql-words)))))

(provide 'ejc-company)

;;; ejc-company.el ends here
