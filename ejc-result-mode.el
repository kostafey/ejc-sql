;;; ejc-result-mode.el

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

(defface ejc-result-table-borders-face '((t :inherit font-lock-comment-face
                                            :italic nil))
  "Face used to font-lock table borders."
  :group 'ejc-sql-faces)

(defconst ejc-result-font-lock-keywords
  '(("|" . 'ejc-result-table-borders-face)
    ("-" . 'ejc-result-table-borders-face)
    ("+" . 'ejc-result-table-borders-face)))

(defun ejc-result-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq major-mode 'ejc-result-mode
        mode-name "SQL-Result")
  (orgtbl-mode 1)
  (setq font-lock-defaults '(ejc-result-font-lock-keywords)))

(provide 'ejc-result-mode)

;;; ejc-result-mode.el ends here
