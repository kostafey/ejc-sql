;;; ejc-eldoc.el -- ejc-sql eldoc support (the part of ejc-sql).

;;; Copyright (C) 2019 - Kostafey <kostafey@gmail.com>

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
(require 'eldoc)
(require 'ejc-format)

(defun ejc-replace-property-mark (text fmt face)
  (while (string-match fmt text)
    (let ((beg (match-beginning 0))
          (end (match-end 0)))
      (setq text (concat (substring text 0 beg)
                         (substring text (1+ beg))))
      (add-face-text-property beg (1- end) face t text)))
  text)

(defun ejc-propertize (text)
  (-> text
      (ejc-replace-property-mark "\\@\\(\\w+_?\\)+"
                                 'font-lock-function-name-face)
      (ejc-replace-property-mark "\\%\\(\\w+_?\\)+"
                                 'font-lock-keyword-face)
      (ejc-replace-property-mark "\\#\\(\\w+_?\\)+"
                                 'eldoc-highlight-function-argument)))

(defconst ejc-sql-expressions
  (list
   "SELECT"
   "%SELECT #field... %FROM table [%WHERE predicate]"
   "FROM"
   "%SELECT field... %FROM #table [%WHERE predicate]"
   "WHERE"
   "%WHERE #predicate [%OR predicate] [%AND predicate]"))

(defun ejc-get-procedure-before-point ()
  "Return stored procedure name before the point."
  (interactive)
  (save-excursion
    (goto-char (nth 1 (syntax-ppss)))
    (thing-at-point 'symbol)))

(defun ejc-get-parameter-index ()
  "Return parameter number around the point."
  (interactive)
  (let ((index 0)
        (ch (string (preceding-char))))
    (save-excursion
      (while (nth 2 (syntax-ppss))
        (let ((pss (nth 2 (syntax-ppss))))
          (if (member ch (list " " "\t" "\n" ","))
              (setq index (1+ index)))
          (goto-char pss)
          (setq index (1+ index)))))
    (max (1- index) 0)))

(defun ejc-eldoc-function ()
  "Returns a doc string appropriate for the current context, or nil."
  (if-let ((stored-procedure (condition-case nil
                                 (ejc-get-procedure-before-point)
                               (error nil))))
      (let ((type (ejc-get-entity-type ejc-db stored-procedure)))
        (if (or (eq type :procedure)
                (eq type :function))
            (let ((params (car (ejc-get-parameters ejc-db
                                                   stored-procedure
                                                   t)))
                  (p-index (ejc-get-parameter-index)))
              (ejc-propertize
               (format "@%s: (%s)"
                       stored-procedure
                       (string-join
                        (-map
                         (lambda (p) (if (eql (cdr p) p-index)
                                    (ejc-split-and-join
                                     (lambda (s) (concat "#" s ))
                                     " ")
                                  (car p)))
                         (-zip params
                               (number-sequence 0 (1- (length params)))))
                        ", "))))))
    (if-let ((sql-word (condition-case nil
                           (ejc-get-word-before-point)
                         (error nil))))
        (if-let ((sql-expression (lax-plist-get
                                  ejc-sql-expressions
                                  sql-word)))
            (ejc-propertize sql-expression)))))

;;;###autoload
(defun ejc-eldoc-setup ()
  "Set up eldoc function and enable eldoc-mode."
  (interactive)
  (setq-local eldoc-documentation-function #'ejc-eldoc-function)
  (eldoc-mode +1))

(provide 'ejc-eldoc)

;;; ejc-eldoc.el ends here
