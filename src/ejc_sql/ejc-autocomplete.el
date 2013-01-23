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

;; TODO: test
(setq ejc-db-type "informix")

(defvar ejc-select-tables
  (cond ((string-match "informix" ejc-db-type)
         "SELECT TRIM(t.tabname) as tablesList
           FROM systables AS t
          WHERE t.tabtype = 'T'
            AND t.tabid >= 100
          ORDER BY t.tabname;")
        ((string-match "mysql" ejc-db-type) 
         "SELECT TRIM(t.tabname) as tablesList
           FROM systables AS t
          WHERE t.tabtype = 'T'
            AND t.tabid >= 100
          ORDER BY t.tabname;")
        ))

;;;###autoload
(defun ejc-get-tables-list ()
  (let ((tables-list-string (ejc-get-nrepl-stdout
                             (concat "(eval-sql-internal-get-column " 
                                     (ejc-add-quotes ejc-select-tables) " )"))))
    (split-string
     (substring tables-list-string
                1 (- (length tables-list-string) 1)))))

;;;###autoload
(defun ejc-candidates ()
  (append '("select" "where" "from" "insert" "update" "delete" "drop")
           (ejc-get-tables-list)))

(defvar ac-source-ejc-sql
  '((candidates . ejc-candidates)))

;;;###autoload
(defun ejc-ac-setup ()
  "Add the completion sources to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-ejc-sql))

;; "SELECT TRIM(t.tabname) || '.' || TRIM(c.colname) AS table_dot_column
;;   FROM systables AS t, syscolumns AS c
;;  WHERE t.tabid = c.tabid
;;    AND t.tabtype = 'T'
;;    AND t.tabid >= 100
;;  ORDER BY t.tabname, c.colno;"

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; (defun nrepl-indent-and-complete-symbol ()
;;   "Indent the current line and perform symbol completion.
;; First indent the line. If indenting doesn't move point, complete
;; the symbol. "
;;   (interactive)
;;   (let ((pos (point)))
;;     (lisp-indent-line)
;;     (when (= pos (point))
;;       (if (save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
;;           (completion-at-point)))))

;; (define-minor-mode nrepl-interaction-mode
;;   "Minor mode for nrepl interaction from a Clojure buffer.

;; \\{nrepl-interaction-mode-map}"
;;    nil
;;    " nREPL"
;;    nrepl-interaction-mode-map
;;    (make-local-variable 'completion-at-point-functions)
;;    (add-to-list 'completion-at-point-functions
;;                 'nrepl-complete-at-point))
;; (defun nrepl-complete-at-point ()
;;   (let ((sap (symbol-at-point)))
;;     (when (and sap (not (in-string-p)))
;;       (let ((bounds (bounds-of-thing-at-point 'symbol)))
;;         (list (car bounds) (cdr bounds)
;;               (completion-table-dynamic #'nrepl-dispatch-complete-symbol))))))

;; (defun nrepl-completion-complete-op-fn (str)
;;   "Return a list of completions using the nREPL \"complete\" op."
;;   (lexical-let ((strlst (plist-get
;;                          (nrepl-send-request-sync
;;                           (list "op" "complete"
;;                                 "session" (nrepl-current-tooling-session)
;;                                 "ns" nrepl-buffer-ns
;;                                 "symbol" str))
;;                          :value)))
;;     (when strlst
;;       (car strlst))))

;; (defun nrepl-dispatch-complete-symbol (str)
;;   (if (nrepl-op-supported-p "complete")
;;       (nrepl-completion-complete-op-fn str)
;;     (nrepl-completion-complete-core-fn str)))


(provide 'ejc-autocomplete)
