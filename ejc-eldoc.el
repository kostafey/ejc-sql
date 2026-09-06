;;; ejc-eldoc.el -- ejc-sql eldoc support (the part of ejc-sql).  -*- lexical-binding: t -*-

;;; Copyright (C) 2019-2026 - Kostafey <kostafey@gmail.com>

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
(require 'ejc-interaction)

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
  '(("SELECT"
     "SELECT"   "%SELECT #field... %FROM table [%WHERE predicate]"
     "FROM"     "%SELECT field... %FROM #table [%WHERE predicate]"
     "JOIN"     "[%INNER|%LEFT|%RIGHT] %JOIN #table %ON predicate"
     "ON"       "%JOIN table %ON #predicate"
     "WHERE"    "%WHERE #predicate [%OR predicate] [%AND predicate]"
     "GROUP BY" "%GROUP %BY #field... [%HAVING predicate]"
     "HAVING"   "%GROUP %BY field... %HAVING #predicate"
     "ORDER BY" "%ORDER %BY #field... [%ASC|%DESC]")
    ("INSERT"
     "INSERT"   "%INSERT %INTO #table (field...) %VALUES (value...)"
     "INTO"     "%INSERT %INTO #table (field...) %VALUES (value...)"
     "INTO ("   "%INSERT %INTO table (#field...) %VALUES (value...)"
     "VALUES"   "%INSERT %INTO table (field...) %VALUES (#value...)")
    ("UPDATE"
     "UPDATE"   "%UPDATE #table %SET field = value [%WHERE predicate]"
     "SET"      "%UPDATE table %SET #field = #value... [%WHERE predicate]"
     "WHERE"    "%UPDATE table %SET field = value %WHERE #predicate")
    ("DELETE"
     "DELETE"   "%DELETE %FROM #table [%WHERE predicate]"
     "FROM"     "%DELETE %FROM #table [%WHERE predicate]"
     "WHERE"    "%DELETE %FROM table %WHERE #predicate")
    ("CREATE"
     "CREATE"     "%CREATE %TABLE|%VIEW|%INDEX|%SEQUENCE #name ..."
     "TABLE"      "%CREATE %TABLE #table (field type [constraint]...)"
     "TABLE ("    "%CREATE %TABLE table (#field #type [constraint]...)"
     "CONSTRAINT" "%CONSTRAINT #name %PRIMARY %KEY (field...)"
     "VIEW"       "%CREATE [%OR %REPLACE] %VIEW #view %AS select_expression"
     "AS"         "%CREATE %VIEW view %AS #select_expression"
     "INDEX"      "%CREATE [%UNIQUE] %INDEX #index %ON table (field...)"
     "ON"         "%CREATE %INDEX index %ON #table (field...)"
     "ON ("       "%CREATE %INDEX index %ON table (#field...)"
     "SEQUENCE"   "%CREATE %SEQUENCE #sequence [%START %WITH n] [%INCREMENT %BY n]")
    ("DROP"
     "DROP"       "%DROP %TABLE|%VIEW|%INDEX|%SEQUENCE #name [%CASCADE|%RESTRICT]"
     "TABLE"      "%DROP %TABLE [%IF %EXISTS] #table [%CASCADE|%RESTRICT]"
     "VIEW"       "%DROP %VIEW [%IF %EXISTS] #view [%CASCADE|%RESTRICT]"
     "INDEX"      "%DROP %INDEX [%IF %EXISTS] #index"
     "SEQUENCE"   "%DROP %SEQUENCE [%IF %EXISTS] #sequence")
    ("ALTER"
     "ALTER"        "%ALTER %TABLE #table [%ADD|%MODIFY|%DROP|%RENAME] column"
     "TABLE"        "%ALTER %TABLE #table [%ADD|%MODIFY|%DROP|%RENAME] column"
     "ADD"          "%ALTER %TABLE table %ADD [%COLUMN] #column #type"
     "MODIFY"       "%ALTER %TABLE table %MODIFY [%COLUMN] #column #type"
     "ALTER COLUMN" "%ALTER %TABLE table %ALTER %COLUMN #column [%TYPE] type"
     "DROP"         "%ALTER %TABLE table %DROP [%COLUMN] #column"
     "RENAME"       "%ALTER %TABLE table %RENAME [%COLUMN column] %TO #new_name"
     "CONSTRAINT"   "%ALTER %TABLE table %ADD %CONSTRAINT #name %PRIMARY %KEY (field...)"))
  "SQL expressions templates, an alist of statements.

The key of the alist is the statement keyword, the value is a plist of
the statement clauses: the clause keyword (one or two words) and the
template to show when the point is inside this clause.

In the template `%' marks a SQL keyword, `#' marks the part of the
expression the user is typing right now, so it is highlighted like the
current argument of a function.  The rest is shown as is, e.g. `[]'
denotes an optional part and `...' - a repeated one.")

(defun ejc-get-procedure-before-point ()
  "Return stored procedure name before the point."
  (interactive)
  (save-excursion
    (goto-char (nth 1 (syntax-ppss)))
    (thing-at-point 'symbol)))

(defun ejc-get-package-before-point ()
  "Return package name of stored procedure before the point."
  (interactive)
  (save-excursion
    (goto-char (nth 1 (syntax-ppss)))
    (beginning-of-thing 'symbol)
    (when (equal (string (char-before)) ".")
      (left-char)
      (thing-at-point 'symbol))))

(defun ejc-get-parameter-index ()
  "Return parameter number around the point."
  (interactive)
  (let ((index 0)
        (ch (string (preceding-char))))
    (save-excursion
      (while (nth 2 (syntax-ppss))
        (let ((pss (nth 2 (syntax-ppss))))
          (when (member ch (list " " "\t" "\n" ","))
            (setq index (1+ index))
            (setq ch nil))
          (goto-char pss)
          (setq index (1+ index)))))
    (max (1- index) 0)))

(defvar ejc-sql-statement-separator ";"
  "Default separator of the statements inside the SQL batch.
Unlike `ejc-sql-separator', which separates the batches evaluated as a
whole, this one is used to find the boundaries of the single statement
around the point, e.g. to show its ElDoc template.
See `ejc-get-statement-beginning' for the cases when it is not used.")

(defconst ejc-sql-delimiter-command-re
  "[ \t\n\r]*delimiter[ \t]+\\([^ \t\n\r]+\\)[ \t]*$"
  "Regex of the `DELIMITER' command redefining the statements separator.
The command is expected at the beginning of the SQL batch.")

(defun ejc-get-statement-beginning (beg)
  "Return the beginning of the SQL statement around the point.
BEG is the beginning of the batch, see `ejc-get-sql-boundaries-at-point'.

The statements of the batch are separated by the `DELIMITER' command
value, when the batch starts with this command, or by
`ejc-sql-statement-separator' otherwise.  The connection having its own
`:separator' evaluates the whole batch as a single statement, unless the
`DELIMITER' command is used - the same way it is done on the Clojure
side, see `ejc-sql.connect/eval-user-sql'."
  (let ((case-fold-search t)
        (delimiter nil)
        (start beg))
    (save-excursion
      (goto-char beg)
      (when (looking-at ejc-sql-delimiter-command-re)
        (setq delimiter (match-string-no-properties 1)
              start (match-end 0))))
    (setq start (min start (point)))
    (let ((separator (or delimiter
                         (unless (alist-get :separator ejc-db)
                           ejc-sql-statement-separator))))
      (if (not separator)
          start
        (save-excursion
          (catch 'found
            (while (search-backward separator start t)
              (let ((state (syntax-ppss)))
                (unless (or (nth 3 state) (nth 4 state))
                  (throw 'found (+ (point) (length separator))))))
            start))))))

(defun ejc-get-words-before-point (beg)
  "Return the words of the SQL expression started at BEG before the point.
The words are upcased and reversed - the nearest to the point is the
first one.  The word being typed right now is not included."
  (let ((end (save-excursion
               (skip-syntax-backward "w_")
               (point)))
        (words nil))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\_<[[:alpha:]][[:alnum:]_]*\\_>" end t)
        (let* ((word (match-string-no-properties 0))
               (word-beg (match-beginning 0))
               (word-end (match-end 0))
               ;; `syntax-ppss' moves the point, so keep it out of the search.
               (state (save-excursion (syntax-ppss word-beg))))
          ;; The words of the comments and the string literals are not the
          ;; SQL keywords, e.g. `-- create the orders table'.
          (unless (or (nth 3 state) (nth 4 state))
            (push (upcase word) words))
          (goto-char word-end))))
    words))

(defun ejc-get-clause-keyword (keys)
  "Return the first of KEYS which is a clause keyword of any SQL statement."
  (let ((clause nil))
    (dolist (key keys clause)
      (dolist (statement ejc-sql-expressions)
        (unless clause
          (when (ejc-plist-get (cdr statement) key)
            (setq clause key)))))))

(defun ejc-get-clause-keys (word previous-word in-parens)
  "Return the possible clause keys for WORD, the most specific first.
The clause of the parenthesized part of the expression, like the fields
list of `INSERT INTO table (...)', is marked by the open paren, hence
the IN-PARENS flag.  The two-words clauses, like `ORDER BY', need the
PREVIOUS-WORD - the word before the WORD in the SQL expression."
  (delq nil (list (and in-parens (concat word " ("))
                  (and previous-word (concat previous-word " " word))
                  word)))

(defun ejc-get-sql-expression-before-point ()
  "Return the SQL expression template for the clause around the point.
Scan the SQL statement backwards from the point for the nearest known
clause keyword, then for the statement this clause belongs to.  So the
template of the `WHERE' clause is different for `SELECT' and `UPDATE'
statements and the clause is shown for the whole clause body, not for
the clause keyword only."
  (let* ((beg (ejc-get-statement-beginning
               (car (ejc-get-sql-boundaries-at-point))))
         (paren (nth 1 (syntax-ppss)))
         (in-parens (and paren (>= paren beg)))
         (words (ejc-get-words-before-point beg))
         (clause nil)
         (expression nil))
    (while (and words (not expression))
      (let ((word (car words))
            (statement (assoc (car words) ejc-sql-expressions)))
        (cond
         ;; The statement keyword is found, so the clause can be resolved.
         ;; The first word of the statement is always the statement keyword,
         ;; the rest of them are the nested statements, like the subqueries.
         ((and statement (or clause (null (cdr words))))
          (setq expression
                (or (and clause (ejc-plist-get (cdr statement) clause))
                    (ejc-plist-get (cdr statement) word))))
         ((not clause)
          ;; A word can be both a statement and a clause keyword, like `DROP'
          ;; in `DROP TABLE' and in `ALTER TABLE ... DROP COLUMN', so prefer
          ;; the clause here - the statement is searched in the words before.
          (setq clause (ejc-get-clause-keyword
                        (ejc-get-clause-keys word (cadr words) in-parens)))
          (unless clause
            (when statement
              (setq expression (ejc-plist-get (cdr statement) word)))))))
      (setq words (cdr words)))
    expression))

(defun ejc-eldoc-procedure-doc ()
  "Return a doc string for the stored procedure around the point, or nil."
  (if-let ((stored-procedure (if (ejc-buffer-connected-p)
                                 (condition-case nil
                                     (ejc-get-procedure-before-point)
                                   (error nil)))))
      (let ((type (ejc-get-entity-type ejc-db stored-procedure))
            (package (ejc-get-package-before-point)))
        (if (or (eq type :procedure)
                (eq type :function))
            (let ((params (car (ejc-get-parameters ejc-db
                                                   package
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
                                     (car p)
                                     (lambda (s) (concat "#" s ))
                                     " ")
                                  (car p)))
                         (-zip-pair params
                                    (number-sequence 0 (1- (length params)))))
                        ", "))))))))

(defun ejc-eldoc-expression-doc ()
  "Return a doc string for the SQL expression around the point, or nil."
  (if-let ((expression (condition-case nil
                           (ejc-get-sql-expression-before-point)
                         (error nil))))
      (ejc-propertize expression)))

(defun ejc-eldoc-function (&rest _ignored)
  "Return a doc string appropriate for the current context, or nil."
  (or (ejc-eldoc-procedure-doc)
      (ejc-eldoc-expression-doc)))

;;;###autoload
(defun ejc-eldoc-setup ()
  "Set up eldoc function and enable eldoc-mode."
  (interactive)
  (cond
   ((boundp 'eldoc-documentation-strategy)
    (setq-local eldoc-documentation-strategy #'ejc-eldoc-function))
   ((boundp 'eldoc-documentation-functions)
    (add-hook 'eldoc-documentation-functions #'ejc-eldoc-function nil t))
   (t (setq-local eldoc-documentation-function #'ejc-eldoc-function)))
  (eldoc-mode +1))

(provide 'ejc-eldoc)

;;; ejc-eldoc.el ends here
