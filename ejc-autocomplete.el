;;; ejc-autocomplete.el -- SQL completitions at point (the part of ejc-sql).

;;; Copyright © 2013-2018 - Kostafey <kostafey@gmail.com>

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
(require 'auto-complete)
(require 'ejc-lib)
(require 'ejc-interaction)
(require 'ejc-doc)
(require 'ejc-format)

(defcustom ejc-candidates-uppercase t
  "Use uppercase candidates or downcase.
Uppercase by default, set to nil to use downcase candidates."
  :type 'boolean
  :safe #'booleanp
  :group 'ejc-sql)

(defcustom ejc-use-flx nil
  "Non-nil enables `flx' fuzzy matching engine autocompletion."
  :group 'ejc-sql
  :type 'boolean)

(defcustom ejc-flx-threshold 3
  "The minimum number of typed chars required to use `flx' for autocompletion."
  :group 'ejc-sql
  :type 'integer)

(defface ejc-flx-highlight-face
  '((t :inherit popup-isearch-match))
  "Face used by flx for highlighting flx match characters in `ejc-sql' buffers."
  :group 'ejc-sql)

(defun ejc-get-prefix-word ()
  "Return the word preceding dot before the typing."
  (save-excursion
    (let ((space-dist (or (save-excursion
                            (re-search-backward "[ \n\t\r(]+" nil t))
                          0))
          (dot (search-backward "." nil t))
          (space (re-search-backward "[ \n\t\r(.]+" nil t)))
      (if (and dot
               space
               (> dot space)
               (<= space-dist space)) ; is a dot completition
          (buffer-substring (1+ space) dot)
        nil))))

(defvar ejc-ansi-sql-words
  '("select" "where" "and" "or" "from" "insert" "update" "delete" "join"
    "order" "by" "distinct" "create" "alter" "drop" "like"
    "grant" "revoke" "deny" "commit" "rollback" "savepoint"))

(defvar ejc-auxulary-sql-words
  '("show" "errors" "desc" "count" "type" "table" "function" "procedure"
    "begin" "end" "for" "return"))

(defun ejc-not-nil-str (s)
  (not (equal s "nil")))

(defmacro ejc-candidates (cand-fn)
  `(if (ejc-buffer-connected-p)
       (let* ((prefix-1 (ejc-get-prefix-word))
              (prefix-2 (save-excursion
                           (search-backward "." nil t)
                           (ejc-get-prefix-word)))
              (result (funcall ,cand-fn
                               ejc-db
                               (apply
                                'buffer-substring
                                (ejc-get-sql-boundaries-at-point))
                               prefix-1
                               prefix-2))
              (pending (car result))
              (candidates-cache (cdr result)))
         (if (ejc-not-nil-str pending)
             (message "Receiving database structure...")
           candidates-cache))))

;;;###autoload
(defun ejc-owners-candidates ()
  (ejc-candidates 'ejc-get-owners-candidates))

;;;###autoload
(defun ejc-tables-candidates ()
  (ejc-candidates 'ejc-get-tables-candidates))

;;;###autoload
(defun ejc-colomns-candidates ()
  (ejc-candidates 'ejc-get-colomns-candidates))

(defun ejc-return-point ()
  "Return point position if point (cursor) is located next to dot char (.#)"
  (let ((curr-char (buffer-substring
                    (save-excursion
                      (left-char 1)
                      (point))
                    (point))))
    (if (equal curr-char ".")
        (point)
      nil)))

(defun ejc-get-ansi-sql-words ()
  (unless (or (ejc-return-point) (ejc-get-prefix-word))
    (if ejc-candidates-uppercase
        (append (mapcar 'upcase ejc-ansi-sql-words)
                (mapcar 'upcase ejc-auxulary-sql-words))
      (append ejc-ansi-sql-words
              ejc-auxulary-sql-words))))

(defun ac-ejc-documentation (symbol-name)
  "Return a documentation string for SYMBOL-NAME."
  (if (not ejc-doc-created-p)
      (ejc-create-doc))
  (gethash (intern (downcase symbol-name)) ejc-sql-doc))

(defvar ac-source-ejc-owners
  '((candidates . ejc-owners-candidates)
    (symbol . "o")
    (requires . 1)
    (cache . t)))

(defvar ac-source-ejc-tables
  '((candidates . ejc-tables-candidates)
    (symbol . "t")
    (requires . 1)
    (cache . t)))

(defvar ac-source-ejc-tables-point
  '((candidates . ejc-tables-candidates)
    (symbol . "t")
    (prefix . ejc-return-point)
    (requires . 0)
    (cache . t)))

(defvar ac-source-ejc-colomns
  '((candidates . ejc-colomns-candidates)
    (symbol . "c")
    (requires . 1)
    (cache . t)))

(defvar ac-source-ejc-colomns-point
  '((candidates . ejc-colomns-candidates)
    (symbol . "c")
    (prefix . ejc-return-point)
    (requires . 0)
    (cache . t)))

(defvar ac-source-ejc-ansi-sql
  '((candidates . ejc-get-ansi-sql-words)
    (symbol . "s")
    (document . ac-ejc-documentation)
    (requires . 1)
    (cache . t)))

;;;###autoload
(defun ejc-ac-setup ()
  "Add the completion sources to the front of `ac-sources'.
This affects only the current buffer.

Check against following cases:
prefix-2.prefix-1.#
prefix-1.#
something#"
  (interactive)
  (add-to-list 'ac-sources 'ac-source-ejc-ansi-sql)
  (add-to-list 'ac-sources 'ac-source-ejc-owners)
  (add-to-list 'ac-sources 'ac-source-ejc-tables)
  (add-to-list 'ac-sources 'ac-source-ejc-tables-point)
  (add-to-list 'ac-sources 'ac-source-ejc-colomns)
  (add-to-list 'ac-sources 'ac-source-ejc-colomns-point))

(defun ejc-ac-hook ()
  (if ejc-use-flx
      (if (functionp 'flx-flex-match)
          (setq-local ac-match-function 'ejc-flx-match-internal)
        (error (concat "Please install flx.el and flx-ido.el "
                       "if you use fuzzy completion"))))
  (delq 'ac-source-dictionary ac-sources)
  (delq 'ac-source-abbrev ac-sources)
  (delq 'ac-source-words-in-same-mode-buffers ac-sources))

(add-hook 'ejc-sql-minor-mode-hook 'ejc-ac-hook)

(defun ejc-flx-propertize (obj score &optional add-score)
  "Return propertized copy of obj according to score.

SCORE of nil means to clear the properties."
  (let ((block-started (cadr score))
        (last-char nil)
        (str (if (consp obj)
                 (substring-no-properties (car obj))
               (substring-no-properties obj))))

    (when score
      (dolist (char (cdr score))
        (when (and last-char
                   (not (= (1+ last-char) char)))
          (put-text-property block-started (1+ last-char) 'face 'ejc-flx-highlight-face str)
          (setq block-started char))
        (setq last-char char))
      (put-text-property block-started  (1+ last-char) 'face 'ejc-flx-highlight-face str)
      (when add-score
        (setq str (format "%s [%s]" str (car score)))))
    (if (consp obj)
        (cons str (cdr obj))
      str)))

(defun ejc-flx-decorate (things &optional clear)
  "Add ido text properties to THINGS.
If CLEAR is specified, clear them instead."
  (if flx-ido-use-faces
      (let ((decorate-count (min ido-max-prospects
                                 (length things))))
        (nconc
         (cl-loop for thing in things
               for i from 0 below decorate-count
               collect (if clear
                           (ejc-flx-propertize thing nil)
                         (ejc-flx-propertize (car thing) (cdr thing))))
         (if clear
             (nthcdr decorate-count things)
           (mapcar 'car (nthcdr decorate-count things)))))
    (if clear
        things
      (mapcar 'car things))))

(defun ejc-flx-match-internal (query items)
  "Match QUERY against ITEMS using flx scores.

If filtered item count is still greater than `flx-ido-threshold', then use flex."
  (if (< (length query) ejc-flx-threshold)
      (all-completions query items)
    (let ((flex-result (flx-flex-match query items)))
      (if (< (length flex-result) flx-ido-threshold)
          (let* ((matches (cl-loop for item in flex-result
                                   for string = (ido-name item)
                                   for score = (flx-score string query flx-file-cache)
                                   if score
                                   collect (cons item score)
                                   into matches
                                   finally return matches)))
            (ejc-flx-decorate (delete-consecutive-dups
                               (sort matches
                                     (lambda (x y) (> (cadr x) (cadr y))))
                               t)))
        flex-result))))

(provide 'ejc-autocomplete)

;;; ejc-autocomplete.el ends here
