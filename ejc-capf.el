;;; ejc-capf.el -- SQL completitions at point by Completion-at-Point Function (Capf)  -*- lexical-binding: t -*-

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

;;; Commentary:

;; `ejc-capf' is a `completion-at-point-functions' (Capf) backend for
;; `ejc-sql'.  It is used by any Capf frontend, like `corfu' or the built-in
;; `completion-at-point':

;;     (require 'ejc-capf)
;;     (add-hook 'ejc-sql-minor-mode-hook 'ejc-capf-setup)

;; The candidates requiring a database round-trip (owners, tables, views and
;; packages) are collected in background by an idle timer and cached, so
;; typing never waits for the database.  The cache is shared between all the
;; buffers connected to the same database and is refreshed not more often
;; than once per `ejc-capf-cache-update-ivl-secs'.

;; The columns candidates depend on the SQL expression around the point
;; (`alias.|'), hence they can't be collected in advance.  They are requested
;; synchronously, but only once per completion session: the result is reused
;; while the user is typing the column name.

;;; Code:

(require 'ejc-completion-common)

(defcustom ejc-capf-cache-update-ivl-secs 60
  "Specify how often to update cached candidates in seconds.
If set to 1.0e+INF, do not update cache after initialization."
  :type 'number :group 'ejc-sql)

(defcustom ejc-capf-cache-retry-ivl-secs 2
  "Time in seconds to wait before the next attempt to update the cache.
Used when the database structure cache is not ready yet on the Clojure
side, so the cached candidates are known to be incomplete."
  :type 'number :group 'ejc-sql)

(defcustom ejc-capf-idle-timer-secs 1
  "Collect candidates after specified amount of idleness in seconds."
  :type 'number :group 'ejc-sql)

(defvar ejc-capf-kinds
  '(("ansi sql" . keyword)
    ("keyword"  . keyword)
    ("owner"    . module)
    ("table"    . struct)
    ("view"     . class)
    ("package"  . module)
    ("column"   . field))
  "Alist of the candidates categories and their `company-kind' symbols.
Used by the icons providers, like `kind-icon' or `nerd-icons-corfu'.")

(defvar ejc-capf--cache (make-hash-table :test 'equal)
  "Cached candidates: maps `ejc-db' to (EXPIRATION-TIME . CANDIDATES).
The cache is keyed by the connection structure, so all the buffers
connected to the same database share the same candidates list.")

(defvar ejc-capf--scheduled (make-hash-table :test 'equal)
  "Set of `ejc-db' connections with already scheduled cache update.
The scheduling is done with `run-with-idle-timer'.")

(defvar-local ejc-capf--dot-cache nil
  "Cached dot completion candidates: (KEY EXPIRATION-TIME . CANDIDATES).
See `ejc-capf--dot-cache-key'.")

(defun ejc-capf--propertize (category candidates)
  "Mark CANDIDATES with the CATEGORY text property.
The property is used later by `ejc-capf-annotation' and `ejc-capf-kind',
so there is no need to search the candidate in the candidates lists."
  (mapcar (lambda (candidate)
            (propertize candidate 'meta-category category))
          candidates))

(defun ejc-capf--keywords ()
  "Return DB-specific keywords, excluding the ANSI SQL words duplicates."
  (let ((ansi-sql-words (make-hash-table :test 'equal))
        (keywords nil))
    (dolist (word (ejc-get-ansi-sql-words))
      (puthash word t ansi-sql-words))
    (dolist (word (ejc-get-keywords) (nreverse keywords))
      (unless (gethash word ansi-sql-words)
        (push word keywords)))))

(defun ejc-capf--collect (&optional on-point)
  "Collect all the completion candidates.
When ON-POINT is non-nil the point is next to a dot (`alias.|'), so
collect the columns of the entity denoted by the prefix word.  Otherwise
collect the packages, since they can't follow a dot."
  (append
   (ejc-capf--propertize "ansi sql" (ejc-get-ansi-sql-words))
   (ejc-capf--propertize "keyword" (ejc-capf--keywords))
   (ejc-capf--propertize "owner" (ejc-owners-candidates))
   (ejc-capf--propertize "table" (ejc-tables-candidates))
   (ejc-capf--propertize "view" (ejc-views-candidates))
   (if on-point
       (ejc-capf--propertize "column" (ejc-colomns-candidates))
     (ejc-capf--propertize "package" (ejc-packages-candidates)))))

(defun ejc-capf--expiration-time (pending)
  "Return the time when the candidates collected right now become stale.
PENDING is non-nil when the database structure cache is not ready yet, so
the collected candidates are incomplete and should be refreshed sooner."
  (+ (float-time) (if pending
                      ejc-capf-cache-retry-ivl-secs
                    ejc-capf-cache-update-ivl-secs)))

(defun ejc-capf--cache-candidates (buffer)
  "Collect candidates for BUFFER and put them into `ejc-capf--cache'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((db ejc-db))
        (unwind-protect
            (save-excursion
              ;; This function is invoked by timer, so the point can be
              ;; anywhere, in particular next to a dot (e.g. `alias.|').  All
              ;; the collect functions check the buffer position and behave
              ;; differently if the point is on the dot.  So we should make
              ;; sure not to be on the dot.  The simplest is just go to the
              ;; beginning of a buffer.
              (goto-char (point-min))
              (let ((ejc-candidates-pending nil))
                (condition-case err
                    (let ((candidates (ejc-capf--collect)))
                      (puthash db
                               (cons (ejc-capf--expiration-time
                                      ejc-candidates-pending)
                                     candidates)
                               ejc-capf--cache))
                  (error (message "%s" (error-message-string err))))))
          (remhash db ejc-capf--scheduled))))))

(defun ejc-capf--schedule-cache-update ()
  "Schedule cache update if cache is empty or it was updated too long ago."
  (let ((cached (gethash ejc-db ejc-capf--cache)))
    (when (and (not (gethash ejc-db ejc-capf--scheduled))
               (or (not cached)
                   (> (float-time) (car cached))))
      (puthash ejc-db t ejc-capf--scheduled)
      (run-with-idle-timer ejc-capf-idle-timer-secs
                           nil
                           #'ejc-capf--cache-candidates (current-buffer)))))

(defun ejc-capf--dot-cache-key (beg)
  "Return the dot completion cache key for the word starting at BEG.
The candidates depend on the current connection and on the SQL expression
around the point, but not on the word being typed after the dot, so this
word is cut out of the key.  Hence the whole database round-trip is done
once per completion session instead of once per typed char."
  (let ((boundaries (ejc-get-sql-boundaries-at-point)))
    (list ejc-db
          (buffer-substring-no-properties (car boundaries) beg)
          (buffer-substring-no-properties (point) (cadr boundaries)))))

(defun ejc-capf--dot-candidates (beg)
  "Return the candidates for the dot completion of the word starting at BEG.
Unlike the rest of the candidates they can't be collected in advance, so
the request is synchronous, but its result is cached, see
`ejc-capf--dot-cache-key'."
  (let ((key (ejc-capf--dot-cache-key beg)))
    (if (and ejc-capf--dot-cache
             (equal key (car ejc-capf--dot-cache))
             (< (float-time) (cadr ejc-capf--dot-cache)))
        (cddr ejc-capf--dot-cache)
      (let* ((ejc-candidates-pending nil)
             (candidates (ejc-capf--collect t)))
        (setq ejc-capf--dot-cache
              (cons key (cons (ejc-capf--expiration-time ejc-candidates-pending)
                              candidates)))
        candidates))))

(defun ejc-capf--candidates (beg)
  "Return the completion candidates for the word starting at BEG.
Unfiltered - the filtering is up to the `completion-styles'."
  (if (ejc-get-prefix-word)
      (ejc-capf--dot-candidates beg)
    (let ((cached (gethash ejc-db ejc-capf--cache)))
      (if cached
          (cdr cached)
        ;; First time invocation, the cache update is already scheduled, so
        ;; return only the words available without the database round-trip.
        (ejc-capf--propertize "ansi sql" (ejc-get-ansi-sql-words))))))

(defun ejc-capf--bounds ()
  "Return the bounds of the symbol being completed.
The symbol is grabbed backwards only, so the completion is triggered right
after a dot (`alias.|') or a whitespace, when there is no symbol at point."
  (cons (save-excursion
          (skip-syntax-backward "w_")
          (point))
        (point)))

(defun ejc-capf-annotation (candidate)
  "Return the annotation of the CANDIDATE - its category."
  (let ((category (get-text-property 0 'meta-category candidate)))
    (when category
      (format " [%s]" category))))

(defun ejc-capf-kind (candidate)
  "Return the `company-kind' symbol of the CANDIDATE.
Used by the icons providers, like `kind-icon' or `nerd-icons-corfu'."
  (cdr (assoc (get-text-property 0 'meta-category candidate)
              ejc-capf-kinds)))

(defun ejc-capf-doc-buffer (candidate)
  "Return a buffer with the documentation of the CANDIDATE.
Used by the documentation popups, like `corfu-popupinfo-mode'."
  (let ((doc (ac-ejc-documentation candidate)))
    (when doc
      (with-current-buffer (get-buffer-create " *ejc-capf-doc*")
        (erase-buffer)
        (insert doc)
        (goto-char (point-min))
        (current-buffer)))))

;;;###autoload
(defun ejc-capf ()
  "SQL completion-at-point function."
  (when (bound-and-true-p ejc-sql-mode)
    ;; Warm up the cache even if the completion frontend decides not to
    ;; complete this time (e.g. the typed prefix is shorter than
    ;; `corfu-auto-prefix'), so the candidates are ready by the time the
    ;; completion actually starts.
    (ejc-capf--schedule-cache-update)
    (let* ((bounds (ejc-capf--bounds))
           (beg (car bounds))
           (candidates 'ejc-capf--not-collected))
      (list beg (cdr bounds)
            ;; The candidates are collected lazily, only when the completion
            ;; frontend actually queries the table, and at most once per this
            ;; `ejc-capf' call.
            (lambda (string predicate action)
              (when (eq candidates 'ejc-capf--not-collected)
                (setq candidates (ejc-capf--candidates beg)))
              (complete-with-action action candidates string predicate))
            :exclusive 'yes
            :annotation-function #'ejc-capf-annotation
            :company-kind #'ejc-capf-kind
            :company-doc-buffer #'ejc-capf-doc-buffer))))

;;;###autoload
(defun ejc-capf-setup ()
  "Add `ejc-capf' to the buffer-local `completion-at-point-functions'."
  (add-hook 'completion-at-point-functions #'ejc-capf nil t))

(defun ejc-capf-flush-cache (&rest _args)
  "Clear the cached completion candidates."
  (interactive)
  (clrhash ejc-capf--cache)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (kill-local-variable 'ejc-capf--dot-cache))))

;; Any successfully executed DDL query clears the Clojure side cache, so the
;; Emacs side cache should be cleared as well.
(advice-add 'ejc-invalidate-cache :after #'ejc-capf-flush-cache)

(provide 'ejc-capf)

;;; ejc-capf.el ends here
