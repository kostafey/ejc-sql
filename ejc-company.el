;;; ejc-company.el -- SQL completitions at point by company-mode (the part of ejc-sql). -*- lexical-binding: t -*-

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

(defcustom ejc-company-cache-update-ivl-secs 60
  "Specify how often to update cached candidates in seconds.
If set to 1.0e+INF, do not update cache after initialization."
  :type 'integer :group 'ejc-sql)

(defcustom ejc-company-idle-timer-secs 1
  "Collect candidates after specified amount of idleness in seconds."
  :type 'integer :group 'ejc-sql)

(defvar-local ejc-company--candidates nil
  "Cached candidates.")

(defvar-local ejc-company--cache-update-ts nil
  "Last timestamp of cache update.")

(defvar-local ejc-company--cache-update-scheduled nil
  "Whether `ejc-company--cache-candidates' is already scheduled with `run-with-idle-timer'.")


(defun ejc-company-make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun ejc-company-add-meta (meta candidates)
  (-map (lambda (k) (list k meta))
        candidates))

(defun ejc-company--collect-all-candidates (&optional on-point)
  (append
   (ejc-append-without-duplicates
    (ejc-company-add-meta
     "ansi sql" (ejc-get-ansi-sql-words))
    (ejc-company-add-meta
     "keyword" (ejc-get-keywords))
    'car :right)
   (ejc-company-add-meta
    "owner" (ejc-owners-candidates))
   (ejc-company-add-meta
    "table" (ejc-tables-candidates))
   (ejc-company-add-meta
    "view" (ejc-views-candidates))
   (when (not on-point)
       (ejc-company-add-meta
        "package" (ejc-packages-candidates)))
   (when on-point
     (ejc-company-add-meta
      "column" (ejc-colomns-candidates)))))

(defun ejc-company-make-candidates (prefix items)
  "Filter `ITEMS' that are not started with `PREFIX' and prepare them for company."
  (mapcar #'ejc-company-make-candidate
          (if (string= "" prefix)
              items
            (cl-remove-if-not
             (lambda (c) (string-prefix-p prefix (car c) t))
             items))))

(defun ejc-company--cache-candidates (buffer)
  "Collect candidates for `BUFFER' and put them into a buffer-local variable."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        ;; we have to collect keywords, table/view names etc.
        ;; Since this function is invoked by timer we can't guarantee that point is not on a dot (e.g. var.|).
        ;; All collect functions will check buffer position and behave differently if point is on the dot.
        ;; So we should make sure not to be on the dot.
        ;; The simplest is just go to the beginning of a buffer
        (goto-char (point-min))
        (condition-case err-cons
            (setq ejc-company--candidates (ejc-company--collect-all-candidates))
          (error (message (cadr err-cons))))
        (setq ejc-company--cache-update-ts (float-time)
              ejc-company--cache-update-scheduled nil)))))

(defun ejc-company--schedule-cache-update ()
  "Schedule cache update if cache is empty or it was updated too long ago."
  (when (and (not ejc-company--cache-update-scheduled)
             (or (not ejc-company--candidates)
                 (not ejc-company--cache-update-ts)
                 (> (- (float-time) ejc-company--cache-update-ts)
                    ejc-company-cache-update-ivl-secs)))
    (run-with-idle-timer ejc-company-idle-timer-secs
                         nil
                         #'ejc-company--cache-candidates (current-buffer))
    (setq ejc-company--cache-update-scheduled t)))

(defun ejc-company-candidates (prefix)
  "If the point is on dot (name.| or name.var|) then synchronously collect candidates.
Otherwise use cached data. When cache is empty (first time invocation),
it returns only common SQL words and schedules the cache update.
`PREFIX' is used for filtering candidates."


  (let* ((on-point (ejc-get-prefix-word)))
    (if on-point
        (ejc-company-make-candidates prefix (ejc-company--collect-all-candidates t))

      (ejc-company--schedule-cache-update)
      (if ejc-company--candidates
          (ejc-company-make-candidates prefix ejc-company--candidates)

        ;; first time invocation, return only sql words
        (ejc-company-make-candidates prefix
                                     (setq ejc-company--candidates
                                           (ejc-company-add-meta
                                            "ansi sql" (ejc-get-ansi-sql-words))))))))

(defun ejc-company-annotation (candidate)
  (format " %s" (get-text-property 0 'meta candidate)))

(defun ejc-company-doc-buffer (candidate)
  (company-doc-buffer (ac-ejc-documentation candidate)))

(defun ejc-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'ejc-company-backend))
    (prefix (and (bound-and-true-p ejc-sql-mode)
                 (company-grab-symbol)))
    (candidates (ejc-company-candidates arg))
    (doc-buffer (ejc-company-doc-buffer arg))
    (annotation (ejc-company-annotation arg))))

(provide 'ejc-company)

;;; ejc-company.el ends here
