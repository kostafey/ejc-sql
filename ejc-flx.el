;;; ejc-flx.el -- Customization flx for ejc (the part of ejc-sql).

;;; Copyright © 2018 - Kostafey <kostafey@gmail.com>

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

(defcustom ejc-use-flx nil
  "Non-nil enables `flx' fuzzy matching engine autocompletion."
  :group 'ejc-sql
  :type 'boolean)

(defcustom ejc-flx-threshold 2
  "The minimum number of typed chars required to use `flx' for autocompletion.
When 1, enables `flx' usage from the first typed char."
  :group 'ejc-sql
  :type 'integer)

(defface ejc-flx-highlight-face
  '((t :inherit popup-isearch-match))
  "Face used by flx for highlighting flx match characters in `ejc-sql' buffers."
  :group 'ejc-sql)

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

(provide 'ejc-flx)

;;; ejc-flx.el ends here
