;;; ejc-result-buffer.el

;;; Copyright © 2019-2020 - Kostafey <kostafey@gmail.com>

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

(require 'ejc-lib)
(require 'ejc-result-mode)

(defvar ejc-results-buffer nil
  "The results buffer.")

(defvar ejc-results-buffer-name "*ejc-sql-output*"
  "The results buffer name.")

(defvar ejc-result-file-template "ejc-sql-result-%d.txt"
  "SQL evaluation results file name template.")

(defvar ejc-result-file-path nil
  "The current result file path. Refreshed by any finished SQL evaluation.")

(defcustom ejc-results-path temporary-file-directory
  "SQL evaluation result files location."
  :group 'ejc-sql
  :type 'string)

(defcustom ejc-ring-length 10
  "The number of SQL evaluation results to keep."
  :group 'ejc-sql
  :type 'integer)

(defcustom ejc-show-result-bottom nil
  "When t show result output buffer in the bottom window."
  :group 'ejc-sql
  :type 'boolean)

(defvar ejc-ring-position 0
  "Current SQL evaluation result position in ring.")

(defvar ejc-modes-ring (list)
  "List of SQL evaluation result modes.")

(defcustom ejc-modes-ring-file (expand-file-name
                                "ejc-modes-ring.el"
                                ejc-results-path)
  "Previous SQL evaluation result modes file location."
  :group 'ejc-sql
  :type 'string)

(defun ejc-update-modes-ring (mode)
  "Update SQL evaluation result modes list, persist it in `ejc-modes-ring-file'."
  (setf (alist-get ejc-ring-position ejc-modes-ring) mode)
  (ejc-save-to-file ejc-modes-ring-file ejc-modes-ring))

(defun ejc-load-modes-ring ()
  "Load SQL evaluation result modes list `ejc-modes-ring' var."
  (setq ejc-modes-ring
        (ejc-load-from-file ejc-modes-ring-file :default (list))))

(defun ejc-get-result-file-path ()
  "Get current SQL evaluation result file path."
  (expand-file-name (format ejc-result-file-template ejc-ring-position)
                    ejc-results-path))

(defun ejc-inc-ring-position ()
  (setq ejc-ring-position (1+ ejc-ring-position)
        ejc-ring-position (if (>= ejc-ring-position ejc-ring-length)
                              0
                            ejc-ring-position)))

(defun ejc-dec-ring-position ()
  (setq ejc-ring-position (1- ejc-ring-position)
        ejc-ring-position (if (< ejc-ring-position 0)
                              (1- ejc-ring-length)
                            ejc-ring-position)))

(defun ejc-iterate-ring (inc-or-dec &optional should-exist)
  (funcall inc-or-dec)
  (if should-exist
      (let ((idx 0))
        (while (and (not (file-exists-p (ejc-get-result-file-path)))
                    (if (< idx ejc-ring-length)
                        t
                      (progn
                        (setq ejc-ring-position 0)
                        nil)))
          (setq idx (1+ idx))
          (funcall inc-or-dec))))
  (setq ejc-result-file-path (ejc-get-result-file-path)))

(defun ejc-next-result-file-path (&optional should-exist)
  (ejc-iterate-ring 'ejc-inc-ring-position should-exist))

(defun ejc-prev-result-file-path (&optional should-exist)
  (ejc-iterate-ring 'ejc-dec-ring-position should-exist))

(defun ejc-get-output-buffer ()
  "Get or create buffer for output SQL evaluation results.
It can be result sets, record affected messages, SQL definition of entities
or error messages."
  (when (not (and ejc-results-buffer
                  (buffer-live-p ejc-results-buffer)))
    (setq ejc-results-buffer (get-buffer-create
                              ejc-results-buffer-name))
    (with-current-buffer ejc-results-buffer
      (ejc-result-mode)))
  (with-current-buffer ejc-results-buffer
    ;; Clear undo history
    (setq buffer-undo-list nil))
  ejc-results-buffer)

(defun ejc-output-mode-specific-customization ()
  (cl-case ejc-result-table-impl
    (orgtbl-mode     (when (org-table-p) (org-table-align)))
    (ejc-result-mode (read-only-mode 1))))

(defun ejc-show-result-buffer (output-buffer)
  (if ejc-show-result-bottom
      (display-buffer output-buffer '(display-buffer-at-bottom . ()))
    (display-buffer output-buffer)))

;;;###autoload
(cl-defun ejc-show-last-result (&key result
                                     mode
                                     connection-name
                                     db
                                     goto-symbol)
  "Popup buffer with last SQL execution result output."
  (interactive)
  (let ((output-buffer (ejc-get-output-buffer)))
    (with-current-buffer output-buffer
      (read-only-mode -1)
      (erase-buffer)
      (when mode
        (ejc-update-modes-ring mode)
        (funcall mode))
      (ejc-add-connection connection-name db)
      (if result
          ;; SQL evaluation result passed directly to fn
          (insert result)
        ;; SQL evaluation result rendered to file
        (insert-file-contents (ejc-get-result-file-path)))
      (ejc-output-mode-specific-customization)
      (beginning-of-buffer)
      (let* ((window (or (get-buffer-window output-buffer t)
                         (progn
                           (ejc-show-result-buffer output-buffer)
                           (get-buffer-window output-buffer t))))
             (frame (window-frame window)))
        (if (not (eq frame (selected-frame)))
            (make-frame-visible frame))
        (if goto-symbol
            ;; Reversed, since if the result buffer is a package, it can
            ;; contain 2 concatenated parts: header and implementation.
            ;; Assume user wants to locate a point (cursor) in
            ;; procedure implementation.
            (if-let* ((items-list (reverse (delq imenu--rescan-item
                                                 (ejc-flatten-index
                                                  (condition-case nil
                                                      (imenu--make-index-alist)
                                                    (error nil))))))
                      ;; Find symbol position (typically to locate the
                      ;; beginning of procedure implementation in the package).
                      (pos (alist-get goto-symbol items-list nil nil 'equal)))
                (set-window-point window pos)))))))

(cl-defun ejc-show-ring-result (prev-or-next)
  (let ((output-buffer (ejc-get-output-buffer)))
    (set-buffer output-buffer)
    (read-only-mode -1)
    (erase-buffer)
    (ejc-add-connection ejc-connection-name ejc-db)
    (let ((file-path (funcall prev-or-next t))
          (mode (alist-get ejc-ring-position (or ejc-modes-ring
                                                 (ejc-load-modes-ring)))))
      (if mode
          (funcall mode))
      (insert-file-contents file-path)
      (ejc-output-mode-specific-customization)
      (beginning-of-buffer)
      (ejc-show-result-buffer output-buffer)
      (message file-path))))

;;;###autoload
(cl-defun ejc-show-prev-result ()
  "Change `ejc-results-buffer' contents: show previous SQL evaluation result."
  (interactive)
  (ejc-show-ring-result 'ejc-prev-result-file-path))

;;;###autoload
(cl-defun ejc-show-next-result ()
  "Change `ejc-results-buffer' contents: show next SQL evaluation result."
  (interactive)
  (ejc-show-ring-result 'ejc-next-result-file-path))

(provide 'ejc-result-buffer)

;;; ejc-result-buffer.el ends here
