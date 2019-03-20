;;; ejc-result-buffer.el

;;; Copyright © 2019 - Kostafey <kostafey@gmail.com>

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

(defvar ejc-results-buffer nil
  "The results buffer.")

(defvar ejc-results-buffer-name "*ejc-sql-output*"
  "The results buffer name.")

(defvar ejc-result-file-template "ejc-sql-result-%d.txt"
  "SQL evaluation results file name template.")

(defvar ejc-ring-length 10)

(defvar ejc-ring-position 0)

(defvar ejc-result-file-path nil
  "The results file path. Refreshed by any finished SQL evaluation.")

(defvar ejc-modes-ring ())

(defun ejc-update-modes-ring (mode)
  (setf (alist-get ejc-ring-position ejc-modes-ring) mode))

(defun ejc-get-result-file-path ()
  (expand-file-name (format ejc-result-file-template ejc-ring-position)
                    (temporary-file-directory)))

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
  ejc-results-buffer)

;;;###autoload
(cl-defun ejc-show-last-result (&key result mode connection-name db)
  "Popup buffer with last SQL execution result output."
  (interactive)
  (let ((output-buffer (ejc-get-output-buffer)))
    (set-buffer output-buffer)
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
    (read-only-mode 1)
    (beginning-of-buffer)
    (display-buffer output-buffer)))

(cl-defun ejc-show-ring-result (prev-next)
  "Popup buffer with last SQL execution result output."
  (let ((output-buffer (ejc-get-output-buffer))
        (mode (alist-get ejc-ring-position ejc-modes-ring)))
    (set-buffer output-buffer)
    (read-only-mode -1)
    (erase-buffer)
    (if mode
        (funcall mode))
    (ejc-add-connection ejc-connection-name ejc-db)
    (let ((file-path (funcall prev-next t)))
      (insert-file-contents file-path)
      (read-only-mode 1)
      (beginning-of-buffer)
      (display-buffer output-buffer)
      (message file-path))))

(cl-defun ejc-show-prev-result ()
  "Popup buffer with last SQL execution result output."
  (interactive)
  (ejc-show-ring-result 'ejc-prev-result-file-path))

(cl-defun ejc-show-next-result ()
  "Popup buffer with last SQL execution result output."
  (interactive)
  (ejc-show-ring-result 'ejc-next-result-file-path))
(ejc-next-result-file-path t)

(global-set-key (kbd "C-M-<next>") (lambda ()
                                     (interactive)
                                     (if (equal (buffer-name)
                                                ejc-results-buffer-name)
                                         (ejc-show-next-result))))

(global-set-key (kbd "C-M-<prior>") (lambda ()
                                      (interactive)
                                      (if (equal (buffer-name)
                                                 ejc-results-buffer-name)
                                          (ejc-show-prev-result))))

(provide 'ejc-result-buffer)

;;; ejc-result-buffer.el ends here
