;;; ejc-autocomplete.el -- SQL completitions at point by auto-complete (the part of ejc-sql).

;;; Copyright © 2013-2020 - Kostafey <kostafey@gmail.com>

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

(require 'auto-complete)
(require 'ejc-completion-common)
(require 'ejc-flx)

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

(defvar ac-source-ejc-views
  '((candidates . ejc-views-candidates)
    (symbol . "v")
    (requires . 1)
    (cache . t)))

(defvar ac-source-ejc-packages
  '((candidates . ejc-packages-candidates)
    (symbol . "p")
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

(defvar ac-source-ejc-keywords
  '((candidates . ejc-get-keywords)
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
  (add-to-list 'ac-sources 'ac-source-ejc-keywords)
  (add-to-list 'ac-sources 'ac-source-ejc-owners)
  (add-to-list 'ac-sources 'ac-source-ejc-tables)
  (add-to-list 'ac-sources 'ac-source-ejc-views)
  (add-to-list 'ac-sources 'ac-source-ejc-packages)
  (add-to-list 'ac-sources 'ac-source-ejc-tables-point)
  (add-to-list 'ac-sources 'ac-source-ejc-colomns)
  (add-to-list 'ac-sources 'ac-source-ejc-colomns-point))

(defun ejc-ac-hook ()
  (if ejc-use-flx
      (if (require 'flx-ido nil 'noerror)
          (setq-local ac-match-function 'ejc-flx-match-internal)
        (error (concat "Please install flx.el and flx-ido.el "
                       "if you use fuzzy completion"))))
  (delq 'ac-source-dictionary ac-sources)
  (delq 'ac-source-abbrev ac-sources)
  (delq 'ac-source-words-in-same-mode-buffers ac-sources))

(add-hook 'ejc-sql-minor-mode-hook 'ejc-ac-hook)

(when (require 'yasnippet nil 'noerror)
  (setq yas-snippet-dirs
        (nconc yas-snippet-dirs
               (list (expand-file-name "snippets"
                                       (file-name-directory
                                        (locate-library "ejc-sql"))))))
  (defun ejc-yas-downcase-key (args)
    (if ejc-sql-mode
        (cl-callf downcase (nth 1 args)))
    args)
  (advice-add 'yas--fetch :filter-args #'ejc-yas-downcase-key))

(provide 'ejc-autocomplete)

;;; ejc-autocomplete.el ends here
