(require 'dash)

(defconst ejc-indentation-modifier-regexp
  ;; The absence of else is deliberate, since it does not modify the
  ;; indentation level per se. It only may cause the line, in which the
  ;; else is, to be shifted to the left.
  (concat
   "\\(\\_<"
   (regexp-opt '("do" "function" "repeat" "then" "if" "else" "elseif" "for" "while") t)
   "\\_>\\|"
   (regexp-opt '("{" "(" "["))
   "\\)\\|\\(\\_<"
   (regexp-opt '("end if" "end" "until") t)
   "\\_>\\|"
   (regexp-opt '("]" ")" "}"))
   "\\)")
  )

;; (defvar ejc-sql-sexp-alist
(setq ejc-sql-sexp-alist
      '(("begin" . "end")
        ("if"    . "end if")
        ("if"    . "elsif")
        ("if"    . "else")
        ("else"  . "end if")
        ("elsif" . "elsif")
        ("elsif" . "else")
        ("elsif" . "end if")
        ))

(defconst ejc-block-token-alist
  '(("begin" "\\_<end\\_>"   nil                                        open)
    ("{"        "}"           nil                                       open)
    ("["        "]"           nil                                       open)
    ("("        ")"           nil                                       open)
    ("if"       "\\_<\\(elseif\\|else\\|end if\\)\\_>"  nil             open)
    ("elseif"   "\\_<\\(elseif\\|else\\|end if\\)\\_>"  nil             middle)
    ("end"      nil           "\\_<\\(do\\|begin\\|then\\|else\\)\\_>"  close)
    ("}"        nil           "{"                                       close)
    ("]"        nil           "\\["                                     close)
    (")"        nil           "("                                       close))
  "This is a list of block token information blocks.
Each token information entry is of the form:
  KEYWORD FORWARD-MATCH-REGEXP BACKWARDS-MATCH-REGEXP TOKEN-TYPE
KEYWORD is the token.
FORWARD-MATCH-REGEXP is a regexp that matches all possible tokens when going forward.
BACKWARDS-MATCH-REGEXP is a regexp that matches all possible tokens when going backwards.
TOKEN-TYPE determines where the token occurs on a statement. open indicates that the token appears at start, close indicates that it appears at end, middle indicates that it is a middle type token, and middle-or-open indicates that it can appear both as a middle or an open type.")

(defun ejc-get-block-token-info (token)
  "Returns the block token info entry for TOKEN from lua-block-token-alist"
  (assoc (downcase token) ejc-block-token-alist))

(defun ejc-get-token-type (token-info)
  "Returns the relevant match regexp from token info"
  (nth 3 token-info))

(defun ejc-get-token-match-re (token-info direction)
  "Returns the relevant match regexp from token info"
  (cond
   ((eq direction 'forward) (cadr token-info))
   ((eq direction 'backward) (nth 2 token-info))
   (t nil)))

(defun ejc-comment-or-string-p (&optional pos)
  "Returns true if the point is in a comment or string."
  (save-excursion (let ((parse-result (syntax-ppss pos)))
                    (or (elt parse-result 3) (elt parse-result 4)))))

(defun ejc-find-regexp (direction regexp &optional limit ignore-p)
  "Searches for a regular expression in the direction specified.
Direction is one of 'forward and 'backward.
By default, matches in comments and strings are ignored, but what to ignore is
configurable by specifying ignore-p. If the regexp is found, returns point
position, nil otherwise.
ignore-p returns true if the match at the current point position should be
ignored, nil otherwise."
  (let ((ignore-func (or ignore-p 'ejc-comment-or-string-p))
        (search-func (if (eq direction 'forward)
                         're-search-forward 're-search-backward))
        (case-fold-search t))
    (catch 'found
      (while (funcall search-func regexp limit t)
        (if (and (not (funcall ignore-func (match-beginning 0)))
                 (not (funcall ignore-func (match-end 0))))
            (throw 'found (point)))))))

;; (ejc-get-token-type (ejc-get-block-token-info "IF"))
;; (re-search-forward ejc-indentation-modifier-regexp)
;; (ejc-find-regexp 'forward ejc-indentation-modifier-regexp)

(defun ejc-find-matching-token-word (token &optional direction)
  "Find matching open- or close-token for TOKEN in DIRECTION.
Point has to be exactly at the beginning of TOKEN, e.g. with | being point

  {{ }|}  -- (lua-find-matching-token-word \"}\" 'backward) will return
          -- the first {
  {{ |}}  -- (lua-find-matching-token-word \"}\" 'backward) will find
          -- the second {.

DIRECTION has to be either 'forward or 'backward."
  (let* ((token-info (ejc-get-block-token-info token))
         (match-type (ejc-get-token-type token-info))
         ;; If we are on a middle token, go backwards. If it is a middle or open,
         ;; go forwards
         (search-direction (or direction
                               (if (or (eq match-type 'open)
                                       (eq match-type 'middle-or-open))
                                   'forward
                                 'backward)
                               'backward))
         (match (ejc-get-token-match-re token-info search-direction))
         maybe-found-pos)
    ;; if we are searching forward from the token at the current point
    ;; (i.e. for a closing token), need to step one character forward
    ;; first, or the regexp will match the opening token.
    (if (eq search-direction 'forward) (forward-char 1))
    (catch 'found
      ;; If we are attempting to find a matching token for a terminating token
      ;; (i.e. a token that starts a statement when searching back, or a token
      ;; that ends a statement when searching forward), then we don't need to look
      ;; any further.
      (if (or (and (eq search-direction 'forward)
                   (eq match-type 'close))
              (and (eq search-direction 'backward)
                   (eq match-type 'open)))
          (throw 'found nil))
      (while (ejc-find-regexp search-direction ejc-indentation-modifier-regexp)
        ;; have we found a valid matching token?
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (let ((found-type (ejc-get-token-type
                             (ejc-get-block-token-info found-token))))
            (if (not (and match (string-match match found-token)))
                ;; no - then there is a nested block. If we were looking for
                ;; a block begin token, found-token must be a block end
                ;; token; likewise, if we were looking for a block end token,
                ;; found-token must be a block begin token, otherwise there
                ;; is a grammatical error in the code.
                (if (not (and
                          (or (eq match-type 'middle)
                              (eq found-type 'middle)
                              (eq match-type 'middle-or-open)
                              (eq found-type 'middle-or-open)
                              (eq match-type found-type))
                          (goto-char found-pos)
                          (ejc-find-matching-token-word found-token
                                                        search-direction)))
                    (when maybe-found-pos
                      (goto-char maybe-found-pos)
                      (throw 'found maybe-found-pos)))
              ;; yes.
              ;; if it is a not a middle kind, report the location
              (when (not (or (eq found-type 'middle)
                             (eq found-type 'middle-or-open)))
                (throw 'found found-pos))
              ;; if it is a middle-or-open type, record location, but keep searching.
              ;; If we fail to complete the search, we'll report the location
              (when (eq found-type 'middle-or-open)
                (setq maybe-found-pos found-pos))
              ;; Cannot use tail recursion. too much nesting on long chains of
              ;; if/elseif. Will reset variables instead.
              (setq token found-token)
              (setq token-info (ejc-get-block-token-info token))
              (setq match (lua-get-token-match-re token-info search-direction))
              (setq match-type (ejc-get-token-type token-info))))))
      maybe-found-pos)))

(defun ejc-sql-forward-sexp (&optional count)
  "Forward to block end"
  (interactive "p")
  ;; negative offsets not supported
  (cl-assert (or (not count) (>= count 0)))
  (save-match-data
    (let ((count (or count 1))
          (block-start (mapcar 'car ejc-sql-sexp-alist)))
      (while (> count 0)
        ;; skip whitespace
        (skip-chars-forward " \t\n")
        (if (looking-at (regexp-opt block-start 'words))
            (let ((keyword (match-string 1)))
              (ejc-find-matching-token-word keyword 'forward)
              )
          ;; If the current keyword is not a "begin" keyword, then just
          ;; perform the normal forward-sexp.
          (forward-sexp 1))
        (setq count (1- count))))))

(provide 'ejc-sexp)

;;; ejc-sexp.el ends here
