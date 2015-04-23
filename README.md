# ejc-sql

The project is going to become beta...

**ejc-sql** provides a simple way to interact with database via java/clojure
  libs to run SQL scripts from emacs.

## Installation

The configuration of ejs-sql might looks like this:

```lisp
; Append ejs-sql to `load-path':
(defvar site-lisp-path "~/.emacs.d/")
(add-to-list
 'load-path
 (expand-file-name "ejc-sql/src/ejc_sql/" site-lisp-path))

(require 'ejc-sql)

; Create your jdbc database connection configuration:
(setq my-db-connection (make-ejc-db-conn
                        :classpath (concat
                                    "/home/user/lib/"
                                    "mysql-connector-java-3.1.13-bin.jar")
                        :classname "com.mysql.jdbc.Driver"
                        :subprotocol "mysql"
                        :subname "//localhost:3306/my_db_name"
                        :user "a_user"
                        :password "secret"))
```

## Usage

First of all, run to connect `(ejc-connect "my-db-connection")` or `M-x
ejc-connect <RET> my-db-connection <RET>`

New keybindings added to `sql-mode-map`:

 Keyboard shortcut  | Command                       | Description
--------------------|-------------------------------|------------------------------------------------------
 <kbd>C-c C-c</kbd> | `ejc-eval-user-sql-at-point`  | Evaluate SQL/JPQL script bounded by the `ejc-sql-separator` or/and buffer boundaries.
 <kbd>C-h t</kbd>   | `ejc-describe-table`          | Describe SQL table.
 <kbd>C-h up</kbd>  | `ejc-show-last-result`        | Show last result.
 <kbd>C-c t</kbd>   | `ejc-show-tables-list`        | Show tables list.
 <kbd>C-c s</kbd>   | `ejc-strinp-sql-at-point`     | Strip SQL (trim java string tokens).
 <kbd>C-c S</kbd>   | `ejc-dress-sql-at-point`      | Dress SQL (to copy-paste it to java code).

List of other interactive functions

 Command                            | Description
------------------------------------|------------------------------------------
 `ejc-format-sql-at-point`          | Format SQL
 `ejc-mark-this-sql`                | Mark SQL script bounded by the `ejc-sql-separator` or/and buffer boundaries
 `ejc-show-tables-list`             | Show tables list
 `ejc-show-constraints-list`        | Show constraints list
 `ejc-show-procedures-list`         | Show procedures list
 `ejc-open-log`                     | Open log
 `ejc-switch-to-sql-editor-buffer`  | Create buffer with `ejc-sql-mode`

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 24.
* [Leiningen](http://leiningen.org) 2.x
* [clomacs](https://github.com/clojure-emacs/clomacs)
* [clojure/java.jdbc](https://github.com/clojure/java.jdbc) 0.3.5
* [dash](https://github.com/magnars/dash.el)
* [auto-complete](https://github.com/auto-complete/auto-complete)

## License

Copyright © 2012-2015 Kostafey <kostafey@gmail.com>

Distributed under the General Public License 2.0+
