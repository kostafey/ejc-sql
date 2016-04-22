[![License GPL 2](https://img.shields.io/badge/license-GPL_2-green.svg)](http://www.gnu.org/licenses/gpl-2.0.txt)

# ejc-sql

ejc-sql turns Emacs into simple SQL client, it uses JDBC connection to
databases via [clojure/java.jdbc](https://github.com/clojure/java.jdbc) lib.

You can use multiple connections at the same time. Autocompletion and basic
SQL scripts formatting are also available.

## Installation

To run Clojure install [Leiningen](http://leiningen.org).

```lisp
;; Append ejs-sql to `load-path':

(defvar site-lisp-path "~/.emacs.d/")
(add-to-list
 'load-path
 (expand-file-name "ejc-sql/src/ejc_sql/" site-lisp-path))

(require 'ejc-sql)
```

## Configuration

Setup connections with `ejc-create-connection` function in your `.emacs`.
It's first arg is your custom database connection name, the rest args
are the same as database connection structure of
[clojure/java.jdbc](https://github.com/clojure/java.jdbc) lib.

The configuration of ejs-sql might looks like this:

```lisp
;; Create your jdbc database connections configuration:

;; MySQL example
(ejc-create-connection
 "MySQL-db-connection"
 :classpath (concat
             "~/.m2/repository/mysql/mysql-connector-java/5.1.6/"
             "mysql-connector-java-5.1.6.jar")
 :classname "com.mysql.jdbc.Driver"
 :subprotocol "mysql"
 :subname "//localhost:3306/my_db_name"
 :user "a_user"
 :password "secret")

;; MS SQL Server example
(ejc-create-connection
 "MS-SQL-db-connection"
 :classpath (concat
             "~/.m2/repository/com/microsoft"
             "/sqlserver/sqljdbc/4.2/sqljdbc-4.2.jar")
 :classname "com.microsoft.sqlserver.jdbc.SQLServerDriver"
 :subprotocol "sqlserver"
 :subname "//localhost:1433"
 :user "a_user"
 :password "secret"
 :database "my_db_name")

;; H2 example
(ejc-create-connection
 "H2-db-connection"
 :classpath "~/.m2/repository/com/h2database/h2/1.4.191/h2-1.4.191.jar"
 :classname "org.h2.Driver"
 :subprotocol "h2"
 :subname "file://~/projects/my_proj/db/database;AUTO_SERVER=TRUE"
 :user "a_user"
 :password "secret")
```

## Usage

First of all, open your SQL buffer file (or any temporary buffer) and connect
to database

`M-x ejc-connect <RET> MySQL-db-connection <RET>`.

and wait until "Connected." message appears.
Since connection information is buffer-local you should use `ejc-connect`
for any new buffer. There is handy function to create temporary buffer for
playing with SQL: `ejc-switch-to-sql-editor-buffer`.

Then type

```SQL
select <something> from <mytable>
```
and press <kbd>C-c C-c</kbd> to run it. Use `\` char to separate expressions to
eval. It's possible to run multiple statements, you can use `;` to separate it.

Have a much fun!

## List of keybindings & functions

New keybindings defined in `ejc-sql-mode` minor mode:

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
 `ejc-connect`                      | Connect to database for current buffer
 `ejc-quit-connection`              | Close all database connections, quit Clojure REPL.
 `ejc-format-sql-at-point`          | Format SQL
 `ejc-mark-this-sql`                | Mark SQL script bounded by the `ejc-sql-separator` or/and buffer boundaries
 `ejc-show-tables-list`             | Show tables list
 `ejc-show-constraints-list`        | Show constraints list
 `ejc-show-procedures-list`         | Show procedures list
 `ejc-open-log`                     | Open log
 `ejc-switch-to-sql-editor-buffer`  | Create buffer with `ejc-sql-mode`
 `ejc-invalidate-cache`             | Clean your current connection cache (database owners and tables list)

## Autocomplete

Autocompletion is available for the following databases:

* Informix
* MySQL
* Oracle
* H2
* MS SQL Server

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 24.
* [Leiningen](http://leiningen.org) 2.x
* [clomacs](https://github.com/clojure-emacs/clomacs)
* [clojure/java.jdbc](https://github.com/clojure/java.jdbc) 0.5.8
* [dash](https://github.com/magnars/dash.el)
* [auto-complete](https://github.com/auto-complete/auto-complete)

## License

Copyright © 2012-2016 Kostafey <kostafey@gmail.com>

Distributed under the General Public License 2.0+
