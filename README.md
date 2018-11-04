[![Emacs](https://img.shields.io/badge/Emacs-25-8e44bd.svg)](https://www.gnu.org/software/emacs/)
[![License GPL 2](https://img.shields.io/badge/license-GPL_2-green.svg)](http://www.gnu.org/licenses/gpl-2.0.txt)
[![MELPA](https://melpa.org/packages/ejc-sql-badge.svg)](https://melpa.org/#/ejc-sql)
[![Melpa Stable](https://stable.melpa.org/packages/ejc-sql-badge.svg)](https://stable.melpa.org/#/ejc-sql)
[![Build Status](https://api.travis-ci.org/kostafey/ejc-sql.svg?branch=master)](https://travis-ci.org/kostafey/ejc-sql#)
[![Coverage Status](https://coveralls.io/repos/kostafey/ejc-sql/badge.svg?branch=master)](https://coveralls.io/github/kostafey/ejc-sql?branch=master)

# ejc-sql

ejc-sql turns Emacs into a simple SQL client; it uses a JDBC connection to
databases via [clojure/java.jdbc](https://github.com/clojure/java.jdbc) lib.

You can use multiple connections at the same time. Autocompletion and basic
formatting of SQL scripts are also available.

- [Installation](#installation)
- [Install JDBC Drivers](#install-jdbc-drivers)
- [Configuration](#configuration)
  - [MySQL connection](#mysqlconnection)
  - [MS SQL Server connection](#mssqlserverconnection)
  - [Oracle connection](#oracleconnection)
  - [H2 connection](#h2connection)
  - [PostgreSQL connection](#postgresqlconnection)
- [Usage](#usage)
- [Autocomplete](#autocomplete)
- [Troubleshooting](#troubleshooting)
- [Requirements](#requirements)
- [License](#license)

## Installation

1. To run Clojure, install [Leiningen](http://leiningen.org) (assumes you have
   already installed Java 7+).

2. Add [MELPA](https://github.com/melpa/melpa#usage) (if not yet present) to your
   `package-archives` list.

   Then you can install ejc-sql with the following command:

   <kbd>M-x package-install [RET] ejc-sql [RET]</kbd>

## Install JDBC drivers

If you are familiar with JDBC, please omit this section.

The most common way is to install JDBC drivers to your `~/.m2` directory.
Here is a list of such installation examples. Anyway, __it will become outdated
soon__, so please consult Google to install your database JDBC driver.

First of all, install [Maven](https://maven.apache.org/), then you can install
your JDBC driver with one of the following commands.

**Oracle**

Download JDBC driver manually from
[oracle.com](http://www.oracle.com/technetwork/database/features/jdbc/index-091264.html)

Fix your actual JDBC version number `-Dversion`, filepath `-Dfile` and run
command like this:
```
mvn install:install-file -Dfile="~/downloads/ojdbc7.jar" -DgroupId=com.oracle.jdbc -DartifactId=ojdbc7 -Dversion=12.1.0.2 -Dpackaging=jar -DgeneratePom=true
```

**MS SQL Server**

Download JDBC driver manually from
[microsoft.com](https://social.msdn.microsoft.com/search/en-US?query=sql%20server%20jdbc)

Fix your actual JDBC version number `-Dversion`, filepath `-Dfile` and run
command like this:
```
mvn install:install-file -Dfile="~/downloads/sqljdbc.jar" -DgroupId=com.microsoft.sqlserver  -DartifactId=sqljdbc -Dversion=6.0 -Dpackaging=jar -DgeneratePom=true
```
or from Maven Central:
```
mvn org.apache.maven.plugins:maven-dependency-plugin:2.10:get -Dartifact=com.microsoft.sqlserver:mssql-jdbc:6.2.2.jre8
```

**JTDS**

```mvn org.apache.maven.plugins:maven-dependency-plugin:2.10:get -Dartifact=net.sourceforge.jtds:jtds:1.3.1```

**PostgreSQL**
```
mvn org.apache.maven.plugins:maven-dependency-plugin:2.10:get -Dartifact=postgresql:postgresql:9.3-1102.jdbc41 -DrepoUrl=http://clojars.org/repo/
```

**MySQL**

```mvn org.apache.maven.plugins:maven-dependency-plugin:2.10:get -Dartifact=mysql:mysql-connector-java:5.1.6```

**MariaDB**

```mvn org.apache.maven.plugins:maven-dependency-plugin:2.10:get -Dartifact=org.mariadb.jdbc:mariadb-java-client:1.1.7```

**H2**

```mvn org.apache.maven.plugins:maven-dependency-plugin:2.10:get -Dartifact=com.h2database:h2:1.4.192```

**SQLite**

```mvn org.apache.maven.plugins:maven-dependency-plugin:2.10:get -Dartifact=org.xerial:sqlite-jdbc:3.8.11.2```

## Configuration

First, load `ejc-sql` package:
```lisp
(require 'ejc-sql)
```

`ejc-set-rows-limit` set limit for the number of records to output (1000 by
default). Set to nil if you want to disable this limit.

```lisp
(ejc-set-rows-limit 1000)
```

Setup connections with `ejc-create-connection` function in your `.emacs`.
Its first arg is your custom database connection name, the remaining args
are the same as database connection structure of
[clojure/java.jdbc](https://github.com/clojure/java.jdbc) lib.

The configuration of `ejs-sql` might looks like this:

```lisp
;; Create your JDBC database connections configuration:
```

<a id="mysqlconnection"></a>
### MySQL connection
```lisp
;; MySQL example
(ejc-create-connection
 "MySQL-db-connection"
 :classpath (concat "~/.m2/repository/mysql/mysql-connector-java/5.1.6/"
                     "mysql-connector-java-5.1.6.jar")
 :subprotocol "mysql"
 :subname "//localhost:3306/my_db_name"
 :user "a_user"
 :password "secret")
```

<a id="mssqlserverconnection"></a>
### MS SQL Server connection
```lisp
;; MS SQL Server example
(ejc-create-connection
 "MS-SQL-db-connection"
 :classpath (concat "~/.m2/repository/com/microsoft"
                     "/sqlserver/sqljdbc/4.2/sqljdbc-4.2.jar")
 :subprotocol "sqlserver"
 :subname "//localhost:1433"
 :user "a_user"
 :password "secret"
 :database "my_db_name")

;; MS SQL Server example (via URI)
(ejc-create-connection
 "MS-SQL-db-connection-uri"
 :classpath (concat "~/.m2/repository/com/microsoft"
                     "/sqlserver/sqljdbc/4.2/sqljdbc-4.2.jar")
 :connection-uri (concat "jdbc:sqlserver://localhost\\\\instance:1433;"
                         "databaseName=my_db_name;"
                         "user=a_user;"
                         "password=secret;"))

;; MS SQL Server example (via JTDS)
(ejc-create-connection
 "MS-SQL-db-connection-JTDS"
 :classpath (concat "~/.m2/repository/net/sourceforge/jtds"
                     "/jtds/1.3.1/jtds-1.3.1.jar")
 :connection-uri (concat "jdbc:jtds:sqlserver://localhost:1433/dbname;"
                         "instance=instance;"
                         "user=a_user;"
                         "password=secret;"))
```

<a id="oracleconnection"></a>
### Oracle connection
```lisp
;; Oracle example (via Service Name)
(ejc-create-connection
 "Oracle-db-connection-sname"
 :classpath (concat "~/.m2/repository/com/oracle/jdbc"
                    "/oracle/jdbc/ojdbc8/12.2.0.1/ojdbc8-12.2.0.1.jar")
 :dbtype "oracle"
 :dbname "my_service_name"
 :host "localhost"
 :port "1521"
 :user "a_user"
 :password "secret"
 :separator "/")

;; Oracle example (via SID)
(ejc-create-connection
 "Oracle-db-connection-sid"
 :classpath (concat "~/.m2/repository/com/oracle/jdbc"
                     "/ojdbc7/12.1.0.2/ojdbc7-12.1.0.2.jar")
 :dbtype "oracle:sid"
 :dbname "my_sid_name"
 :host "localhost"
 :port "1521"
 :user "a_user"
 :password "secret"
 :separator "/")

;; Oracle example (via URI)
(ejc-create-connection
 "Oracle-db-connection-uri"
 :classpath (concat "~/.m2/repository/com/oracle/jdbc"
                     "/ojdbc7/12.1.0.2/ojdbc7-12.1.0.2.jar")
 :connection-uri "jdbc:oracle:thin:@localhist:1521:dbname"
 :user "a_user"
 :password "secret"
 :separator "/")
```

<a id="h2connection"></a>
### H2 connection
```lisp
;; H2 example
(ejc-create-connection
 "H2-db-connection"
 :classpath (file-truename
             "~/.m2/repository/com/h2database/h2/1.4.191/h2-1.4.191.jar")
 :subprotocol "h2"
 :subname "file://~/projects/my_proj/db/database;AUTO_SERVER=TRUE"
 :user "a_user"
 :password "secret")

;; H2 remote example
;; run on remote server first:
;; java -jar ~/.m2/repository/com/h2database/h2/1.4.192/h2-1.4.192.jar -tcpAllowOthers
(ejc-create-connection
 "H2-remote-db-connection"
 :classpath "~/.m2/repository/com/h2database/h2/1.4.192/h2-1.4.192.jar"
 :connection-uri (concat "jdbc:h2:tcp://192.168.0.1:9092/~/db/database;ifexists=true;"
                         "user=a_user;"
                         "password=secret;"))
```

<a id="postgresqlconnection"></a>
### PostgreSQL connection
```lisp
;; PostgreSQL example
(ejc-create-connetion
 "PostgreSQL-db-connection"
 :classpath (concat "~/.m2/repository/postgresql/postgresql/9.3.1102.jdbc41/"
                     "postgresql-9.3-1102.jdbc41.jar")
 :subprotocol "postgresql"
 :subname "//localhost:5432/my_db_name"
 :user "a_user"
 :password "secret")
```

## Usage

First of all, open your SQL buffer file (or any temporary buffer) and connect
to your database

`M-x ejc-connect <RET> MySQL-db-connection <RET>`.

and wait until "Connected." message appears.
Since connection information is buffer-local, you should use `ejc-connect`
for any new buffer. There is a handy function to create temporary buffer for
playing with SQL: `ejc-switch-to-sql-editor-buffer`.

Then type you queries like this:

```SQL
select something from my_table
```
and press <kbd>C-c C-c</kbd> to run it.

Use `/` char to separate expressions to evaluate (actually `\n/`), e.g.:
```SQL
select something from my_table
/
select other from other_table
```

It's possible to run multiple statements, you can use `;` to separate them:
```SQL
insert into my_table (product, price) values ('socks', 1.25);
insert into my_table (product, price) values ('sweater', 14.56);
insert into my_table (product, price) values ('jeans', 25.30);
/
select * from my_table
```

Have much fun!

## Use existing nREPL

If you have to restart Emacs multiple times, you can keep the ejc-sql clojure
backend alive between Emacs restarts by running this backend out of Emacs, and
connect to it from Emacs.

To accomplish that, you should `cd` to your ejc-sql project folder (typically
`~/.emacs.d/elpa/ejc-sql-<version>`) and launch the nREPL via `lein repl`.

Then run in Emacs `M-x ejc-connect-existing-repl`, type `Host` and `Port`
from your `lein repl` console output.

Finally, use `M-x ejc-connect` from any SQL buffer to connect to the exact database,
as always.

## List of keybindings & functions

New keybindings defined in `ejc-sql-mode` minor mode:

 Keyboard shortcut   | Command                         | Description
---------------------|---------------------------------|------------------------------------------------------
 <kbd>C-c C-c</kbd>  | `ejc-eval-user-sql-at-point`    | Evaluate SQL/JPQL script bounded by the `ejc-sql-separator` or/and buffer boundaries.
 <kbd>C-g</kbd>      | `ejc-cancel-query`              | Terminate current running query or run `keyboard-quit` if there is no running queries.
 <kbd>C-h t</kbd>    | `ejc-describe-table`            | Describe SQL table.
 <kbd>C-h T</kbd>    | `ejc-describe-entity`           | Describe SQL entity entity - function, procedure or type.
 <kbd>C-c e up</kbd> | `ejc-show-last-result`          | Show last result.
 <kbd>C-c e t</kbd>  | `ejc-show-tables-list`          | Show tables list.
 <kbd>C-c e T</kbd>  | `ejc-show-user-types-list`      | Show user types list.
 <kbd>C-c e s</kbd>  | `ejc-strinp-sql-at-point`       | Strip SQL (trim java string tokens).
 <kbd>C-c e S</kbd>  | `ejc-dress-sql-at-point`        | Dress SQL (to copy-paste it to java code).
 <kbd>C-c e p</kbd>  | `ejc-pretty-print-sql-at-point` | Pretty-print this SQL statement.
 <kbd>C-M-b</kbd>    | `ejc-previous-sql`              | Goto previous SQL statement.
 <kbd>C-M-f</kbd>    | `ejc-next-sql`                  | Goto next SQL statement.
 <kbd>C-M-S-b</kbd>  | `ejc-previous-sql`              | Select from point to previous SQL statement.
 <kbd>C-M-S-f</kbd>  | `ejc-next-sql`                  | Select from point to next SQL statement.

List of other interactive functions

 Command                            | Description
------------------------------------|------------------------------------------
 `ejc-connect`                      | Connect to database for current buffer
 `ejc-quit-connection`              | Close all database connections, quit Clojure REPL.
 `ejc-format-sql-at-point`          | Format SQL
 `ejc-pretty-print-sql-region`      | Pretty-print selected SQL snippet
 `ejc-mark-this-sql`                | Mark SQL script bounded by the `ejc-sql-separator` or/and buffer boundaries
 `ejc-show-tables-list`             | Show tables list
 `ejc-show-constraints-list`        | Show constraints list
 `ejc-show-procedures-list`         | Show procedures list
 `ejc-open-log`                     | Open log
 `ejc-switch-to-sql-editor-buffer`  | Create buffer with `ejc-sql-mode`
 `ejc-invalidate-cache`             | Clean your current connection cache (database owners and tables list)
 `ejc-direx:pop-to-buffer`          | Create buffer with database structure tree

## Autocomplete

Autocompletion is available for the following databases:

* Oracle
* MS SQL Server
* PostgreSQL
* MySQL
* Informix
* H2

## Troubleshooting

```
Error running timer ‘ac-update-greedy’: (error "Sync nREPL request timed out (op eval session...
```

Increase `nrepl-sync-request-timeout`, e.g.:

```lisp
(setq nrepl-sync-request-timeout 60)
```

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 25.
* [Leiningen](http://leiningen.org) 2.x
* [clomacs](https://github.com/clojure-emacs/clomacs)
* [clojure/java.jdbc](https://github.com/clojure/java.jdbc) 0.5.8
* [dash](https://github.com/magnars/dash.el)
* [auto-complete](https://github.com/auto-complete/auto-complete)
* [spinner.el](https://github.com/Malabarba/spinner.el)
* [direx.el](https://github.com/m2ym/direx-el)

## License

Copyright © 2012-2018 Kostafey <kostafey@gmail.com> and
[contributors](https://github.com/kostafey/ejc-sql/contributors)

Distributed under the General Public License 2.0+
