[![Emacs](https://img.shields.io/badge/Emacs-26-8e44bd.svg)](https://www.gnu.org/software/emacs/)
[![License GPL 2](https://img.shields.io/badge/license-GPL_2-green.svg)](http://www.gnu.org/licenses/gpl-2.0.txt)
[![MELPA](https://melpa.org/packages/ejc-sql-badge.svg)](https://melpa.org/#/ejc-sql)
[![Melpa Stable](https://stable.melpa.org/packages/ejc-sql-badge.svg)](https://stable.melpa.org/#/ejc-sql)
[![Build Status](https://api.travis-ci.org/kostafey/ejc-sql.svg?branch=master)](https://travis-ci.org/kostafey/ejc-sql#)
[![Coverage Status](https://coveralls.io/repos/kostafey/ejc-sql/badge.svg?branch=master)](https://coveralls.io/github/kostafey/ejc-sql?branch=master)

# ejc-sql

<img src="https://github.com/kostafey/ejc-sql/blob/master/img/ejc-sql-logo.png" width="220px"
 alt="ejc-sql logo" align="right" />

ejc-sql turns Emacs into a simple SQL client; it uses a JDBC connection to
databases via [clojure/java.jdbc](https://github.com/clojure/java.jdbc) lib.

You can use multiple connections at the same time. Autocompletion and basic
formatting of SQL scripts are also available.

- [Installation](#installation)
- [Configuration](#configuration)
  - [Set httpd port](#set-httpd-port)
  - [Autocomplete](#autocomplete)
  - [Fuzzy matching](#fuzzy-matching)
  - [Company mode](#company-mode)
  - [Minibuffer completion](#minibuffer-completion)
  - [ElDoc](#eldoc)
  - [Performance & output customization](#performance-output-customization)
  - [Create connections interactively](#create-connections-interactively)
  - [Create connections manualy](#create-connections-manualy)
    - [Install JDBC drivers](#install-jdbc-drivers)
    - [MySQL connection](#mysqlconnection)
    - [MariaDB connection](#mariadbconnection)
    - [MS SQL Server connection](#mssqlserverconnection)
    - [Oracle connection](#oracleconnection)
    - [H2 connection](#h2connection)
    - [SQLite connection](#sqliteconnection)
    - [PostgreSQL connection](#postgresqlconnection)
    - [Informix connection](#informixconnection)
    - [Presto connection](#prestoconnection)
    - [ClickHouse connection](#clickhouseconnection)
    - [ElasticSearch connection](#elasticsearchconnection)
- [Usage](#usage)
  - [Basic use case](#basic-use-case)
  - [Separators & delimiters](#separators-delimiters)
  - [Use with org-mode](#use-with-org-mode)
  - [Use existing nREPL](#use-existing-nrepl)
    - [Dedicated ejc-sql nREPL](#dedicated-nrepl)
    - [Different project nREPL](#different-nrepl)
  - [Goto definition & results ring](#goto-definition-results-ring)
- [List of keybindings & functions](#keybindings)
- [Yasnippet](#yasnippet)
- [Troubleshooting](#troubleshooting)
- [Requirements](#requirements)
- [License](#license)

![ejc-screenshot](https://user-images.githubusercontent.com/1282079/73614659-476e7b80-4612-11ea-8681-1c37bda9422e.png)

## Installation

1. To run Clojure, install [Leiningen](http://leiningen.org) (assuming you have
   already installed Java 7+).

2. Add [MELPA](https://github.com/melpa/melpa#usage) (if not yet present) to your
   `package-archives` list.

   Then you can install ejc-sql with the following command:

   <kbd>M-x package-install [RET] ejc-sql [RET]</kbd>

## Configuration

Here is an full-fledged real-world `ejc-sql` configuration example:
[ejc-sql-conf](https://github.com/kostafey/kostafeys-emacs-confik/blob/master/custom/ejc-sql-conf.el).

First, load `ejc-sql` package:
```lisp
(require 'ejc-sql)
```

## Set httpd port

To achieve async SQL queries evaluation, both Emacs and JVM side is an HTTP
client and HTTP server. Emacs as HTTP client via CIDER pass a SQL query to JVM
and don't expect any data response from the database. The JVM part prints the
result dataset to the file (in pain text table format). Then JVM as HTTP client
notifies Emacs: "data printed into `filepath`, please refresh the output
buffer". The JVM side port can be configured by related CIDER customizations,
whereas the default Emacs side HTTP server port can be customized by
`clomacs-httpd-default-port` variable (`8080` by default):
```lisp
(setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.
```

### Autocomplete

Install `auto-complete` e.g. by the following command:
<kbd>M-x package-install [RET] auto-complete [RET]</kbd>
Enable autocomplete for `ejc-sql` minor mode:
```lisp
(require 'ejc-autocomplete)
(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (ejc-ac-setup)))
```

Autocompletion is available for the following databases:

* Oracle
* MS SQL Server
* PostgreSQL
* MySQL
* Informix
* H2
* SQLite

Autocompletion data is stored in the database structure cache. This cache is
located on Clojure side, so it's global: the same database structure information
is shared between different buffers connected to the same database. An attempt
to autocomplete requires data from cache or lanches a thread aimed to create
it. If Clojure side has the database structure cache, autocompletion variants
are returned immediately. If not, the database structure cache creation process
starts. It's async, so the process of Emacs is not blocked, and the user can
move the point (cursor), edit SQL, and so on. If the user waits for
autocompletion and doesn't move point (cursor) during this process, he will get
autocompletion variants. In order to checkout the current database connection
cache run `ejc-print-cache`.

Any successfully executed DDL query (`CREATE`, `ALTER`, `DROP`, `RENAME`) clears
current connection cache, so next autocompletion attempt will recreate it.
To clean the current  connection cache manually, you can run
`ejc-invalidate-cache`.

<a id="fuzzy-matching"></a>
### Fuzzy matching

Non-nil `ejc-use-flx` enables `flx` fuzzy matching engine for autocompletion.
[flx-ido](https://github.com/lewang/flx) is required in this case, it can
be installed by your favorite approach. E.g. by `MEPLA`:
<kbd>M-x package-install [RET] flx-ido [RET]</kbd>

```lisp
(setq ejc-use-flx t)
```

To customize the minimum number of typed chars use `flx` for autocompletion,
2 by default:

```lisp
(setq ejc-flx-threshold 2)
```

<a id="company-mode"></a>
### Company mode

Install `company-mode` e.g. by the following command:
<kbd>M-x package-install [RET] company [RET]</kbd>
Enable `company-mode` completion frontend for `ejc-sql` minor mode:

```lisp
(require 'ejc-company)
(push 'ejc-company-backend company-backends)
(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (company-mode t)))
```

If you want to automatically start completion after inserting a dot for
`company-mode` despite `company-minimum-prefix-length` is bigger than `0`,
set `ejc-complete-on-dot` to `t`:

```elisp
(setq ejc-complete-on-dot t)
```

To show documentation quickhelp install `company-quickhelp` by:
<kbd>M-x package-install [RET] company-quickhelp [RET]</kbd>

To activate `company-quickhelp` add the following to your `.emacs`:

```lisp
(company-quickhelp-mode)
```

<a id="minibuffer-completion"></a>
### Minibuffer completion

By default `ido` is used as minibuffer the completion system. You can change
this to leverage another option by editing `ejc-completion-system` and
selecting `standard`. This will allow you to use it with any configured
completion mechanism for example, [ivy](https://github.com/abo-abo/swiper):

```elisp
(setq ejc-completion-system 'standard)
```

### ElDoc

Enable ElDoc for `ejc-sql` minor mode:
```lisp
(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (ejc-eldoc-setup)))
```

ElDoc for functions and procedures is available for the following databases:

* Oracle
* PostgreSQL
* MySQL

<a id="performance-output-customization"></a>
### Performance & output customization

`ejc-set-fetch-size` sets limit for the number of records to output (`50` by
default). Set to `nil` if you want to disable this limit.

`ejc-set-max-rows` sets the limit for the number of records to contain in
ResultSet (`99` by default). Set to `nil` if you want to disable this limit, or
you can set it the same value as `ejc-set-fetch-size` to increase select query
execution performance.

Any time your ResultSet is bigger than `ejc-set-fetch-size` you will receive
messages like `"Too many rows. Only 50 from 99+ are shown."`. To inhibit this
messages you can set `ejc-set-show-too-many-rows-message` to `nil` (`t` by
default).

`ejc-set-column-width-limit` sets limit for outputing the number of chars per
column (`30` by default). The rest will be replaced by `...`. Set to
`nil` if you want to disable this limit. This setting is applied to the text
representation of any field type, but it is especially useful for `varchar` and
`CLOB` fields.

`ejc-set-use-unicode` sets using unicode for grid borders, e.g. use `─┼─`
instead of `-+-` (`nil` by default).

All these functions change Clojure variables, so if you want to change
defaults, to avoid Clojure nREPL autolaunch on Emacs start, you should add
them to the `ejc-sql-connected-hook` in your `.emacs`, e.g.:
```lisp
(add-hook 'ejc-sql-connected-hook
          (lambda ()
            (ejc-set-fetch-size 50)
            (ejc-set-max-rows 50)
            (ejc-set-show-too-many-rows-message t)
            (ejc-set-column-width-limit 25)
            (ejc-set-use-unicode t)))
```

Current result set table minor-mode is `orgtbl-mode`. This mode provides some
functionality for post-processing and browsing the query results.
```lisp
(setq ejc-result-table-impl 'orgtbl-mode)
```
Alternatively, you can use a simple and bare result set mode to maximize the
buffer performance by setting `ejc-result-table-impl` to `'ejc-result-mode`.

If you want to see the full text of some field (e.g. the full text of `CLOB`
field) despite `ejc-set-column-width-limit`, and your `ejc-result-table-impl`
is `'ejc-result-mode` you can select a single-record result set
(e.g. `SELECT * FROM table WHERE id = 1`).

If you want to see the full text of some field with newlines in case of
multiline fields, you should select single-record and single-column result set
(e.g. `SELECT field FROM table WHERE id = 1`). So, you will get a field value
**as-is** despite `ejc-set-column-width-limit` and `ejc-result-table-impl`.

To illustrate the description above here are some output examples of query
results that depend on configuration.

Assume you have the following database (this example uses MySQL):

```sql
CREATE TABLE product (
  id    INT UNSIGNED  NOT NULL AUTO_INCREMENT PRIMARY KEY,
  name  VARCHAR(30)   NOT NULL,
  quantity INT,
  price DECIMAL(7,2),
  description VARCHAR(255)
);
INSERT INTO product (name, price, quantity, description)
VALUES ('socks', 1.25, 10, CONCAT('A sock is an item of clothing worn\n',
                                  'on the feet and often covering the\n',
                                  'ankle or some part of the calf.\n',
                                  'Some type of shoe or boot is\n',
                                  'typically worn over socks.'));
INSERT INTO product (name, price, quantity, description)
VALUES ('sweater', 14.56, 5, CONCAT('A sweater, also called a jumper\n'
                                    'in British English, is a piece\n'
                                    'of clothing, typically with long\n'
                                    'sleeves, made of knitted or\n'
                                    'crocheted material that covers\n'
                                    'the upper part of the body.'));
```
**output examples for** `orgtbl-mode` (by default):
```sql
SELECT * FROM product
```
```
| id | name    | quantity | price | description                    |
|----+---------+----------+-------+--------------------------------|
|  1 | socks   |       10 |  1.25 | A sock is an item of clothi... |
|  2 | sweater |        5 | 14.56 | A sweater, also called a ju... |
```
```sql
SELECT * FROM product WHERE id = 1
```
```
| id | name  | quantity | price | description                    |
|----+-------+----------+-------+--------------------------------|
|  1 | socks |       10 |  1.25 | A sock is an item of clothi... |
```
```sql
SELECT description FROM product WHERE id = 1
```
```
| description                        |
|------------------------------------|
| A sock is an item of clothing worn |
| on the feet and often covering the |
| ankle or some part of the calf.    |
| Some type of shoe or boot is       |
| typically worn over socks.         |
```
**output examples for** `ejc-result-mode`:
```sql
SELECT * FROM product
```
```
id | name    | quantity | price | description
---+---------+----------+-------+-------------------------------
1  | socks   | 10       | 1.25  | A sock is an item of clothi...
2  | sweater | 5        | 14.56 | A sweater, also called a ju...
```
```sql
SELECT * FROM product WHERE id = 1
```
```
id          | 1
name        | socks
quantity    | 10
price       | 1.25
description | A sock is an item of clothing worn on the feet and often covering the ankle or some part of the calf. Some type of shoe or boot is typically worn over socks.
```
```sql
SELECT description FROM product WHERE id = 1
```
```
description
----------------------------------
A sock is an item of clothing worn
on the feet and often covering the
ankle or some part of the calf.
Some type of shoe or boot is
typically worn over socks.
```

<a id="create-connections-interactively"></a>
## Create connections interactively

The easiest way to create connections configuration is to use interactive
connections creation.

In any `sql-mode` buffer run (<kbd>C-c ei</kbd>):

```
M-x ejc-connect-interactive <RET>
```

Then follow the creation steps: type your connection name, choose
database type, host (or file path depends on selected database type), port,
user name and password.

`ejc-sql` uses [Aether](https://github.com/cemerick/pomegranate) API of
Maven-resolver to automatically resolve and download the required JDBC
driver (if not yet) for selected database type.

You can customize artifacts and their versions used as JDBC drivers for each
database type in Leiningen format in `ejc-jdbc-drivers` custom variable.

After you type all required data a and new connection will be created, it
will attempt to immediately connect `current-buffer` to this connection.
Then you can use this connection name to connect from different buffers.
Type (<kbd>C-c ec</kbd>):
```
M-x ejc-connect <RET> your-connection-name <RET>
```

This connection will be available during the current Emacs session. To keep
it between Emacs restarts, you can open your `.emacs` file or any file,
loaded as Emacs configuration, locate point (cursor) somewhere after
`(require 'ejc-sql)` expression and run:
```
M-x ejc-insert-connection-data <RET> your-connection-name <RET>
```

This function inserts `ejc-create-connection` expression the same as you can
accomplish via manual connection creation.

<a id="create-connections-manualy"></a>
## Create connections manualy

<a id="install-jdbc-drivers"></a>
### Install JDBC drivers

In most cases, you don't need to install JDBC drivers manually.
Simply put, you can set a parameter `:dependencies` in `ejc-create-connection`
function as a vector of the required artifacts in Leiningen format.
In this case, `ejc-sql` will resolve, download (if not yet) all
required jar dependencies via [Aether](https://github.com/cemerick/pomegranate)
and load them to `CLASSPATH` during the `ejc-connect` function run. E.g.:

```lisp
(ejc-create-connection
 "Informix-db-connection"
 :dependencies [[com.ibm.informix/jdbc "4.50.3"]]
 ...
 )
```

Alternatively, you can pass the exact JDBC driver jar file in the
`:classpath` parameter of `ejc-create-connection` function. E.g.:

```lisp
(ejc-create-connection
 "Informix-db-connection"
 :classpath (concat "~/.m2/repository/com/ibm/informix/jdbc/4.50.3/"
                    "jdbc-4.50.3.jar")
 ...
 )
```

`ejc-sql` will try to resolve all required dependencies if this JBDC driver
requires some dependencies itself anyway. But you can pass all requred
dependencies manually as a vector of jar files paths. E.g.:

```lisp
(ejc-create-connection
 "Informix-db-connection"
 :classpath (vector
              (concat "~/.m2/repository/org/mongodb/bson/3.8.0/"
                      "bson-3.8.0.jar")
              (concat "~/.m2/repository/com/ibm/informix/jdbc/4.50.3/"
                      "jdbc-4.50.3.jar"))
 ...
 )
```

The rest of this section describes how to download and install JDBC drivers
manually. If you are familiar with JDBC, please omit it.

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
mvn install:install-file -Dfile="~/downloads/sqljdbc.jar" -DgroupId=com.microsoft.sqlserver -DartifactId=sqljdbc -Dversion=6.0 -Dpackaging=jar -DgeneratePom=true
```
or from Maven Central:
```
mvn org.apache.maven.plugins:maven-dependency-plugin:get -Dartifact=com.microsoft.sqlserver:mssql-jdbc:6.2.2.jre8
```

**JTDS**
```
mvn org.apache.maven.plugins:maven-dependency-plugin:get -Dartifact=net.sourceforge.jtds:jtds:1.3.1
```

**PostgreSQL**
```
mvn org.apache.maven.plugins:maven-dependency-plugin:get -Dartifact=postgresql:postgresql:9.3-1102.jdbc41 -DrepoUrl=http://clojars.org/repo/
```

**MySQL**
```
mvn org.apache.maven.plugins:maven-dependency-plugin:get -Dartifact=mysql:mysql-connector-java:5.1.6
```

**MariaDB**
```
mvn org.apache.maven.plugins:maven-dependency-plugin:get -Dartifact=org.mariadb.jdbc:mariadb-java-client:1.1.7
```

**H2**
```
mvn org.apache.maven.plugins:maven-dependency-plugin:get -Dartifact=com.h2database:h2:1.4.192
```

**SQLite**
```
mvn org.apache.maven.plugins:maven-dependency-plugin:get -Dartifact=org.xerial:sqlite-jdbc:3.8.11.2
```

**Informix**
```
mvn org.apache.maven.plugins:maven-dependency-plugin:get -Dartifact=com.ibm.informix:jdbc:4.50.3
```

Setup connections with `ejc-create-connection` function in your `.emacs`.
It's first arg is your custom database connection name, the remaining args
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

If you want to see MySQL-specific keywords in autocompletion
list, please provide access to `mysql.help_keyword` table
for your user, e.g.:
```sql
GRANT SELECT ON mysql.help_keyword TO a_user;
```

<a id="mariadbconnection"></a>
### MariaDB connection
```lisp
;; MariaDB example
(ejc-create-connection
 "MariaDB-db-connection"
 :dependencies [[org.mariadb.jdbc/mariadb-java-client "2.6.0"]]
 :classname "org.mariadb.jdbc.Driver"
 :connection-uri "jdbc:mariadb://localhost:3306/db_name"
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
                    "/ojdbc8/12.2.0.1/ojdbc8-12.2.0.1.jar")
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

<a id="sqliteconnection"></a>
### SQLite connection
```lisp
;; SQLite example
(ejc-create-connection
 "SQLite-conn"
 :classpath (concat "~/.m2/repository/org/xerial/sqlite-jdbc/"
                    "3.23.1/sqlite-jdbc-3.23.1.jar")
 :subprotocol "sqlite"
 ;; Use absolute path, e.g.:
 ;;   "file:///home/user/projects/my_proj/db/sqdb.db"
 ;;   "/home/user/projects/my_proj/db/sqdb.db"
 ;;   "file:///C:/Projects/my_proj/db/sqdb.db"
 ;;   "C:/Projects/my_proj/db/sqdb.db"
 ;; or expand it by file-truename (not applicable for Windows paths):
 :subname (concat "file://"
                  (file-truename "~/projects/my_proj/db/sqdb.db")))
```

<a id="postgresqlconnection"></a>
### PostgreSQL connection
```lisp
;; PostgreSQL example
(ejc-create-connection
 "PostgreSQL-db-connection"
 :classpath (concat "~/.m2/repository/postgresql/postgresql/9.3.1102.jdbc41/"
                     "postgresql-9.3-1102.jdbc41.jar")
 :subprotocol "postgresql"
 :subname "//localhost:5432/my_db_name"
 :user "a_user"
 :password "secret")
```

<a id="informixconnection"></a>
### Informix connection
```lisp
;; Informix example
(ejc-create-connection
 "Informix-db-connection"
 :dependencies [[com.ibm.informix/jdbc "4.50.3"]]
 :classname "com.informix.jdbc.IfxDriver"
 :connection-uri (concat
                  ;; In the case of IPv6, ::1 should be used
                  ;; as the host instead of localhost.
                  "jdbc:informix-sqli://localhost:8201/test:"
                  "INFORMIXSERVER=myserver;"
                  "user=a_user;"
                  "password=secret;"))

```

<a id="prestoconnection"></a>
### Presto connection
```lisp
;; Presto example
(ejc-create-connection
 "Presto-db-connection"
 :subprotocol "presto"
 :dependencies [[com.facebook.presto/presto-jdbc "0.232"]]
 :connection-uri (concat
                  "jdbc:presto://localhost:1234/dbName/schemaName?"
                  "user=a_user"))
```

<a id="clickhouseconnection"></a>
### ClickHouse connection
```lisp
;; ClickHouse example
(ejc-create-connection
  "ch@180"
  :dependencies [[ru.yandex.clickhouse/clickhouse-jdbc "0.2.6"]]
  :dbtype "clickhouse"
  :classname "ru.yandex.clickhouse.ClickHouseDriver"
  :connection-uri (concat "jdbc:clickhouse://10.1.4.180:8123/" "testdb"))
```

<a id="elasticsearchconnection"></a>
### ElasticSearch connection
```lisp
;; ElasticSearch example
(ejc-create-connection
  "es@177"
  :dependencies [[org.elasticsearch.plugin/x-pack-sql-jdbc "7.9.1"]]
  :dbtype "elasticsearch"
  :classname "org.elasticsearch.xpack.sql.jdbc.EsDriver"
  :connection-uri (concat "jdbc:es://172.16.13.177:9200/"))
```

## Usage

<a id="basic-use-case"></a>
### Basic use case

First of all, open your SQL source file (or any `sql-mode` buffer).

On the other hand, there is a handy function to create temporary `sql-mode`
buffers for playing with SQL: `ejc-get-temp-editor-buffer`.
If you bind it, e.g. to:
```lisp
(global-set-key (kbd "C-c eb") 'ejc-get-temp-editor-buffer)
```
then, when you press <kbd>C-c eb</kbd>, `*ejc-sql-editor*` buffer will be
created; when you press <kbd>M-1 C-c eb</kbd>, `*ejc-sql-editor-1*` buffer will
created and so on. This buffers can be saved as ordinary file buffers by
`save-buffer` command to the appropriate files, located in
`ejc-temp-editor-file-path` directory ("~/tmp/ejc-sql/" by default).

In any selected SQL buffer connect to your database:
```
M-x ejc-connect <RET> MySQL-db-connection <RET>
```
and wait until "Connected." message appears. This will add connection
information to buffer local variables. Furthermore, if there is no `ejc-sql`
dedicated Clojure REPL running, it will start it.

Since connection information is **`buffer-local`**, you should run `ejc-connect`
for any new buffer. Any of `ejc-sql-mode` buffers can keep connection
information to different databases and database types. But they use the same
`ejc-sql` dedicated Clojure REPL to interact with databases via JDBC.

Then type your queries like this:

```SQL
select something from my_table
```
and press <kbd>C-c C-c</kbd> to run it.

Have much fun!

<a id="separators-delimiters"></a>
### Separators & delimiters

Use `/` char to separate expressions to evaluate (actually `\n/`), e.g.:
```SQL
select something from my_table
/
select other from other_table
```
So, you don't need to select SQL snippet, simply put point (cursor) into code
snippet and press <kbd>C-c C-c</kbd> (or desired keybinding). Borders of SQL
will be found by Emacs buffer begin/end or this `/` separator.

It's possible to pass multiple statements, you can use `;` delimiter to separate
them:
```SQL
insert into my_table (product, price) values ('socks', 1.25);
insert into my_table (product, price) values ('sweater', 14.56);
insert into my_table (product, price) values ('jeans', 25.30);
/
select * from my_table
```
Here, the first part is a single SQL snippet passed to `ejc-sql` backend but
evaluated by 3 independent SQL statements (transactions). The output will looks
like this:
```
Records affected: 1
Records affected: 1
Records affected: 1
```

Furthermore, you can change the delimiter inside SQL snippet. E.g. in this
MySQL snippet `;` replaced by `$$` as transaction delimiter:
```SQL
DELIMITER $$
CREATE PROCEDURE GetAllProducts()
BEGIN
  SELECT * FROM products;
END $$
/
CALL GetAllProducts();
```

Since `;` symbols can be used very often as part of procedure syntax
(e.g. in Oracle), you can disable splitting SQL code snippet to the sequence of
separate transactions by setting `:separator` in DB connection configuration
(see https://github.com/kostafey/ejc-sql#oracle-connection).

<a id="use-with-org-mode"></a>
### Use with org-mode

You can run `M-x ejc-connect <RET> my-db-connection <RET>` in `org-mode`
buffers. In this case, `major-mode` will persists as `org-mode`, but all
connection-related data will be added to the buffer.

```markdown
* Create DB
** Product table
*** Create
#+begin_src sql
CREATE TABLE product (
  id    INT UNSIGNED  NOT NULL AUTO_INCREMENT,
  name  VARCHAR(30)   NOT NULL,
  price DECIMAL(7,2)
);
#+end_src

*** Fill
#+begin_src sql
INSERT INTO product (name, price) VALUES ('socks', 1.25);
INSERT INTO product (name, price) VALUES ('sweater', 14.56);
INSERT INTO product (name, price) VALUES ('jeans', 25.30);
#+end_src

*** Select
#+begin_src sql
SELECT * FROM product;
/
SELECT * FROM product WHERE name = 'jeans';
#+end_src

#+RESULTS:
: id | name    | price
: ---+---------+------
: 1  | socks   | 1.25
: 2  | sweater | 14.56
: 3  | jeans   | 25.30
```

Place point (cursor) into code snippet and run SQL statements via
<kbd>C-c C-c</kbd> as always. For `org-mode` buffers code snippets borders
considered as batch of SQL statement(s) boundaries.
Furthermore, you can use `ejc-sql-separator` (`/` by default) to divide
batch of SQL statement(s) inside code block as in `sql-mode` buffers.

The SQL query evaluation result will be added to this `org-mode` buffer in
`#+RESULTS:` section - an expected behaviour for `org-mode` users by default
(see example above).

To avoid this behaviour and get results in popup window - as `ejc-sql` users
expected, add to your `.emacs`:

```lisp
(setq ejc-org-mode-show-results nil)
```

If your `org-mode` buffer connected via `ejc-connect`, any time you run
<kbd>C-c '</kbd> (`org-edit-special`) for code snippets, you will get new
buffer with this minor-mode (`ejc-sql-mode`) and all connection-related data.
So, you can operate inside it like in ordinary `sql-mode` buffer, which is
already connected to the database.

You can use both `ejc-sql` and `org-mode` original `org-babel` execution
engine simultaneously in one buffer.

To disable `ejc-sql` wrapper around `org-mode` SQL source code blocks, set
`ejc-org-mode-babel-wrapper` to `nil` (enabled by default).

If `ejc-org-mode-babel-wrapper` is enabled and the current SQL source code block
has a connection header arguments, you will be asked for confirmation.

Reference this [discussion](https://github.com/kostafey/ejc-sql/pull/74).

<a id="use-existing-nrepl"></a>
### Use existing nREPL

<a id="dedicated-nrepl"></a>
#### Dedicated ejc-sql nREPL

If you have to restart Emacs multiple times, you can keep the `ejc-sql` Clojure
backend alive between Emacs restarts by running this backend out of Emacs, and
connect to it from Emacs.

To accomplish that, you should `cd` to your ejc-sql project folder (typically
`~/.emacs.d/elpa/ejc-sql-<version>`) and launch the nREPL via `lein repl`.

Then run in Emacs `M-x ejc-connect-existing-repl`, type `Host` and `Port`
from your `lein repl` console output.

Finally, use `M-x ejc-connect` from any SQL buffer to connect to the exact
database, as always.

<a id="different-nrepl"></a>
#### Different project nREPL

You can use different nREPL for `ejc-sql`, e.g. if you develop a Clojure project
via CIDER, you can use your project nREPL to interact with the database by JDBC
and `ejc-sql`. To achieve this, enable using any CIDER nREPL for `clomacs`
projects in your `.emacs`:
```lisp
(setq clomacs-allow-other-repl t)
```

Then add `ejc-sql` to your project as a dependency in `project.clj`:
```clojure
(defproject some-project "0.1.0-SNAPSHOT"
  ...
  :dependencies [[org.clojure/clojure "1.10.0"]
                 ...
                 [ejc-sql "0.4.1-SNAPSHOT"]]
  ...
  )
```
or if you don't want to change your `project.clj` file, you can add it globally
in `~/.lein/profiles.clj`, e.g.:
```edn
{:user {:plugins [[cider/cider-nrepl "0.25.0-alpha1"]]
        :dependencies [[ejc-sql "0.4.1-SNAPSHOT"]]}}
```
The actual version of `ejc-sql` backend in Clojars:&nbsp;
[![Clojars Project](https://clojars.org/ejc-sql/latest-version.svg)](https://clojars.org/ejc-sql)

So, when you start your project nREPL via `cider-jack-in`, you can open any SQL
file (`sql-mode` or `org-mode` buffer) and connect to the database by
`ejc-connect` as usual and it will reuse the existing nREPL.

<a id="goto-definition-results-ring"></a>
### Goto definition & results ring

In terms of `ejc-sql`, **SQL evaluation results** can be result sets, record
affected messages, SQL definition of entities or error messages.

Any SQL evaluation result saved to results ring - list of files
`ejc-sql-result-0.txt`, `ejc-sql-result-1.txt`, and so on.
They located in the `TEMP` folder, it can be customized by `ejc-results-path`.
The number of files (number of previous results) can be customized by setting
`ejc-ring-length` (10 by default).

You can see previous SQL evaluation result by <kbd>C-M-b</kbd>
(`ejc-show-prev-result`) in `*ejc-sql-output*` buffer.
To return back use <kbd>C-M-f</kbd> (`ejc-show-next-result`). This way you can
navigate through the results ring.

Since `*ejc-sql-output*` buffer contains `ejc-sql` connection information, it
makes possible to navigate through views & stored procedures code definitions.
E.g. you can require `ejc-describe-entity` for some stored procedure, then
require `ejc-describe-entity` inside `*ejc-sql-output*` for some stored
procedure, used in this (just described) procedure definition, and so on. Then
you can return to previous procedure definition by `ejc-show-prev-result`. So,
it looks like goto definition, then return back. For the purpose of convenience,
the following keybindings are provided:
* <kbd>M-.</kbd> `ejc-describe-entity`
* <kbd>M-,</kbd> `ejc-show-prev-result`

<a id="keybindings"></a>
## List of keybindings & functions

New keybindings defined in `ejc-sql-mode` minor mode:

 Keyboard shortcut   | Command                         | Description
---------------------|---------------------------------|------------------------------------------------------
 <kbd>C-c e c</kbd>  | `ejc-connect`                   | Select DB connection (configured by `ejc-create-connection`) and connect to it.
 <kbd>C-c e i</kbd>  | `ejc-connect-interactive`       | Create new connection interactively and connect to it.
 <kbd>C-c C-c</kbd>  | `ejc-eval-user-sql-at-point`    | Evaluate SQL script bounded by the `ejc-sql-separator` or/and buffer boundaries.
 <kbd>C-c C-r</kbd>  | `ejc-eval-user-sql-region`     | Evaluate region selected SQL code.
 <kbd>C-g</kbd>      | `ejc-cancel-query`              | Terminate current running query or run `keyboard-quit` if there is no running queries.
 <kbd>C-h t</kbd>    | `ejc-describe-table`            | Describe SQL table.
 <kbd>C-h d</kbd>    | `ejc-describe-entity`           | Get entity definition: show creation SQL of view, package, function, procedure or type.
 <kbd>M-.</kbd>      | `ejc-describe-entity`           | Get entity definition: show creation SQL of view, package, function, procedure or type.
 <kbd>M-,</kbd>      | `ejc-show-prev-result`          | Load previous SQL eval result in `*ejc-sql-output*` buffer.
 <kbd>C-c e up</kbd> | `ejc-show-last-result`          | Show last result.
 <kbd>C-c e t</kbd>  | `ejc-show-tables-list`          | Show tables list.
 <kbd>C-c e v</kbd>  | `ejc-show-views-list`           | Show views list.
 <kbd>C-c e p</kbd>  | `ejc-show-procedures-list`      | Show stored procedures list.
 <kbd>C-c e T</kbd>  | `ejc-show-user-types-list`      | Show user types list.
 <kbd>C-c e s</kbd>  | `ejc-strinp-sql-at-point`       | Strip SQL (trim java string tokens).
 <kbd>C-c e S</kbd>  | `ejc-dress-sql-at-point`        | Dress SQL (to copy-paste it to java code).
 <kbd>C-c e f</kbd>  | `ejc-format-sql-at-point`       | Format (pretty-print) this SQL statement.
 <kbd>C-M-b</kbd>    | `ejc-previous-sql`              | Goto previous SQL statement (or load previous SQL eval result in `*ejc-sql-output*`).
 <kbd>C-M-f</kbd>    | `ejc-next-sql`                  | Goto next SQL statement (or load next SQL eval result in `*ejc-sql-output*`).
 <kbd>C-M-S-b</kbd>  | `ejc-previous-sql`              | Select from point to previous SQL statement.
 <kbd>C-M-S-f</kbd>  | `ejc-next-sql`                  | Select from point to next SQL statement.

List of other interactive functions

 Command                            | Description
------------------------------------|------------------------------------------
 `ejc-connect`                      | Connect to database for current buffer
 `ejc-quit-connection`              | Close all database connections, quit Clojure REPL.
 `ejc-format-sql-region`            | Format (pretty-print) selected SQL snippet
 `ejc-mark-this-sql`                | Mark SQL script bounded by the `ejc-sql-separator` or/and buffer boundaries
 `ejc-show-tables-list`             | Show tables list
 `ejc-show-constraints-list`        | Show constraints list
 `ejc-open-log`                     | Open log
 `ejc-get-temp-editor-buffer`       | Create ad-hoc SQL editor buffer, use prefix arg number to get many buffers
 `ejc-print-cache`                  | Output current connection cache
 `ejc-invalidate-cache`             | Clean your current connection cache (database owners and tables list)
 `ejc-direx:pop-to-buffer`          | Create buffer with database structure tree

## Yasnippet

List of snippets:

__select__ | __where__ | __inner__
-----------|-----------|------------
__insert__ | __begin__ | __left__
__update__ | __grant__ | __right__
__delete__ | __revoke__| __alter__


## Troubleshooting

```
Error running timer ‘ac-update-greedy’: (error "Sync nREPL request timed out (op eval session...
```

Increase `nrepl-sync-request-timeout`, e.g.:

```lisp
(setq nrepl-sync-request-timeout 60)
```

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 26.
* [Leiningen](http://leiningen.org) 2.x
* [clomacs](https://github.com/clojure-emacs/clomacs)
* [clojure/java.jdbc](https://github.com/clojure/java.jdbc) 0.5.8
* [dash](https://github.com/magnars/dash.el)
* [spinner.el](https://github.com/Malabarba/spinner.el)
* [direx.el](https://github.com/m2ym/direx-el)
* [auto-complete](https://github.com/auto-complete/auto-complete) *(optional)*
* [company-mode](https://github.com/company-mode/company-mode) *(optional)*
* [company-quickhelp](https://github.com/company-mode/company-quickhelp) *(optional)*
* [flx-ido](https://github.com/lewang/flx) *(optional)*
* [yasnippet](https://github.com/joaotavora/yasnippet) *(optional)*.

## License

Copyright © 2012-2020 Kostafey <kostafey@gmail.com> and
[contributors](https://github.com/kostafey/ejc-sql/contributors)

Distributed under the General Public License 2.0+
