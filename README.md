# ejc-sql

The project is still in deep alpha...

**ejc-sql** provides a simple way to interact with database via java/clojure
  libs to run SQL/JPQL scripts from emacs:

* SQL support by clojure jdbc lib.
* JPQL support by OpenJPA (experimental).
* HQL is planning...

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

; Create your JPA configuration:
(setq my-jpa-connection
  (make-ejc-jpa
       ; persistence-unit tag, name attrubute
       :connection-name    "connectionName"
       ; path to META-INF/persistence.xml file
       :persistent-xml-url "/home/user/workspace/project/src/java/"
       ; path to domain classes
       :domain-objects-url "/home/user/workspace/project/classes/"
       :jdbc-driver-url    (concat "/home/user/lib/"
                                   "mysql-connector-java-3.1.13-bin.jar")))
```

## Usage

First of all, run to connect `(ejc-connect "my-db-connection")` or `M-x
ejc-connect <RET> my-db-connection <RET>`

New keybindings added to `sql-mode-map`:

 Key                  | Command                            | Description
----------------------|------------------------------------|------------------------------------------------------
 <kbd>`C-c C-c`</kbd> | `ejc-eval-user-sql-at-point`       | Evaluate SQL/JPQL script bounded by the `ejc-sql-separator` or/and buffer boundaries.
 <kbd>C-x t</kbd>     | `ejc-toggle-popup-results-buffer`  | Swithes between auto hidding results buffer, or not.
 <kbd>C-h t</kbd>     | `ejc-describe-table`               | Describe SQL table.

Some handy interactive functions:

* `ejc-show-last-result`
* `ejc-switch-to-sql-editor-buffer`

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 24.
* [Leiningen](http://leiningen.org) 2.x
* [clomacs](https://github.com/clojure-emacs/clomacs)
* [clojure/java.jdbc](https://github.com/clojure/java.jdbc) 0.3.0
* [OpenJPA](http://openjpa.apache.org/) 2.2.2
* [popwin-el](https://github.com/m2ym/popwin-el)
* [auto-complete](https://github.com/auto-complete/auto-complete)

## License

Copyright © 2012-2014 Kostafey <kostafey@gmail.com>

Distributed under the General Public License 2.0+
