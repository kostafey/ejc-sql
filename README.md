# ejc-sql

The project is still in deep alpha...

The idea is to use clojure jdbc lib to eval sql scripts from emacs.

## Installation

The configuration of ejs-sql might looks like this:

```lisp
; Append ejs-sql to `load-path':
(defvar site-lisp-path "~/.emacs.d/")
(add-to-list 
 'load-path 
 (expand-file-name "ejc-sql/src/ejc_sql/" site-lisp-path))

(require 'ejc-sql)

; Create your database connection configuration:
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

## Configuration

```lisp
; Some keybindings - modify this on your taste:
(global-set-key (kbd "C-x <up>") 'ejc-show-last-result)
(global-set-key (kbd "C-x C-s") 'ejc-switch-to-sql-editor-buffer)
```

## Usage

New keybindings added to `sql-mode-map`:

* <kbd>C-c C-c</kbd> `ejc-eval-user-sql-at-point`
* <kbd>C-x t</kbd> `ejc-toggle-popup-results-buffer`
* <kbd>C-h t</kbd> `ejc-describe-table`

Some usage remarks:

* Using ejc-sql reqires nrepl process is running, so execution
`ejc-ensure-nrepl-runnig` ensures this.
* Run to connect `(ejc-connect "my-db-connection")`
or `M-x ejc-connect <RET> my-db-connection <RET>`
* `ejc-toggle-popup-results-buffer` -- Swithes between auto hidding results
buffer, or not.
* `ejc-eval-user-sql-at-point` -- Evaluate SQL bounded by the
`ejc-sql-separator` or/and buffer boundaries.

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 24.
* [Leiningen](http://leiningen.org) 2.x
* [nrepl.el](https://github.com/kingtim/nrepl.el)
* [clomacs](https://github.com/kostafey/clomacs)
* [popwin-el](https://github.com/m2ym/popwin-el)
* [auto-complete](https://github.com/auto-complete/auto-complete)

## License

Copyright © 2012-2013 kostafey <kostafey@gmail.com>

Distributed under the General Public License 2.0+

The included clojure.java.jdbc is distributed under the 
Eclipse Public License 1.0
