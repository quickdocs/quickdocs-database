# Quickdocs-Database

## Installation

```
$ git clone https://github.com/quickdocs/quickdocs-database
$ cd quickdocs-database
$ mysql -uroot -e 'CREATE DATABASE quickdocs DEFAULT CHARACTER SET utf8'
$ mysql -uroot quickdocs-database < db/schema.sql
```

```common-lisp
(ql:quickload :quickdocs-database)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
