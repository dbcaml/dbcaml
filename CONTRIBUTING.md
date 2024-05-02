# Contributing to DBCaml

*This file is under development and will lack information. More to come *

Thanks for taking the time to contribute to DBCaml ✨ All contributions are
welcomed! This includes:

* PRs with bug fixes or features
* Examples
* Doc fixes
* Bug reports
* Links blogs featuring DBCaml
* Links to projects using DBCaml that can serve as large examples
* Links to libraries that can be used with DBCaml

More information on how DBCaml work exist at [the documentation](https://dbca.ml)

## Installing from Sources

To install DBCaml from sources, make sure to include all its dependencies:

```sh
```

You can run builds with:

```sh
; dune build
```

You can run all tests with

```sh
; dune test
```

# Local development

To help with testing locally do it exist a `docker-compose.yaml` file in examples, This file contains all the database currently in use or planned to be used. 
Running  `docker compose up` should setup the database for you, migrate and add data, and expose it. The database urls for each database are:

- Postgres:postgresql://postgres:mysecretpassword@localhost:6432/development
- MariaDB: mariadb://root:password@localhost:3307/development 
- MySQL: mysql://root:password@localhost:3306/development

# Building a driver

A driver within DBCaml is the thing who actually talking to the database, the main DBCaml is just a interface between the user and the database. The main package do contact the driver to ask it for executing jobs.
The way for the main package to interact with the driver is that the driver should export a module that hold the type `Driver.t`. 

[Example driver](./dbcaml_driver_postgres)

## Purpose of a Driver

The purpose of a driver is to bridge DBCaml with a database and with that also implementing security such as TLS and escaping queries.
When a driver is being developed is it important to have a extra look at the security.

# Tests
TBA

