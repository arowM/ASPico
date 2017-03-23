# ASPico

## What is ASPico

This is an experimental project to create a tiny ASP (Affiliate Service Provider).

## Features

TODO

## Steps to install postgres

Use the following steps to install postgres and set it up to be used with ASPico.

```sh
# install postgres

# become postgres user
$ sudo -i -u postgres

# create the aspico user for developement and testing
$ sudo -u postgres -- psql --command "CREATE ROLE aspico NOSUPERUSER NOCREATEDB NOCREATEROLE INHERIT LOGIN ENCRYPTED PASSWORD '3pUiRmS2Rv6f28uW'"

# create the aspico database for developement
$ sudo -u postgres -- createdb aspico

# create the aspico database for testing
$ sudo -u postgres -- createdb aspico_test

# grant access to both dev db and testing db for aspico user
$ sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE aspico TO aspico"
$ sudo -u postgres -- psql --command "GRANT ALL PRIVILEGES ON DATABASE aspico_test TO aspico"

# restart postgres service
$ sudo service postgresql restart

# as normal user, try accessing database
$ psql -U aspico -d aspico -h 127.0.0.1
```

## Step to install

### Install `stack`

Follow instructions [here](https://github.com/commercialhaskell/stack#how-to-install).

### Build

```sh
$ make build
```

### Run on command line

```sh
$ make run
```

### Other make target

Look in the `Makefile` for other targets to run.
