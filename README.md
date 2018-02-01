# ASPico

## What is ASPico

This is an experimental project to create a tiny ASP (Affiliate Service Provider).

## APIs

### Create new affiliate

```bash
$ curl -v -X POST -H "Content-Type: application/json" -d '
  {"partner": "partner_id", "advertizer": "advertizer_id", "product": "product_id", "redirectTo": "redirect_url"}
' 'http://localhost:8081/v0/affiliate'
```

### Start tracking

```bash
$ curl -v -X GET -H "Content-Type: application/json" "http://localhost:8081/v0/track/${affiliate_id}"
```

If a cookie value named "aspico-aff" has already exists, its value is used.

### Save conversion

```bash
$ curl -v -X GET --cookie "aspico-aff=${affiliate_id}" "http://localhost:8081/v0/cv?cid=${conversion_id}"
```

## Environment variables

See `src/ASPico/Config.hs`.
Note that `ASPICO_PUSH_URL` is called on conversion with parameters like:

```
{"partner":"partner_id","advertizer":"advertizer_id","product":"10004","conversion":"test-cv3","created":"2017-04-20T07:07:07.429143Z"}
```

## Steps to setup postgres

Use the following steps to setup postgres and set it up to be used with ASPico.

```sh
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
