# gn-data-vault

# Using

Drop into a development environment with

``` shell
$ guix shell -m manifest.scm
```

If the path is not picked up add

```
export PATH=$GUIX_ENVIRONMENT/bin:$PATH
```

Build the sources.

``` shell
$ make
```

or for a container

```shell
mkdir test
guix shell -C --network --share=/run/mysqld/ --manifest=manifest.scm
export GUILE_LOAD_PATH=.:$GUILE_LOAD_PATH
guile json-dump.scm conn.scm test/
```


### TODO

- [x] genofile reading

- [] genofile parsing to obtain strains

- [] obtain strain values for datasets with samples from genofiles

- []  lmdb dump

