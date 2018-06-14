# pgpwords

A tool to encode/decode data and numbers using the
[PGP word list](https://en.wikipedia.org/wiki/PGP_word_list)

## Examples

```
$ pgpwords -n 2018
tiger amusement
```

```
$ pgpwords -nd "tiger amusement"
2018
```

```
$ echo -n 験 | pgpwords
treadmill paramount playhouse
```

```
$ echo treadmill paramount playhouse | pgpwords -d
験
```
