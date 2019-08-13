Ellipsis lines are preserved if possible even when the output changes

```sh
$ for i in `seq 1 21`; do echo $i; done
1
2
...
10
...
19
20
```

```sh
$ for i in `seq 1 19`; do echo $i; done
1
2
...
10
...
20
```
