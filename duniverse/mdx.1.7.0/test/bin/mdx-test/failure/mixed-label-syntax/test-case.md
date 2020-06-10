You cannot mix legacy labels with HTML comment labels for a single block

<!-- $MDX set-SOME_VAR="a" -->
```sh set-SOME_OTHER_VAR="b"
echo $SOME_VAR
a
echo $SOME_OTHER_VAR
b
```
