Support for heredoc syntax:

```sh
$ cat <<EOF \
> hello\
> world\
> EOF
hello
world
$ echo foo
foo
```

And

```sh
$ cat <<EOF > foo \
> hello\
> world\
> EOF
$ cat foo
hello
world
$ echo foo
foo
```
