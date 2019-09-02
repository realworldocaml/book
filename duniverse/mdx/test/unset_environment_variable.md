Environment variables can be blacklisted in an shell and bash blocks.

```sh
  $ echo $HOME
  ...
```

```sh unset-HOME
  $ echo $HOME
  
```

By default, the variable INSIDE_DUNE is blacklisted, in order to keep the wanted outputs from dune related commands.
