  $ cat A.ml
  let v = "hello"
  $ cat B.ml
  let w = 42
  $ cat _tags
  <*.cmx> and not "X.cmx": for-pack(X)
  $ cat X.mlpack
  A
  B
