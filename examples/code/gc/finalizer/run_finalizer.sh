  $ jbuilder build finalizer.exe
        ocamlc .finalizer.eobjs/finalizer.{cmi,cmo,cmt}
  File "finalizer.ml", line 16, characters 23-36:
  Warning 3: deprecated: Core.String.create
  [since 2017-10] Use [Bytes.create] instead
      ocamlopt .finalizer.eobjs/finalizer.{cmx,o}
  File "finalizer.ml", line 16, characters 23-36:
  Warning 3: deprecated: Core.String.create
  [since 2017-10] Use [Bytes.create] instead
  $ ./_build/default/finalizer.exe
         immediate int: FAIL
       immediate float: FAIL
        allocated bool: FAIL
      allocated record: OK
      allocated string: OK
     allocated variant: OK
