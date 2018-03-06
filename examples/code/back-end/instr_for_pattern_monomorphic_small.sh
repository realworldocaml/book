  $ ocamlc -dinstr pattern_monomorphic_small.ml 2>&1
  	branch L2
  L1:	acc 0
  	push
  	const 0
  	neqint
  	branchifnot L3
  	const 101
  	return 1
  L3:	const 100
  	return 1
  L2:	closure L1, 0
  	push
  	acc 0
  	makeblock 1, 0
  	pop 1
  	setglobal Pattern_monomorphic_small!
  
