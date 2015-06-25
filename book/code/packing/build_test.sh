$ corebuild test.inferred.mli test.cmi
$ cat _build/test.inferred.mli
val v : string
val w : int
$ ocamlobjinfo _build/test.cmi
File _build/test.cmi
Unit name: Test
Interfaces imported:
	906fc1b74451f0c24ceaa085e0f26e5f	Test
	36b5bc8227dc9914c6d9fd9bdcfadb45	Pervasives
	25f4b4e10ec64c56b2987f5900045fec	X
