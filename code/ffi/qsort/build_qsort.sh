jbuilder build qsort.exe
cat input.txt
./_build/default/qsort.exe < input.txt
corebuild -pkg ctypes.foreign qsort.inferred.mli
cp _build/qsort.inferred.mli qsort.mli
