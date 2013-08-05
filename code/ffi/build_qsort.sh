corebuild -pkg ctypes.foreign qsort.native
cat input.txt
./qsort.native < input.txt
corebuild -pkg ctypes.foreign qsort.inferred.mli
cp _build/qsort.inferred.mli qsort.mli
