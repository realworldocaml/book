QUOTA=2
args="-quota $QUOTA $@"
./inline_benchmarks_runner -matching "hash_init" $args
./inline_benchmarks_runner -matching "hash a" $args
./inline_benchmarks_runner -matching "hash b" $args
./inline_benchmarks_runner -matching "hash c__1" $args
./inline_benchmarks_runner -matching "hash c__2" $args
./inline_benchmarks_runner -matching "hash c_10" $args
./inline_benchmarks_runner -matching "hash c100" $args
./inline_benchmarks_runner -matching "hash d_10" $args
./inline_benchmarks_runner -matching "hash d100" $args
