# Thread_pool_cpu_affinity

A very small library that defines a type for configuring how
`Thread_pool` affinitizes worker threads to CPUs.  This is a separate
library from `Thread_pool` because this only depends on `Core_kernel`,
and hence can be used in `Async_kernel`.
