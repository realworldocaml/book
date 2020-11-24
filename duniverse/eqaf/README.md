# Eq(af) - Constant time equal function

From some crypto libraries like [digestif][digestif], it needed to have a
constant time equal function to avoid timing attacks. To avoid replication of
code and ensure maintainability of this kind of function, we decide to provide a
little package which implements `equal` function on `string`.

This package, if `cstruct` or `base-bigarray` is available, will make this
`equal` function for them too (as `eqaf.cstruct` and `eqaf.bigarray`).

## Check tool

The main purpose of `eqaf` is to test the constant time execution of the equal
function. About that, the distribution provide a `check` tool which will compute
several times how many times we need to execute the equal function on different
inputs and equal inputs.

Then, by a linear regression, we compare results and expect that we did not have
any difference: the regression coefficient should be close to `0.0`.

You can test `eqaf` with this:

```sh
$ dune exec check/check.exe
```

This tool does not have any dependencies to be sure that `eqaf** is
self-contained.

### Q/A

**Q** How to update `eqaf` implementation?

**A** `eqaf` is fragile where the most important assumption is times needed to
compute `equal`. So `eqaf` provides the `check` tool but results from it can be
disturb by side-channel (like hypervisor). In a bare-metal environment, `check`
strictly works and should return `0`.

**Q** `eqaf` is slower than `String.compare`, it's possible to optimize it?

**A** The final goal of `eqaf` is to provide a _safe_ equal function. Speed is
clearly not what we want where we prefer to provide an implementation which does
not leak informations like: where is the first byte which differs between `a`
and `b`.

**Q** Which attack `eqaf` prevents?

**A** `eqaf` provide an equal function to avoid a timing attack. Most of equal
or compare functions (like `String.compare`) leave at the first byte which
differs. A possible attack is to see how long we need to compare two values,
like an user input and a password.

Logically, the longer this time is, the more user input is the password. So when
we need to compare sensible values (like hashes), we should use something like
`eqaf`. The distribution provides an example of this attack:

```sh
$ dune exec attack/attack.exe
Random: [|218;243;59;121;8;57;151;218;212;91;181;41;|].
471cd8bc03992a31f8f0f0c55e9e477d
471cd8bc03992a31f8f0f0c55e9e477d
```

The first value is the hash, the second is what we found just by an
introspection of time needed by our `equal` function.

**Q** `eqaf` provides only equal function on `string`?

**A** The first implementation use `string`, then, we copy/paste the code with
`bigarray` and provide it only if `base-bigarray` is available. Finally, we
provide an `equal` function for `cstruct` only if this package is available.

So, it's not only about `string` but for some others data-structures.

**Q** Why we need to do a linear regression to check assumptions on `eqaf`?

**A** As we said, times are noisy by several side-computation (hypervisor,
kernel, butterfly...). So, if we record two times how long we spend to compute
`equal`, we will have 2 different values - close each others but different.

So we need to have a bunch of samples and do an analyze on them to get an
approximation. From that, we do 2 analyzes:
- get the approximation where we compare 2 same values
- get the approximation where we compare 2 different values

From these results, we need to do an other analyze on these approximations to
see if they are close each others or not. In the case of `eqaf`, it should be
the case (and if it is, that means `eqaf` does not leak a time information
according inputs). In the case of `String.compare`, we should have a big
difference - and confirm expected behaviors.

[digestif]: https://github.com/mirage/digestif.git
