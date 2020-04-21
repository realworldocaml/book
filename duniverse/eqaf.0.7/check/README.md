# Check tool - a small benchmark tool

`eqaf` comes with a small benchmark tool to record time/tick spend by our functions:
- `equal`
- `compare`
- `find_uint8`
- `exists_uint8`

This README.md wants to explain into details this tool.

## Some problems

Try to record time spend is hard. Indeed, the operating system or, at least, the
CPU can disturb this specific sample. Of course, into a virtualized
operating-system, it's more difficult to rely on this sample. At least,
`check.exe` should be executed into a bare-metal operating system.

By this fact, when we try to record time/tick, we have some noise. It's easily
understable by this simple code (assume a `Clock.now ()` function which gives
you the current time).

```ocaml
let bench f =
  let a = Clock.now () in
  f () ;
  let b = Clock.now () in
  Format.printf "%Ld ns.\n%!" (Int64.sub b a)
```

If you run `bench` two times, results are surely differents. The difference is
small but enough to not be able to assert an equality. By this fact, try to
check a predicate such as the _constant-time_ of our function is not so easy
than that.

**NOTE**: when we talk about _constant-time_, it's not an algorithmic
_constant-time_ as we can believe. Currently, our equal function respects 2
predicates (and we will fold them into one term, _constant-time_):
- for 2 strings where lengths differs, `equal` must spend `B` ns where `B` is a
  real constant
- for 2 strings where lengths are equal but contents __can__ differs (or not),
  `equal` must spend `A * L + B` where `L` is a the length of input strings and
  `A` and `B` are real constants.

Then, the first predicate should be a subset of the second where `L = 0`.

## Metrics

Currently, `check` relies (when you compile it into a Linux operating-system) on
ticks with the _Time Stamp Counter_ instead to use `clock_gettime`. For MacOS or
Windows, we use `clock_gettime` or something equivalent. It's a special ASM
instruction. So, we possibly not handle your architecture.

In fact, `RDTSC` is more reliable than `clock_gettime`.

## Linear regresssion to infer `A * L + B`

As we said, samples can be disturbed. So we are not able to just compare samples
and see that they are equal or not. We must infer our equation `A * L + B` (eg.
our _constant-time_ predicate). Our experience is done as follow:
1) we infer our equation `A1 * L + B1` when we compare 2 strictly equal strings
2) we infer our equation `A2 * L + B2` when we compare 2 strictly different
strings

Even if a computation of `Eqaf.equal` will return expected result (see _fuzzer_
and basic tests):
1) The function always returns `true`
2) The function always returns `false`

We want to check that `A1 = A2` and `B1 = B2`, or, in other words, we infer the
same equation independently than inputs (different or not) - if we check that
time spent by `Eqaf.equal` does not depend on inputs. At the end, this check
means that `Eqaf.equal` does not leak any information by the _time side
channel_.

To be able to infer our equation, we use a linear regression: 1) we follow a
sequence to execute our function `R0 = 1`, `Rn = max (R(n-1) * 1.01, R(n-1) +
1)` when `R` is how many times we execute our function and `n` is our iteration
Arbitrary, we do 750 iterations 2) For each iteration, we record our time metric
as follow: `samples.(n).(0) <- tick` `samples.(n).(1) <- R` 3) At the end, from
these samples, we can infer by a linear regression our equation. The resulted
coefficient of determination should be higher than 0.95. It tells to us that the
infered equation is good enough.

## A counter example

However, we should show a counter example of our results and we can do that with
`String.equal`. So we redo the experience with `String.equal` and we should have
an inequality betwen our equations.

**NOTE**: If 2 strings are physically equal, `String.equal` does not introspect
contents and it returns directly `true`. So we ensure that strings can be equal
(and they are) but they are not physically equal.

## Predictible results

When we generate different strings, we do that randomly. The first diff
character can be at the beginning of the string or at the end. Because of that,
time spent by `String.equal` is not very predictable - it depends, as we want to
solve, from contents. By this way, `check.exe` use a constant seed to generate
strings to be more predictable about results.

## How to compare our equations

Come back earlier, we want to check that our first equation when inputs are
equal is the same than our equation when inputs are not equal. However, due to
our problems (noise on our samples), we can not strictly assert that they are
equal. However, we can infer the difference between them with 2 techniques:
1) CCEA technique
2) SPSS technique

Both are explained into `check.ml` and they are not really formalized as we
expect. However, they give to us a way to compare our results. From them, it
comes a _coefficient_ which should be between `30.0` and `30.0`. Of course, this
segment is arbitrary but it's far from what we can get when we do the same
process with `String.equal`.

So if we don't have a good _coefficient_ with the CCEA technique, we restart the
process with the SPSS technique as the final result.

## Tries

At the end, if we have a _good_ final coefficient, we can say that our function
respects our predicate. If the coefficient of the determination is not good
enough (`<= 0.95`), we restart the process. If we don't have a good final
_coefficient_, we restart the process.

At least, we give 20 chances to our process to check our predicate.
