## 0.2.0 (2021-08-04)

* 32 bit compatibility:
  * Provide of_us_64, of_ms_64, of_sec_64 that take an int64 as input
  * Provide to_us_64, to_ms_64, to_sec_64 that produce an int64 as output
* Revise Duration.pp to print in a more concise way

## 0.1.3 (2019-10-26)

* Duration.pp: don't emit trailing space

## 0.1.2 (2019-02-16)

* move build system to dune

## 0.1.1 (2017-11-18)

* test 4.05 and 4.06
* alcotest 0.8.1 compatibility (#2 by @yomimono)
* Don't ship with -warn-error +A, but use it in `./build`

## 0.1.0 (2016-07-28)

* initial
