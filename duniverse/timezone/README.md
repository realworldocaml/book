Timezone for OCaml
===================================

Timezone handles parsing timezone data and create [Timezone.t] that
can later be used to manipulate time in
[core_kernel](https://github.com/janestreet/core_kernel) and
[core](https://github.com/janestreet/core) (using [Time] and [Time_ns] modules).

Timezone is currently only able to read the Timezone Database
provided by [IANA](https://www.iana.org/time-zones). It should work
out of the box on Linux and macOS.

## Where are the timezone data located ?

The location of the timezone files can be set using the environment
variable `TZDIR`. If not set, [Timezone] will fallback to
`/usr/share/zoneinfo/`.

## What is the local timezone ?

The local timezone can be set using the environment variable `TZ`. If
not set, [Timezone] will fallback to `/etc/localtime`.
In a JavaScript context, we automatically set the environment variable
`TZ` to `Intl.DateTimeFormat().resolvedOptions().timeZone`.

## Compatibility with JavaScript.

The [Timezone] library can be used when constructing JavaScript
applications with
[Js_of_ocaml](https://github.com/ocsigen/js_of_ocaml/).

### Node.js
[Timezone] should work out of the box when running on a Node.js
environment on Linux or macOS. The file-system is accessible in that
case.

### Web browsers
In a web browser environment, the [Timezone] library requires some
additional setup. One should generate a JavaScript file that will
embed all timezone data required by the application. This can be done
by using a tool provided by `js_of_ocaml` called `jsoo_fs`. The
generated JavaScript file will have to be included in the HTML page
before the JavaScript program itself.

For example, one can embed all timezones in `all-tz.js` with the following command:
```sh
grep -r TZif /usr/share/zoneinfo -l | xargs -n 1 -I {} echo {}:{} | xargs jsoo_fs -o all-tz.js
```



## Example

```ocaml
open Core_kernel

let now = Time_ns.now ()

let () =
  Time_ns.to_sec_string now ~zone:(Lazy.force (Timezone.local))
  |> print_endline

let () =
  Time_ns.to_sec_string now ~zone:(Timezone.find_exn "America/New_York")
  |> print_endline
```

------

Please report bugs and feature requests on
[GitHub](https://github.com/janestreet/timezone).

You can find all of Jane Street's open-source libraries on
[GitHub](https://github.com/janestreet).

Documentation can be found
[here](https://ocaml.janestreet.com/ocaml-core/latest/doc/timezone/index.html).
