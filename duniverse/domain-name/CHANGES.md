## v0.3.0 (2019-07-08)

* all optional ?back arguments are now ?rev
* compare_sub is now compare_label
* new function: equal_label : ?case_sensitive:bool -> string -> string -> bool
* new function: find_label : ?rev:bool -> 'a t -> (string -> bool) -> int option
  which searches for the predicate (3rd argument) in t (2nd arguments)

## v0.2.1 (2019-06-30)

* getter functions for labels:
  get_label : 'a t -> int -> (string, [> `Msg of string ]) result
  get_label_exn : 'a t -> int -> string
* count_labels : 'a t -> int

## v0.2.0 (2019-06-25)

* type t is now a phantom type 'a t, where 'a carries whether it is a hostname,
  a service name or a raw domain name. this lead to removal of various
  ?hostname:bool arguments
* val host : 'a t -> ([`host] t, [> `Msg of string ]) result
* analog host_exn, service, service_exn, raw
* removed is_service, is_hostname
* new submodules Host_set, Host_map, Service_set, Service_map
* new function: append : 'a t -> 'b t -> ([`raw] t, [> `Msg of string ]) result
* renamed: drop_labels{,_exn} is now drop_label{,_exn}
* renamed: prepend{,_exn} is now prepend_label{,_exn}

## 0.1.2 (2019-02-16)

* `is_service` accepts numeric service names, used for ports in TLSA records (#1 by @cfcs)
* port to dune

## 0.1.1 (2018-07-07)

* `to_string` and `to_strings` now have an optional labeled `trailing` argument
  of type bool
* support for FQDN with trailing dot: `of_string "example.com."` now returns
  `Ok`, and is equal to `of_string "example.com"`
* fix and add tests for `drop_labels` and `drop_labels_exn`, where the semantics
  of the labeled `back` argument was inversed.

## 0.1.0 (2018-06-26)

* Initial release
