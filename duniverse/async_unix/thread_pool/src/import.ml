open! Core
module Time_ns = Time_ns_unix
include Int.Replace_polymorphic_compare

let sec = Time_ns.Span.of_sec
