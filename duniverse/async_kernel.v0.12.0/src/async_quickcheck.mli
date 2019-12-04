open! Core_kernel
open Async_quickcheck_intf
module Generator = Quickcheck.Generator
module Observer = Quickcheck.Observer
module Shrinker = Quickcheck.Shrinker
include Quickcheck_async_configured
module Configure (Config : Quickcheck.Quickcheck_config) : Quickcheck_async_configured
