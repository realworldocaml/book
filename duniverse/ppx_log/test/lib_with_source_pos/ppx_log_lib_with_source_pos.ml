open! Core
open! Async

let log_global_info msg = [%log.global.info msg]
let log_info log msg = [%log.info log msg]
