val pp_options : string list Fmt.t

type dir = {
  test_file : string;
  target_file : string;
  expected_file : string;
  options : string list;
  dir_name : string;
  enabled_if : string list option;
}

type generator = {
  pp_expect_action : Format.formatter -> dir -> unit;
  pp_failure_action : Format.formatter -> dir -> unit;
}

val run : generator -> unit
