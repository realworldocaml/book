open Expect_test_common.Std

type filename = File.Name.t

class virtual ['a] lift_filename =
  object
    method virtual filename : filename -> 'a
  end

type location = File.Location.t =
  { filename : filename
  ; line_number : int
  ; line_start : int
  ; start_pos : int
  ; end_pos : int
  }

and 'a body = 'a Expectation.Body.t =
  | Exact of string
  | Output
  | Pretty of 'a
  | Unreachable

and 'a expectation = 'a Expectation.t =
  { tag : string option
  ; body : 'a body
  ; extid_location : location
  ; body_location : location
  }

and raw = string expectation [@@deriving traverse_lift]
