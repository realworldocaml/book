type t =
  { last_trading : Week_date.Spec.t;
    first_notice : Week_date.Spec.t option;
    first_notice_exceptions : Date.t Year_month.Map.t
      with default(Year_month.Map.empty);
    offset       : Week_date.Offset.t;
    (* n > 0 *)
    new_contract_expires_in_n_months : int
  }
[@@deriving sexp, compare]

module M = struct
  type t = { x: int }
  [@@deriving sexp]
end
