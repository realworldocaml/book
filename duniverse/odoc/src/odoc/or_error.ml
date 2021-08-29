type ('a, 'e) result = ('a, 'e) Result.result =
  | Ok of 'a
  | Error of 'e

type msg = [ `Msg of string ]

let (>>=) r f =
  match r with
  | Ok v -> f v
  | Error _ as e -> e
