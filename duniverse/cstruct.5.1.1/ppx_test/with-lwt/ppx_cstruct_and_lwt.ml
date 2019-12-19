[%%cstruct type foo = {
  magic: uint8_t [@len 16];
}[@@little_endian]]

[%%cenum
type foo64 =
  | ONE64
  | TWO64
  | THREE64
  [@@uint64_t]
]

let%lwt foo = Lwt.return ()

