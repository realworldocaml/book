type source = int * string

type bits = int

exception Unseeded_generator

exception No_default_generator

let setup_rng =
  "\nTo initialize the RNG with a default generator, and set up entropy \
   collection and periodic reseeding as a background task, do the \
   following:\
   \n  If you are using MirageOS, use the random device in config.ml: \
   `let main = Mirage.foreign \"Unikernel.Main\" (random @-> job)`, \
   and `let () = register \"my_unikernel\" [main $ default_random]`. \
   \n  If you are using Lwt, execute \
   `Mirage_crypto_rng_lwt.initialize ()` at the beginning of \
   your event loop (`Lwt_main.run`) execution. \
   \n  If you're using neither MirageOS nor lwt, there is no periodic \
   reseeding. For an initial seed from getrandom(), execute \
   `Mirage_crypto_rng_unix.initialize ()`. You can use \
   `Mirage_crypto_rng.accumulate` and `Mirage_crypto_rng.reseed` to \
   reseed the RNG manually."

let () = Printexc.register_printer (function
    | Unseeded_generator ->
      Some ("The RNG has not been seeded." ^ setup_rng)
    | No_default_generator ->
      Some ("The default generator is not yet initialized. " ^ setup_rng)
    | _ -> None)

module type Generator = sig
  type g
  val block      : int
  val create     : ?time:(unit -> int64) -> unit -> g
  val generate   : g:g -> int -> Cstruct.t
  val reseed     : g:g -> Cstruct.t -> unit
  val accumulate : g:g -> source -> [`Acc of Cstruct.t -> unit]
  val seeded     : g:g -> bool
  val pools      : int
end

type 'a generator = (module Generator with type g = 'a)
type g = Generator : ('a * bool * 'a generator) -> g

let create (type a) ?g ?seed ?(strict=false) ?time (m : a generator) =
  let module M = (val m) in
  let g = Option.value g ~default:(M.create ?time ()) in
  Option.iter (M.reseed ~g) seed;
  Generator (g, strict, m)

let _default_generator = ref None

let set_default_generator g = _default_generator := Some g

let default_generator () =
  match !_default_generator with
  | None -> raise No_default_generator
  | Some g -> g

let get = function Some g -> g | None -> default_generator ()

let generate ?(g = default_generator ()) n =
  let Generator (g, _, m) = g in let module M = (val m) in M.generate ~g n

let reseed ?(g = default_generator ()) cs =
  let Generator (g, _, m) = g in let module M = (val m) in M.reseed ~g cs

let accumulate g source =
  let Generator (g, _, m) = get g in
  let module M = (val m) in
  M.accumulate ~g source

let seeded g =
  let Generator (g, _, m) = get g in let module M = (val m) in M.seeded ~g

let block g =
  let Generator (_, _, m) = get g in let module M = (val m) in M.block

let pools g =
  let Generator (_, _, m) = get g in let module M = (val m) in M.pools

let strict g =
  let Generator (_, s, _) = get g in s
