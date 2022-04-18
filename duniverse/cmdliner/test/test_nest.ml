
open Cmdliner

let kind =
  let doc = "Kind of entity" in
  Arg.(value & opt (some string) None & info ["k";"kind"] ~doc)

let speed =
  let doc = "Movement $(docv) in m/s" in
  Arg.(value & opt int 2 & info ["speed"] ~doc ~docv:"SPEED")

let birds =
  let bird =
    let doc = "Use $(docv) specie." in
    Arg.(value & pos 0 string "pigeon" & info [] ~doc ~docv:"BIRD")
  in
  let fly =
    let info = Cmd.info "fly" ~doc:"Fly birds." in
    Cmd.v info Term.(const (fun n v -> ()) $ bird $ speed)
  in
  let land' =
    let info = Cmd.info "land" ~doc:"Land birds." in
    Cmd.v info Term.(const (fun n -> ()) $ bird)
  in
  let info = Cmd.info "birds" ~doc:"Operate on birds." in
  Cmd.group ~default:Term.(const (fun k -> ()) $ kind) info [fly; land']

let mammals =
  let man_xrefs = [`Main; `Cmd "birds" ] in
  let info = Cmd.info "mammals" ~doc:"Operate on mammals." ~man_xrefs in
  Cmd.v info Term.(const (fun () -> ()) $ const ())

let fishs =
  let name' =
    let doc = "Use fish named $(docv)." in
    Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"NAME")
  in
  let info = Cmd.info "fishs" ~doc:"Operate on fishs." in
  Cmd.v info Term.(const (fun n -> ()) $ name')

let camels =
  let herd =
    let doc = "Find in herd $(docv)." and docv = "HERD" in
    let deprecated = "deprecated, herds are ignored." in
    Arg.(value & pos 0 (some string) None & info [] ~deprecated ~doc ~docv)
  in
  let bactrian =
    let deprecated = "deprecated, use nothing instead." in
    let doc = "Specify a bactrian camel." in
    let env = Cmd.Env.info "BACTRIAN" ~deprecated in
    Arg.(value & flag & info ["bactrian"; "b"] ~deprecated ~env ~doc)
  in
  let deprecated = "deprecated, use 'mammals' instead."
  in
  let info = Cmd.info "camels" ~deprecated ~doc:"Operate on camels." in
  Cmd.v info Term.(const (fun n h -> ()) $ bactrian $ herd)

let cmd =
  let info = Cmd.info "test_nest" ~version:"X.Y.Z" in
  Cmd.group info [birds; mammals; fishs; camels]

let () = exit (Cmd.eval cmd)
