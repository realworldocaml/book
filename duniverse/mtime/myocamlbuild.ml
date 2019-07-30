
open Ocamlbuild_plugin
open Command

let os = Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let system_support_lib = match os with
| "Linux\n" -> [A "-cclib"; A "-lrt"]
| _ -> []

let js_rule () =
  let dep = "%.byte" in
  let prod = "%.js" in
  let f env _ =
    let dep = env dep in
    let prod = env prod in
    let tags = tags_of_pathname prod ++ "js_of_ocaml" in
    Cmd (S [A "js_of_ocaml"; T tags; A "-o";
            Px prod; P dep])
  in
  rule "js_of_ocaml: .byte -> .js" ~dep ~prod f

let () =
  dispatch begin function
  | After_rules ->
      js_rule ();

      (* mtime *)

      ocaml_lib ~tag_name:"use_mtime" ~dir:"src" "src/mtime";

      (* mtime-clock-os *)

      dep ["record_mtime_clock_os_stubs"] ["src-os/libmtime_clock_stubs.a"];
      flag_and_dep
        ["link"; "ocaml"; "link_mtime_clock_os_stubs"]
        (P "src-os/libmtime_clock_stubs.a");
      flag ["library"; "ocaml"; "byte"; "record_mtime_clock_os_stubs"]
        (S ([A "-dllib"; A "-lmtime_clock_stubs"] @ system_support_lib));
      flag ["library"; "ocaml"; (* byte and native *)
            "record_mtime_clock_os_stubs"]
        (S ([A "-cclib"; A "-lmtime_clock_stubs"] @ system_support_lib));

      ocaml_lib ~tag_name:"use_mtime_clock_os"
        ~dir:"src-os" "src-os/mtime_clock";
      flag ["link"; "ocaml"; "use_mtime_clock_os"]
        (S [A "-ccopt"; A "-Lsrc-os"]);

      (* mtime-jsoo *)
      ocaml_lib ~tag_name:"use_mtime_clock_jsoo" ~dir:"src-jsoo"
        "src-jsoo/mtime_clock";

  | _ -> ()
  end
