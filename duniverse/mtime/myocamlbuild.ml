open Ocamlbuild_plugin
open Command

let os = try Sys.getenv "MTIME_OS" with
| Not_found -> Ocamlbuild_pack.My_unix.run_and_read "uname -s"

let system_support_lib = match os with
| "Linux\n" -> [A "-cclib"; A "-lrt"]
| _ -> []

let lib s =
  match !Ocamlbuild_plugin.Options.ext_lib with
  | "" -> s ^ ".a"
  | x -> s ^ "." ^ x

let () =
  dispatch begin function
  | After_rules ->

      (* mtime *)

      ocaml_lib ~tag_name:"use_mtime" ~dir:"src" "src/mtime";

      (* mtime-clock-os *)

      flag_and_dep ["link"; "ocaml"; "link_mtime_clock_os_stubs"]
        (P (lib "src-clock/libmtime_clock_stubs"));

      dep ["record_mtime_clock_os_stubs"]
        [lib "src-clock/libmtime_clock_stubs"];

      flag ["library"; "ocaml"; "byte"; "record_mtime_clock_os_stubs"]
        (S ([A "-dllib"; A "-lmtime_clock_stubs"] @ system_support_lib));
      flag ["library"; "ocaml"; "record_mtime_clock_os_stubs"] (* byt + nat *)
        (S ([A "-cclib"; A "-lmtime_clock_stubs"] @ system_support_lib));

      ocaml_lib ~tag_name:"use_mtime_clock_os" ~dir:"src-clock"
        "src-clock/mtime_clock";

      flag ["link"; "ocaml"; "use_mtime_clock_os"]
        (S [A "-ccopt"; A "-Lsrc-clock"]);
  | _ -> ()
  end
