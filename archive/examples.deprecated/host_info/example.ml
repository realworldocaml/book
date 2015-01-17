open Core.Std
module Shell = Core_extended.Std.Shell

type host_info =
  { hostname       : string;
    os_release     : string;
    cpu_arch       : string;
  }
with sexp

let this_machine () =
  let sh = Shell.sh_one in
  { hostname   = sh "hostname";
    os_release = sh "uname -r";
    cpu_arch   = sh "uname -p";
  }


let () =
  Sexp.output_hum stdout (sexp_of_host_info (this_machine ()));
  Out_channel.newline stdout
