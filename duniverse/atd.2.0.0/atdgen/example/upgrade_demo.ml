open Printf

let old_data = {
  Format_v1.a = Some 1;
  b = true;
  c = Some 3;
  d = 4.0;
}

let print_old_data () =
  let ob = Bi_outbuf.create_channel_writer stdout in
  Format_v1.write_t ob old_data;
  Bi_outbuf.flush_channel_writer ob;
  flush stdout

let convert x =
  Format_v2.t_of_string (Format_v1.string_of_t ~len:100 x)

let print_new_data () =
  let new_data = convert old_data in
  let ob = Bi_outbuf.create_channel_writer stdout in
  Format_v2.write_t ob new_data;
  Bi_outbuf.flush_channel_writer ob;
  flush stdout

let upgrade () =
  let ib = Bi_inbuf.from_channel stdin in
  let x = Format_v2.read_t ib in
  let ob = Bi_outbuf.create_channel_writer stdout in
  Format_v2.write_t ob x;
  Bi_outbuf.flush_channel_writer ob;
  flush stdout


let usage () =
  eprintf "\
Usage: %s [old|new|up]

old    print sample data in the old format
new    print sample data in the new format
up     read data in the new format from stdin and print data in the new format
%!"
    Sys.argv.(0);
  exit 1

let main () =
  match Sys.argv with
      [| _; action |] ->
        (match action with
             "old" -> print_old_data ()
           | "new" -> print_new_data ()
           | "up" -> upgrade ()
           | _ -> usage ()
        )
    | _ -> usage ()

let () = main ()
