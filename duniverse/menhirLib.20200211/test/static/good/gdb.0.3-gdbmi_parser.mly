(* Original file: gdb.0.3/ocaml-gdb-0.3/src/gdbmi_parser.mly *)
/*
  GDB/MI parser
*/

%{
  open! Gdbmi_types
%}

%token <string> IDENT TOKEN STRING
%token LCURLY RCURLY LBRACKET RBRACKET COMMA EOF
%token PROMPT MINUS CARET ASTERISK PLUS EQUAL TILDE AT AMPERSAND

%start <Gdbmi_types.output_record> output
%start <Gdbmi_types.input_output> input_output

%%

input_output:
| PROMPT EOF { Prompt }
| input_prefix { Input $1 }
| output { Output $1 }

input_prefix:
| token=TOKEN? MINUS { MI token }
| IDENT { CLI }

output: output_record EOF { $1 }

output_record:
| typ=stream_type s=STRING { Stream (typ, s) }
| token=TOKEN? CARET state=IDENT r=list(preceded(COMMA,result))
  {
    let r = match state with
    | "done"
    | "running" -> Done r
    | "exit" -> Exit
    | "connected" -> Connected
    | "error" ->
      let k n = try match List.assoc n r with String s -> Some s | _ -> None with Not_found -> None in
      let msg = match k "msg" with Some s -> s | None -> "" in
      OpError (msg, k "code")
    | s -> failwith ("bad result type : " ^ s)
    in
    Result (token, r)
  }
| token=TOKEN? typ=async_type cls=IDENT r=list(preceded(COMMA,result)) { Async (token,typ,cls,r) }

stream_type:
| TILDE     { Console }
| AT        { Target }
| AMPERSAND { Log }

async_type:
| ASTERISK { Exec }
| PLUS { Status }
| EQUAL { Notify }

result: n=IDENT EQUAL v=value { (n, v) }
value:
| s=STRING { String s }
| LCURLY l=separated_list(COMMA,result) RCURLY { Tuple l }
| LBRACKET l=separated_list(COMMA,result) RBRACKET { List l }
| LBRACKET l=separated_nonempty_list(COMMA,value) RBRACKET { Values l }
