open! Import
open! Sexp_pretty

let normalized_sexp t =
  let rec of_t = function
    | Normalize.Sexp (sexp, _) -> Some (of_sexp sexp)
    | Normalize.Comment _ -> None
  and of_sexp = function
    | Normalize.Atom str -> Sexp.Atom str
    | Normalize.List ts -> Sexp.List (of_t_list ts)
  and of_t_list ts = List.filter_map ts ~f:of_t in
  match of_t t with
  | Some sexp -> sexp
  | None -> assert false
;;

let normalize conf sexp =
  sexp |> sexp_to_sexp_or_comment |> Normalize.of_sexp_or_comment conf |> normalized_sexp
;;

(* Tests with atom interpretation *)

let conf = { Config.default with Config.atom_printing = Interpreted }

let test ~input:input_sexp sexp =
  [%test_result: Sexp.t] ~expect:sexp (normalize conf input_sexp)
;;

let%test_unit _ =
  let atom = Sexp.Atom "Not connected to oculus monitor" in
  test ~input:atom atom
;;

let%test_unit _ =
  test
    ~input:(Sexp.Atom "Not connected to oculus monitor (connect to monitor please)")
    (List
       [ Atom "Not connected to oculus monitor"
       ; Sexp.of_string "(connect to monitor please)"
       ])
;;

let%test_unit _ =
  test
    ~input:(Sexp.Atom "Not connected (not that you have to) to oculus monitor")
    (List
       [ Atom "Not connected"
       ; Sexp.of_string "(not that you have to)"
       ; Atom "to oculus monitor"
       ])
;;

let%test_unit _ =
  let sexp = Sexp.of_string "(this is a (bona fide) sexp)" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.of_string "(issue (error_fields (message \"A message\")(int 5)))" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.of_string "(message \"A message\")" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.Atom "\"   space   \"" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.Atom "   space   " in
  test ~input:sexp sexp
;;

(* Tests with [Escaped] *)

let conf = { Config.default with Config.atom_printing = Escaped }

let test ~input:input_sexp sexp =
  [%test_result: Sexp.t] ~expect:sexp (normalize conf input_sexp)
;;

let%test_unit _ =
  let atom = Sexp.Atom "Not connected to oculus monitor" in
  test ~input:atom atom
;;

let%test_unit _ =
  let sexp = Sexp.Atom "(this is a (bona fide) sexp)" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.Atom "(message \"A message\")" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.Atom "\"   space   \"" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.Atom "   space   " in
  test ~input:sexp sexp
;;

let%expect_test "long atoms with newlines are hard to read" =
  let s = String.concat ~sep:"\n" (List.init 10 ~f:Int.to_string) in
  print_endline (pretty_string Config.(update default ~color:false) (Atom s));
  [%expect {| "0\n1\n2\n3\n4\n5\n6\n7\n8\n9" |}];
  print_endline (Sexp.to_string_hum (Atom s));
  [%expect
    {|
     "0\
    \n1\
    \n2\
    \n3\
    \n4\
    \n5\
    \n6\
    \n7\
    \n8\
    \n9" |}]
;;

let test
      ?(sticky_comments = Sexp_pretty.Config.default.sticky_comments)
      ?(atom_printing = Sexp_pretty.Config.default.atom_printing)
      string
  =
  let lexbuf = Lexing.from_string string in
  let next () =
    Option.try_with (fun () ->
      Sexp.With_layout.Parser.sexp Sexp.With_layout.Lexer.main lexbuf)
  in
  let config =
    { (Sexp_pretty.Config.create ~color:false ~new_line_separator:true ()) with
      sticky_comments
    ; atom_printing
    }
  in
  Sexp_pretty.Sexp_with_layout.pp_formatter' ~next config Format.std_formatter
;;

let%expect_test "block comments should not have extra escape sequence at the end using \
                 pp_formatter'"
  =
  test "(this is a sexp with #| comment |# block comments)";
  [%expect {| (this is a sexp with #| comment |# block comments) |}]
;;

let%expect_test "block comments should not have escape sequence when pretty_string" =
  let s = "(this is a sexp with #| my delightful comment |# block comments)" in
  let config = Sexp_pretty.Config.create ~color:false () in
  print_endline (pretty_string config (Atom s));
  [%expect {| "(this is a sexp with #| my delightful comment |# block comments)" |}]
;;

let%expect_test "multi-lines atom before comments" =
  let string =
    {|
the_first_atom "a long string
in multiple
lines" ;; comment about the string
       ;; and this as well

(this is (a list) "another long
string" ;; comment too
        ;; another comment
)
)
|}
  in
  test string ~sticky_comments:Before;
  [%expect
    {|
    the_first_atom

    "a long string\nin multiple\nlines"

    ;; comment about the string
    ;; and this as well
    (this is
      (a list)
      ;; comment too
      ;; another comment
      "another long\nstring") |}]
;;

let%expect_test "test comments on all [sticky_comments] config" =
  let string =
    {|
(AAAAA ;; BBBBB
       ;; CCCCC
)

(XXXXX) ;; YYYYY
        ;; ZZZZZ

((iii
  (jjj kkk) ;; jkjkjk
            ;; kjkjkj
  lll))

(AAAAA #| BBBBB |#
       #| CCCCC |#
)

(XXXXX) #| YYYYY |#
        #| ZZZZZ |#

((iii
  (jjj kkk) #| jkjkjk |#
            #| kjkjkj |#
  lll))

(AAAAA #; BBBBB
       #; CCCCC
)

(XXXXX) #; YYYYY
        #; ZZZZZ

((iii
  (jjj kkk) #; jkjkjk
            #; kjkjkj
  lll))
|}
  in
  test string ~sticky_comments:Before;
  [%expect
    {|
    (;; BBBBB
     ;; CCCCC
     AAAAA)

    (XXXXX)

    ;; YYYYY
    ;; ZZZZZ
    ((
      iii
      ;; jkjkjk
      ;; kjkjkj
      (jjj kkk)
      lll))

    (AAAAA #| BBBBB |# #| CCCCC |#)

    (XXXXX)

    #| YYYYY |#
    #| ZZZZZ |#
    ((iii (jjj kkk) #| jkjkjk |# #| kjkjkj |# lll))

    (AAAAA #; BBBBB #; CCCCC)

    (XXXXX)

    #;
    YYYYY
    #;
    ZZZZZ
    ((iii (jjj kkk) #; jkjkjk #; kjkjkj lll)) |}];
  test string ~sticky_comments:Same_line;
  [%expect
    {|
    (AAAAA ;; BBBBB
           ;; CCCCC
    )

    (XXXXX)

    ;; YYYYY
    ;; ZZZZZ
    ((
      iii
      (jjj kkk) ;; jkjkjk
                ;; kjkjkj
      lll))

    (AAAAA #| BBBBB |# #| CCCCC |#)

    (XXXXX)

    #| YYYYY |#
    #| ZZZZZ |#
    ((iii (jjj kkk) #| jkjkjk |# #| kjkjkj |# lll))

    (AAAAA #; BBBBB #; CCCCC)

    (XXXXX)

    #;
    YYYYY
    #;
    ZZZZZ
    ((iii (jjj kkk) #; jkjkjk #; kjkjkj lll)) |}];
  test string ~sticky_comments:After;
  [%expect
    {|
    (AAAAA
      ;; BBBBB
      ;; CCCCC
    )

    (XXXXX)

    ;; YYYYY
    ;; ZZZZZ
    ((
      iii
      (jjj kkk)
      ;; jkjkjk
      ;; kjkjkj
      lll))

    (AAAAA #| BBBBB |# #| CCCCC |#)

    (XXXXX)

    #| YYYYY |#
    #| ZZZZZ |#
    ((iii (jjj kkk) #| jkjkjk |# #| kjkjkj |# lll))

    (AAAAA #; BBBBB #; CCCCC)

    (XXXXX)

    #;
    YYYYY
    #;
    ZZZZZ
    ((iii (jjj kkk) #; jkjkjk #; kjkjkj lll)) |}]
;;

let%expect_test "aligned data" =
  let string =
    {|
((address       (String "50 St. James Street")) ;; 1
 (url           (String http://www.JSON.org/))  ;; 2
 (comment       (String "// /* <!-- --"))       ;; 3
 ("# -- --> */" (String " "))                   ;; 4
 ("single new
line" (String "should not be aligend"))   ;; 5
 (address (String "50 St. James Street")) ;; 6
 (url     (String http://www.JSON.org/))  ;; 7
)
|}
  in
  test string ~sticky_comments:Same_line;
  [%expect
    {|
    ((address            (String "50 St. James Street")) ;; 1
     (url                (String http://www.JSON.org/)) ;; 2
     (comment            (String "// /* <!-- --")) ;; 3
     ("# -- --> */"      (String " ")) ;; 4
     ("single new\nline" (String "should not be aligend")) ;; 5
     (address            (String "50 St. James Street")) ;; 6
     (url                (String http://www.JSON.org/)) ;; 7
    ) |}];
  test string ~sticky_comments:Same_line ~atom_printing:Minimal_escaping;
  [%expect
    {|
    ((address       (String "50 St. James Street")) ;; 1
     (url           (String http://www.JSON.org/)) ;; 2
     (comment       (String "// /* <!-- --")) ;; 3
     ("# -- --> */" (String " ")) ;; 4
     ("single new
    line" (String "should not be aligend")) ;; 5
     (address (String "50 St. James Street")) ;; 6
     (url     (String http://www.JSON.org/)) ;; 7
    ) |}]
;;

let%expect_test "singleton lists" =
  let string =
    {|
(((at1 at2 at3  (elm1 elm2 elm3 ))))
(((at1 at2 at3  ((elm1 elm2 elm3)))))
(at1 at2 at3  ;; comment 1
              ;; comment 2
      (elm1 elm2 elm3))
(at1 at2 at3 (elm1 elm2 elm3) ;; comment 1
                              ;; comment 2
)
(at1 at2 at3 (elm1 elm2 elm3 ;; comment 1
                             ;; comment 2
             )
)
    |}
  in
  test string ~sticky_comments:Same_line;
  [%expect
    {|
    (((at1 at2 at3 (elm1 elm2 elm3))))

    (((at1 at2 at3 ((elm1 elm2 elm3)))))

    (at1 at2
      at3 ;; comment 1
          ;; comment 2
      (elm1 elm2 elm3))

    (at1 at2 at3
      (elm1 elm2 elm3) ;; comment 1
                       ;; comment 2
    )

    (at1 at2 at3 (
      elm1 elm2
      elm3 ;; comment 1
           ;; comment 2
    )) |}]
;;

(* Tests with [Minimal_escaping] *)

let conf =
  { (Config.create ~color:false ()) with Config.atom_printing = Minimal_escaping }
;;

let%expect_test "long atom with newlines" =
  let sexp = Sexp.Atom (String.concat ~sep:"\n" (List.init 10 ~f:Int.to_string)) in
  let pretty = pretty_string conf sexp in
  print_endline pretty;
  [%expect {|
    "0
    1
    2
    3
    4
    5
    6
    7
    8
    9" |}];
  [%test_result: Sexp.t] ~expect:sexp (Sexp.of_string pretty)
;;

let%expect_test "list with an atom having newlines" =
  let sexp = Sexp.of_string "(ABCD EFG (123 456 \"abc\ndef ghi\njkl\" 7890) HI)" in
  let pretty = pretty_string conf sexp in
  print_endline pretty;
  [%expect
    {|
    (ABCD EFG
      (123 456
        "abc
    def ghi
    jkl"
        7890)
      HI) |}];
  [%test_result: Sexp.t] ~expect:sexp (Sexp.of_string pretty)
;;

let%expect_test "list with the middle leading atom having newlines" =
  let sexp = Sexp.of_string "(ABCD \"EFG\nEEFFGG\" HIJ)" in
  let pretty = pretty_string conf sexp in
  print_endline pretty;
  [%expect {|
    (ABCD
      "EFG
    EEFFGG"
      HIJ) |}]
;;

let%expect_test "list with the last leading atom having newlines" =
  let sexp = Sexp.of_string "(ABCD EFG \"HIHI\nHIJ\")" in
  let pretty = pretty_string conf sexp in
  print_endline pretty;
  [%expect {|
    (ABCD EFG
      "HIHI
    HIJ") |}]
;;

let%expect_test "atom with characters to be escaped" =
  let sexp = Sexp.Atom "AB\bCD\tEF\nGH\011IJ\012KL\rMN\\OP" in
  let pretty = pretty_string conf sexp in
  print_endline pretty;
  [%expect {|
    "AB\bCD	EF
    GH\011IJ\012KL\rMN\\OP" |}];
  [%test_result: Sexp.t] ~expect:sexp (Sexp.of_string pretty)
;;

let%expect_test "atom with escaped characters" =
  let sexp = Sexp.Atom {|AB\bCD\tEF\nGH\011IJ\012KL\rMN\\OP\"QR|} in
  let pretty = pretty_string conf sexp in
  print_endline pretty;
  [%expect {|
  "AB\\bCD\\tEF\\nGH\\011IJ\\012KL\\rMN\\\\OP\\\"QR" |}];
  [%test_result: Sexp.t] ~expect:sexp (Sexp.of_string pretty)
;;

let%expect_test "all characters" =
  let chars = List.chunks_of Char.all ~length:8 in
  let test sexp =
    let string = pretty_string conf sexp in
    print_string string;
    Expect_test_helpers_base.require_equal
      [%here]
      (module Sexp)
      sexp
      (Sexp.of_string string)
  in
  (* grouped in atoms *)
  List.iter chars ~f:(fun list -> test (Atom (String.of_char_list list)));
  [%expect
    {xxx|
    "\000\001\002\003\004\005\006\007"
    "\b
    \011\012\r\014\015"
    "\016\017\018\019\020\021\022\023"
    "\024\025\026\027\028\029\030\031"
    " !\"#$%&'"
    "()*+,-./"
    01234567
    "89:;<=>?"
    @ABCDEFG
    HIJKLMNO
    PQRSTUVW
    "XYZ[\\]^_"
    `abcdefg
    hijklmno
    pqrstuvw
    "xyz{|}~\127"
    "\128\129\130\131\132\133\134\135"
    "\136\137\138\139\140\141\142\143"
    "\144\145\146\147\148\149\150\151"
    "\152\153\154\155\156\157\158\159"
    "\160\161\162\163\164\165\166\167"
    "\168\169\170\171\172\173\174\175"
    "\176\177\178\179\180\181\182\183"
    "\184\185\186\187\188\189\190\191"
    "\192\193\194\195\196\197\198\199"
    "\200\201\202\203\204\205\206\207"
    "\208\209\210\211\212\213\214\215"
    "\216\217\218\219\220\221\222\223"
    "\224\225\226\227\228\229\230\231"
    "\232\233\234\235\236\237\238\239"
    "\240\241\242\243\244\245\246\247"
    "\248\249\250\251\252\253\254\255" |xxx}];
  (* one atom each *)
  List.iter chars ~f:(fun list ->
    test (List (List.map list ~f:(fun char -> Sexp.Atom (String.of_char char)))));
  [%expect
    {|
    ("\000" "\001" "\002" "\003" "\004" "\005" "\006" "\007")
    ("\b" "	"
      "
    "
      "\011"
      "\012"
      "\r"
      "\014"
      "\015")
    ("\016" "\017" "\018" "\019" "\020" "\021" "\022" "\023")
    ("\024" "\025" "\026" "\027" "\028" "\029" "\030" "\031")
    (" " ! "\"" # $ % & ')
    ("(" ")" * + , - . /)
    (0 1 2 3 4 5 6 7)
    (8 9 : ";" < = > ?)
    (@ A B C D E F G)
    (H I J K L M N O)
    (P Q R S T U V W)
    (X Y Z [ \ ] ^ _)
    (` a b c d e f g)
    (h i j k l m n o)
    (p q r s t u v w)
    (x y z { | } ~ "\127")
    ("\128" "\129" "\130" "\131" "\132" "\133" "\134" "\135")
    ("\136" "\137" "\138" "\139" "\140" "\141" "\142" "\143")
    ("\144" "\145" "\146" "\147" "\148" "\149" "\150" "\151")
    ("\152" "\153" "\154" "\155" "\156" "\157" "\158" "\159")
    ("\160" "\161" "\162" "\163" "\164" "\165" "\166" "\167")
    ("\168" "\169" "\170" "\171" "\172" "\173" "\174" "\175")
    ("\176" "\177" "\178" "\179" "\180" "\181" "\182" "\183")
    ("\184" "\185" "\186" "\187" "\188" "\189" "\190" "\191")
    ("\192" "\193" "\194" "\195" "\196" "\197" "\198" "\199")
    ("\200" "\201" "\202" "\203" "\204" "\205" "\206" "\207")
    ("\208" "\209" "\210" "\211" "\212" "\213" "\214" "\215")
    ("\216" "\217" "\218" "\219" "\220" "\221" "\222" "\223")
    ("\224" "\225" "\226" "\227" "\228" "\229" "\230" "\231")
    ("\232" "\233" "\234" "\235" "\236" "\237" "\238" "\239")
    ("\240" "\241" "\242" "\243" "\244" "\245" "\246" "\247")
    ("\248" "\249" "\250" "\251" "\252" "\253" "\254" "\255") |}]
;;
