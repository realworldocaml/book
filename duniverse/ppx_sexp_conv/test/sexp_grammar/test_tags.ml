open Base

module _ = struct
  module type S = sig
    type t [@@deriving sexp_grammar]
  end

  let show_grammar (module M : S) =
    Expect_test_helpers_base.print_s ([%sexp_of: _ Sexp_grammar.t] [%sexp_grammar: M.t])
  ;;

  let%expect_test "basic" =
    show_grammar
      (module struct
        type nonrec t = (unit[@tag "key" = Atom "value"]) [@@deriving sexp_grammar]
      end);
    [%expect
      {|
      (Tagged (
        (key   key)
        (value value)
        (grammar (List Empty)))) |}]
  ;;

  let%expect_test "tag ordering" =
    show_grammar
      (module struct
        type nonrec t =
          (unit
           [@tag
             "key1" = Atom "value1";
             "key2" = Atom "value2"])
        [@@deriving sexp_grammar]
      end);
    [%expect
      {|
      (Tagged (
        (key   key1)
        (value value1)
        (grammar (
          Tagged (
            (key   key2)
            (value value2)
            (grammar (List Empty))))))) |}]
  ;;

  let%expect_test "tag idents/expressions" =
    show_grammar
      (module struct
        let k = "key"
        let v = Sexp.Atom "value"
        let kf () = k
        let vf () = v

        type nonrec t =
          (unit
           [@tag
             k = v;
             kf () = vf ()])
        [@@deriving sexp_grammar]
      end);
    [%expect
      {|
      (Tagged (
        (key   key)
        (value value)
        (grammar (
          Tagged (
            (key   key)
            (value value)
            (grammar (List Empty))))))) |}]
  ;;

  let%expect_test "tag nesting" =
    show_grammar
      (module struct
        type nonrec t' = (unit[@sexp_grammar.tag "inner" = Atom "inner value"])
        [@@deriving sexp_grammar]

        type nonrec t = (t'[@sexp_grammar.tag "outer" = Atom "outer value"])
        [@@deriving sexp_grammar]
      end);
    [%expect
      {|
      (Tagged (
        (key   outer)
        (value "outer value")
        (grammar (
          Tagged (
            (key   inner)
            (value "inner value")
            (grammar (List Empty))))))) |}]
  ;;

  let%expect_test "doc comments - variant clauses" =
    show_grammar
      (module struct
        [@@@ocaml.warning "-37"]

        (** IGNORED *)
        type t =
          | Clause0 of (unit[@tag "k0" = Atom "v0"]) (** first clause *)
          | Clause1 [@tag "k1" = Atom "v1"] (** second clause *)
        [@@deriving sexp_grammar ~tags_of_doc_comments]
        (** IGNORED *)
      end);
    [%expect
      {|
      (Variant (
        (case_sensitivity Case_sensitive_except_first_character)
        (clauses (
          (Tag (
            (key   sexp_grammar.doc_comment)
            (value " first clause ")
            (grammar (
              No_tag (
                (name Clause0)
                (clause_kind (
                  List_clause (
                    args (
                      Cons
                      (Tagged (
                        (key   k0)
                        (value v0)
                        (grammar (List Empty))))
                      Empty)))))))))
          (Tag (
            (key   sexp_grammar.doc_comment)
            (value " second clause ")
            (grammar (
              Tag (
                (key   k1)
                (value v1)
                (grammar (
                  No_tag (
                    (name        Clause1)
                    (clause_kind Atom_clause))))))))))))) |}]
  ;;

  let%expect_test "doc comments - poly variant clauses" =
    show_grammar
      (module struct
        [@@@ocaml.warning "-37"]

        (** IGNORED *)
        type t =
          ([ `Clause0 of (unit[@tag "k0" = Atom "v0"]) (** first clause *)
           | `Clause1 [@tag "k1" = Atom "v1"] (** second clause *)
           ]
           [@tag "kouter" = Atom "vouter"])
        [@@deriving sexp_grammar ~tags_of_doc_comments]
        (** IGNORED *)
      end);
    [%expect
      {|
      (Tagged (
        (key   kouter)
        (value vouter)
        (grammar (
          Variant (
            (case_sensitivity Case_sensitive)
            (clauses (
              (Tag (
                (key   sexp_grammar.doc_comment)
                (value " first clause ")
                (grammar (
                  No_tag (
                    (name Clause0)
                    (clause_kind (
                      List_clause (
                        args (
                          Cons
                          (Tagged (
                            (key   k0)
                            (value v0)
                            (grammar (List Empty))))
                          Empty)))))))))
              (Tag (
                (key   sexp_grammar.doc_comment)
                (value " second clause ")
                (grammar (
                  Tag (
                    (key   k1)
                    (value v1)
                    (grammar (
                      No_tag (
                        (name        Clause1)
                        (clause_kind Atom_clause)))))))))))))))) |}]
  ;;

  let%expect_test "doc comments - record fields" =
    show_grammar
      (module struct
        (** IGNORED *)
        type t =
          { field0 : (unit[@tag "k0" = Atom "v0"]) (** first field *)
          ; field1 : unit [@tag "k1" = Atom "v1"] (** second field *)
          }
        [@@deriving sexp_grammar ~tags_of_doc_comments]
        (** IGNORED *)
      end);
    [%expect
      {|
      (List (
        Fields (
          (allow_extra_fields false)
          (fields (
            (Tag (
              (key   sexp_grammar.doc_comment)
              (value " first field ")
              (grammar (
                No_tag (
                  (name     field0)
                  (required true)
                  (args (
                    Cons
                    (Tagged (
                      (key   k0)
                      (value v0)
                      (grammar (List Empty))))
                    Empty)))))))
            (Tag (
              (key   sexp_grammar.doc_comment)
              (value " second field ")
              (grammar (
                Tag (
                  (key   k1)
                  (value v1)
                  (grammar (
                    No_tag (
                      (name     field1)
                      (required true)
                      (args (Cons (List Empty) Empty))))))))))))))) |}]
  ;;

  let%expect_test "deriving sexp_grammar without tags_of_doc_comments" =
    show_grammar
      (module struct
        type t = { field : unit (** IGNORED *) } [@@deriving sexp_grammar]
      end);
    [%expect
      {|
      (List (
        Fields (
          (allow_extra_fields false)
          (fields ((
            No_tag (
              (name     field)
              (required true)
              (args (Cons (List Empty) Empty))))))))) |}]
  ;;

  let%expect_test "doc comments on subexpressions" =
    show_grammar
      (module struct
        [@@@ocaml.warning "-37"]

        type t = Foo of { bar : int (** inner *) } (** outer *)
        [@@deriving sexp_grammar ~tags_of_doc_comments]
      end);
    [%expect
      {|
      (Variant (
        (case_sensitivity Case_sensitive_except_first_character)
        (clauses ((
          Tag (
            (key   sexp_grammar.doc_comment)
            (value " outer ")
            (grammar (
              No_tag (
                (name Foo)
                (clause_kind (
                  List_clause (
                    args (
                      Fields (
                        (allow_extra_fields false)
                        (fields ((
                          Tag (
                            (key   sexp_grammar.doc_comment)
                            (value " inner ")
                            (grammar (
                              No_tag (
                                (name     bar)
                                (required true)
                                (args (Cons Integer Empty))))))))))))))))))))))) |}];
    show_grammar
      (module struct
        [@@@ocaml.warning "-37"]

        type t = [ `A of [ `B (** inner *) ] (** outer *) ]
        [@@deriving sexp_grammar ~tags_of_doc_comments]
      end);
    [%expect
      {|
      (Variant (
        (case_sensitivity Case_sensitive)
        (clauses ((
          Tag (
            (key   sexp_grammar.doc_comment)
            (value " outer ")
            (grammar (
              No_tag (
                (name A)
                (clause_kind (
                  List_clause (
                    args (
                      Cons
                      (Variant (
                        (case_sensitivity Case_sensitive)
                        (clauses ((
                          Tag (
                            (key   sexp_grammar.doc_comment)
                            (value " inner ")
                            (grammar (
                              No_tag (
                                (name        B)
                                (clause_kind Atom_clause))))))))))
                      Empty))))))))))))) |}];
    show_grammar
      (module struct
        [@@@ocaml.warning "-37"]

        type t = { a : [ `B of int (** inner *) ] (** outer *) }
        [@@deriving sexp_grammar ~tags_of_doc_comments]
      end);
    [%expect
      {|
      (List (
        Fields (
          (allow_extra_fields false)
          (fields ((
            Tag (
              (key   sexp_grammar.doc_comment)
              (value " outer ")
              (grammar (
                No_tag (
                  (name     a)
                  (required true)
                  (args (
                    Cons
                    (Variant (
                      (case_sensitivity Case_sensitive)
                      (clauses ((
                        Tag (
                          (key   sexp_grammar.doc_comment)
                          (value " inner ")
                          (grammar (
                            No_tag (
                              (name B)
                              (clause_kind (List_clause (args (Cons Integer Empty)))))))))))))
                    Empty)))))))))))) |}];
    show_grammar
      (module struct
        [@@@ocaml.warning "-37"]

        type t = [ `A of [ `B (** inner *) ] option (** outer *) ]
        [@@deriving sexp_grammar ~tags_of_doc_comments]
      end);
    [%expect
      {|
      (Variant (
        (case_sensitivity Case_sensitive)
        (clauses ((
          Tag (
            (key   sexp_grammar.doc_comment)
            (value " outer ")
            (grammar (
              No_tag (
                (name A)
                (clause_kind (
                  List_clause (
                    args (
                      Cons
                      (Option (
                        Variant (
                          (case_sensitivity Case_sensitive)
                          (clauses ((
                            Tag (
                              (key   sexp_grammar.doc_comment)
                              (value " inner ")
                              (grammar (
                                No_tag (
                                  (name        B)
                                  (clause_kind Atom_clause)))))))))))
                      Empty))))))))))))) |}]
  ;;
end
