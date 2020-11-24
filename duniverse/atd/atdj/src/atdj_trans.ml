open Atd.Import
open Atdj_names
open Atdj_env
open Atdj_util
(* Calculate the JSON representation of an ATD type.
 *
 * Values of sum types t are encoded as either Strings or two-element
 * JSONArrays, depending upon the arity of the particular constructor.
 * A nullary constructor C is denoted by the String "C", whilst
 * an application of a unary constructor C to an ATD value v is denoted by the
 * JSONArray ["C", <v>], where <v> is the JSON representation of v.
 *
 * Option types other than in optional fields (e.g. '?foo: int option')
 * are not supported.
*)
let json_of_atd env atd_ty =
  let atd_ty = norm_ty ~unwrap_option:true env atd_ty in
  match atd_ty with
  | Sum    _ -> "Object" (* Either a String or a two element JSONArray *)
  | Record _ -> "JSONObject"
  | List   _ -> "JSONArray"
  | Name (_, (_, ty, _), _) ->
      (match ty with
       | "bool"   -> "boolean"
       | "int"    -> "int"
       | "float"  -> "double"
       | "string" -> "String"
       | _        -> type_not_supported atd_ty
      )
  | x -> type_not_supported x

(* Calculate the method name required to extract the JSON representation of an
 * ATD value from either a JSONObject or a JSONArray ("get", "opt",
 * "getInt", "optInt", ...)
*)
let get env atd_ty opt =
  let atd_ty = norm_ty ~unwrap_option:true env atd_ty in
  let prefix = if opt then "opt" else "get" in
  let suffix =
    match atd_ty with
    | Sum _ -> ""
    | _ -> String.capitalize_ascii (json_of_atd env atd_ty) in
  prefix ^ suffix

let extract_from_edgy_brackets s =
  Re.Str.global_replace
    (Re.Str.regexp "^[^<]*<\\|>[^>]*$") "" s
(*
extract_from_edgy_brackets "ab<cd<e>>f";;
- : string = "cd<e>"
*)

(* Assignment with translation.  Suppose that atd_ty is an ATD type, with
 * corresponding Java and (Javafied) JSON types java_ty and json_ty. Then this
 * function assigns to a variable `dst' of type java_ty from a variable `src' of
 * type `json_ty'.
*)
let rec assign env opt_dst src java_ty atd_ty indent =
  let atd_ty = norm_ty env atd_ty in
  match opt_dst with
  | None ->
      (match atd_ty with
       | Sum _ ->
           sprintf "new %s(%s)" java_ty src
       | Record _ ->
           sprintf "new %s(%s)" java_ty src
       | Name (_, (_, ty, _), _) ->
           (match ty with
            | "bool" | "int" | "float" | "string" -> src
            | _  -> type_not_supported atd_ty
           )
       | x -> type_not_supported x
      )
  | Some dst ->
      (match atd_ty with
       | Sum _ ->
           sprintf "%s%s = new %s(%s);\n" indent dst java_ty src
       | Record _ ->
           sprintf "%s%s = new %s(%s);\n" indent dst java_ty src
       | List (_, sub_ty, _) ->
           let java_sub_ty = (*ahem*) extract_from_edgy_brackets java_ty in
           let sub_expr = assign env None "_tmp" java_sub_ty sub_ty "" in

           sprintf "%s%s = new %s();\n" indent dst java_ty
           ^ sprintf "%sfor (int _i = 0; _i < %s.length(); ++_i) {\n" indent src

           ^ sprintf "%s  %s _tmp = %s.%s(_i);\n" indent
             (json_of_atd env sub_ty) src (get env sub_ty false)

           ^ sprintf "%s  %s.add(%s);\n" indent
             dst sub_expr
           ^ sprintf "%s}\n" indent

       | Name (_, (_, ty, _), _) ->
           (match ty with
            | "bool" | "int" | "float" | "string" ->
                sprintf "%s%s = %s;\n" indent dst src
            | _  -> type_not_supported atd_ty
           )
       | x -> type_not_supported x
      )

(* Assign from an object field, with support for optional fields.  The are two
 * kinds of optional fields: `With_default (~) and `Optional (?).  For both
 * kinds, we return the following values if the field is absent:
 *
 *   bool   -> false
 *   int    -> 0
 *   float  -> 0.0
 *   string -> ""
 *   list   -> []
 *   option -> None
 *
 * Optional fields of record and sum types are not supported. They are
 * treated as required fields.
 *
 * Fields of the `Optional kind extend this behaviour by automatically lifting
 * values of type t to option t by wrapping within a `Some'.
 * Hence `Optional may only be applied to fields of type option t.
 * Note that absent fields are still
 * assigned `None', as before.
 *
 * For `With_default fields, of types bool, int, float, string and list, we use
 * the org.json opt methods to extract the field.  These methods already return
 * the appropriate defaults if field is absent.  For option types, we manually
 * check for the field and manually create a default.  If the field is present,
 * then we wrap its values as necessary.
*)
let assign_field env
    (`Field (_, (atd_field_name, kind, annots), atd_ty)) java_ty =
  let json_field_name = get_json_field_name atd_field_name annots in
  let field_name = get_java_field_name atd_field_name annots in
  (* Check whether the field is optional *)
  let is_opt =
    match kind with
    | Atd.Ast.Optional
    | With_default -> true
    | Required -> false in
  let src = sprintf "jo.%s(\"%s\")" (get env atd_ty is_opt) json_field_name in
  if not is_opt then
    assign env (Some field_name) src java_ty atd_ty "    "
  else
    let mk_else = function
      | Some default ->
          sprintf "    } else {\n      %s = %s;\n    }\n"
            field_name default
      | None ->
          "    }\n"
    in
    let opt_set_default =
      match kind with
      | Atd.Ast.With_default ->
          (match norm_ty ~unwrap_option:true env atd_ty with
           | Name (_, (_, name, _), _) ->
               (match name with
                | "bool" -> mk_else (Some "false")
                | "int" -> mk_else (Some "0")
                | "float" -> mk_else (Some "0.0")
                | "string" -> mk_else (Some "\"\"")
                | _ -> mk_else None (* TODO: fail if no default is provided *)
               )
           | List _ ->
               (* java_ty is supposed to be of the form "ArrayList<...>" *)
               mk_else (Some (sprintf "new %s()" java_ty))
           | _ ->
               mk_else None (* TODO: fail if no default is provided *)
          )
      | _ ->
          mk_else None
    in
    let atd_ty = norm_ty ~unwrap_option:true env atd_ty in
    sprintf "    if (jo.has(\"%s\")) {\n" json_field_name
    ^ assign env (Some field_name) src java_ty atd_ty "      "
    ^ opt_set_default


(* Generate a toJsonBuffer command *)
let rec to_string env id atd_ty indent =
  let atd_ty = norm_ty env atd_ty in
  match atd_ty with
  | List (_, atd_sub_ty, _) ->
      sprintf "%s_out.append(\"[\");\n" indent
      ^ sprintf "%sfor (int i = 0; i < %s.size(); ++i) {\n" indent id
      ^ to_string env (id ^ ".get(i)") atd_sub_ty (indent ^ "  ")
      ^ sprintf "%s  if (i < %s.size() - 1)\n" indent id
      ^ sprintf "%s    _out.append(\",\");\n" indent
      ^ sprintf "%s}\n" indent
      ^ sprintf "%s_out.append(\"]\");\n" indent
  | Name (_, (_, "string", _), _) ->
      (* TODO Check that this is the correct behaviour *)
      sprintf
        "%sUtil.writeJsonString(_out, %s);\n"
        indent id
  | Name _ ->
      sprintf "%s_out.append(String.valueOf(%s));\n" indent id
  | _ ->
      sprintf "%s%s.toJsonBuffer(_out);\n" indent id

(* Generate a toJsonBuffer command for a record field. *)
let to_string_field env = function
  | (`Field (_, (atd_field_name, kind, annots), atd_ty)) ->
      let json_field_name = get_json_field_name atd_field_name annots in
      let field_name = get_java_field_name atd_field_name annots in
      let atd_ty = norm_ty ~unwrap_option:true env atd_ty in
      (* In the case of an optional field, create a predicate to test whether
       * the field has its default value. *)
      let if_part =
        sprintf "
    if (%s != null) {
      if (_isFirst)
        _isFirst = false;
      else
        _out.append(\",\");
      _out.append(\"\\\"%s\\\":\");
%s    }
"
          field_name
          json_field_name
          (to_string env field_name atd_ty "      ")
      in
      let else_part =
        let is_opt =
          match kind with
          | Atd.Ast.Optional | With_default -> true
          | Required -> false in
        if is_opt then
          ""
        else
          sprintf "    \
                   else
      throw new JSONException(\"Uninitialized field %s\");
"
            field_name
      in
      if_part ^ else_part

(* Generate a javadoc comment *)
let javadoc loc annots indent =
  let from_inline_text text = indent ^ " * " ^ text ^ "\n" in
  (* Assume that code is the name of a field that is defined
     in the same class *)
  let from_inline_code code = indent ^ " * {@link #" ^ code ^ "}\n" in
  let from_doc_para =
    List.fold_left (fun acc -> function
      | Atd.Doc.Text text -> (from_inline_text text) :: acc
      | Code code -> (from_inline_code code) :: acc
    ) in
  let from_doc =
    List.fold_left (fun acc -> function
      | Atd.Doc.Paragraph para -> from_doc_para acc para
      | Pre _ -> failwith "Preformatted doc blocks are not supported"
    ) []
  in
  (match Atd.Doc.get_doc loc annots with
   | Some doc ->
       let header = indent ^ "/**\n" in
       let footer = indent ^ " */\n" in
       let body   =
         String.concat "" (List.rev (from_doc doc)) in
       header ^ body ^ footer
   | None     -> ""
  )


(* ------------------------------------------------------------------------- *)
(* Translation of ATD types into Java types *)

(* For option, sum and record types, we generate a Java class.  Each such class
 * implements the following interface:
 *
 *  interface Atdj {
 *    String toJson() throws JSONException;
 *    void toJsonBuffer(StringBuilder out) throws JSONException;
 *  }
 *
 * The toJson() method outputs a JSON representation of the
 * associated value.
 *
 * Each class also has a String constructor for a JSON string as well as a
 * constructor from the corresponding org.json type (see json_of_atd, above).
 *
 * We do not generate classes for types bool, int, float, string and list;
 * instead we `inline' these types directly into the class in which they
 * occur.  We do this so that the Java programmer can access such values
 * directly, thereby avoiding the overhead of having to manually unbox each such
 * value upon access.
*)

let open_class env cname =
  let out = open_out (env.package_dir ^ "/" ^ cname ^ ".java") in
  fprintf out "\
// Automatically generated; do not edit
package %s;
import org.json.*;

"
    env.package;
  out

let rec trans_module env items = List.fold_left trans_outer env items

and trans_outer env (Atd.Ast.Type (_, (name, _, _), atd_ty)) =
  match unwrap atd_ty with
  | Sum (loc, v, a) ->
      trans_sum name env (loc, v, a)
  | Record (loc, v, a) ->
      trans_record name env (loc, v, a)
  | Name (_, (_, _name, _), _) ->
      (* Don't translate primitive types at the top-level *)
      env
  | x -> type_not_supported x

(* Translation of sum types.  For a sum type
 *
 *   type ty = Foo | Bar of whatever
 *
 * we generate a class Ty implemented in Ty.java and an enum TyEnum defined
 * in a separate file TyTag.java.
*)
and trans_sum my_name env (_, vars, _) =
  let class_name = Atdj_names.to_class_name my_name in

  let cases = List.map (function
    | Atd.Ast.Variant (_, (atd_name, an), opt_ty) ->
        let json_name = get_json_variant_name atd_name an in
        let func_name, enum_name, field_name =
          get_java_variant_names atd_name an in
        let opt_java_ty =
          opt_ty |> Option.map (fun ty ->
            let (java_ty, _) = trans_inner env (unwrap_option env ty) in
            (ty, java_ty)
          ) in
        (json_name, func_name, enum_name, field_name, opt_java_ty)
    | Inherit _ -> assert false
  ) vars
  in

  let tags = List.map (fun (_, _, enum_name, _, _) -> enum_name) cases in

  let out = open_class env class_name in

  fprintf out "\
/**
 * Construct objects of type %s.
 */

public class %s {
  Tag t = null;

  public %s() {
  }

  public Tag tag() {
    return t;
  }
"
    my_name
    class_name
    class_name;

  fprintf out "
  /**
   * Define tags for sum type %s.
   */
  public enum Tag {
    %s
  }
"
    my_name
    (String.concat ", " tags);

  fprintf out "
  public %s(Object o) throws JSONException {
    String tag = Util.extractTag(o);
   %a
      throw new JSONException(\"Invalid tag: \" + tag);
  }
"
    class_name
    (fun out l ->
       List.iter (fun (json_name, _func_name, enum_name, field_name, opt_ty) ->
         match opt_ty with
         | None ->
             fprintf out " \
                          if (tag.equals(\"%s\"))
      t = Tag.%s;
    else"
               json_name (* TODO: java-string-escape this *)
               enum_name

         | Some (atd_ty, java_ty) ->
             let src = sprintf "((JSONArray)o).%s(1)" (get env atd_ty false) in
             let set_value =
               assign env
                 (Some ("field_" ^ field_name)) src
                 java_ty atd_ty "      "
             in
             fprintf out " \
                          if (tag.equals(\"%s\")) {
%s
      t = Tag.%s;
    }
    else"
               json_name (* TODO: java-string-escape this *)
               set_value
               enum_name
       ) l
    ) cases;

  List.iter (fun (_, func_name, enum_name, field_name, opt_ty) ->
    match opt_ty with
    | None ->
        fprintf out "
  public void set%s() {
    /* TODO: clear previously-set field and avoid memory leak */
    t = Tag.%s;
  }
"
          func_name
          enum_name;
    | Some (_atd_ty, java_ty) ->
        fprintf out "
  %s field_%s = null;
  public void set%s(%s x) {
    /* TODO: clear previously-set field in order to avoid memory leak */
    t = Tag.%s;
    field_%s = x;
  }
  public %s get%s() {
    if (t == Tag.%s)
      return field_%s;
    else
      return null;
  }
"
          java_ty field_name
          func_name java_ty
          enum_name
          field_name
          java_ty func_name
          enum_name
          field_name;
  ) cases;

  fprintf out "
  public void toJsonBuffer(StringBuilder _out) throws JSONException {
    if (t == null)
      throw new JSONException(\"Uninitialized %s\");
    else {
      switch(t) {%a
      default:
        break; /* unused; keeps compiler happy */
      }
    }
  }

  public String toJson() throws JSONException {
    StringBuilder out = new StringBuilder(128);
    toJsonBuffer(out);
    return out.toString();
  }
"
    class_name
    (fun out l ->
       List.iter (fun (json_name, _func_name, enum_name, field_name, opt_ty) ->
         match opt_ty with
         | None ->
             fprintf out "
      case %s:
        _out.append(\"\\\"%s\\\"\");
        break;"
               enum_name
               json_name (* TODO: java-string-escape *)

         | Some (atd_ty, _) ->
             fprintf out "
      case %s:
         _out.append(\"[\\\"%s\\\",\");
%s         _out.append(\"]\");
         break;"
               enum_name
               json_name
               (to_string env ("field_" ^ field_name) atd_ty "         ")
       ) l
    ) cases;

  fprintf out "}\n";
  close_out out;
  env

(* Translate a record into a Java class.  Each record field becomes a field
 * within the class.
*)
and trans_record my_name env (loc, fields, annots) =
  (* Remove `Inherit values *)
  let fields = List.map
      (function
        | `Field _ as f -> f
        | `Inherit _ -> assert false
      )
      fields in
  (* Translate field types *)
  let (java_tys, env) = List.fold_left
      (fun (java_tys, env) -> function
         | `Field (_, (field_name, _, annots), atd_ty) ->
             let field_name = get_java_field_name field_name annots in
             let (java_ty, env) = trans_inner env (unwrap_option env atd_ty) in
             ((field_name, java_ty) :: java_tys, env)
      )
      ([], env) fields in
  let java_tys = List.rev java_tys in
  (* Output Java class *)
  let class_name = Atdj_names.to_class_name my_name in
  let out = open_class env class_name in
  (* Javadoc *)
  output_string out (javadoc loc annots "");
  fprintf out "\
public class %s implements Atdj {
  /**
   * Construct from a fresh record with null fields.
   */
  public %s() {
  }

  /**
   * Construct from a JSON string.
   */
  public %s(String s) throws JSONException {
    this(new JSONObject(s));
  }

  %s(JSONObject jo) throws JSONException {
"
    class_name
    class_name
    class_name
    class_name;

  let env = List.fold_left
      (fun env (`Field (_, (field_name, _, annots), _) as field) ->
         let field_name = get_java_field_name field_name annots in
         let cmd =
           assign_field env field (List.assoc_exn field_name java_tys) in
         fprintf out "%s" cmd;
         env
      )
      env fields in
  fprintf out "\n  \
               }

  public void toJsonBuffer(StringBuilder _out) throws JSONException {
    boolean _isFirst = true;
    _out.append(\"{\");%a
    _out.append(\"}\");
  }

  public String toJson() throws JSONException {
    StringBuilder out = new StringBuilder(128);
    toJsonBuffer(out);
    return out.toString();
  }
"
    (fun out l ->
       List.iter (fun field ->
         output_string out (to_string_field env field)
       ) l;
    ) fields;

  List.iter
    (function `Field (loc, (field_name, _, annots), _) ->
       let field_name = get_java_field_name field_name annots in
       let java_ty = List.assoc_exn field_name java_tys in
       output_string out (javadoc loc annots "  ");
       fprintf out "  public %s %s;\n" java_ty field_name)
    fields;
  fprintf out "}\n";
  close_out out;
  env

(* Translate an `inner' type i.e. a type that occurs within a record or sum *)
and trans_inner env atd_ty =
  match atd_ty with
  | Name (_, (_, name1, _), _) ->
      (match norm_ty env atd_ty with
       | Name (_, (_, name2, _), _) ->
           (* It's a primitive type e.g. int *)
           (Atdj_names.to_class_name name2, env)
       | _ ->
           (Atdj_names.to_class_name name1, env)
      )
  | List (_, sub_atd_ty, _)  ->
      let (ty', env) = trans_inner env sub_atd_ty in
      ("java.util.ArrayList<" ^ ty' ^ ">", env)
  | x -> type_not_supported x
