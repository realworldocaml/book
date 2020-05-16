(* Helper classes *)

open Atd.Import
open Atdj_env

let output_atdj env =
  let out = Atdj_trans.open_class env "Atdj" in
  fprintf out "\
/**
 * Common utility interface.
 */
public interface Atdj {
  /**
   * Get the JSON string representation, failing if some of the data
   * was not initialized.
   * @return The JSON string.
   */
  String toJson() throws JSONException;

  /**
   * Write the JSON representation to a buffer, failing if some of the data
   * was not initialized.
   */
  void toJsonBuffer(StringBuilder out) throws JSONException;
}
";
  close_out out

let output_util env =
  let out = Atdj_trans.open_class env "Util" in
  fprintf out "\
class Util {
  // Extract the tag of sum-typed value
  static String extractTag(Object value) throws JSONException {
    if (value instanceof String)
      return (String)value;
    else if (value instanceof JSONArray)
      return ((JSONArray)value).getString(0);
    else throw new JSONException(\"Cannot extract type\");
  }

  // Is an option value a none?
  static boolean isNone(Object value) throws JSONException {
    return (value instanceof String) && (((String)value).equals(\"None\"));
  }

  // Is an option value a Some?
  static boolean isSome(Object value) throws JSONException {
    return (value instanceof JSONArray)
      && ((JSONArray)value).getString(0).equals(\"Some\");
  }

  /*
    Encode a JSON string into a buffer
   */
  static void writeJsonString(StringBuilder out, String s) {
    out.append(\"\\\"\");
    for (int i = 0; i < s.length(); ++i) {
      char c = s.charAt(i);
      switch (c) {
      case '\\b':
        out.append(\"\\\\b\");
        break;
      case '\\f':
        out.append(\"\\\\f\");
        break;
      case '\\n':
        out.append(\"\\\\n\");
        break;
      case '\\r':
        out.append(\"\\\\r\");
        break;
      case '\\t':
        out.append(\"\\\\t\");
        break;
      case '\\\\':
        out.append(\"\\\\\\\\\");
        break;
      case '\"':
        out.append(\"\\\\\\\"\");
        break;
      default:
        if (c < 32 || c == 127)
          out.append(String.format(\"\\\\u%%04x\", (int) c));
        else
          out.append(c);
      }
    }
    out.append(\"\\\"\");
  }

  static String jsonStringOfString(String s) {
    StringBuilder out = new StringBuilder();
    writeJsonString(out, s);
    return out.toString();
  }

  // Unescape escaped backslashes and double quotations.
  // All other escape sequences are considered invalid
  // (this is probably too strict).
  static String unescapeString(String str) throws JSONException {
    StringBuilder buf = new StringBuilder();
    for (int i = 0; i < str.length(); ++i) {
      if (str.charAt(i) == '\\\\') {
        if (i == str.length() - 1 ||
            (str.charAt(i + 1) != '\\\\' && str.charAt(i + 1) != '\"'))
          throw new JSONException(\"Invalid escape\");
        else {
          buf.append(str.charAt(i + 1));
          ++i;
        }
      } else {
        buf.append(str.charAt(i));
      }
    }
    return buf.toString();
  }
}
";
  close_out out

let output_package_javadoc env (loc, annots) =
  let out = open_out (env.package_dir ^ "/" ^ "package.html") in
  output_string out "<body>\n";
  let from_doc_para =
    List.fold_left (fun acc -> function
      | Atd.Doc.Text text -> text :: acc
      | Code _ -> failwith "Not yet implemented: code in javadoc comments"
    ) in
  let from_doc =
    List.fold_left (fun acc -> function
      | Atd.Doc.Paragraph para -> from_doc_para acc para
      | Pre _ ->
          failwith "Not yet implemented: \
                    preformatted text in javadoc comments"
    ) [] in
  (match Atd.Doc.get_doc loc annots with
   | Some doc ->
       let str = String.concat "\n<p>\n" (List.rev (from_doc doc)) in
       output_string out str
   | _ -> ()
  );
  output_string out "\n</body>";
  close_out out
