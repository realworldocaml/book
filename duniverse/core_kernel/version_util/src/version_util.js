//Provides: generated_build_info
//Requires: caml_read_file_content, caml_string_of_jsbytes
function generated_build_info () {
  try {
    return caml_read_file_content("/static/build_info.sexp");
  } catch (e) {
    return caml_string_of_jsbytes(
      '('
        + '(username "")'
        + '(hostname "")'
        + '(kernel   "")'
        + '(build_time "1970-01-01 00:00:00Z")'
        + '(x_library_inlining false)'
        + '(portable_int63 true)'
        + '(dynlinkable_code false)'
        + '(ocaml_version "")'
        + '(executable_path "")'
        + '(build_system "")'
        + ')'
    );
  }
}


//Provides: generated_hg_version
//Requires: caml_read_file_content, caml_string_of_jsbytes
function generated_hg_version () {
  try {
    return caml_read_file_content("/static/hg_version.out");
  } catch (e) {
    return caml_string_of_jsbytes("NO_VERSION_UTIL");
  }
}
