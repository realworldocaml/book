(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Testing
open Result

let windows = Sys.os_type = "Win32"

let eqp = eq ~eq:Fpath.equal ~pp:Fpath.pp
let v = Fpath.v

let of_string = test "Fpath.{v,of_string}" @@ fun () ->
  let eq r o = match r, o with
  | Ok v, Some v' -> eqp v v'
  | Ok v, None -> fail "Ok %a <> Error _" Fpath.pp v
  | Error (`Msg m), Some v -> fail "Error (`Msg %s) <> Ok %a" m Fpath.pp v
  | Error _, None -> pass ()
  in
  let ok s = (Some (v s)) in
  let error = None in
  eq (Fpath.of_string "/\x00") error;
  eq (Fpath.of_string "/") (Some (Fpath.v "/"));
  eq_bool (Fpath.equal (v "/") (v "/ ")) false;
  eq (Fpath.of_string "//") (if windows then error else ok "//");
  eq (Fpath.of_string "/a/b/c") (ok "/a/b/c");
  eq_bool (Fpath.equal (v "/a/b/c/") (v "/a/b/c")) false;
  eq (Fpath.of_string "") error; (* no empty path *)
  eq (Fpath.of_string "a///b///////c///") (ok "a/b/c/"); (* seg collapse *)
  eq (Fpath.of_string "a///b///////c") (ok "a/b/c"); (* seg collapse *)
  if windows then begin
    eq (Fpath.of_string "C:\x00") error;
    eq (Fpath.of_string "C:") error; (* no empty path *)
    eq (Fpath.of_string "C:\\") (ok "C:\\");
    eq (Fpath.of_string "C:rel") (ok "C:rel");
    eq (Fpath.of_string "\\\\") error;
    eq (Fpath.of_string "\\\\server") error;
    eq (Fpath.of_string "\\\\server\\") error;
    eq (Fpath.of_string "\\\\server\\share")
      (ok "\\\\server\\share\\") (* root add *);
    eq (Fpath.of_string "\\\\?") error;
    eq (Fpath.of_string "\\\\?\\") error;
    eq (Fpath.of_string "\\\\?\\a") error;
    eq (Fpath.of_string "\\\\?\\a:") (ok "\\\\?\\a:\\"); (* root add *)
    eq (Fpath.of_string "\\\\?\\a:\\") (ok "\\\\?\\a:\\");
    eq (Fpath.of_string "\\\\?\\a:\\c") (ok "\\\\?\\a:\\c");
    eq (Fpath.of_string "\\\\?\\server\\") error;
    eq (Fpath.of_string "\\\\?\\server\\\\") error;
    eq (Fpath.of_string "\\\\?\\server\\share")
      (ok "\\\\?\\server\\share\\"); (* root add *)
    eq (Fpath.of_string "\\\\?\\server\\\\share")
      (ok "\\\\?\\server\\share\\"); (* seg collapse and root add *)
    eq (Fpath.of_string "\\\\?\\server\\share\\")
      (ok "\\\\?\\server\\share\\");
    eq (Fpath.of_string "\\\\?\\server\\share\\a")
      (ok "\\\\?\\server\\share\\a");
    eq (Fpath.of_string "\\\\?\\UNC") error;
    eq (Fpath.of_string "\\\\?\\UNC\\") error;
    eq (Fpath.of_string "\\\\?\\UNC\\server") error;
    eq (Fpath.of_string "\\\\?\\UNC\\server\\") error;
    eq (Fpath.of_string "\\\\?\\UNC\\server\\\\") error;
    eq (Fpath.of_string "\\\\?\\UNC\\server\\share")
      (ok "\\\\?\\UNC\\server\\share\\"); (* root add *)
    eq (Fpath.of_string "\\\\?\\UNC\\server\\share\\")
      (ok "\\\\?\\UNC\\server\\share\\");
    eq (Fpath.of_string "\\\\?\\UNC\\server\\share\\a")
      (ok "\\\\?\\UNC\\server\\share\\a");
    eq (Fpath.of_string "\\\\.") error;
    eq (Fpath.of_string "\\\\.\\") error;
    eq (Fpath.of_string "\\\\.\\device") (ok "\\\\.\\device\\")(* root add *);
    eq (Fpath.of_string "\\\\.\\device\\") (ok "\\\\.\\device\\");
    eq (Fpath.of_string "\\\\.\\device\\a") (ok "\\\\.\\device\\a");
  end;
  ()

let dir_sep = test "Fpath.dir_sep" @@ fun () ->
  eq_str Fpath.dir_sep (if windows then "\\" else "/");
  ()

let is_seg = test "Fpath.is_seg" @@ fun () ->
  eq_bool (Fpath.is_seg "abc") true;
  eq_bool (Fpath.is_seg "ab/c") false;
  eq_bool (Fpath.is_seg "ab\x00c") false;
  if windows then eq_bool (Fpath.is_seg "ab\\c") false;
  ()

let add_seg = test "Fpath.add_seg" @@ fun () ->
  app_raises ~pp:Fpath.pp (Fpath.add_seg (v "a/b/c")) "a\x00o";
  app_raises ~pp:Fpath.pp (Fpath.add_seg (v "a/b/c")) "a/o";
  if windows then app_raises ~pp:Fpath.pp (Fpath.add_seg (v "a/b/c")) "a\\o";
  eqp (Fpath.add_seg (v "/a") "b") (v "/a/b");
  eqp (Fpath.add_seg (v "/a/") "b") (v "/a/b");
  eqp (Fpath.add_seg (v "a/b") "") (v "a/b/");
  eqp (Fpath.add_seg (v "a/b/") "") (v "a/b/");
  eqp (Fpath.add_seg (v "/a/b") "") (v "/a/b/");
  eqp (Fpath.add_seg (v "/a/b/") "") (v "/a/b/");
  eqp (Fpath.add_seg (v "/a/b/") "e") (v "/a/b/e");
  eqp (Fpath.add_seg (v "/a/b") "e") (v "/a/b/e");
  eqp (Fpath.add_seg (v "/") "") (v "/");
  eqp (Fpath.add_seg (v "/") "a") (v "/a");
  eqp (Fpath.add_seg (v ".") "a") (v "./a");
  eqp (Fpath.add_seg (v ".") "") (v "./");
  eqp (Fpath.add_seg (v "..") "a") (v "../a");
  eqp (Fpath.add_seg (v "..") "") (v "../");
  ()

let append = test "Fpath.append" @@ fun () ->
  eqp (Fpath.append (v "/a/b/") (v "e/f")) (v "/a/b/e/f");
  eqp (Fpath.append (v "/a/b") (v "e/f")) (v "/a/b/e/f");
  eqp (Fpath.append (v "/a/b/") (v "/e/f")) (v "/e/f");
  eqp (Fpath.append (v "a/b/") (v "e/f")) (v "a/b/e/f");
  eqp (Fpath.append (v "bla") (v "/bli")) (v "/bli");
  if not windows then eqp (Fpath.append (v "bla") (v "//bli")) (v "//bli");
  if windows then begin
    eqp (Fpath.append (v "a/b") (v "C:e")) (v "C:e");
    eqp (Fpath.append (v "C:bla") (v "blu")) (v "C:bla/blu");
    eqp (Fpath.append (v "C:\\bla") (v "blu")) (v "C:\\bla\\blu");
    eqp (Fpath.append (v "C:\\bla") (v "\\blu")) (v "\\blu");
    eqp (Fpath.append (v "\\\\srv\\share\\a") (v "b"))
      (v "\\\\srv\\share\\a\\b");
    eqp (Fpath.append (v "\\\\srv\\share\\a\\") (v "b"))
      (v "\\\\srv\\share\\a\\b");
  end;
  ()

let split_volume = test "Fpath.split_volume" @@ fun () ->
  let eq_split p vol q =
    let p = v p in
    let vol', q' = Fpath.split_volume p in
    eq_str vol vol';
    eqp (v q) q';
    eqp (v (vol' ^ (Fpath.to_string q'))) p
  in
  eq_split "/bla" "" "/bla";
  eq_split "bla" "" "bla";
  eq_split "bla/a" "" "bla/a";
  eq_split "bla/a/" "" "bla/a/";
  if not windows then begin
    eq_split "//" "/" "/";
    eq_split "//a/b/c" "/" "/a/b/c";
    eq_split "//a/b/c/" "/" "/a/b/c/";
  end;
  if windows then begin
    eq_split "C:." "C:" ".";
    eq_split "C:\\" "C:" "\\";
    eq_split "C:\\a" "C:" "\\a";
    eq_split "C:rel" "C:" "rel";
    eq_split "\\\\server\\share\\" "\\\\server\\share" "\\";
    eq_split "\\\\server\\share\\a" "\\\\server\\share" "\\a";
    eq_split "\\\\?\\a:\\" "\\\\?\\a:" "\\";
    eq_split "\\\\?\\a:\\c" "\\\\?\\a:" "\\c";
    eq_split "\\\\?\\server\\share\\" "\\\\?\\server\\share" "\\";
    eq_split "\\\\?\\server\\share\\a" "\\\\?\\server\\share" "\\a";
    eq_split "\\\\?\\UNC\\server\\share\\" "\\\\?\\UNC\\server\\share" "\\";
    eq_split "\\\\?\\UNC\\server\\share\\a" "\\\\?\\UNC\\server\\share" "\\a";
    eq_split "\\\\.\\device\\" "\\\\.\\device" "\\";
    eq_split "\\\\.\\device\\a" "\\\\.\\device" "\\a";
  end;
  ()

let segs = test "Fpath.segs" @@ fun () ->
  let eq = eq_list ~eq:(=) ~pp:pp_str in
  eq (Fpath.segs @@ v "/a/b/") [""; "a"; "b"; ""];
  eq (Fpath.segs @@ v "/a/b") [""; "a"; "b"];
  eq (Fpath.segs @@ v "a/b/") ["a"; "b"; ""];
  eq (Fpath.segs @@ v "a/b") ["a"; "b"];
  eq (Fpath.segs @@ v "a") ["a"];
  eq (Fpath.segs @@ v "/") [""; ""];
  eq (Fpath.segs @@ v "/a/b/c") [""; "a"; "b"; "c"];
  eq (Fpath.segs @@ v "/a/b/c/") [""; "a"; "b"; "c"; ""];
  eq (Fpath.segs @@ v "a/b/c") ["a"; "b"; "c";];
  eq (Fpath.segs @@ v "a/b/c/") ["a"; "b"; "c"; ""];
  if not windows then begin
    eq (Fpath.segs @@ v "//") [""; ""];
    eq (Fpath.segs @@ v "//a/b") [""; "a"; "b"];
  end;
  if windows then begin
    eq (Fpath.segs @@ v "C:\\bla") [""; "bla"];
    eq (Fpath.segs @@ v "C:bla") ["bla"];
    eq (Fpath.segs @@ v "\\\\Server\\share\\bla") [""; "bla"];
    eq (Fpath.segs @@ v "\\\\?\\C:\\bla") ["";"bla"];
    eq (Fpath.segs @@ v "\\\\?\\Server\\share\\bla") [""; "bla"];
    eq (Fpath.segs @@ v "\\\\?\\UNC\\Server\\share\\bla") [""; "bla"];
    eq (Fpath.segs @@ v "\\\\.\\dev\\bla") [""; "bla"];
    eq (Fpath.segs @@ v "\\a") [""; "a"];
    eq (Fpath.segs @@ v "\\a\\b") [""; "a"; "b"];
    eq (Fpath.segs @@ v "\\a\\b\\") [""; "a"; "b"; ""];
    eq (Fpath.segs @@ v "C:.") ["."];
    eq (Fpath.segs @@ v "C:\\") ["";""];
    eq (Fpath.segs @@ v "C:\\a") ["";"a"];
    eq (Fpath.segs @@ v "C:rel") ["rel";];
    eq (Fpath.segs @@ v "\\\\server\\share\\") [""; ""];
    eq (Fpath.segs @@ v "\\\\server\\share\\a") [""; "a"];
    eq (Fpath.segs @@ v "\\\\?\\a:\\") [""; ""];
    eq (Fpath.segs @@ v "\\\\?\\a:\\c") [""; "c"];
    eq (Fpath.segs @@ v "\\\\?\\server\\share\\") [""; ""];
    eq (Fpath.segs @@ v "\\\\?\\server\\share\\a") [""; "a"];
    eq (Fpath.segs @@ v "\\\\?\\UNC\\server\\share\\") [""; ""];
    eq (Fpath.segs @@ v "\\\\?\\UNC\\server\\share\\a") [""; "a"];
    eq (Fpath.segs @@ v "\\\\.\\device\\") ["";""];
    eq (Fpath.segs @@ v "\\\\.\\device\\a") ["";"a"];
    eq (Fpath.segs @@ v "\\\\server\\share\\a") ["";"a"];
    eq (Fpath.segs @@ v "C:a") ["a"];
    eq (Fpath.segs @@ v "C:\\a") ["";"a"];
  end;
  ()

let is_dir_path = test "Fpath.is_dir_path" @@ fun () ->
  eq_bool (Fpath.is_dir_path (v ".")) true;
  eq_bool (Fpath.is_dir_path (v "..")) true;
  eq_bool (Fpath.is_dir_path (v "../")) true;
  eq_bool (Fpath.is_dir_path (v "/a/b/")) true;
  eq_bool (Fpath.is_dir_path (v "/a/b")) false;
  eq_bool (Fpath.is_dir_path (v "a/")) true;
  eq_bool (Fpath.is_dir_path (v "a")) false;
  eq_bool (Fpath.is_dir_path (v "a/.")) true;
  eq_bool (Fpath.is_dir_path (v "a/..")) true;
  eq_bool (Fpath.is_dir_path (v "a/..b")) false;
  eq_bool (Fpath.is_dir_path (v "/")) true;
  if windows then begin
    eq_bool (Fpath.is_dir_path (v "C:\\")) true;
    eq_bool (Fpath.is_dir_path (v "C:a")) false;
  end;
  ()

let is_file_path = test "Fpath.is_file_path" @@ fun () ->
  eq_bool (Fpath.is_file_path (v ".")) false;
  eq_bool (Fpath.is_file_path (v "..")) false;
  eq_bool (Fpath.is_file_path (v "../")) false;
  eq_bool (Fpath.is_file_path (v "/a/b/")) false;
  eq_bool (Fpath.is_file_path (v "/a/b")) true;
  eq_bool (Fpath.is_file_path (v "a/")) false;
  eq_bool (Fpath.is_file_path (v "a")) true;
  eq_bool (Fpath.is_file_path (v "a/.")) false;
  eq_bool (Fpath.is_file_path (v "a/..")) false;
  eq_bool (Fpath.is_file_path (v "a/..b")) true;
  eq_bool (Fpath.is_file_path (v "/")) false;
  if windows then begin
    eq_bool (Fpath.is_file_path (v "C:\\")) false;
    eq_bool (Fpath.is_file_path (v "C:a")) true;
  end;
  ()

let to_dir_path = test "Fpath.to_dir_path" @@ fun () ->
  eqp (Fpath.to_dir_path @@ v ".") (v "./");
  eqp (Fpath.to_dir_path @@ v "..") (v "../");
  eqp (Fpath.to_dir_path @@ v "../") (v "../");
  eqp (Fpath.to_dir_path @@ v "/a/b/") (v "/a/b/");
  eqp (Fpath.to_dir_path @@ v "/a/b") (v "/a/b/");
  eqp (Fpath.to_dir_path @@ v "a/") (v "a/");
  eqp (Fpath.to_dir_path @@ v "a") (v "a/");
  eqp (Fpath.to_dir_path @@ v "a/.") (v "a/./");
  eqp (Fpath.to_dir_path @@ v "a/..") (v "a/../");
  eqp (Fpath.to_dir_path @@ v "a/..b") (v "a/..b/");
  eqp (Fpath.to_dir_path @@ v "/") (v "/");
  if not windows then begin
    eqp (Fpath.to_dir_path @@ v "//") (v "//");
    eqp (Fpath.to_dir_path @@ v "//a") (v "//a/");
  end;
  if windows then begin
    eqp (Fpath.to_dir_path @@
         v "\\\\server\\share\\") (v "\\\\server\\share\\");
    eqp (Fpath.to_dir_path @@ v "C:a") (v "C:a/");
    eqp (Fpath.to_dir_path @@ v "C:\\") (v "C:\\");
  end;
  ()

let filename = test "Fpath.filename" @@ fun () ->
  eq_str (Fpath.filename @@ v ".") "";
  eq_str (Fpath.filename @@ v "./") "";
  eq_str (Fpath.filename @@ v "..") "";
  eq_str (Fpath.filename @@ v "../") "";
  eq_str (Fpath.filename @@ v "../..") "";
  eq_str (Fpath.filename @@ v "../../") "";
  eq_str (Fpath.filename @@ v "/a/b/") "";
  eq_str (Fpath.filename @@ v "/a/b") "b";
  eq_str (Fpath.filename @@ v "a/") "";
  eq_str (Fpath.filename @@ v "a") "a";
  eq_str (Fpath.filename @@ v "a/.") "";
  eq_str (Fpath.filename @@ v "a/..") "";
  eq_str (Fpath.filename @@ v "a/..b") "..b";
  eq_str (Fpath.filename @@ v "/") "";
  eq_str (Fpath.filename @@ v "/a/b/") "";
  eq_str (Fpath.filename @@ v "/a/b") "b";
  eq_str (Fpath.filename @@ v "a") "a";
  eq_str (Fpath.filename @@ v "a/") "";
  eq_str (Fpath.filename @@ v "/") "";
  if not windows then begin
    eq_str (Fpath.filename @@ v "//") "";
    eq_str (Fpath.filename @@ v "//..") "";
    eq_str (Fpath.filename @@ v "//a/b") "b";
    eq_str (Fpath.filename @@ v "//a/b/") "";
  end;
  if windows then begin
    eq_str (Fpath.filename @@ v "\\\\server\\share\\a") "a";
    eq_str (Fpath.filename @@ v "\\\\.\\device\\") "";
    eq_str (Fpath.filename @@ v "\\\\.\\device\\a") "a";
    eq_str (Fpath.filename @@ v "C:\\") "";
    eq_str (Fpath.filename @@ v "C:a") "a";
  end;
  ()

let split_base = test "Fpath.split_base" @@ fun () ->
  let eq_split p (d, b) =
    let d', b' = Fpath.split_base (v p) in
    eqp (v d) d';
    eqp (v b) b';
  in
  eq_split "." ("./", ".");
  eq_split "./" ("./", "./");
  eq_split ".." ("./", "..");
  eq_split "../" ("./", "../");
  eq_split "../../" ("../", "../");
  eq_split ".././" ("../", "./");
  eq_split "../../../" ("../../", "../");
  eq_split "/" ("/", "./");
  eq_split "/a/b/" ("/a/", "b/");
  eq_split "/a/b" ("/a/", "b");
  eq_split "a/" ("./", "a/");
  eq_split "a" ("./", "a");
  eq_split "a/b" ("a/", "b");
  eq_split "a/b/" ("a/", "b/");
  eq_split "a/." ("a/", ".");
  eq_split "a/.." ("a/", "..");
  eq_split "a/../.." ("a/../", "..");
  eq_split "a/..b" ("a/", "..b");
  eq_split "./a" ("./", "a");
  eq_split "./a/" ("./", "a/");
  eq_split "../a" ("../", "a");
  eq_split "../a/" ("../", "a/");
  if not windows then begin
    eq_split "//" ("//", "./");
    eq_split "//a/b" ("//a/", "b");
    eq_split "//a/b/" ("//a/", "b/");
    eq_split "//a" ("//", "a");
    eq_split "//a/" ("//", "a/");
    eq_split "//a/." ("//a/", ".");
    eq_split "//a/./" ("//a/", "./");
  end;
  if windows then begin
    eq_split "\\\\server\\share\\a" ("\\\\server\\share\\", "a");
    eq_split "\\\\.\\device\\" ("\\\\.\\device\\", ".\\");
    eq_split "\\\\.\\device\\a" ("\\\\.\\device\\", "a");
    eq_split "\\\\.\\device\\a\\" ("\\\\.\\device\\", "a\\");
    eq_split "C:\\" ("C:\\", ".\\");
    eq_split "C:a" ("C:.\\", "a");
  end;
  ()

let base = test "Fpath.base" @@ fun () ->
  eqp (Fpath.base @@ v ".") (v ".");
  eqp (Fpath.base @@ v "./") (v "./");
  eqp (Fpath.base @@ v "..") (v "..");
  eqp (Fpath.base @@ v "../") (v "../");
  eqp (Fpath.base @@ v "../../") (v "../");
  eqp (Fpath.base @@ v ".././") (v "./");
  eqp (Fpath.base @@ v "../../../") (v "../");
  eqp (Fpath.base @@ v "/") (v "./");
  eqp (Fpath.base @@ v "/a/b/") (v "b/");
  eqp (Fpath.base @@ v "/a/b") (v "b");
  eqp (Fpath.base @@ v "a/") (v "a/");
  eqp (Fpath.base @@ v "a") (v "a");
  eqp (Fpath.base @@ v "a/b") (v "b");
  eqp (Fpath.base @@ v "a/b/") (v "b/");
  eqp (Fpath.base @@ v "a/.") (v ".");
  eqp (Fpath.base @@ v "a/..") (v "..");
  eqp (Fpath.base @@ v "a/../..") (v "..");
  eqp (Fpath.base @@ v "a/..b") (v "..b");
  eqp (Fpath.base @@ v "./a") (v "a");
  eqp (Fpath.base @@ v "./a/") (v "a/");
  eqp (Fpath.base @@ v "../a") (v "a");
  eqp (Fpath.base @@ v "../a/") (v "a/");
  if not windows then begin
    eqp (Fpath.base @@ v "//") (v "./");
    eqp (Fpath.base @@ v "//a/b") (v "b");
    eqp (Fpath.base @@ v "//a/b/") (v "b/");
    eqp (Fpath.base @@ v "//a") (v "a");
    eqp (Fpath.base @@ v "//a/") (v "a/");
    eqp (Fpath.base @@ v "//a/.") (v ".");
    eqp (Fpath.base @@ v "//a/./") (v "./");
  end;
  if windows then begin
    eqp (Fpath.base @@ v "\\\\server\\share\\a") (v "a");
    eqp (Fpath.base @@ v "\\\\.\\device\\") (v ".\\");
    eqp (Fpath.base @@ v "\\\\.\\device\\a") (v "a");
    eqp (Fpath.base @@ v "\\\\.\\device\\a\\") (v "a\\");
    eqp (Fpath.base @@ v "C:\\") (v ".\\");
    eqp (Fpath.base @@ v "C:a") (v "a");
  end;
  ()

let basename = test "Fpath.basename" @@ fun () ->
  eq_str (Fpath.basename @@ v ".") "";
  eq_str (Fpath.basename @@ v "..") "";
  eq_str (Fpath.basename @@ v "../") "";
  eq_str (Fpath.basename @@ v "../../") "";
  eq_str (Fpath.basename @@ v "/") "";
  eq_str (Fpath.basename @@ v "/a/b/") "b";
  eq_str (Fpath.basename @@ v "/a/b") "b";
  eq_str (Fpath.basename @@ v "a/") "a";
  eq_str (Fpath.basename @@ v "a") "a";
  eq_str (Fpath.basename @@ v "a/.") "";
  eq_str (Fpath.basename @@ v "a/./") "";
  eq_str (Fpath.basename @@ v "a/..") "";
  eq_str (Fpath.basename @@ v "a/..b") "..b";
  eq_str (Fpath.basename @@ v "./a") "a";
  eq_str (Fpath.basename @@ v "../a") "a";
  if not windows then begin
    eq_str (Fpath.basename @@ v "//") "";
    eq_str (Fpath.basename @@ v "//a/b") "b";
    eq_str (Fpath.basename @@ v "//a/b/") "b";
  end;
  if windows then begin
    eq_str (Fpath.basename @@ v "\\\\server\\share\\a") "a";
    eq_str (Fpath.basename @@ v "\\\\server\\share\\a\\") "a";
    eq_str (Fpath.basename @@ v "\\\\.\\device\\") "";
    eq_str (Fpath.basename @@ v "\\\\.\\device\\a") "a";
    eq_str (Fpath.basename @@ v "C:\\") "";
    eq_str (Fpath.basename @@ v "C:a") "a";
  end;
  ()

let parent = test "Fpath.parent" @@ fun () ->
  eqp (Fpath.parent @@ v ".") (v "./../");
  eqp (Fpath.parent @@ v "..") (v "../../");
  eqp (Fpath.parent @@ v "../") (v "../../");
  eqp (Fpath.parent @@ v "../../") (v "../../../");
  eqp (Fpath.parent @@ v "/") (v "/");
  eqp (Fpath.parent @@ v "/a/b/") (v "/a/");
  eqp (Fpath.parent @@ v "/a/b") (v "/a/");
  eqp (Fpath.parent @@ v "a/") (v "./");
  eqp (Fpath.parent @@ v "a") (v "./");
  eqp (Fpath.parent @@ v "a/.") (v "a/./../");
  eqp (Fpath.parent @@ v "a/./") (v "a/./../");
  eqp (Fpath.parent @@ v "a/..") (v "a/../../");
  eqp (Fpath.parent @@ v "a/../") (v "a/../../");
  eqp (Fpath.parent @@ v "a/..b") (v "a/");
  eqp (Fpath.parent @@ v "./a") (v "./");
  eqp (Fpath.parent @@ v "../a") (v "../");
  eqp (Fpath.parent @@ v "../../a") (v "../../");
  if not windows then begin
    eqp (Fpath.parent @@ v "//") (v "//");
    eqp (Fpath.parent @@ v "//.") (v "//./../");
    eqp (Fpath.parent @@ v "//a/b") (v "//a/");
    eqp (Fpath.parent @@ v "//a/b/") (v "//a/");
    eqp (Fpath.parent @@ v "//a/b/..") (v "//a/b/../../");
    eqp (Fpath.parent @@ v "//a/b/../") (v "//a/b/../../");
    eqp (Fpath.parent @@ v "//a") (v "//");
    eqp (Fpath.parent @@ v "//abcd") (v "//");
  end;
  if windows then begin
    eqp (Fpath.parent @@ v "\\\\server\\share\\") (v "\\\\server\\share\\");
    eqp (Fpath.parent @@ v "C:a") (v "C:.\\");
    eqp (Fpath.parent @@ v "C:\\") (v "C:\\");
    eqp (Fpath.parent @@ v "C:\\a\\b\\") (v "C:\\a\\");
    eqp (Fpath.parent @@ v "C:\\a\\b") (v "C:\\a\\");
    eqp (Fpath.parent @@ v "C:a\\b\\") (v "C:a\\");
    eqp (Fpath.parent @@ v "C:a\\b") (v "C:a\\");
    eqp (Fpath.parent @@ v "C:a\\..") (v "C:a\\..\\..\\");
  end;
  ()

let rem_empty_seg = test "Fpath.rem_empty_seg" @@ fun () ->
  eqp (Fpath.rem_empty_seg @@ v ".") (v ".");
  eqp (Fpath.rem_empty_seg @@ v "..") (v "..");
  eqp (Fpath.rem_empty_seg @@ v "../") (v "..");
  eqp (Fpath.rem_empty_seg @@ v "../../") (v "../..");
  eqp (Fpath.rem_empty_seg @@ v "/") (v "/");
  eqp (Fpath.rem_empty_seg @@ v "/a/b/") (v "/a/b");
  eqp (Fpath.rem_empty_seg @@ v "/a/b") (v "/a/b");
  eqp (Fpath.rem_empty_seg @@ v "a/") (v "a");
  eqp (Fpath.rem_empty_seg @@ v "a") (v "a");
  eqp (Fpath.rem_empty_seg @@ v "a/.") (v "a/.");
  eqp (Fpath.rem_empty_seg @@ v "a/./") (v "a/.");
  eqp (Fpath.rem_empty_seg @@ v "a/..") (v "a/..");
  eqp (Fpath.rem_empty_seg @@ v "a/../") (v "a/..");
  eqp (Fpath.rem_empty_seg @@ v "a/..b") (v "a/..b");
  eqp (Fpath.rem_empty_seg @@ v "./a") (v "./a");
  eqp (Fpath.rem_empty_seg @@ v "../a") (v "../a");
  eqp (Fpath.rem_empty_seg @@ v "../../a") (v "../../a");
  if not windows then begin
    eqp (Fpath.rem_empty_seg @@ v "//") (v "//");
    eqp (Fpath.rem_empty_seg @@ v "//a") (v "//a");
    eqp (Fpath.rem_empty_seg @@ v "//a/") (v "//a");
  end;
  if windows then begin
    eqp (Fpath.rem_empty_seg @@ v "\\\\server\\share\\")
      (v "\\\\server\\share\\");
    eqp (Fpath.rem_empty_seg @@ v "\\\\server\\share\\a\\")
      (v "\\\\server\\share\\a");
    eqp (Fpath.rem_empty_seg @@ v "C:a") (v "C:a");
    eqp (Fpath.rem_empty_seg @@ v "C:a\\") (v "C:a");
    eqp (Fpath.rem_empty_seg @@ v "C:\\") (v "C:\\");
  end;
  ()

let normalize = test "Fpath.normalize" @@ fun () ->
  eqp (Fpath.normalize @@ v ".") (v "./");
  eqp (Fpath.normalize @@ v "..") (v "../");
  eqp (Fpath.normalize @@ v "../") (v "../");
  eqp (Fpath.normalize @@ v "../..") (v "../../");
  eqp (Fpath.normalize @@ v "../../") (v "../../");
  eqp (Fpath.normalize @@ v "/") (v "/");
  eqp (Fpath.normalize @@ v "/a/b/") (v "/a/b/");
  eqp (Fpath.normalize @@ v "/a/b") (v "/a/b");
  eqp (Fpath.normalize @@ v "a/") (v "a/");
  eqp (Fpath.normalize @@ v "a") (v "a");
  eqp (Fpath.normalize @@ v "a/.") (v "a/");
  eqp (Fpath.normalize @@ v "a/./") (v "a/");
  eqp (Fpath.normalize @@ v "a/..") (v "./");
  eqp (Fpath.normalize @@ v "a/../") (v "./");
  eqp (Fpath.normalize @@ v "a/..b") (v "a/..b");
  eqp (Fpath.normalize @@ v "./a") (v "a");
  eqp (Fpath.normalize @@ v "../a") (v "../a");
  eqp (Fpath.normalize @@ v "a/..") (v "./");
  eqp (Fpath.normalize @@ v "../../a") (v "../../a");
  eqp (Fpath.normalize @@ v "./a/..") (v "./");
  eqp (Fpath.normalize @@ v "/a/b/./..") (v "/a/");
  eqp (Fpath.normalize @@ v "/../..") (v "/");
  eqp (Fpath.normalize @@ v "/a/../..") (v "/");
  eqp (Fpath.normalize @@ v "./../..") (v "../../");
  eqp (Fpath.normalize @@ v "../../a/") (v "../../a/");
  eqp (Fpath.normalize @@ v "a/../a/") (v "a/");
  eqp (Fpath.normalize @@ v "a/../a/../..") (v "../");
  eqp (Fpath.normalize @@ v "/a/../a/../..") (v "/");
  eqp (Fpath.normalize @@ v "/a/b/c/./../../g") (v "/a/g");
  eqp (Fpath.normalize @@ v "/a/b/c/./../../g/") (v "/a/g/");
  eqp (Fpath.normalize @@ v "a/b/c/./../../g") (v "a/g");
  eqp (Fpath.normalize @@ v "a/b/c/./../../g/") (v "a/g/");
  eqp (Fpath.normalize @@ v "././.") (v "./");
  eqp (Fpath.normalize @@ v "./././") (v "./");
  eqp (Fpath.normalize @@ v "./a/..") (v "./");
  eqp (Fpath.normalize @@ v "./a/../") (v "./");
  eqp (Fpath.normalize @@ v "..") (v "../");
  eqp (Fpath.normalize @@ v "../../../a") (v "../../../a");
  eqp (Fpath.normalize @@ v "../../../a/") (v "../../../a/");
  eqp (Fpath.normalize @@ v "/") (v "/");
  eqp (Fpath.normalize @@ v "/.") (v "/");
  eqp (Fpath.normalize @@ v "/..") (v "/");
  eqp (Fpath.normalize @@ v "/./../../.") (v "/");
  eqp (Fpath.normalize @@ v "/./../../.") (v "/");
  eqp (Fpath.normalize @@ v "../../a/..") (v "../../");
  eqp (Fpath.normalize @@ v "../../a/../.") (v "../../");
  eqp (Fpath.normalize @@ v "../../a/.././..") (v "../../../");
  eqp (Fpath.normalize @@ v "../../a/../..") (v "../../../");
  eqp (Fpath.normalize @@ v "/a/b/c/./../../g") (v "/a/g");
  eqp (Fpath.normalize @@ v "./a/b/c/./../../g") (v "a/g");
  eqp (Fpath.normalize @@ v "./a/b/c/./../../g/") (v "a/g/");
  if not windows then begin
    eqp (Fpath.normalize @@ v "//a/b/c/./../../g") (v "//a/g");
    eqp (Fpath.normalize @@ v "//a/b/c/./../../g/") (v "//a/g/");
  end;
  if windows then begin
    eqp (Fpath.normalize @@ v "C:/a/b/c/./../../g") (v "C:/a/g");
    eqp (Fpath.normalize @@ v "C:/a/b/c/./../../g/") (v "C:/a/g/");
    eqp (Fpath.normalize @@ v "\\\\?\\UNC\\server\\share\\..")
      (v "\\\\?\\UNC\\server\\share\\");
  end;
  ()

let is_prefix = test "Fpath.is_prefix" @@ fun () ->
  eq_bool (Fpath.is_prefix (v "/a/b") (v "/a/b")) true;
  eq_bool (Fpath.is_prefix (v "/a/b") (v "/a/bc")) false;
  eq_bool (Fpath.is_prefix (v "/a/b") (v "/a/b/")) true;
  eq_bool (Fpath.is_prefix (v "a/b/") (v "a/b")) false;
  eq_bool (Fpath.is_prefix (v "a/b/") (v "a/b/")) true;
  eq_bool (Fpath.is_prefix (v "a/b/") (v "a/b/c")) true;
  eq_bool (Fpath.is_prefix (v ".") (v "./")) true;
  eq_bool (Fpath.is_prefix (v "..") (v ".")) false;
  eq_bool (Fpath.is_prefix (v ".") (v "..")) false;
  eq_bool (Fpath.is_prefix (v "/a/b") (v "/a/b/c")) true;
  eq_bool (Fpath.is_prefix (v "/a/b/") (v "/a/b/c")) true;
  eq_bool (Fpath.is_prefix (v "/a/b/") (v "/a/b")) false;
  eq_bool (Fpath.is_prefix (v "/a/b/") (v "/a/b")) false;
  eq_bool (Fpath.is_prefix (v "a/b") (v "/a/b")) false;
  eq_bool (Fpath.is_prefix (v "abcd/") (v "abcd")) false;
  eq_bool (Fpath.is_prefix (v "abcd") (v "abcd/bla")) true;
  if not windows then begin
    eq_bool (Fpath.is_prefix (v "//a/b") (v "/a/b")) false
  end;
  if windows then begin
    eq_bool (Fpath.is_prefix (v "C:a") (v "a")) false;
  end;
  ()

let find_prefix = test "Fpath.find_prefix" @@ fun () ->
  let eq = eq_option ~eq:Fpath.equal ~pp:Fpath.pp in
  let find_prefix p0 p1 r =
    eq (Fpath.find_prefix p0 p1) r;
    eq (Fpath.find_prefix p1 p0) r;
  in
  find_prefix (v "a/b/c") (v "a/b/d") (Some (v "a/b/"));
  find_prefix (v "a/b/c") (v "a/b/cd") (Some (v "a/b/"));
  find_prefix (v "a/b") (v "a/b") (Some (v "a/b"));
  find_prefix (v "a/b") (v "a/b/") (Some (v "a/b"));
  find_prefix (v "a/b") (v "e/f") None;
  find_prefix (v "/a/b") (v "/e/f") (Some (v "/"));
  find_prefix (v "/a/b") (v "e/f") None;
  find_prefix (v "/a/b/c") (v "/a/b/d") (Some (v "/a/b/"));
  find_prefix (v "ab") (v "abc") None;
  find_prefix (v "ab") (v "ab") (Some (v "ab"));
  find_prefix (v "/") (v "/") (Some (v "/"));
  find_prefix (v "a/") (v "a") (Some (v "a"));
  find_prefix (v "abc/") (v "abc") (Some (v "abc"));
  find_prefix (v "abcd/") (v "abc") None;
  find_prefix (v "a/") (v "a/a") (Some (v "a/"));
  if not windows then begin
    find_prefix (v "//") (v "/") None;
    find_prefix (v "/") (v "//") None;
    find_prefix (v "//") (v "/a/b") None;
    find_prefix (v "//a/b/c") (v "/") None;
    find_prefix (v "//a/b/c") (v "//") (Some (v "//"));
    find_prefix (v "//a/b") (v "/a/b") None;
    find_prefix (v "//a/c") (v "/a/b") None;
    find_prefix (v "//a/c") (v "a/b") None;
  end;
  if windows then begin
    find_prefix (v "C:\\a") (v "\\a") None;
    find_prefix (v "C:\\a") (v "C:\\a") (Some (v "C:\\a"));
    find_prefix (v "C:a") (v "C:a") (Some (v "C:a"));
    find_prefix (v "C:a") (v "C:b") None;
    find_prefix (v "C:a") (v "C:b/c") None;
    find_prefix (v "C:a/f") (v "C:b/c") None;
    find_prefix (v "C:a/f") (v "C:/b/c") None;
    find_prefix (v "C:\\") (v "C:\\") (Some (v "C:\\"));
    find_prefix (v "\\\\server\\share\\") (v "\\\\server\\share\\")
      (Some (v "\\\\server\\share\\"));
    find_prefix (v "\\\\server\\share\\") (v "\\\\server\\share\\a")
      (Some (v "\\\\server\\share\\"));
    find_prefix (v "\\\\server\\share\\a") (v "\\\\server\\share\\a")
      (Some (v "\\\\server\\share\\a"));
    find_prefix (v "\\\\server\\share\\a") (v "\\\\server\\share\\b")
      (Some (v "\\\\server\\share\\"));
  end;
  ()

let rem_prefix = test "Fpath.rem_prefix" @@ fun () ->
  let eq = eq_option ~eq:Fpath.equal ~pp:Fpath.pp in
  eq (Fpath.rem_prefix (v "a/b/") (v "a/b")) None;
  eq (Fpath.rem_prefix (v "a/b/") (v "a/b/")) None;
  eq (Fpath.rem_prefix (v "a/b") (v "a/b")) None;
  eq (Fpath.rem_prefix (v "a/b") (v "a/b/")) (Some (v "./"));
  eq (Fpath.rem_prefix (v "a/b") (v "a/b/c")) (Some (v "c"));
  eq (Fpath.rem_prefix (v "a/b") (v "a/b/c/")) (Some (v "c/"));
  eq (Fpath.rem_prefix (v "a/b/") (v "a/b/c")) (Some (v "c"));
  eq (Fpath.rem_prefix (v "a/b/") (v "a/b/c/")) (Some (v "c/"));
  eq (Fpath.rem_prefix (v "a/b") (v "a/b")) None;
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b")) None;
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b/")) None;
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/bc")) None;
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/b")) None;
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b")) None;
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/b/")) (Some (v "./"));
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b/")) None;
  eq (Fpath.rem_prefix (v "/a/b") (v "/a/b/c")) (Some (v "c"));
  eq (Fpath.rem_prefix (v "/a/b/") (v "/a/b/c")) (Some (v "c"));
  eq (Fpath.rem_prefix (v "a") (v "a/b/c")) (Some (v "b/c"));
  if windows then begin
    eq (Fpath.rem_prefix (v "C:\\a") (v "C:\\a\\b")) (Some (v "b"));
  end;
  ()

let relativize = test "Fpath.relativize" @@ fun () ->
  let eq_opt = eq_option ~eq:Fpath.equal ~pp:Fpath.pp in
  let relativize root p result = match Fpath.relativize root p with
  | None -> eq_opt None result
  | Some rel as r ->
      eq_opt r result;
      eqp (Fpath.normalize (Fpath.append root rel)) (Fpath.normalize p);
  in
  relativize (v "/a/") (v "/a") (Some (v "../a"));
  relativize (v "/a/") (v "/a/") (Some (v "./"));
  relativize (v "/a/") (v "/") (Some (v "../"));
  relativize (v "/a/") (v "/../") (Some (v "../"));
  relativize (v "/a/") (v "/../c/d") (Some (v "../c/d"));
  relativize (v "/a/") (v "/../c/d/") (Some (v "../c/d/"));
  relativize (v "/") (v "/../c/d/") (Some (v "c/d/"));
  relativize (v "/") (v "/../c/d") (Some (v "c/d"));
  relativize (v "/") (v "/") (Some (v "./"));
  relativize (v "/") (v "/a") (Some (v "a"));
  relativize (v "/") (v "/a/../b") (Some (v "b"));
  relativize (v "/") (v "/a/../b/") (Some (v "b/"));
  relativize (v "/a/b/") (v "c") None;
  relativize (v "/a/b/") (v "./") None;
  relativize (v "/a/b/") (v "../") None;
  relativize (v "/a/b/") (v "/c") (Some (v "../../c"));
  relativize (v "/a/b/") (v "/c/") (Some (v "../../c/"));
  relativize (v "/a/b/") (v "/c/d/e") (Some (v "../../c/d/e"));
  relativize (v "/a/b/") (v "/c/d/e/../../f") (Some (v "../../c/f"));
  relativize (v "/a/b/") (v "/c/d/e/../../f/") (Some (v "../../c/f/"));
  relativize (v "/a/b/") (v "/./c/d/e/../../f/") (Some (v "../../c/f/"));
  relativize (v "/a/b/") (v "/a/b/c") (Some (v "c"));
  relativize (v "/a/b/") (v "/a/b") (Some (v "../b"));
  relativize (v "/a/b/") (v "/a/b/") (Some (v "./"));
  relativize (v "/a/b/c") (v "/d/e/f") (Some (v "../../../d/e/f"));
  relativize (v "/a/b/c") (v "/a/b/d") (Some (v "../d"));
  relativize (v "a/b") (v "/c") None;
  relativize (v "a/b") (v "c") (Some (v "../../c"));
  relativize (v "a/b") (v "../c") (Some (v "../../../c"));
  relativize (v "a/b") (v "../c/") (Some (v "../../../c/"));
  relativize (v "a/b") (v "c/") (Some (v "../../c/"));
  relativize (v "a/b") (v "a/b/c") (Some (v "c"));
  relativize (v "a/b") (v "a") (Some (v "../../a"));
  relativize (v "a/b") (v "b") (Some (v "../../b"));
  relativize (v "a/b") (v "c") (Some (v "../../c"));
  relativize (v "a/b/c/") (v "a/d") (Some (v "../../d"));
  relativize (v "a/b/c/") (v "a/b") (Some (v "../../b"));
  relativize (v "a/b/c/") (v "a/b/../../../") (Some (v "../../../../"));
  relativize (v "a/b/c/") (v "a/b/../../../a") (Some (v "../../../../a"));
  relativize (v "a/b") (v "a/b/") (Some (v "./"));
  relativize (v "../") (v "./") None;
  relativize (v "../a") (v "b") None;
  relativize (v "../../a") (v "../b") None;
  relativize (v "../a") (v "../b/c") (Some (v "../b/c"));
  relativize (v "../a") (v "../../b") (Some (v "../../b"));
  relativize (v "a") (v "../../b") (Some (v "../../../b"));
  relativize (v "a/c") (v "../../b") (Some (v "../../../../b"));
  if windows then begin
    relativize (v "C:a\\c") (v "C:..\\..\\b") (Some (v "..\\..\\..\\..\\b"));
    relativize (v "C:a\\c") (v "..\\..\\b") None;
    relativize (v "\\\\?\\UNC\\server\\share\\a\\b\\c")
      (v "\\\\?\\UNC\\server\\share\\d\\e\\f") (Some (v "../../../d/e/f"));
  end;
  ()

let is_rooted = test "Fpath.is_rooted" @@ fun () ->
  eq_bool (Fpath.is_rooted ~root:(v "a/b") (v "a/b")) false;
  eq_bool (Fpath.is_rooted ~root:(v "a/b") (v "a/b/")) true;
  eq_bool (Fpath.is_rooted ~root:(v "a/b/") (v "a/b")) false;
  eq_bool (Fpath.is_rooted ~root:(v "a/b/") (v "a/b/")) true;
  eq_bool (Fpath.is_rooted ~root:(v "./") (v "a")) true;
  eq_bool (Fpath.is_rooted ~root:(v "./") (v "a/")) true;
  eq_bool (Fpath.is_rooted ~root:(v "./") (v "a/../")) true;
  eq_bool (Fpath.is_rooted ~root:(v "./") (v "..")) false;
  eq_bool (Fpath.is_rooted ~root:(v "../") (v "./")) false;
  eq_bool (Fpath.is_rooted ~root:(v "../") (v "a")) false;
  eq_bool (Fpath.is_rooted ~root:(v "../") (v "../a")) true;
  eq_bool (Fpath.is_rooted ~root:(v "../a") (v "./")) false;
  eq_bool (Fpath.is_rooted ~root:(v "/a") (v "/a/..")) false;
  eq_bool (Fpath.is_rooted ~root:(v "/a") (v "/a/../a/")) true;
  eq_bool (Fpath.is_rooted ~root:(v "/a") (v "/a/../a")) false;
  eq_bool (Fpath.is_rooted ~root:(v "/") (v "/..")) true;
  ()

let is_abs_rel = test "Fpath.is_abs_rel" @@ fun () ->
  let is_abs bool p =
    let p = v p in
    eq_bool (Fpath.is_abs p) bool;
    eq_bool (Fpath.is_rel p) (not bool);
  in
  is_abs true "/a/b/c";
  if not windows then is_abs true "//a/b/c";
  is_abs false ".";
  is_abs false "..";
  is_abs false "../";
  is_abs false "a";
  is_abs false "a/b";
  is_abs true "/";
  if windows then begin
    is_abs false "C:.";
    is_abs true "C:\\";
    is_abs true "C:/";
    is_abs false "C:bli/bla";
    is_abs false "C:bli/bla";
    is_abs false  "C:rel";
    is_abs true "\\\\server\\share\\";
    is_abs true "\\\\?\\a:\\";
    is_abs true "\\\\?\\a:\\c";
    is_abs true "\\\\?\\server\\share\\";
    is_abs true "\\\\?\\server\\share\\a";
    is_abs true "\\\\?\\UNC\\server\\share\\";
    is_abs true "\\\\?\\UNC\\server\\share\\a";
    is_abs true "\\\\.\\device\\";
    is_abs true "\\\\.\\device\\a";
  end;
  ()

let is_root = test "Fpath.is_root" @@ fun () ->
  eq_bool (Fpath.is_root (v "/")) true;
  eq_bool (Fpath.is_root (v "/..")) false;
  eq_bool (Fpath.is_root (v "/.")) false;
  eq_bool (Fpath.is_root (v "/a")) false;
  eq_bool (Fpath.is_root (v "/a/..")) false;
  eq_bool (Fpath.is_root (v "a")) false;
  eq_bool (Fpath.is_root (v ".")) false;
  eq_bool (Fpath.is_root (v "..")) false;
  if not windows then (eq_bool (Fpath.is_root (v "//")) true);
  if windows then begin
    eq_bool (Fpath.is_root (v "\\\\.\\dev\\")) true;
    eq_bool (Fpath.is_root (v "\\\\.\\dev\\..")) false;
    eq_bool (Fpath.is_root (v "\\\\.\\dev\\a")) false;
    eq_bool (Fpath.is_root (v "\\\\server\\share\\")) true;
    eq_bool (Fpath.is_root (v "\\\\server\\share\\a")) false;
    eq_bool (Fpath.is_root (v "C:\\")) true;
    eq_bool (Fpath.is_root (v "C:a")) false;
    eq_bool (Fpath.is_root (v "C:\\a")) false;
  end;
  ()

let is_current_dir = test "Fpath.is_current_dir" @@ fun () ->
  eq_bool (Fpath.is_current_dir (v ".")) true;
  eq_bool (Fpath.is_current_dir ~prefix:true (v ".")) true;
  eq_bool (Fpath.is_current_dir (v "./")) true;
  eq_bool (Fpath.is_current_dir ~prefix:true (v "./")) true;
  eq_bool (Fpath.is_current_dir (v "./a/..")) false;
  eq_bool (Fpath.is_current_dir ~prefix:true (v "./a/..")) true;
  eq_bool (Fpath.is_current_dir (v "/.")) false;
  if windows then begin
    eq_bool (Fpath.is_current_dir (v "\\\\.\\dev\\.")) false;
    eq_bool (Fpath.is_current_dir ~prefix:true (v "\\\\.\\dev\\.")) false;
    eq_bool (Fpath.is_current_dir (v "\\\\.\\dev\\.\\")) false;
    eq_bool (Fpath.is_current_dir (v "\\\\server\\share\\.")) false;
    eq_bool (Fpath.is_current_dir (v "\\\\server\\share\\.\\")) false;
    eq_bool (Fpath.is_current_dir (v "C:.")) true;
    eq_bool (Fpath.is_current_dir ~prefix:true (v "C:.")) true;
    eq_bool (Fpath.is_current_dir (v "C:./")) true;
    eq_bool (Fpath.is_current_dir ~prefix:true (v "C:./")) true;
    eq_bool (Fpath.is_current_dir (v "C:./a/..")) false;
    eq_bool (Fpath.is_current_dir ~prefix:true (v "C:./a/..")) true;
  end;
  ()

let is_parent_dir = test "Fpath.is_parent_dir" @@ fun () ->
  eq_bool (Fpath.is_parent_dir (v ".")) false;
  eq_bool (Fpath.is_parent_dir (v "./")) false;
  eq_bool (Fpath.is_parent_dir (v "..")) true;
  eq_bool (Fpath.is_parent_dir ~prefix:true (v "..")) true;
  eq_bool (Fpath.is_parent_dir (v "../")) true;
  eq_bool (Fpath.is_parent_dir ~prefix:true (v "../")) true;
  eq_bool (Fpath.is_parent_dir (v "./a/../..")) false;
  eq_bool (Fpath.is_parent_dir ~prefix:true (v "../a/../..")) true;
  eq_bool (Fpath.is_parent_dir (v "../..")) false;
  eq_bool (Fpath.is_parent_dir (v "/..")) false;
  if windows then begin
    eq_bool (Fpath.is_parent_dir (v "\\\\.\\dev\\.")) false;
    eq_bool (Fpath.is_parent_dir (v "\\\\.\\dev\\.\\")) false;
    eq_bool (Fpath.is_parent_dir (v "\\\\server\\share\\.")) false;
    eq_bool (Fpath.is_parent_dir (v "\\\\server\\share\\.\\")) false;
    eq_bool (Fpath.is_parent_dir (v "C:..")) true;
    eq_bool (Fpath.is_parent_dir (v "C:../")) true;
    eq_bool (Fpath.is_parent_dir (v "C:../a/..")) false;
    eq_bool (Fpath.is_parent_dir ~prefix:true (v "C:../a/..")) true;
  end;
  ()

let is_dotfile = test "Fpath.is_dotfile" @@ fun () ->
  eq_bool (Fpath.is_dotfile (v ".")) false;
  eq_bool (Fpath.is_dotfile (v "..")) false;
  eq_bool (Fpath.is_dotfile (v "a/.")) false;
  eq_bool (Fpath.is_dotfile (v "a/..")) false;
  eq_bool (Fpath.is_dotfile (v "/a/.")) false;
  eq_bool (Fpath.is_dotfile (v "/a/..")) false;
  eq_bool (Fpath.is_dotfile (v "...")) true;
  eq_bool (Fpath.is_dotfile (v ".../")) true;
  eq_bool (Fpath.is_dotfile (v "a/...")) true;
  eq_bool (Fpath.is_dotfile (v "a/.../")) true;
  eq_bool (Fpath.is_dotfile (v "/a/...")) true;
  eq_bool (Fpath.is_dotfile (v "/a/.../")) true;
  eq_bool (Fpath.is_dotfile (v "/a/.../a")) false;
  if windows then begin
    eq_bool (Fpath.is_dotfile (v "\\\\.\\dev\\.")) false;
    eq_bool (Fpath.is_dotfile (v "\\\\.\\dev\\.\\")) false;
    eq_bool (Fpath.is_dotfile (v "\\\\server\\share\\.")) false;
    eq_bool (Fpath.is_dotfile (v "\\\\server\\share\\.\\")) false;
    eq_bool (Fpath.is_dotfile (v "C:.")) false;
    eq_bool (Fpath.is_dotfile (v "C:./")) false;
    eq_bool (Fpath.is_dotfile (v "C:./a/..")) false;
    eq_bool (Fpath.is_dotfile (v "C:..")) false;
    eq_bool (Fpath.is_dotfile (v "C:../")) false;
    eq_bool (Fpath.is_dotfile (v "C:../a/..")) false;
    eq_bool (Fpath.is_dotfile (v "C:../a/...")) true;
    eq_bool (Fpath.is_dotfile (v "C:...")) true;
  end;
  ()

let get_ext = test "Fpath.get_ext" @@ fun () ->
  let eq_ext ?multi p e =
    let p = Fpath.v p in
    eq_str (Fpath.get_ext ?multi p) e;
    eq_str Fpath.(get_ext ?multi (to_dir_path p)) e;
  in
  eq_ext "/" "";
  eq_ext "a/b" "";
  eq_ext "a/b.mli/.." "";
  eq_ext "a/b.mli/..." "";
  eq_ext "a/b." ".";
  eq_ext "a/b.mli" ".mli";
  eq_ext ~multi:true "a/b.mli" ".mli";
  eq_ext "a/b.mli/" ".mli";
  eq_ext "a/.ocamlinit" "";
  eq_ext "a.tar.gz" ".gz";
  eq_ext ~multi:true "a.tar.gz" ".tar.gz";
  eq_ext "a/.emacs.d" ".d";
  eq_ext "a/.emacs.d/" ".d";
  eq_ext ~multi:true "a/.emacs.d" ".d";
  eq_ext "." "";
  eq_ext ".." "";
  eq_ext "..." "";
  eq_ext "...." "";
  eq_ext "....." "";
  eq_ext ".a" "";
  eq_ext ".a." ".";
  eq_ext ".a.." ".";
  eq_ext ".a..." ".";
  eq_ext ".a...." ".";
  eq_ext "a/..." "";
  eq_ext "a.mli/." "";
  eq_ext "a.mli/.." "";
  eq_ext "a/.a" "";
  eq_ext "a/..b" "";
  eq_ext "a/..b.a" ".a";
  eq_ext "a/..b..ac" ".ac";
  eq_ext "/a/b" "";
  eq_ext "/a/b." ".";
  eq_ext "./a." ".";
  eq_ext "./a.." ".";
  eq_ext "./.a." ".";
  eq_ext ~multi:true "." "";
  eq_ext ~multi:true ".." "";
  eq_ext ~multi:true "..." "";
  eq_ext ~multi:true "...." "";
  eq_ext ~multi:true "....." "";
  eq_ext ~multi:true ".a" "";
  eq_ext ~multi:true ".a." ".";
  eq_ext ~multi:true ".a.." "..";
  eq_ext ~multi:true ".a..." "...";
  eq_ext ~multi:true ".a...." "....";
  eq_ext ~multi:true "a/..." "";
  eq_ext ~multi:true "a/.a" "";
  eq_ext ~multi:true "a/.." "";
  eq_ext ~multi:true "a/..b" "";
  eq_ext ~multi:true "a/..b.a" ".a";
  eq_ext ~multi:true "a/..b..ac" "..ac";
  eq_ext ~multi:true "a/.emacs.d" ".d";
  eq_ext ~multi:true "/a/b.mli" ".mli";
  eq_ext ~multi:true "a.tar.gz" ".tar.gz";
  eq_ext ~multi:true "./a." ".";
  eq_ext ~multi:true "./a.." "..";
  eq_ext ~multi:true "./.a." ".";
  eq_ext ~multi:true "./.a.." "..";
  ()

let has_ext = test "Fpath.has_ext" @@ fun () ->
  let has_ext e p bool =
    let p = Fpath.v p in
    eq_bool (Fpath.has_ext e p) bool;
    eq_bool (Fpath.has_ext e (Fpath.to_dir_path p)) bool;
  in
  has_ext "mli" "a/b.mli" true;
  has_ext ".mli" "a/b.mli" true;
  has_ext ".mli" "a/b.mli/" true;
  has_ext ".mli" "a/bmli" false;
  has_ext ".tar.gz" "a/f.tar.gz" true;
  has_ext "tar.gz" "a/f.tar.gz" true;
  has_ext ".gz" "a/f.tar.gz" true;
  has_ext ".tar" "a/f.tar.gz" false;
  has_ext ".cache" "a/.cache" false;
  has_ext "" "a/b" false;
  has_ext "" "a/b." true;
  has_ext "." "a/b." true;
  has_ext "." "." false;
  has_ext "." ".." false;
  has_ext "." "..." false;
  has_ext "." "...a" false;
  has_ext "." "...a." true;
  has_ext "." "...a.." true;
  has_ext ".." "...a.." true;
  has_ext ".." "...a.." true;
  has_ext "" "." false;
  has_ext "" ".." false;
  has_ext "" "..." false;
  has_ext "" "...a" false;
  has_ext "" "...a." true;
  has_ext "" "...a.." true;
  has_ext ".." "." false;
  has_ext ".." ".." false;
  has_ext ".." "..a." false;
  has_ext ".." "..a.." true;
  has_ext ".." "..." false;
  has_ext ".." "...a." false;
  has_ext ".." "...a.." true;
  has_ext "..." ".." false;
  has_ext "..." "..." false;
  has_ext "..." "...." false;
  has_ext "..." ".a..." true;
  has_ext "tar.gz" "a/ftar.gz" false;
  has_ext "tar.gz" "a/tar.gz" false;
  has_ext "tar.gz" "a/.tar.gz" false;
  has_ext ".tar" "a/f.tar.gz" false;
  has_ext ".ocamlinit" ".ocamlinit" false;
  has_ext ".ocamlinit/" ".ocamlinit" false;
  has_ext ".ocamlinit" "..ocamlinit" false;
  has_ext "..ocamlinit" "...ocamlinit" false;
  has_ext "..ocamlinit" ".a..ocamlinit" true;
  has_ext "..a" ".." false;
  ()

let exists_ext = test "Fpath.exists_ext" @@ fun () ->
  let exists_ext ?multi p bool =
    let p = Fpath.v p in
    eq_bool (Fpath.exists_ext ?multi p) bool;
    eq_bool (Fpath.exists_ext ?multi (Fpath.to_dir_path p)) bool;
  in
  exists_ext "a/f" false;
  exists_ext "a/f." true;
  exists_ext "a/f.gz" true;
  exists_ext ~multi:true "a/f.gz" false;
  exists_ext "a/f.tar.gz" true;
  exists_ext ~multi:true "a/f.tar.gz" true;
  exists_ext "a/f.tar.gz/" true;
  exists_ext ".emacs.d" true;
  exists_ext ".emacs.d/" true;
  exists_ext ~multi:true ".emacs.d/" false;
  exists_ext ~multi:true "..emacs.d/" false;
  exists_ext ~multi:true "..emacs..d/" true;
  exists_ext ".ocamlinit" false;
  exists_ext ~multi:true "a/.a.." true;
  exists_ext "a/.a." true;
  exists_ext "a/..." false;
  exists_ext "a/.." false;
  exists_ext "a/." false;
  ()

let add_ext = test "Fpath.add_ext" @@ fun () ->
  app_raises ~pp:Fpath.pp (Fpath.add_ext "/") (v "a/b/c");
  let eq_add_ext ext p p' =
    let p, p' = Fpath.v p, Fpath.v p' in
    eqp (Fpath.add_ext ext p) p';
    eqp (Fpath.add_ext ext (Fpath.to_dir_path p)) (Fpath.to_dir_path p');
  in
  eq_add_ext "mli" "a/b" "a/b.mli";
  eq_add_ext ".mli" "a/b" "a/b.mli";
  eq_add_ext ".mli" "a/b/" "a/b.mli/";
  eq_add_ext ".mli" "/" "/";
  eq_add_ext ".mli" "a/b/.." "a/b/..";
  eq_add_ext "." "a/b" "a/b.";
  eq_add_ext "" "a/b" "a/b";
  eq_add_ext "tar.gz" "a/f" "a/f.tar.gz";
  eq_add_ext ".tar.gz" "a/f" "a/f.tar.gz";
  eq_add_ext "gz" "a/f.tar" "a/f.tar.gz";
  eq_add_ext ".gz" "a/f.tar" "a/f.tar.gz";
  eq_add_ext "" "/" "/";
  eq_add_ext "a" "/" "/";
  eq_add_ext ".a" "/" "/";
  ()

let rem_ext = test "Fpath.rem_ext" @@ fun () ->
  let eq_rem_ext ?multi p p' =
    let p, p' = Fpath.v p, Fpath.v p' in
    eqp (Fpath.rem_ext ?multi p) p';
    eqp (Fpath.rem_ext ?multi (Fpath.to_dir_path p)) (Fpath.to_dir_path p');
  in
  eq_rem_ext "/" "/";
  eq_rem_ext "/a/b" "/a/b";
  eq_rem_ext "/a/b.mli" "/a/b";
  eq_rem_ext "/a/b.mli/" "/a/b/";
  eq_rem_ext "/a/b.mli/.." "/a/b.mli/..";
  eq_rem_ext "/a/b.mli/." "/a/b.mli/.";
  eq_rem_ext "a/.ocamlinit" "a/.ocamlinit";
  eq_rem_ext ~multi:true "a/.ocamlinit" "a/.ocamlinit";
  eq_rem_ext "a/.emacs.d" "a/.emacs";
  eq_rem_ext "f.tar.gz" "f.tar";
  eq_rem_ext ~multi:true "f.tar.gz" "f";
  eq_rem_ext ~multi:true "f.tar.gz/" "f/";
  eq_rem_ext "a/..." "a/...";
  eq_rem_ext "a/..a." "a/..a";
  eq_rem_ext "a/..a.." "a/..a.";
  eq_rem_ext ~multi:true "a/..a.." "a/..a";
  eq_rem_ext ".tar.gz" ".tar";
  eq_rem_ext ~multi:true "a/.tar.gz" "a/.tar";
  eq_rem_ext ~multi:true ".tar" ".tar";
  eq_rem_ext ~multi:true "/.tar" "/.tar";
  ()

let set_ext = test "Fpath.set_ext" @@ fun () ->
  app_raises ~pp:Fpath.pp (Fpath.set_ext "/") (v "a/b/c");
  let eq_set_ext ?multi ext p p' =
    let p, p' = Fpath.v p, Fpath.v p' in
    eqp (Fpath.set_ext ?multi ext p) p';
    eqp (Fpath.set_ext ?multi ext (Fpath.to_dir_path p)) (Fpath.to_dir_path p');
  in
  eq_set_ext ".bla" "/a/b" "/a/b.bla";
  eq_set_ext "bla" "/a/b" "/a/b.bla";
  eq_set_ext ".bla" "/a/b.mli" "/a/b.bla";
  eq_set_ext "bla" "/a/b.mli" "/a/b.bla";
  eq_set_ext "bla" "a/.ocamlinit" "a/.ocamlinit.bla";
  eq_set_ext "bla" "a/.emacs.d" "a/.emacs.bla";
  eq_set_ext "bla" "f.tar.gz" "f.tar.bla";
  eq_set_ext ~multi:true "bla" "f.tar.gz" "f.bla";
  eq_set_ext ~multi:true "" "f.tar.gz" "f";
  ()

let split_ext = test "Fpath.split_ext" @@ fun () ->
  let eq_split ?multi p q ext =
    let p, q = Fpath.v p, Fpath.v q in
    let check p q =
      let q', ext' = Fpath.split_ext ?multi p in
      eq_str ext ext';
      eqp q q';
      eqp p (Fpath.add_ext ext q');
    in
    check p q;
    check (Fpath.to_dir_path p) (Fpath.to_dir_path q)
  in
  eq_split "/a/b" "/a/b" "";
  eq_split "/a/b.mli" "/a/b" ".mli";
  eq_split "a/.ocamlinit" "a/.ocamlinit" "";
  eq_split "f.tar.gz" "f.tar" ".gz";
  eq_split ~multi:true "f.tar.gz" "f" ".tar.gz";
  eq_split ~multi:true ".tar" ".tar" "";
  eq_split ~multi:true "/.tar" "/.tar" "";
  eq_split ~multi:true "/.tar.gz" "/.tar" ".gz";
  eq_split ~multi:true "/.tar.gz/.." "/.tar.gz/.." "";
  ()

let suite = suite "Fpath module"
    [ of_string;
      dir_sep;
      is_seg;
      add_seg;
      append;
      split_volume;
      segs;
      is_dir_path;
      is_file_path;
      to_dir_path;
      filename;
      split_base;
      base;
      basename;
      parent;
      rem_empty_seg;
      normalize;
      is_prefix;
      find_prefix;
      rem_prefix;
      relativize;
      is_rooted;
      is_abs_rel;
      is_root;
      is_current_dir;
      is_parent_dir;
      is_dotfile;
      get_ext;
      has_ext;
      exists_ext;
      add_ext;
      rem_ext;
      set_ext;
      split_ext; ]

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
