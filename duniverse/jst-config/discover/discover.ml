open Base
module C = Configurator.V1

let eventfd_code = {|
#include <sys/eventfd.h>

int main()
{
  int fd = eventfd(0, 0);
  return 0;
}
|}

let posix_timers_code = {|
#include <time.h>

int main()
{
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  clock_settime(CLOCK_REALTIME, &ts);
  clock_getres(CLOCK_REALTIME, &ts);
  return 0;
}
|}

type posix_timers =
  | Available of { need_lrt : bool }
  | Not_available

let timerfd_code = {|
#include <sys/timerfd.h>

int main()
{
  timerfd_create(0, 0);
  return 0;
}
|}

let wordexp_code = {|
#include <wordexp.h>

int main()
{
  wordexp_t w;
  wordexp("", &w, 0);
  return 0;
}
|}

let thread_id_code ~thread_id_method ~thread_id_header = Printf.sprintf {|
#define JSC_THREAD_ID_METHOD %d
#include "%s"

int main ()
{
  GET_THREAD_ID;
  return 0;
}
|} thread_id_method thread_id_header

let msg_nosignal_code = {|
#include <sys/types.h>
#include <sys/socket.h>

int main()
{
   send(0, "", 0, MSG_NOSIGNAL);
   return 0;
}
|}

let so_nosigpipe_code = {|
#include <sys/types.h>
#include <sys/socket.h>

int main()
{
   send(0, "", 0, SO_NOSIGPIPE);
   return 0;
}
|}

let fdatasync_code = {|
#include <unistd.h>

int main()
{
  fdatasync(0);
  return 0;
}
|}

let thread_cputime_code = {|
#include <pthread.h>
#include <time.h>

int main()
{
   clockid_t clock;
   pthread_getcpuclockid(pthread_self(), &clock);
   return 0;
}
|}

let recvmmsg_code = {|
#define _GNU_SOURCE
#include <sys/socket.h>

int main () {
  recvmmsg(0, 0, 0, 0, 0);
  return 0;
}
|}

let mkostemp_code = {|
#include <stdlib.h>

int main () {
  mkostemp("", 0);
  return 0;
}
|}

let pthread_np = {|
#define _GNU_SOURCE
#include <pthread.h>
#include <sys/types.h>

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__APPLE__)
#include <pthread_np.h>
#endif

int main()
{
  pthread_t tid;
  cpu_set_t cpuset;

  tid = pthread_self();
  pthread_getaffinity_np(tid, sizeof(cpu_set_t), &cpuset);

  return 0;
}
|}

let () =
  C.main ~name:"config_h" (fun c ->
    let posix_timers =
      if C.c_test c posix_timers_code ~link_flags:["-lrt"] then
        Available { need_lrt = true }
      else if C.c_test c posix_timers_code then
        Available { need_lrt = false }
      else
        Not_available
    in

    let posix_timers, need_lrt =
      match posix_timers with
      | Available { need_lrt } -> true, need_lrt
      | Not_available         -> false, false
    in

    let thread_id_method =
      let thread_id_header = Caml.Filename.concat (Caml.Sys.getcwd ()) "thread_id.h" in
      List.find [1; 2] ~f:(fun thread_id_method ->
        C.c_test c (thread_id_code ~thread_id_method ~thread_id_header))
      |> Option.value ~default:(-1)
    in

    let linux =
      let system = C.ocaml_config_var_exn c "system" in
      (* Possible values for this field: linux, linux_elf, linux_eabi, ... *)
      String.is_prefix system ~prefix:"linux" ||
      String.equal system "elf"
    in

    let simple_vars =
      List.map ~f:(fun (v, code, link_flags) ->
        (v, C.C_define.Value.Switch (C.c_test c code ~link_flags)))
        [ "EVENTFD"          , eventfd_code          , []
        ; "TIMERFD"          , timerfd_code          , []
        ; "WORDEXP"          , wordexp_code          , []
        ; "MSG_NOSIGNAL"     , msg_nosignal_code     , []
        ; "SO_NOSIGPIPE"     , so_nosigpipe_code     , []
        ; "FDATASYNC"        , fdatasync_code        , []
        ; "RECVMMSG"         , recvmmsg_code         , []
        ; "THREAD_CPUTIME"   , thread_cputime_code   , ["-lpthread"]
        ; "PTHREAD_NP"       , pthread_np            , ["-lpthread"]
        ; "MKOSTEMP"         , mkostemp_code         , []
        ]
    in

    let rlimit_vars =
      if C.c_test c "#include <sys/resource.h>\nint main() { return 0; }" then
        C.C_define.import c ~includes:["sys/resource.h"]
          [ "RLIMIT_AS"  , Switch
          ; "RLIMIT_NICE", Switch
          ]
      else
        [ "RLIMIT_AS"  , Switch false
        ; "RLIMIT_NICE", Switch false
        ]
    in

    let ocaml_vars =
      C.C_define.import c ~includes:["caml/config.h"]
        [ "ARCH_BIG_ENDIAN", Switch
        ; "ARCH_SIXTYFOUR" , Switch
        ]
    in

    let vars =
      List.concat
        [ rlimit_vars
        ; ocaml_vars
        ; simple_vars
        ; [ "POSIX_TIMERS"    , Switch posix_timers
          ; "THREAD_ID_METHOD", Int thread_id_method
          ; "LINUX_EXT"       , Switch linux
          ]
        ]
    in

    let jsc_vars =
      List.map vars ~f:(fun (name, v) -> ("JSC_" ^ name, v))
    in

    C.C_define.gen_header_file c ~fname:"config.h" jsc_vars;

    let rt_flags : Sexp.t =
      if need_lrt
      then List [Atom "-lrt"]
      else List []
    in
    Stdio.Out_channel.write_all "rt-flags" ~data:(Sexp.to_string rt_flags))
