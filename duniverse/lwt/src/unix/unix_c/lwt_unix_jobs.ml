(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



module type Job = sig
  type 'a t
end

module Make(Job : Job) = struct
  external access_job : string -> Unix.access_permission list -> unit Job.t = "lwt_unix_access_job"
  external chdir_job : string -> unit Job.t = "lwt_unix_chdir_job"
  external chmod_job : string -> int -> unit Job.t = "lwt_unix_chmod_job"
  external chown_job : string -> int -> int -> unit Job.t = "lwt_unix_chown_job"
  external chroot_job : string -> unit Job.t = "lwt_unix_chroot_job"
  external close_job : Unix.file_descr -> unit Job.t = "lwt_unix_close_job"
  external fchmod_job : Unix.file_descr -> int -> unit Job.t = "lwt_unix_fchmod_job"
  external fchown_job : Unix.file_descr -> int -> int -> unit Job.t = "lwt_unix_fchown_job"
  external fdatasync_job : Unix.file_descr -> unit Job.t = "lwt_unix_fdatasync_job"
  external fsync_job : Unix.file_descr -> unit Job.t = "lwt_unix_fsync_job"
  external ftruncate_job : Unix.file_descr -> int -> unit Job.t = "lwt_unix_ftruncate_job"
  external ftruncate_64_job : Unix.file_descr -> int64 -> unit Job.t = "lwt_unix_ftruncate_64_job"
  external link_job : string -> string -> unit Job.t = "lwt_unix_link_job"
  external lseek_job : Unix.file_descr -> int -> Unix.seek_command -> int Job.t = "lwt_unix_lseek_job"
  external lseek_64_job : Unix.file_descr -> int64 -> Unix.seek_command -> int64 Job.t = "lwt_unix_lseek_64_job"
  external mkdir_job : string -> int -> unit Job.t = "lwt_unix_mkdir_job"
  external mkfifo_job : string -> int -> unit Job.t = "lwt_unix_mkfifo_job"
  external rename_job : string -> string -> unit Job.t = "lwt_unix_rename_job"
  external rmdir_job : string -> unit Job.t = "lwt_unix_rmdir_job"
  external symlink_job : string -> string -> unit Job.t = "lwt_unix_symlink_job"
  external tcdrain_job : Unix.file_descr -> unit Job.t = "lwt_unix_tcdrain_job"
  external tcflow_job : Unix.file_descr -> Unix.flow_action -> unit Job.t = "lwt_unix_tcflow_job"
  external tcflush_job : Unix.file_descr -> Unix.flush_queue -> unit Job.t = "lwt_unix_tcflush_job"
  external tcsendbreak_job : Unix.file_descr -> int -> unit Job.t = "lwt_unix_tcsendbreak_job"
  external truncate_job : string -> int -> unit Job.t = "lwt_unix_truncate_job"
  external truncate_64_job : string -> int64 -> unit Job.t = "lwt_unix_truncate_64_job"
  external unlink_job : string -> unit Job.t = "lwt_unix_unlink_job"
end
