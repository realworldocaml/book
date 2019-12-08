/* Core_unix support functions written in C. */

#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/syscall.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include <sys/statvfs.h>

#if defined (__FreeBSD__) || defined (__OpenBSD__)
#include <ufs/ufs/quota.h>
#else
#include <sys/quota.h>
#endif

#include <sys/mount.h>

#ifndef __USE_ISOC99
# define __USE_ISOC99
#endif
#include <math.h>
#include <arpa/inet.h>

#include "config.h"
#include "ocaml_utils.h"

#define MAX_ERROR_LEN 4096

CAMLprim value extended_ml_seteuid(value euid)
{
  if (seteuid(Int_val(euid))) uerror("seteuid", Nothing);
  return Val_unit;
}

CAMLprim value extended_ml_setreuid(value uid, value euid)
{
  if (setreuid(Int_val(uid),Int_val(euid))) uerror("setreuid", Nothing);
  return Val_unit;
}

CAMLprim value extended_ml_setegid(value egid)
{
  if (seteuid(Int_val(egid)) == -1) uerror("setegid", Nothing);
  return Val_unit;
}

CAMLprim value statvfs_stub (value v_path)
{
  CAMLparam1(v_path);
  CAMLlocal1(v_stat);
  struct statvfs s;
  int ret, len = caml_string_length(v_path) + 1;
  char *pathname = caml_stat_alloc(len);
  memcpy(pathname, String_val(v_path), len);
  caml_enter_blocking_section();
  ret = statvfs(pathname,&s);
  caml_leave_blocking_section();
  caml_stat_free(pathname);
  if (ret != 0) uerror("statvfs",v_path);
  v_stat = caml_alloc(11, 0);
  Store_field(v_stat, 0, Val_int(s.f_bsize));
  Store_field(v_stat, 1, Val_int(s.f_frsize));
  Store_field(v_stat, 2, Val_int(s.f_blocks));
  Store_field(v_stat, 3, Val_int(s.f_bfree));
  Store_field(v_stat, 4, Val_int(s.f_bavail));
  Store_field(v_stat, 5, Val_int(s.f_files));
  Store_field(v_stat, 6, Val_int(s.f_ffree));
  Store_field(v_stat, 7, Val_int(s.f_favail));
  Store_field(v_stat, 8, Val_int(s.f_fsid));
  Store_field(v_stat, 9, Val_int(s.f_flag));
  Store_field(v_stat,10, Val_int(s.f_namemax));
  CAMLreturn(v_stat);
}

CAMLprim value getloadavg_stub (value v_unit __unused)
{
  CAMLparam0();
  CAMLlocal1(v_ret);
  double loadavg[3];
  int ret = getloadavg(loadavg,3);
  if (ret < 0) uerror("getloadavg",Nothing);
  v_ret = caml_alloc_tuple(3);
  Store_field(v_ret, 2, caml_copy_double(ret >= 3 ? loadavg[2] : NAN));
  Store_field(v_ret, 1, caml_copy_double(ret >= 2 ? loadavg[1] : NAN));
  Store_field(v_ret, 0, caml_copy_double(ret >= 1 ? loadavg[0] : NAN));
  CAMLreturn(v_ret);
}

#if defined (__FreeBSD__) || defined (__OpenBSD__) /* BSD */

#  define quota_control(device, cmd, id, parg)  \
     quotactl((device), (cmd), (id), (parg))
#  define QUOTA_BYTES_PER_SPACE_UNIT DEV_BSIZE
#  define QUOTA_SPACE_USED(quota) ((quota).dqb_curblocks)
#  define QUOTA_MODIFY_COMMAND Q_SETQUOTA
#  define QUOTA_SET_VALID_FIELDS(quota) ((void)quota)

#elif defined (__APPLE__) /* Mac OS */

#  define quota_control(device, cmd, id, parg)  \
     quotactl((device), (cmd), (id), (parg))
#  define QUOTA_BYTES_PER_SPACE_UNIT 1
#  define QUOTA_SPACE_USED(quota) ((quota).dqb_curbytes)
#  define QUOTA_MODIFY_COMMAND Q_SETQUOTA
#  define QUOTA_SET_VALID_FIELDS(quota) ((void)quota)

#elif defined (_LINUX_QUOTA_VERSION) && _LINUX_QUOTA_VERSION < 2

#include <linux/fs.h> /* needed to build with musl */
#  define quota_control(device, cmd, id, parg)  \
     quotactl((cmd), (device), (id), (parg))
#  define QUOTA_BYTES_PER_SPACE_UNIT BLOCK_SIZE
#  define QUOTA_SPACE_USED(quota) ((quota).dqb_curblocks)
#  define QUOTA_MODIFY_COMMAND Q_SETQLIM
#  define QUOTA_SET_VALID_FIELDS(quota) ((void)quota)

#else /* _LINUX_QUOTA_VERSION >= 2 or not defined, GLIBC 2.25+ */

#include <linux/fs.h> /* needed to build with musl */
#  define quota_control(device, cmd, id, parg)  \
     quotactl((cmd), (device), (id), (parg))
#  define QUOTA_BYTES_PER_SPACE_UNIT BLOCK_SIZE
#  define QUOTA_SPACE_USED(quota) ((quota).dqb_curspace)
#  define QUOTA_MODIFY_COMMAND Q_SETQUOTA
#  define QUOTA_SET_VALID_FIELDS(quota) \
     do { (quota).dqb_valid = QIF_LIMITS | QIF_TIMES; } while (0)

#endif

int quota_command (value v_user_or_group, int command) {
  if (v_user_or_group == caml_hash_variant("User"))
    return QCMD(command, USRQUOTA);

  if (v_user_or_group == caml_hash_variant("Group"))
    return QCMD(command, GRPQUOTA);

  caml_failwith("Unix.Quota: I only know about `User and `Group");
}

CAMLprim value quota_query (value v_user_or_group, value v_id, value v_path)
{
  int id, cmd;
  struct dqblk quota;
  int64_t bytes_used, bytes_soft, bytes_hard;
  CAMLparam3(v_user_or_group, v_id, v_path);
  CAMLlocal3(v_ret, v_bytes_limit, v_inodes_limit);

  id  = Int_val(v_id);
  cmd = quota_command(v_user_or_group, Q_GETQUOTA);

  memset(&quota, 0, sizeof(quota));
  if (quota_control(String_val(v_path), cmd, id, (caddr_t)&quota))
    unix_error(errno, "Unix.Quota: unable to query quota", v_path);

  bytes_used = QUOTA_BYTES_PER_SPACE_UNIT * (int64_t) QUOTA_SPACE_USED(quota);
  bytes_soft = QUOTA_BYTES_PER_SPACE_UNIT * (int64_t) quota.dqb_bsoftlimit;
  bytes_hard = QUOTA_BYTES_PER_SPACE_UNIT * (int64_t) quota.dqb_bhardlimit;

  v_bytes_limit = caml_alloc_small(3, 0);
  Store_field(v_bytes_limit, 0, caml_alloc_int63(bytes_soft));
  Store_field(v_bytes_limit, 1, caml_alloc_int63(bytes_hard));
  Store_field(v_bytes_limit, 2, caml_copy_double((double)quota.dqb_btime));

  v_inodes_limit = caml_alloc_small(3, 0);
  Store_field(v_inodes_limit, 0, caml_alloc_int63(quota.dqb_isoftlimit));
  Store_field(v_inodes_limit, 1, caml_alloc_int63(quota.dqb_ihardlimit));
  Store_field(v_inodes_limit, 2, caml_copy_double((double)quota.dqb_itime));

  v_ret = caml_alloc_small(4, 0);
  Store_field(v_ret, 0, v_bytes_limit);
  Store_field(v_ret, 1, caml_alloc_int63(bytes_used));
  Store_field(v_ret, 2, v_inodes_limit);
  Store_field(v_ret, 3, caml_alloc_int63(quota.dqb_curinodes));

  CAMLreturn(v_ret);
}

CAMLprim value quota_modify (value v_user_or_group, value v_id,
                             value v_path, value v_bytes_limit, value v_inodes_limit)
{
  int id, cmd;
  struct dqblk quota;
  CAMLparam5(v_user_or_group, v_id, v_path, v_bytes_limit, v_inodes_limit);

  id  = Int_val(v_id);
  cmd = quota_command(v_user_or_group, QUOTA_MODIFY_COMMAND);

  memset(&quota, 0, sizeof(quota));

  quota.dqb_bsoftlimit = Int63_val(Field(v_bytes_limit, 0)) / QUOTA_BYTES_PER_SPACE_UNIT;
  quota.dqb_bhardlimit = Int63_val(Field(v_bytes_limit, 1)) / QUOTA_BYTES_PER_SPACE_UNIT;
  quota.dqb_btime      = (time_t) Double_val(Field(v_bytes_limit, 2));

  quota.dqb_isoftlimit = Int63_val(Field(v_inodes_limit, 0));
  quota.dqb_ihardlimit = Int63_val(Field(v_inodes_limit, 1));
  quota.dqb_itime      = (time_t) Double_val(Field(v_inodes_limit, 2));

  QUOTA_SET_VALID_FIELDS(quota);

  if (quota_control(String_val(v_path), cmd, id, (caddr_t)&quota))
    unix_error(errno, "Unix.Quota: unable to set quota", v_path);

  CAMLreturn(Val_unit);
}

CAMLprim value extended_ml_htonl (value v_num) {
  return caml_copy_int32(htonl(Int32_val(v_num)));
}

CAMLprim value extended_ml_ntohl (value v_num) {
  return caml_copy_int32(ntohl(Int32_val(v_num)));
}
