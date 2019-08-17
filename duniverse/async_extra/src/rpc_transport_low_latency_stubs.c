#include <stdlib.h>
#include <errno.h>
#include <sys/uio.h>

#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>

#include <jane_common.h>

CAMLprim value
async_extra_rpc_writev2(value v_fd,
                        value v_buf1,
                        value v_pos1,
                        value v_len1,
                        value v_buf2,
                        value v_pos2,
                        value v_len2)
{
  ssize_t ret;
  struct iovec iovecs[2];
  if (v_len2 == Val_long(0))
    ret = write(Int_val(v_fd),
                (char*)Caml_ba_data_val(v_buf1) + Long_val(v_pos1),
                Long_val(v_len1));
  else {
    iovecs[0].iov_base = (char*)Caml_ba_data_val(v_buf1) + Long_val(v_pos1);
    iovecs[0].iov_len  = Long_val(v_len1);
    iovecs[1].iov_base = (char*)Caml_ba_data_val(v_buf2) + Long_val(v_pos2);
    iovecs[1].iov_len  = Long_val(v_len2);
    ret = writev(Int_val(v_fd), iovecs, 2);
  }
  if (ret == -1) ret = -errno;
  return Val_long(ret);
}

CAMLprim value
async_extra_rpc_writev2_byte(value *argv, int argn __unused)
{
  return async_extra_rpc_writev2(argv[0],
                                 argv[1],
                                 argv[2],
                                 argv[3],
                                 argv[4],
                                 argv[5],
                                 argv[6]);
}
