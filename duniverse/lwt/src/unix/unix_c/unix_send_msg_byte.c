/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#include <caml/mlvalues.h>

extern value lwt_unix_send_msg(value val_fd, value val_n_iovs, value val_iovs,
                               value val_n_fds, value val_fds, value val_dest);

CAMLprim value lwt_unix_send_msg_byte(value * argv, int argc)
{
  value val_fd = argv[0];
  value val_n_iovs = argv[1];
  value val_iovs = argv[2];
  value val_n_fds = argv[3];
  value val_fds = argv[4];
  value val_dest = argv[5];

  return lwt_unix_send_msg(val_fd, val_n_iovs, val_iovs, val_n_fds, val_fds, val_dest);
}

#endif
