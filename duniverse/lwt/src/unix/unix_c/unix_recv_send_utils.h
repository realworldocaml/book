/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#pragma once

#include "lwt_config.h"

/*
 * header included in:
 * unix_send
 * unix_bytes_send
 * unix_recv
 * unix_bytes_recv
 * unix_recvfrom
 * unix_bytes_recvfrom
 * unix_sendto
 * unix_bytes_sendto
 * unix_recv_msg
 * unix_bytes_recv_msg
 * unix_send_msg
 * unix_getaddrinfo_job
 */

#if !defined(LWT_ON_WINDOWS)

#include <caml/mlvalues.h>
#include <caml/socketaddr.h>
#include <caml/version.h>
#include <sys/socket.h>
#include <sys/uio.h>

#if OCAML_VERSION_MAJOR < 5
#define caml_unix_socket_domain_table socket_domain_table
#define caml_unix_socket_type_table socket_type_table
#endif

extern int msg_flag_table[];
extern int caml_unix_socket_domain_table[];
extern int caml_unix_socket_type_table[];
extern void get_sockaddr(value mladdr, union sock_addr_union *addr /*out*/,
                         socklen_t *addr_len /*out*/);
value wrapper_recv_msg(int fd, int n_iovs, struct iovec *iovs);
value wrapper_send_msg(int fd, int n_iovs, struct iovec *iovs,
                       value val_n_fds, value val_fds, value dest);
#endif
