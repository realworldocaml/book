%{
open Nettypes;;
open Holparselib;;
open Holtypes;;
open Parserlib;;
open Int64;;
open Ocamllib;;
open Libcalls;;
open Tcpcbtypes;;
open Tcpcbparserlib;;

exception Fatal of string;;

let uint_ipaddr_to_dstr ip fill=
  (Int64.to_string ((ip &. ((*uint*) 0xFF000000L)) >> 24)) ^ fill ^
  (Int64.to_string ((ip &. ((*uint*) 0x00FF0000L)) >> 16)) ^ fill ^
  (Int64.to_string ((ip &. ((*uint*) 0x0000FF00L)) >> 8)) ^ fill ^
  (Int64.to_string ((ip &. ((*uint*) 0x000000FFL))));;

let uint_ip_to_ip ip =
  ip_of_string (uint_ipaddr_to_dstr ip ".");;

let uint_netmask_to_netmask nm =
  netmask_of_int (Int64.to_int nm);;

let uint_ipopt_to_ipopt ipopt =
  match ipopt with
    None -> None
  | Some x -> Some (uint_ip_to_ip x);;

let uint_port_to_port port =
  port_of_int (Int64.to_int port);;

let uint_portopt_to_portopt portopt =
  match portopt with
    None -> None
  | Some x -> Some (uint_port_to_port x);;

let uint_pairopt_to_int_pairopt x =
  match x with
    None -> None
  | Some (x,y) -> Some (Int64.to_int x, Int64.to_int y);;

let uint_pair_to_int_pair x =
  match x with
    (a,b) -> (Int64.to_int a, Int64.to_int b);;

let rec foldlist l s r =
  match l with
    [] ->  (s, r)
  | x::xs ->
      let n = x s r in
      foldlist xs (fst n) (snd n) ;;

%}

/* Common tokens */
%token <int64> INT
%token <string> STRING
%token <string> COMMENT
%token <string> IDENT
%token LPAREN RPAREN UNIT RECSTART RECEND
%token COMMA DOT SC ASSIGN HASH LSQBRKT RSQBRKT
%token TRUE FALSE IP PORT IFID NETMASK NONE SOME SCOMMENTSTART
%token SCOMMENTEND MINUS SOCK_DGRAM SOCK_STREAM

/* Spec3 tokens */
%token LN1_HOST LN1_EPSILON

/* LIB tokens */
%token ACCEPT BIND CLOSE CONNECT DISCONNECT DUP DUPFD GETIFADDRS GETFILEFLAGS
%token SETFILEFLAGS GETSOCKNAME GETPEERNAME GETSOCKBOPT
%token GETSOCKNOPT GETSOCKTOPT SETSOCKBOPT SETSOCKNOPT
%token SETSOCKTOPT LISTEN PSELECT RECV SEND SHUTDOWN
%token SOCKATMARK SOCKET GETSOCKERR GETSOCKLISTENING
%token LHCALL LHRETURN LHOK LHFAIL TID FD

/* LIB flags */
%token TO_NONBLOCK TO_ASYNC TSO_BSDCOMPAT TSO_REUSEADDR
%token TSO_KEEPALIVE TSO_OOBINLINE TSO_DONTROUTE
%token TSO_SNDBUF TSO_RCVBUF TSO_SNDLOWAT TSO_RCVLOWAT
%token TSO_LINGER TSO_SNDTIMEO TSO_RCVTIMEO TSO_BROADCAST
%token TMSG_PEEK TMSG_OOB TMSG_WAITALL TMSG_DONTWAIT
%token TSIGABRT TSIGALRM TSIGBUS TSIGCHLD TSIGCONT TSIGFPE
%token TSIGHUP TSIGILL TSIGINT TSIGKILL TSIGPIPE TSIGQUIT
%token TSIGSEGV TSIGSTOP TSIGTERM TSIGTSTP TSIGTTIN TSIGTTOU
%token TSIGUSR1 TSIGUSR2 TSIGPOLL TSIGPROF TSIGSYS TSIGTRAP
%token TSIGURG TSIGVTALRM TSIGXCPU TSIGXFSZ

/* Network datagram tokens */
%token IS1 IS2 IS3 IS4 PS1 PS2 PS3 PS4 SEQNO ACKNO URG ACK PSH RST
%token SYN FIN WIN WS URP MSS TS DATA ICMP_UNREACH ICMP_PARAMPROB ICMP_SOURCE_QUENCH
%token ICMP_TIME_EXCEEDED ICMP_HOST ICMP_PORT
%token ICMP_NET ICMP_SRCFAIL ICMP_NET_UNKNOWN
%token ICMP_HOST_UNKNOWN ICMP_ISOLATED ICMP_TOSNET
%token ICMP_TOSHOST ICMP_PREC_VIOLATION ICMP_PREC_CUTOFF
%token ICMP_NEEDFRAG ICMP_PROTOCOL ICMP_NET_PROHIB
%token ICMP_HOST_PROHIB ICMP_FILTER_PROHIB ICMP_BADHDR
%token ICMP_NEEDOPT ICMP_QUENCH ICMP_INTRANS
%token ICMP_REASS PROTO_TCP PROTO_UDP PROTO
%token TCP UDP ICMP WORD16 TSSEQ SEQLOCAL
%token SEQFOREIGN LHSEND LHLOOP LHRECV TYPE BYTE
%token TIMEWINDOW TIMEWINDOWCLOSED NEVERTIMER

/* TCP debug tokens */
%token TRACE T_SND_UNA T_SND_MAX T_SND_NXT
%token T_SND_WL1 T_SND_WL2 T_ISS T_SND_WND T_SND_CWND
%token T_SND_SSTHRESH T_RCV_WND T_RCV_NXT T_RCV_UP
%token T_IRS T_RCV_ADV T_SND_RECOVER T_T_MAXSEG
%token T_T_DUPACKS T_T_RTTSEG T_SND_SCALE T_RCV_SCALE
%token T_TS_RECENT T_LAST_ACK_SENT T_TIMED SID

/* TCP debug states */
%token T_CLOSED T_LISTEN T_SYN_SENT T_ESTABLISHED
%token T_CLOSE_WAIT T_FIN_WAIT_1 T_CLOSING T_SYN_RECEIVED
%token T_LAST_ACK T_FIN_WAIT_2 T_TIME_WAIT

/* TCP trace actions */
%token T_TA_OUTPUT T_TA_INPUT T_TA_USER T_TA_RESPOND T_TA_DROP

/* Lh_epsilon transitions */
%token T_LH_EPSILON T_DURATION T_ABSTIME

/* HOL Unix Errors */
%token T_ERR_E2BIG T_ERR_EACCES T_ERR_EADDRINUSE T_ERR_EADDRNOTAVAIL T_ERR_EAFNOSUPPORT
%token T_ERR_EAGAIN  T_ERR_EALREADY T_ERR_EBADF T_ERR_EBADMSG T_ERR_EBUSY, T_ERR_EWOULDBLOCK
%token T_ERR_ECANCELED T_ERR_ECHILD T_ERR_ECONNABORTED T_ERR_ECONNREFUSED T_ERR_ECONNRESET
%token T_ERR_EDEADLK T_ERR_EDESTADDRREQ T_ERR_EDOM T_ERR_EDQUOT T_ERR_EEXIST T_ERR_EFAULT
%token T_ERR_EFBIG T_ERR_EHOSTUNREACH T_ERR_EIDRM T_ERR_EILSEQ T_ERR_EINPROGRESS T_ERR_EINTR
%token T_ERR_EINVAL T_ERR_EIO T_ERR_EISCONN T_ERR_EISDIR T_ERR_ELOOP T_ERR_EMFILE T_ERR_EMLINK
%token T_ERR_EMSGSIZE T_ERR_EMULTIHOP T_ERR_ENAMETOOLONG T_ERR_ENETDOWN T_ERR_ENETRESET
%token T_ERR_ENETUNREACH T_ERR_ENFILE T_ERR_ENOBUFS T_ERR_ENODATA T_ERR_ENODEV T_ERR_ENOENT
%token T_ERR_ENOEXEC T_ERR_ENOLCK T_ERR_ENOLINK T_ERR_ENOMEM T_ERR_ENOMSG T_ERR_ENOPROTOOPT
%token T_ERR_ENOSPC T_ERR_ENOSR T_ERR_ENOSTR T_ERR_ENOSYS T_ERR_ENOTCONN T_ERR_ENOTDIR
%token T_ERR_ENOTEMPTY T_ERR_ENOTSOCK T_ERR_ENOTSUP T_ERR_ENOTTY T_ERR_ENXIO T_ERR_EOPNOTSUPP
%token T_ERR_EOVERFLOW T_ERR_EPERM T_ERR_EPIPE T_ERR_EPROTO T_ERR_EPROTONOSUPPORT T_ERR_EPROTOTYPE
%token T_ERR_ERANGE T_ERR_EROFS T_ERR_ESPIPE T_ERR_ESRCH T_ERR_ESTALE T_ERR_ETIME T_ERR_ETIMEDOUT
%token T_ERR_ETXTBSY T_ERR_EXDEV T_ERR_ESHUTDOWN T_ERR_EHOSTDOWN T_ERR_EUNKNOWN_UNIX_ERROR

%start main
%type <Parserlib.ns_parse_return> main
%start spec3main
%type <Parserlib.spec3_parse_return> spec3main
%%

main:
  timecomment comment main_h { PARSE_RETURN($1,$2,$3) }
;

main_h:
  netcall SC { $1 }
| lib SC    { $1 }
| trace SC  { $1 }
| epsilon SC { $1 }
| abstime COMMENT { $1 } /* no trailing semicolon */

spec3main:
  timecomment comment spec3main_h { SPEC3_PARSE_RETURN($1,$2,$3) }
;

spec3main_h:
| ln1host SC { $1 }
| ln1epsilon SC { $1 }
| ln1abstime COMMENT { $1 } /* no trailing semicolon */

/* Spec3 rules */

ln1host:
| LN1_HOST LPAREN STRING COMMA ln1host_h RPAREN { HOLLN1_HOST($3,$5) }

ln1host_h:
| netcall { $1}
| lib { $1 }
| trace { $1 }

ln1epsilon:
| LN1_EPSILON LPAREN T_DURATION intr intr RPAREN { HOLLN1_EPSILON(DURATION($4,$5)) }

ln1abstime:
| T_ABSTIME intr intr { HOLLN1_ABSTIME($2, $3) }


/* ---------------------- */
/* Network datagram rules */
/* ---------------------- */

netcall:
  LHSEND LPAREN netparam RPAREN { HOLSNDMSG $3 }
| LHLOOP LPAREN netparam RPAREN { HOLLOOPMSG $3 }
| LHRECV LPAREN netparam RPAREN { HOLRCVMSG $3 }
;

netparam:
  TCP LPAREN RECSTART tcpdetail RECEND RPAREN
    { let p = foldlist $4 clear_tcp_status clear_tcp_hol in
      let _ = chk_missing_tcp (fst p) in
      let m = TCPMSG (snd p) in m }
| UDP LPAREN RECSTART udpdetail RECEND RPAREN
    { let p = foldlist $4 clear_udp_status clear_udp_hol in
      let _ = chk_missing_udp (fst p) in
      let m = UDPMSG (snd p) in m }
| ICMP LPAREN RECSTART icmpdetail RECEND RPAREN
    { let p = foldlist $4 clear_icmp_status clear_icmp_hol in
      let _ = chk_missing_icmp (fst p) in
      let m = ICMPMSG (snd p) in m }
;

tcpdetail:
  tcpval { [$1] }
| tcpval SC tcpdetail { $1::$3 }
;

icmpdetail:
  icmpval { [$1] }
| icmpval SC icmpdetail { $1::$3 }
;

udpdetail:
  udpval { [$1] }
| udpval SC udpdetail { $1::$3 }
;

tcpval:
  IS1 ASSIGN ipoption { update_tcp (HOL_TCP_IS1 $3) }
| IS2 ASSIGN ipoption { update_tcp (HOL_TCP_IS2 $3) }
| PS1 ASSIGN portoption { update_tcp (HOL_TCP_PS1 $3) }
| PS2 ASSIGN portoption { update_tcp (HOL_TCP_PS2 $3) }
| SEQNO ASSIGN seqlocal { update_tcp (HOL_TCP_SEQ $3) }
| ACKNO ASSIGN seqforeign { update_tcp (HOL_TCP_ACK $3)  }
| URG ASSIGN bool { update_tcp (HOL_TCP_URG $3) }
| ACK ASSIGN bool { update_tcp (HOL_TCP_ACKF $3) }
| PSH ASSIGN bool { update_tcp (HOL_TCP_PSH $3) }
| RST ASSIGN bool { update_tcp (HOL_TCP_RST $3) }
| SYN ASSIGN bool { update_tcp (HOL_TCP_SYN $3) }
| FIN ASSIGN bool { update_tcp (HOL_TCP_FIN $3) }
| WIN ASSIGN word16 { update_tcp (HOL_TCP_WIN $3) }
| URP ASSIGN word16 { update_tcp (HOL_TCP_URP $3) }
| MSS ASSIGN word16option { update_tcp (HOL_TCP_MSS $3) }
| WS ASSIGN byteoption { update_tcp (HOL_TCP_SCALE $3) }
| TS ASSIGN tsseqoption { update_tcp (HOL_TCP_TS $3) }
| DATA ASSIGN datalist comment { update_tcp (HOL_TCP_DATA $3) }
;

icmpval:
  IS1 ASSIGN ipoption { update_icmp (HOL_ICMP_IS1 $3) }
| IS2 ASSIGN ipoption { update_icmp (HOL_ICMP_IS2 $3) }
| IS3 ASSIGN ipoption { update_icmp (HOL_ICMP_IS3 $3) }
| IS4 ASSIGN ipoption { update_icmp (HOL_ICMP_IS4 $3) }
| PS3 ASSIGN portoption { update_icmp (HOL_ICMP_PS3 $3) }
| PS4 ASSIGN portoption { update_icmp (HOL_ICMP_PS4 $3) }
| PROTO ASSIGN protocol { update_icmp (HOL_ICMP_PROTO $3) }
| SEQNO ASSIGN NONE { update_icmp (HOL_ICMP_SEQ None) }
| TYPE ASSIGN icmptype { update_icmp (HOL_ICMP_TYPE $3) }
;

protocol:
  PROTO_TCP { PROTO_TCP }
| PROTO_UDP { PROTO_UDP }
;

icmptype:
  ICMP_UNREACH icmp_unreach { ICMP_UNREACH $2 }
| ICMP_PARAMPROB icmp_paramprob { ICMP_PARAMPROB $2 }
| ICMP_SOURCE_QUENCH icmp_source_quench { ICMP_SOURCE_QUENCH $2 }
| ICMP_TIME_EXCEEDED icmp_time_exceeded { ICMP_TIME_EXCEEDED $2 }
;

icmp_unreach:
  ICMP_NET { NET }
| ICMP_HOST { HOST }
| ICMP_PROTOCOL { PROTOCOL }
| ICMP_PORT { PORT }
| ICMP_SRCFAIL { SRCFAIL }
| ICMP_NEEDFRAG word16option { NEEDFRAG $2 }
| ICMP_NET_UNKNOWN { NET_UNKNOWN }
| ICMP_HOST_UNKNOWN { HOST_UNKNOWN }
| ICMP_ISOLATED { ISOLATED }
| ICMP_NET_PROHIB { NET_PROHIB }
| ICMP_HOST_PROHIB { HOST_PROHIB }
| ICMP_TOSNET { TOSNET }
| ICMP_TOSHOST { TOSHOST }
| ICMP_FILTER_PROHIB { FILTER_PROHIB }
| ICMP_PREC_VIOLATION { PREC_VIOLATION }
| ICMP_PREC_CUTOFF { PREC_CUTOFF }
;

icmp_paramprob:
  ICMP_BADHDR { BADHDR }
| ICMP_NEEDOPT { NEEDOPT }
;

icmp_source_quench:
  ICMP_QUENCH { QUENCH }
;

icmp_time_exceeded:
  ICMP_INTRANS { INTRANS }
| ICMP_REASS { REASS }
;

udpval:
  IS1 ASSIGN ipoption { update_udp (HOL_UDP_IS1 $3) }
| IS2 ASSIGN ipoption { update_udp (HOL_UDP_IS2 $3) }
| PS1 ASSIGN portoption { update_udp (HOL_UDP_PS1 $3) }
| PS2 ASSIGN portoption { update_udp (HOL_UDP_PS2 $3) }
| DATA ASSIGN datalist comment { update_udp (HOL_UDP_DATA $3) }
;

seqlocal:
  LPAREN seqlocal RPAREN { $2 }
| SEQLOCAL word16 { $2 }

seqforeign:
  LPAREN seqforeign RPAREN { $2 }
| SEQFOREIGN word16 { $2 }

tsseqoption:
  LPAREN tsseqoption RPAREN { $2 }
| NONE { None }
| SOME LPAREN TSSEQ word16 COMMA TSSEQ word16 RPAREN { Some($4, $7) }
;

datalist:
  LPAREN datalist RPAREN { $2 }
| LSQBRKT RSQBRKT { [] }
| LSQBRKT datalist2 RSQBRKT { $2 }
;

datalist2:
  BYTE intr { [$2] }
| BYTE intr SC datalist2 { $2::$4 }
;

/* --------- */
/* LIB rules */
/* --------- */

lib:
  LHCALL LPAREN tid COMMA call RPAREN { LIBCALL ($3, $5) }
| LHRETURN LPAREN tid COMMA return RPAREN { LIBRETURN ($3, $5) }
;

return:
  LPAREN return RPAREN  { $2 }
| returnok { $1 }
| returnfail { $1 }
;

returnok:
  LHOK returnok_inner { $2 }
;

returnok_inner:
  UNIT { OK_UNIT () }
| LPAREN fd RPAREN { OK_FD (fd_of_int_private (Int64.to_int $2)) }
| LPAREN bool RPAREN { OK_BOOL $2 }
| LPAREN intr RPAREN { OK_INT (Int64.to_int $2) }
| LPAREN ip COMMA port RPAREN
    { OK_IP_PORT (uint_ip_to_ip $2, uint_port_to_port $4) }
| LPAREN filebflaglist RPAREN { OK_FILEFLAGLIST ($2) }
| LPAREN fdlisttriple RPAREN { OK_FDLISTTRIPLE $2 }
| LPAREN stringr RPAREN { OK_STRING $2 }
| LPAREN fdipport RPAREN { OK_FD_IP_PORT $2 }
/* (* Really want to say: *)
   (*    ipoption COMMA portoption { } *)
   (*  | intpairoption { } *)
   (* here but can't because of a reduce-reduce conflict on the NONE case *) */
| LPAREN NONE COMMA portoption RPAREN { OK_IPOPT_PORTOPT (None, uint_portopt_to_portopt $4) }
| LPAREN SOME ip COMMA portoption RPAREN { OK_IPOPT_PORTOPT (uint_ipopt_to_ipopt (Some $3),
						uint_portopt_to_portopt $5) }
| LPAREN NONE RPAREN { OK_INT_INT_OPTION None }
| LPAREN SOME intpair RPAREN { OK_INT_INT_OPTION (Some (uint_pair_to_int_pair $3)) }
| LPAREN stringr COMMA ipportboolop RPAREN{ OK_STRING_IP_PORT_BOOL ($2, $4) }
| LPAREN interfacelist RPAREN { OK_INTERFACE_LIST $2 }
;

returnfail:
  LHFAIL LPAREN unixerror RPAREN { FAIL $3 }
;

call:
  LPAREN call RPAREN  { $2 }
| ACCEPT fd
    { ACCEPT (fd_of_int_private (Int64.to_int $2)) }
| BIND LPAREN fd COMMA ipoption COMMA portoption RPAREN
    { BIND (fd_of_int_private (Int64.to_int $3),
	    uint_ipopt_to_ipopt $5,
	    uint_portopt_to_portopt $7) }
| CLOSE fd { CLOSE (fd_of_int_private (Int64.to_int $2)) }
| CONNECT LPAREN fd COMMA ip COMMA portoption RPAREN
    { CONNECT (fd_of_int_private (Int64.to_int $3),
	       uint_ip_to_ip $5 ,
	       uint_portopt_to_portopt $7) }
| DISCONNECT fd { DISCONNECT (fd_of_int_private (Int64.to_int $2)) }
| DUP fd
    { DUP (fd_of_int_private (Int64.to_int $2)) }
| DUPFD LPAREN fd COMMA INT RPAREN
    { DUPFD (fd_of_int_private (Int64.to_int $3),
	     Int64.to_int $5) }
| GETFILEFLAGS fd
    { GETFILEFLAGS (fd_of_int_private (Int64.to_int $2))  }
| SETFILEFLAGS LPAREN fd COMMA filebflaglist RPAREN
    { SETFILEFLAGS (fd_of_int_private (Int64.to_int $3), $5) }
| GETIFADDRS UNIT { GETIFADDRS () }
| GETSOCKNAME fd
    { GETSOCKNAME (fd_of_int_private (Int64.to_int $2)) }
| GETPEERNAME fd
    { GETPEERNAME (fd_of_int_private (Int64.to_int $2)) }
| GETSOCKBOPT LPAREN fd COMMA sockbflag RPAREN
    { GETSOCKBOPT (fd_of_int_private (Int64.to_int $3), $5) }
| GETSOCKNOPT LPAREN fd COMMA socknflag RPAREN
    { GETSOCKNOPT (fd_of_int_private (Int64.to_int $3), $5) }
| GETSOCKTOPT LPAREN fd COMMA socktflag RPAREN
    { GETSOCKTOPT (fd_of_int_private (Int64.to_int $3), $5) }
| SETSOCKBOPT LPAREN fd COMMA sockbflag COMMA bool RPAREN
    { SETSOCKBOPT (fd_of_int_private (Int64.to_int $3), $5, $7) }
| SETSOCKNOPT LPAREN fd COMMA socknflag COMMA intr RPAREN
    { SETSOCKNOPT (fd_of_int_private (Int64.to_int $3), $5,
		   Int64.to_int $7) }
| SETSOCKTOPT LPAREN fd COMMA socktflag COMMA topt RPAREN
    { SETSOCKTOPT (fd_of_int_private (Int64.to_int $3), $5,
		   uint_pairopt_to_int_pairopt $7 ) }
| LISTEN LPAREN fd COMMA intr RPAREN
    { LISTEN (fd_of_int_private (Int64.to_int $3), Int64.to_int $5) }
| PSELECT pselectopts { $2 }
| RECV LPAREN fd COMMA intr COMMA msgbflaglist RPAREN
    { RECV (fd_of_int_private (Int64.to_int $3), Int64.to_int $5, $7) }
| SEND LPAREN fd COMMA addrop COMMA stringr COMMA msgbflaglist RPAREN
    { SEND (fd_of_int_private (Int64.to_int $3), $5, $7, $9) }
| SHUTDOWN LPAREN fd COMMA bool COMMA bool RPAREN
    { SHUTDOWN (fd_of_int_private (Int64.to_int $3), $5, $7) }
| SOCKATMARK fd { SOCKATMARK (fd_of_int_private (Int64.to_int $2)) }
| SOCKET LPAREN socktype RPAREN { SOCKET ($3) }
| GETSOCKERR fd
    { GETSOCKERR (fd_of_int_private (Int64.to_int $2))  }
| GETSOCKLISTENING fd
    { GETSOCKLISTENING (fd_of_int_private (Int64.to_int $2)) }
;


/* ----------------- */
/* LIB support rules */
/* ----------------- */

tid:
  LPAREN tid RPAREN { $2 }
| TID intr { Ocamllib.tid_of_int_private (Int64.to_int $2) }
;

filebflag:
  LPAREN filebflag RPAREN { $2 }
| TO_NONBLOCK { O_NONBLOCK }
| TO_ASYNC { O_ASYNC  }
;

sockbflag:
  LPAREN sockbflag RPAREN { $2 }
| TSO_BSDCOMPAT { SO_BSDCOMPAT }
| TSO_REUSEADDR { SO_REUSEADDR }
| TSO_KEEPALIVE { SO_KEEPALIVE }
| TSO_OOBINLINE { SO_OOBINLINE }
| TSO_DONTROUTE { SO_DONTROUTE }
| TSO_BROADCAST { SO_BROADCAST }
;

socknflag:
  LPAREN socknflag RPAREN { $2 }
| TSO_SNDBUF { SO_SNDBUF }
| TSO_RCVBUF { SO_RCVBUF }
| TSO_SNDLOWAT { SO_SNDLOWAT }
| TSO_RCVLOWAT { SO_RCVLOWAT }
;

socktflag:
  LPAREN socktflag RPAREN { $2 }
| TSO_LINGER { SO_LINGER }
| TSO_SNDTIMEO { SO_SNDTIMEO }
| TSO_RCVTIMEO { SO_RCVTIMEO }
;

msgbflag:
  LPAREN msgbflag RPAREN { $2 }
| TMSG_PEEK { MSG_PEEK }
| TMSG_OOB { MSG_OOB }
| TMSG_WAITALL { MSG_WAITALL }
| TMSG_DONTWAIT { MSG_DONTWAIT }
;

signal:
  LPAREN signal RPAREN { $2 }
| TSIGABRT { SIGABRT }
| TSIGALRM { SIGALRM  }
| TSIGBUS  { SIGBUS }
| TSIGCHLD { SIGCHLD }
| TSIGCONT { SIGCONT }
| TSIGFPE  { SIGFPE }
| TSIGHUP  { SIGHUP }
| TSIGILL  { SIGILL }
| TSIGINT  { SIGINT }
| TSIGKILL { SIGKILL }
| TSIGPIPE { SIGPIPE }
| TSIGQUIT { SIGQUIT }
| TSIGSEGV { SIGSEGV }
| TSIGSTOP { SIGSTOP }
| TSIGTERM { SIGTERM }
| TSIGTSTP { SIGTSTP }
| TSIGTTIN { SIGTTIN }
| TSIGTTOU { SIGTTOU }
| TSIGUSR1 { SIGUSR1 }
| TSIGUSR2 { SIGUSR2 }
| TSIGPOLL { SIGPOLL }
| TSIGPROF { SIGPROF }
| TSIGSYS  { SIGSYS }
| TSIGTRAP { SIGTRAP }
| TSIGURG  { SIGURG }
| TSIGVTALRM  { SIGVTALRM }
| TSIGXCPU { SIGXCPU }
| TSIGXFSZ { SIGXFSZ }
;


topt:
  LPAREN topt RPAREN { $2 }
| NONE { None }
| SOME intpair { Some $2 }
;

filebflaglist:
  LPAREN filebflaglist RPAREN { $2 }
|  LSQBRKT filebflaglisth RSQBRKT { $2 }
| LSQBRKT RSQBRKT { [] }
;

filebflaglisth:
  filebflag { [$1] }
| filebflaglisth SC filebflag { $3 :: $1 }
;

msgbflaglist:
  LPAREN msgbflaglist RPAREN { $2 }
| LSQBRKT msgbflaglisth RSQBRKT { $2 }
| LSQBRKT RSQBRKT { [] }
;

msgbflaglisth:
  msgbflag { [$1] }
| msgbflaglisth SC msgbflag { $3 :: $1 }
;

fdlist:
/*  LPAREN fdlist RPAREN { $2 } */
  LSQBRKT fdlisth RSQBRKT { $2 }
| LSQBRKT RSQBRKT { [] }
;

fdlisth:
  fd { [fd_of_int_private (Int64.to_int $1)] }
| fdlisth SC fd { (fd_of_int_private (Int64.to_int $3))::$1 }
;

siglistopt:
  LPAREN siglistopt RPAREN { $2 }
| NONE  { None }
| SOME siglist { Some $2 }
;

siglist:
  LPAREN siglist RPAREN { $2 }
| LSQBRKT siglisth RSQBRKT { $2 }
| LSQBRKT RSQBRKT { [] }
;

siglisth:
  signal { [$1] }
| siglisth SC signal { $3 :: $1 }
;

pselectopts:
  LPAREN pselectopts RPAREN { $2 }
| fdlist COMMA fdlist COMMA fdlist COMMA topt COMMA siglistopt
    { PSELECT ($1, $3, $5, uint_pairopt_to_int_pairopt $7, $9) }
;

fdlisttriple:
  LPAREN fdlisttriple RPAREN { $2 }
| fdlist COMMA LPAREN fdlist COMMA fdlist RPAREN { ($1, ($4, $6)) }
;

fdipport:
  LPAREN fdipport RPAREN { $2 }
| fd COMMA LPAREN ip COMMA port RPAREN
    { (fd_of_int_private(Int64.to_int $1), (uint_ip_to_ip $4, uint_port_to_port $6)) }
;

interfacelist:
  LPAREN interfacelist RPAREN { $2 }
| LSQBRKT interfacelisth RSQBRKT { $2 }
/* | LSQBRKT RSQBRKT { [] } - Assumption here is that there will always be at least 1 interface. If we don't do this then we get a reduce-reduce conflict in returnok_inner which can't be gotten rid of */
;

interfacelisth:
  interface { [$1] }
| interfacelisth SC interface { $3 :: $1 }
;

interface:
  LPAREN ifid COMMA ip COMMA iplist COMMA netmask RPAREN { ($2, uint_ip_to_ip $4, $6, uint_netmask_to_netmask $8) }
;

ifid:
  LPAREN ifid RPAREN { $2 }
| IFID IDENT { ifid_of_string $2 }
;

netmask:
  LPAREN netmask RPAREN { $2 }
| NETMASK netmaskaddr   { $2 }
;

netmaskaddr:
  LPAREN netmaskaddr RPAREN { $2 }
|  INT { $1 }
;


/* --------------- */
/* TCP Trace rules */
/* --------------- */
trace:
/*  LPAREN trace RPAREN { $2 } FIXME tjr I don't believe this is used, and we need to move SC to top level */
| TRACE LPAREN traceaction COMMA tracesid COMMA traceaddr COMMA tracest COMMA RECSTART tcpcbdetail RECEND RPAREN /* SC */
    { let p = foldlist $12 clear_tcpcb_status clear_tcpcb in
      let _ = chk_missing_tcpcb (fst p) in
      let m = TCPTRACE($3, $5, $7, $9, (snd p)) in m }
;

traceaction:
  LPAREN traceaction RPAREN { $2 }
| T_TA_OUTPUT { TA_OUTPUT }
| T_TA_INPUT { TA_INPUT }
| T_TA_USER { TA_USER }
| T_TA_RESPOND { TA_RESPOND }
| T_TA_DROP { TA_DROP }
;

tracesid:
  LPAREN tracesid RPAREN { $2 }
| SID INT { $2 }
;

traceaddr:
  LPAREN traceaddr RPAREN { $2 }
| SOME traceaddr_h { TRACEADDR(Some($2)) }
| NONE { TRACEADDR(None) }
;

traceaddr_h:
  LPAREN traceaddr_h RPAREN { $2 }
| ipoption COMMA portoption COMMA ipoption COMMA portoption
    { ($1, $3, $5, $7) }
;

tracest:
  T_CLOSED { TCPCB_CLOSED }
| T_LISTEN { TCPCB_LISTEN }
| T_SYN_SENT { TCPCB_SYN_SENT }
| T_SYN_RECEIVED { TCPCB_SYN_RCVD }
| T_ESTABLISHED { TCPCB_ESTABLISHED }
| T_CLOSE_WAIT { TCPCB_CLOSE_WAIT }
| T_FIN_WAIT_1 { TCPCB_FIN_WAIT_1 }
| T_CLOSING { TCPCB_CLOSING }
| T_LAST_ACK { TCPCB_LAST_ACK }
| T_FIN_WAIT_2 { TCPCB_FIN_WAIT_2 }
| T_TIME_WAIT { TCPCB_TIME_WAIT }
;

tcpcbdetail:
  tcpcb { [$1] }
| tcpcb SC tcpcbdetail { $1::$3 }
;

timepair:
  LPAREN timepair RPAREN { $2 }
| TSSEQ word16 COMMA NEVERTIMER { ($2, NEVER_TIMER) }
;

tsrecent:
  LPAREN tsrecent RPAREN { $2 }
| TIMEWINDOWCLOSED { TimeWindowClosed }
| TIMEWINDOW timepair { TimeWindow (fst $2, snd $2) }
;


tcpcb:
  T_SND_UNA ASSIGN seqlocal { update_tcpcb (TCPCB_SND_UNA $3) }
| T_SND_MAX ASSIGN seqlocal { update_tcpcb (TCPCB_SND_MAX $3) }
| T_SND_NXT ASSIGN seqlocal { update_tcpcb (TCPCB_SND_NXT $3) }
| T_SND_WL1 ASSIGN seqforeign { update_tcpcb (TCPCB_SND_WL1 $3) }
| T_SND_WL2 ASSIGN seqlocal { update_tcpcb (TCPCB_SND_WL2 $3) }
| T_ISS ASSIGN seqlocal { update_tcpcb (TCPCB_ISS $3) }
| T_SND_WND ASSIGN intr { update_tcpcb (TCPCB_SND_WND $3) }
| T_SND_CWND ASSIGN intr { update_tcpcb (TCPCB_SND_CWND $3) }
| T_SND_SSTHRESH ASSIGN intr { update_tcpcb (TCPCB_SND_SSTHRESH $3) }
| T_RCV_WND ASSIGN intr { update_tcpcb (TCPCB_RCV_WND $3) }
| T_RCV_NXT ASSIGN seqforeign { update_tcpcb (TCPCB_RCV_NXT $3) }
| T_RCV_UP ASSIGN seqforeign { update_tcpcb (TCPCB_RCV_UP $3) }
| T_IRS ASSIGN seqforeign { update_tcpcb (TCPCB_IRS $3) }
| T_RCV_ADV ASSIGN seqforeign { update_tcpcb (TCPCB_RCV_ADV $3) }
| T_SND_RECOVER ASSIGN seqlocal { update_tcpcb (TCPCB_SND_RECOVER $3) }
| T_T_MAXSEG ASSIGN intr { update_tcpcb (TCPCB_T_MAXSEG $3) }
| T_T_DUPACKS ASSIGN intr { update_tcpcb (TCPCB_T_DUPACKS $3) }
| T_T_RTTSEG ASSIGN rttsegopt { update_tcpcb (TCPCB_T_RTTSEG $3) }
| T_SND_SCALE ASSIGN intr { update_tcpcb (TCPCB_SND_SCALE $3) }
| T_RCV_SCALE ASSIGN intr { update_tcpcb (TCPCB_RCV_SCALE $3) }
| T_TS_RECENT ASSIGN tsrecent { update_tcpcb (TCPCB_TS_RECENT $3) }
| T_LAST_ACK_SENT ASSIGN seqforeign { update_tcpcb (TCPCB_LAST_ACK_SENT $3) }
;

rttsegopt:
  LPAREN rttsegopt RPAREN { $2 }
| NONE { None }
| SOME rttseg { Some($2) }
;

rttseg:
  LPAREN rttseg RPAREN { $2 }
| tsseq COMMA seqlocal { ($1, $3) }

tsseq:
  LPAREN tsseq RPAREN { $2 }
| TSSEQ word16 { $2 }
;

/* --------------- */
/* Lh_epsilon      */
/* --------------- */

epsilon:
/*  LPAREN epsilon RPAREN { $2 } FIXME tjr doesn't believe this is used, and need to move SC to top level */
| T_LH_EPSILON LPAREN T_DURATION intr intr RPAREN { HOLEPSILON (DURATION($4, $5)) }
;

abstime:
  LPAREN abstime RPAREN { $2 }
| T_ABSTIME intr intr { HOLABSTIME($2, $3) }

/* ------------ */
/* Common rules */
/* ------------ */

bool:
  LPAREN bool RPAREN { $2 }
| TRUE { true }
| FALSE { false }
;

intr:
  LPAREN intr RPAREN { $2 }
| INT { $1 }
| MINUS INT { Int64.neg($2) }
;

intpair:
  LPAREN intpair RPAREN { $2 }
| intr COMMA intr { ($1, $3) }
;

ip:
  LPAREN ip RPAREN { $2 }
| IP ipaddr { $2 }
;

ipaddr:
  LPAREN ipaddr RPAREN { $2 }
| INT INT INT INT { ($1 << 24) |. ($2 << 16) |. ($3 << 8) |. $4 }
;

ipoption:
  LPAREN ipoption RPAREN { $2 }
| NONE { None }
| SOME ip { ip_option_of_uint $2 }
;

iplist:
  LSQBRKT ips RSQBRKT { $2 }
| LSQBRKT RSQBRKT     { [] }
;

ips:
  ip { [uint_ip_to_ip $1] }
| ip SC ips { (uint_ip_to_ip $1)::$3 }
;

port:
  LPAREN port RPAREN { $2 }
| PORT intr { $2 }
;

portoption:
  LPAREN portoption RPAREN { $2 }
| NONE { None }
| SOME port { port_option_of_uint $2 }
;

word16:
  LPAREN word16 RPAREN { $2 }
| WORD16 intr { $2 }
;

word16option:
  LPAREN word16option RPAREN { $2 }
| NONE { None }
| SOME word16 { Some $2 }
;

byte:
  LPAREN byte RPAREN { $2 }
| BYTE intr { $2 }

byteoption:
  LPAREN byteoption RPAREN { $2 }
| NONE { None }
| SOME byte { Some $2 }
;

stringr:
  LPAREN stringr RPAREN { $2 }
| STRING { $1 }
;

fd:
  LPAREN fd RPAREN { $2 }
| FD intr { $2 }
;

addrop:
  NONE { None }
| LPAREN addrop RPAREN { $2 }
| SOME LPAREN ip COMMA port RPAREN { Some(uint_ip_to_ip $3, uint_port_to_port $5) }
;

ipportboolop:
  NONE { None }
| LPAREN ipportboolop RPAREN { $2 }
| SOME LPAREN ipport COMMA bool RPAREN { Some($3, $5) }
;

ipportop:
  NONE { None }
| LPAREN ipportop RPAREN { $2 }
| SOME LPAREN ipoption COMMA portoption RPAREN { Some(uint_ipopt_to_ipopt $3, uint_portopt_to_portopt $5) }
;

ipport:
  LPAREN ipport RPAREN { $2 }
| LPAREN ipoption COMMA portoption RPAREN { (uint_ipopt_to_ipopt $2, uint_portopt_to_portopt $4) }
;

socktype:
  SOCK_DGRAM { SOCK_DGRAM }
| SOCK_STREAM { SOCK_STREAM }
;

/* -------------------------- */
/* Special comment processing */
/* -------------------------- */

timecomment:
  SCOMMENTSTART intr DOT intr stringr SCOMMENTEND
    { Some(TIMECOMMENT($2*.(uint 1000000)+.$4, $5)) }
| { None }
;

comment_h:
  COMMENT { [$1] }
| comment_h COMMENT { $2 :: $1 } /* N.B. the stupid reversal of the comment order */
;

comment:
  comment_h { Some $1 }
| { None }
;

/* ------------ */
/* Unix errors  */
/* ------------ */

unixerror:
  T_ERR_E2BIG { E2BIG }
| T_ERR_EACCES { EACCES }
| T_ERR_EADDRINUSE { EADDRINUSE }
| T_ERR_EADDRNOTAVAIL { EADDRNOTAVAIL }
| T_ERR_EAFNOSUPPORT { EAFNOSUPPORT }
| T_ERR_EAGAIN { EAGAIN }
| T_ERR_EWOULDBLOCK { EWOULDBLOCK }
| T_ERR_EALREADY { EALREADY }
| T_ERR_EBADF { EBADF }
| T_ERR_EBADMSG { EBADMSG }
| T_ERR_EBUSY { EBUSY }
| T_ERR_ECANCELED { ECANCELED }
| T_ERR_ECHILD { ECHILD }
| T_ERR_ECONNABORTED { ECONNABORTED }
| T_ERR_ECONNREFUSED { ECONNREFUSED }
| T_ERR_ECONNRESET { ECONNRESET }
| T_ERR_EDEADLK { EDEADLK }
| T_ERR_EDESTADDRREQ { EDESTADDRREQ }
| T_ERR_EDOM { EDOM }
| T_ERR_EDQUOT { EDQUOT }
| T_ERR_EEXIST { EEXIST }
| T_ERR_EFAULT { EFAULT }
| T_ERR_EFBIG { EFBIG }
| T_ERR_EHOSTUNREACH { EHOSTUNREACH }
| T_ERR_EIDRM { EIDRM }
| T_ERR_EILSEQ { EILSEQ }
| T_ERR_EINPROGRESS { EINPROGRESS }
| T_ERR_EINTR { EINTR }
| T_ERR_EINVAL { EINVAL }
| T_ERR_EIO { EIO }
| T_ERR_EISCONN { EISCONN }
| T_ERR_EISDIR { EISDIR }
| T_ERR_ELOOP { ELOOP }
| T_ERR_EMFILE { EMFILE }
| T_ERR_EMLINK { EMLINK }
| T_ERR_EMSGSIZE { EMSGSIZE }
| T_ERR_EMULTIHOP { EMULTIHOP }
| T_ERR_ENAMETOOLONG { ENAMETOOLONG }
| T_ERR_ENETDOWN { ENETDOWN }
| T_ERR_ENETRESET { ENETRESET }
| T_ERR_ENETUNREACH { ENETUNREACH }
| T_ERR_ENFILE { ENFILE }
| T_ERR_ENOBUFS { ENOBUFS }
| T_ERR_ENODATA { ENODATA }
| T_ERR_ENODEV { ENODEV }
| T_ERR_ENOENT { ENOENT }
| T_ERR_ENOEXEC { ENOEXEC }
| T_ERR_ENOLCK { ENOLCK }
| T_ERR_ENOLINK { ENOLINK }
| T_ERR_ENOMEM { ENOMEM }
| T_ERR_ENOMSG { ENOMSG }
| T_ERR_ENOPROTOOPT { ENOPROTOOPT }
| T_ERR_ENOSPC { ENOSPC }
| T_ERR_ENOSR { ENOSR }
| T_ERR_ENOSTR { ENOSTR }
| T_ERR_ENOSYS { ENOSYS }
| T_ERR_ENOTCONN { ENOTCONN }
| T_ERR_ENOTDIR { ENOTDIR }
| T_ERR_ENOTEMPTY { ENOTEMPTY }
| T_ERR_ENOTSOCK { ENOTSOCK }
| T_ERR_ENOTSUP { ENOTSUP }
| T_ERR_ENOTTY { ENOTTY }
| T_ERR_ENXIO { ENXIO }
| T_ERR_EOPNOTSUPP { EOPNOTSUPP }
| T_ERR_EOVERFLOW { EOVERFLOW }
| T_ERR_EPERM { EPERM }
| T_ERR_EPIPE { EPIPE }
| T_ERR_EPROTO { EPROTO }
| T_ERR_EPROTONOSUPPORT { EPROTONOSUPPORT }
| T_ERR_EPROTOTYPE { EPROTOTYPE }
| T_ERR_ERANGE { ERANGE }
| T_ERR_EROFS { EROFS }
| T_ERR_ESPIPE { ESPIPE }
| T_ERR_ESRCH { ESRCH }
| T_ERR_ESTALE { ESTALE }
| T_ERR_ETIME { ETIME }
| T_ERR_ETIMEDOUT { ETIMEDOUT }
| T_ERR_ETXTBSY { ETXTBSY }
| T_ERR_EXDEV { EXDEV }
| T_ERR_ESHUTDOWN { ESHUTDOWN }
| T_ERR_EHOSTDOWN { EHOSTDOWN }
| T_ERR_EUNKNOWN_UNIX_ERROR { EUNKNOWN_UNIX_ERROR }
