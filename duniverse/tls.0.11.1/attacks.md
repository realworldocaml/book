### Attacks on TLS

TLS the most widely deployed security protocol on the Internet and, at
over 15 years, is also showing its age. As such, a flaw is a valuable
commodity due to the commercially sensitive nature of data that is
encrypted with TLS. Various vulnerabilities on different layers of TLS
have been found - [heartbleed][] and others are implementation
specific, advancements in cryptanalysis such as [collisions of
MD5][md5_collision] lead to vulnerabilities, and even others are due
to incorrect usage of TLS ([truncation attack][truncation] or
[BREACH][breach]). Finally, some weaknesses are in the protocol
itself. Extensive [overviews][tls_attacks] of [attacks on
TLS][mitls_attacks] are available.

We look at protocol level attacks of TLS and how [ocaml-tls][ocaml-tls]
implements mitigations against these.  [TLS 1.2 RFC][RFC5246] provides an
overview of attacks and mitigations, and we [track][issue31] our progress in
covering them. This is slightly out of date as the RFC is roughly six years old and
in the meantime more attacks have been published, such as the [renegotiation
flaw][understanding_reneg].

We track all our [mitigated][closed] and [open][open] security issues
on our GitHub issue tracker.

Due to the choice of using OCaml, a memory managed programming
language, we obstruct entire bug classes, namely temporal and spatial
memory safety.

Cryptanalysis and improvement of computational power weaken some
ciphers, such as RC4 and 3DES (see [issue 8][issue8] and [issue
10][issue10]). If we phase these two ciphers out, there wouldn't be
any matching ciphersuite left to communicate with some compliant TLS-1.0
implementations, such as Windows XP, that do not support AES.

[issue8]: https://github.com/mirleft/ocaml-tls/issues/8
[issue10]: https://github.com/mirleft/ocaml-tls/issues/10
[open]: https://github.com/mirleft/ocaml-tls/issues?labels=security+concern&page=1&state=open
[closed]: https://github.com/mirleft/ocaml-tls/issues?labels=security+concern&page=1&state=closed
[ocaml-tls]: https://github.com/mirleft/ocaml-tls
[understanding_reneg]: http://www.educatedguesswork.org/2009/11/understanding_the_tls_renegoti.html
[heartbleed]: https://en.wikipedia.org/wiki/Heartbleed
[md5_collision]: http://eprint.iacr.org/2005/067
[truncation]: http://www.theregister.co.uk/2013/08/01/gmail_hotmail_hijacking/
[breach]: http://breachattack.com/
[RFC5246]: https://tools.ietf.org/html/rfc5246#appendix-D.4
[tls_attacks]: http://eprint.iacr.org/2013/049.pdf
[mitls_attacks]: http://www.mitls.org/wsgi/tls-attacks
[issue31]: https://github.com/mirleft/ocaml-tls/issues/31

**Timing attacks**

When the timing characteristics between the common case and the error
case are different, this might potentially leak confidential
information. Timing is a very prominent side-channel and there are a huge
variety of timing attacks on different layers, which are observable by
different attackers. Small differences in timing behaviour might
initially be exploitable only by a local attacker, but advancements to
the attack (e.g. increasing the number of tests) might allow a 
remote attacker to filter the noise and exploit the different timing
behaviour.

**Timing of cryptographic primitives**

We [already mentioned][nocrypto-intro] [cache][] [timing][cache_timing]
attacks on our AES implementation, and that we use [blinding][]
techniques to mitigate RSA timing attacks.

By using a memory managed programming language, we open the attack
vector of garbage collector (GC) timing attacks (also mentioned [in
our nocrypto introduction][nocrypto-intro]).

Furthermore, research has been done on virtual machine side channels
([l3][], [cross vm][cross_vm] and [cache timing][cache_vm]), which we
will need to study and mitigate appropriately.

**For the time being we suggest to not use the stack on a multi-tenant
shared host or on a shared host which malicious users might have
access to.**

[blinding]: https://en.wikipedia.org/wiki/Blinding_(cryptography)
[cache]: http://www.cs.tau.ac.il/~tromer/papers/cache.pdf
[cache_timing]: http://cr.yp.to/antiforgery/cachetiming-20050414.pdf
[l3]: http://eprint.iacr.org/2013/448.pdf
[cross_vm]: http://www.cs.unc.edu/~reiter/papers/2012/CCS.pdf
[cache_vm]: http://fc12.ifca.ai/pre-proceedings/paper_70.pdf

**Bleichenbacher**

In 1998, Daniel Bleichenbacher discovered a [timing flaw in the
PKCS1][bleichenbacher] encoding of the premaster secret: the TLS server
failed faster when the padding was wrong than when the decryption
failed. Using this timing, an attacker can run an adaptive chosen
ciphertext attack and find out the plain text of a PKCS1 encrypted
message. In TLS, when RSA is used as the key exchange method, this
leads to discovery of the premaster secret, which is used to derive the
keys for the current session.

The mitigation is to have both padding and decryption failures use the
exact same amount of time, thus there should not be any data-dependent
branches or different memory access patterns in the code. We
implemented this mitigation in [Handshake_server][answer_client_key_exchange].

[bleichenbacher]: http://archiv.infsec.ethz.ch/education/fs08/secsem/Bleichenbacher98.pdf
[answer_client_key_exchange]: https://github.com/mirleft/ocaml-tls/blob/c06cbaaffe49024d8570916b70f7839603a54692/lib/handshake_server.ml#L45

**Padding oracle and CBC timing**

[Vaudenay][] discovered a vulnerability involving block ciphers: if an
attacker can distinguish between bad mac and bad padding, recovery of
the plaintext is possible (within an adaptive chosen ciphertext
attack). Another approach using the same issue is to use
[timing][practical] information instead of separate error messages.
Further details are described [here][tls_cbc].

The countermeasure, which we implement [here][cbc_mit], is to continue
with the mac computation even though the padding is
incorrect. Furthermore, we send the same alert (`bad_record_mac`)
independent of whether the padding is malformed or the mac is
incorrect.

[tls_cbc]: https://www.openssl.org/~bodo/tls-cbc.txt
[Vaudenay]: http://www.iacr.org/archive/eurocrypt2002/23320530/cbc02_e02d.pdf
[practical]: http://lasecwww.epfl.ch/memo/memo_ssl.shtml
[cbc_mit]: https://github.com/mirleft/ocaml-tls/blob/c06cbaaffe49024d8570916b70f7839603a54692/lib/engine.ml#L100

**Lucky 13**

An advancement of the CBC timing attack was discovered in 2013, named
[Lucky 13][Lucky13]. Due to the fact that the mac is computed over the
plaintext without padding, there is a slight (but measurable)
difference in timing between computing the mac of the plaintext and
computing the fake mac of the ciphertext. This leaks information. We
do not have proper mitigation against Lucky 13 in place yet.  You can
find further discussion in [issue 7][issue7] and [pull request
49][pull49].

[Lucky13]: http://www.isg.rhul.ac.uk/tls/Lucky13.html
[issue7]: https://github.com/mirleft/ocaml-tls/issues/7
[pull49]: https://github.com/mirleft/ocaml-tls/pull/49

**Renegotiation not authenticated**

In 2009, Marsh Ray published a vulnerability of the TLS protocol which
lets an attacker prepend arbitrary data to a session due to
[unauthenticated renegotiation][understanding_reneg]. The attack
exploits the fact that a renegotiation of ciphers and key material is
possible within a session, and this renegotiated handshake is not
authenticated by the previous handshake. A man in the middle can
initiate a session with a server, send some data, and hand over the
session to a client. Neither the client nor the server can detect the
man in the middle.

A fix for this issue is the [secure renegotiation extension][RFC5746],
which embeds authenticated data of the previous handshake into the
client and server hello messages. Now, if a man in the middle
initiates a renegotiation, the server will not complete it due to
missing authentication data (the client believes this is the first
handshake).

We implement and require the secure renegotiation extension by
default, but it is possible to configure `ocaml-tls` to not require
it -- to be able to communicate with servers and
clients which do not support this extension.

Implementation of the mitigation is on the server side in
[ensure_reneg][] and on the client side in [validate_reneg][]. The
data required for the secure renegotiation is stored in
[`handshake_state`][reneg_state] while sending and receiving Finished
messages. You can find further discussion in [issue 3][issue3].

[RFC5746]: https://tools.ietf.org/html/rfc5746
[validate_reneg]: https://github.com/mirleft/ocaml-tls/blob/c06cbaaffe49024d8570916b70f7839603a54692/lib/handshake_client.ml#L50
[ensure_reneg]: https://github.com/mirleft/ocaml-tls/blob/c06cbaaffe49024d8570916b70f7839603a54692/lib/handshake_server.ml#L85
[issue3]: https://github.com/mirleft/ocaml-tls/issues/3
[reneg_state]: https://github.com/mirleft/ocaml-tls/blob/c06cbaaffe49024d8570916b70f7839603a54692/lib/state.ml#L97

**TLS 1.0 and known-plaintext (BEAST)**

TLS 1.0 reuses the last ciphertext block as IV in CBC mode. If an attacker
has a (partially) known plaintext, she can find the remaining plaintext.
This is known as the [BEAST][] attack and there is a [long discussion][mozilla-bug]
about mitigations. Our mitigation is to prepend each TLS-1.0
application data fragment with an empty fragment to randomize the IV.
We do this exactly [here][empty_iv]. There is further discussion in
[issue 2][issue2].

Our mitigation is slightly different from the 1/n-1 splitting proposed
[here][qualys]: we split every application data frame into a 0 byte
and n byte frame, whereas they split into a 1 byte and a n-1 byte
frame.

Researchers have exploited this vulnerability in 2011, although it was
known since [2006][]. TLS versions 1.1 and 1.2 use an explicit IV,
instead of reusing the last cipher block on the wire.

[qualys]: https://community.qualys.com/blogs/securitylabs/2013/09/10/is-beast-still-a-threat
[mozilla-bug]: https://bugzilla.mozilla.org/show_bug.cgi?id=665814
[BEAST]: http://vnhacker.blogspot.co.uk/2011/09/beast.html
[empty_iv]: https://github.com/mirleft/ocaml-tls/blob/c06cbaaffe49024d8570916b70f7839603a54692/lib/engine.ml#L375
[2006]: http://eprint.iacr.org/2006/136
[issue2]: https://github.com/mirleft/ocaml-tls/issues/2

**Compression and information leakage (CRIME)**

When using compression on a chosen-plaintext, encrypting this can leak
information, known as [CRIME][crime]. [BREACH][breach] furthermore
exploits application layer compression, such as HTTP compression. We
mitigate CRIME by not providing any TLS compression support, while we
cannot do anything to mitigate BREACH.

[crime]: http://arstechnica.com/security/2012/09/crime-hijacks-https-sessions/

**Traffic analysis**

Due to limited amount of padding data, the actual size of transmitted
data can be recovered. The mitigation is to implement [length hiding
policies][length_hiding]. This is tracked as [issue 162][issue162].

[issue162]: https://github.com/mirleft/ocaml-tls/issues/162
[length_hiding]: http://tools.ietf.org/html/draft-pironti-tls-length-hiding-02

**Version rollback**

SSL-2.0 is insecure, a man in the middle can downgrade the version to
SSL-2.0. The mitigation we implement is that we do not support
SSL-2.0, and thus cannot be downgraded. Also, we check that the
version of the client hello matches the first two bytes in the
premaster secret [here][client_version]. You can find further discussion in
[issue 5][issue5].

[client_version]: https://github.com/mirleft/ocaml-tls/blob/c06cbaaffe49024d8570916b70f7839603a54692/lib/handshake_server.ml#L55
[issue5]: https://github.com/mirleft/ocaml-tls/issues/5

**Triple handshake**

A vulnerability including session resumption and renegotiation was
discovered by the [miTLS team][mitls], named [triple
handshake][triple].  Mitigations include disallowing renegotiation,
disallowing modification of the certificate during renegotiation, or
a hello extension. Since we do not support session resumption yet, we
have not yet implemented any of the mentioned mitigations. There is
further discussion in [issue 9][issue9].

[mitls]: http://www.mitls.org
[issue9]: https://github.com/mirleft/ocaml-tls/issues/9
[triple]: https://secure-resumption.com/

**Alert attack**

A [fragment of an alert][alert_attack] can be sent by a man in the
middle during the initial handshake. If the fragment is not cleared
once the handshake is finished, the authentication of alerts is
broken. This was discovered in 2012; our mitigation is to discard
fragmented alerts.

[alert_attack]: http://www.mitls.org/wsgi/alert-attack

### EOF.

Within six months, two hackers managed to develop a clean-slate TLS
stack, together with required crypto primitives, ASN.1, and X.509
handling, in a high-level pure language. We interoperate with widely
deployed TLS stacks, as shown by our [demo server][demo].  The code
size is nearly two orders of magnitude smaller than OpenSSL, the most
widely used open source library (written in C, which a lot of
programming languages wrap instead of providing their own TLS
implementation). Our code base seems to be robust -- the [demo
server][demo] successfully finished over 22500 sessions in less than a
week, with only 11 failing traces.

There is a huge need for high quality TLS implementations, because
several TLS implementations suffered this year from severe security
problems, such as [heartbleed][], [goto fail][CVE-2014-1266], [session
id][CVE-2014-3466], [Bleichenbacher][java], [change cipher
suite][CVE-2014-0224] and [GCM DoS][polar]. The main cause is
implementation complexity due to lack of abstraction, and memory
safety issues.

We still need to address some security issues, and improve our performance. We
invite people to do rigorous code audits (both manual and automated) and try
testing our code in their services.

**Please be aware that this release is a *beta* and is missing external code audits.
It is not yet intended for use in any security critical applications.**

[demo]: https://tls.openmirage.org
[polar]: https://polarssl.org/tech-updates/security-advisories/polarssl-security-advisory-2014-02
[java]: http://armoredbarista.blogspot.de/2014/04/easter-hack-even-more-critical-bugs-in.html
[CVE-2014-1266]: https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2014-1266
[CVE-2014-3466]: https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2014-3466
[CVE-2014-0224]: https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2014-0224

[nocrypto-intro]: http://openmirage.org/blog/introducing-nocrypto
