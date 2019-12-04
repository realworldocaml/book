This repository contains core libraries and tools used to develop ppx
rewriters. The code was originally developed and is still maintained
and used by [Jane Street][js].

This repository is not the first piece of open source software
released by Jane Street, however it is the first to be entirely
developed on GitHub. We are hoping that opening the development of
this repository will help collaboration with other open source users.

We welcome contributions and we will be happy to add contributors,
given that they are motivated to help maintain and grow the
project. However, given how important this code is to the functioning
of Jane Street, we do require that at least one Jane Street developer
reads every pull request that modifies the source code.

Additionally, all contributors must sign-off their commits, see
below for details.

### Developing patches

We ask that patches changing the code respect the overall coding
style. In particular, the code should be indented using
[ocp-indent][ocpi]. Additionally the test suite should pass on the
contributor's machine before a patch is submitted for review.

Note that in addition to the normal dependencies, you need to install
[cinaps][cinaps] in order to modify the code. This is because some
parts of the code are auto-generated and committed in the repository.

So before submitting a PR, make sure to check all the following
points:

- all the modified code is correctly indented according to ocp-indent
- `make` succeeds
- `make test` succeeds

### Submitting patches and code review

Once a patch is ready according to the criteria stated in the
previous section, it should be submitted via the GitHub website. When
submitting a pull request, we prefer if you tick the `Allow edits from
maintainers` box as it is much simpler to fix typos or do simple
improvements directly rather than go back and forth through the web
interface.

### Signing commits

We require that you sign your contributions. Your signature certifies
that you wrote the patch or otherwise have the right to pass it on as
an open-source patch. The rules are pretty simple: if you can certify
the below (from [developercertificate.org][dco]):

```
Developer Certificate of Origin
Version 1.1

Copyright (C) 2004, 2006 The Linux Foundation and its contributors.
1 Letterman Drive
Suite D4700
San Francisco, CA, 94129

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.


Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```

Then you just add a line to every git commit message:

```
Signed-off-by: Joe Smith <joe.smith@email.com>
```

Use your real name (sorry, no pseudonyms or anonymous contributions.)

If you set your `user.name` and `user.email` git configs, you can sign
your commit automatically with git commit -s.

[js]:     https://opensource.janestreet.com/
[ocpi]:   https://github.com/OCamlPro/ocp-indent
[cinaps]: https://github.com/janestreet/cinaps
[dco]:    http://developercertificate.org/
