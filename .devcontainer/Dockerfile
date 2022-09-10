FROM ocaml/opam:debian-11-ocaml-4.14 as opam
RUN opam-2.1 pin add -n odoc --dev
RUN opam-2.1 install --confirm-level=unsafe-yes dune ocaml-lsp-server opam-monorepo ocamlformat.0.24.1 dune-release odoc mdx
# Use the [Choice] comment to indicate option arguments that should appear in VS Code UX. Use a comma separated list.
#
# [Choice] Debian OS version: bullseye
FROM buildpack-deps:bullseye-curl
# Use the [Option] comment to specify true/false arguments that should appear in VS Code UX
#
# [Option] Install zsh
ARG INSTALL_ZSH="true"
# [Option] Upgrade OS packages to their latest versions
ARG UPGRADE_PACKAGES="false"

# Install needed packages and setup non-root user. Use a separate RUN statement to add your own dependencies.
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
COPY library-scripts/*.sh library-scripts/*.env /tmp/library-scripts/
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && /bin/bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" "true" "true"\
    && apt-get -y install build-essential curl \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* /tmp/library-scripts
RUN cd /root && curl -OL https://github.com/ocaml/ocaml/archive/refs/tags/4.14.0.tar.gz && tar -xf 4.14.0.tar.gz && cd ocaml-4.14.0 && ./configure --prefix=/usr && make -j world.opt && make install && cd / && rm -f /root/4.14.0.tar.gz && rm -rf /root/ocaml-4.14.0
COPY --from=opam /usr/bin/opam-2.1 /usr/bin/opam
COPY --from=opam /home/opam/.opam/4.14/bin/dune /usr/bin/dune
COPY --from=opam /home/opam/.opam/4.14/bin/ocamllsp /usr/bin/ocamllsp
COPY --from=opam /home/opam/.opam/4.14/bin/opam-monorepo /usr/bin/opam-monorepo
COPY --from=opam /home/opam/.opam/4.14/bin/odoc /usr/bin/odoc
COPY --from=opam /home/opam/.opam/4.14/bin/ocaml-mdx /usr/bin/ocaml-mdx
