# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.134.1/containers/alpine/.devcontainer/base.Dockerfile
ARG OCAML_VERSION="4.11"

# Once this gets fixed: https://discuss.ocaml.org/t/psa-ocaml-opam2-docker-images-updated-to-4-08-0-release/3948
# We can use smaller image
# FROM ocaml/opam2:debian-10
# This is fixed, use newer one, but as of 18.11.2020, ocaml 4.13 is not yet supported
FROM ocurrent/opam:debian-10-ocaml-${OCAML_VERSION}
# Redeclare arg: https://docs.docker.com/engine/reference/builder/#understand-how-arg-and-from-interact
ARG OCAML_VERSION

# Needed for merlin
USER root
RUN apt-get install -y m4
USER opam
# Install IDE dependencies
RUN opam update && opam install ocaml-lsp-server ocamlformat utop
# Install environment tools
RUN opam install dune
# Install libraries
RUN opam install base stdio tyre
RUN opam user-setup install

# Get the correct ocaml
# This is a manual version of: RUN eval $(opam env)
ENV OPAM_SWITCH_PREFIX='/home/opam/.opam/'${OCAML_VERSION}
ENV CAML_LD_LIBRARY_PATH='/home/opam/.opam/'${OCAML_VERSION}'/lib/stublibs:Updated by package ocaml'
ENV OCAML_TOPLEVEL_PATH='/home/opam/.opam/'${OCAML_VERSION}'/lib/toplevel'
ENV MANPATH="$MANPATH"':/home/opam/.opam/'${OCAML_VERSION}'/man'
ENV OCAML_DOCKER_PATH='/home/opam/.opam/'${OCAML_VERSION}'/bin'
ENV PATH="$OCAML_DOCKER_PATH:$PATH"


