forge-dump-tools - API and tools to migrate data dump from forge.ocamlcore.org
=============================================================================

Features of the project:

* Library to read the .zip files dumped from the forge.ocamlcore.org.
* Migrate bugs, features requests and patches from the .zip file and create
  Github Issues.

The goal of this project is not to be long term maintained. It is provided as
a one off solution to migrate the OCaml forge dump.

<!-- TODO: add a link to website/announce for the dump. -->

[opam]: https://opam.ocaml.org

Installation
------------

The recommended way to install fileutils is via the [opam package manager][opam]:

```sh
$ opam pin add forge-dump-tools https://github.com/ocaml-forge/forge-dump-tools.git
```

Documentation
-------------

API documentation is
[available online](https://gildor478.github.io/forge-dump-tools).

forge-dump-migrate-bugs-to-github
---------------------------------

This executable helps to migrate issues. It also lists the actions that need
to be done afterwards, like uploading file. Credential are obtained through
`git-jar`.


Example:

```sh
$> opam install lwt_ssl github forge-dump-tools
$> export SSL_CERT_DIR=/etc/ssl/certs/
$> git jar make gildor478 forge.o.o -s public_repo
$> forge-dump-migrate-bugs-to-github \
   --zip ounit.zip \
   --todo ounit-todo.txt \
   --user gildor478 \
   --repo forge-dump-migration-test
```

See the results of running the script:
https://github.com/gildor478/forge-dump-migration-test/issues
