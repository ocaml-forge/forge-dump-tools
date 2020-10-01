forge-dump-tools - API and tools to migrate data dump from forge.ocamlcore.org
=============================================================================

Features of the project:

* Library to read the .zip files dumped from the forge.ocamlcore.org.
* Migrate bugs, features requests and patches from the .zip file and create
  Github Issues.

<!-- TODO: add a link to website/announce for the dump. -->

[opam]: https://opam.ocaml.org

Installation
------------

The recommended way to install fileutils is via the [opam package manager][opam]:

```sh
$ opam install forge-dump-tools
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
