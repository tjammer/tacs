(lang dune 2.7)
(name tacs)
(generate_opam_files true)

(license GPL)
(authors "Tobias Mock")
(maintainers "tobiasjammer@gmail.com")

(package
 (name tacs)
 (synopsis "Small game")
 (depends
  (cmdliner (>= 1.0.4))
  (raylib (>= 0.4.0))
  (ppx_sexp_conv (>= v0.14.1))
  (ppx_deriving (>= 5.1))
  dune
  containers
  lwt
  sexplib))

(package
 (name tacs-server)
 (synopsis "Server for tacs")
 (depends
  (ppx_sexp_conv (>= v0.14.1))
  (ppx_deriving (>= 5.1))
  (containers (>= 3.1))
  dune
  lwt
  sexplib))
