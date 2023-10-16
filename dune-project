(lang dune 3.10)

(name diff)

(source (github skolemlabs/diff))

(generate_opam_files true)

(version 1.0.0)

(authors "Zach Baylin <zach.baylin@skolem.com>")
(maintainers "Zach Baylin <zach.baylin@skolem.com>")

(package
 (name diff)
 (synopsis "A library for generating diffs from sum types")
 (depends
  (dune (>= 3.10.0))
  (ppxlib (>= 0.25.0))
  (ppx_deriving :with-test)
  (tezt (and :with-test (and (>= 3.1.1) (< 4.0.0))))))
