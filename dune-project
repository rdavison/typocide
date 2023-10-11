(lang dune 3.10)

(name typocide)

(generate_opam_files true)

(source
 (github rdavison/typocide))

(authors "Richard Davison")

(maintainers "Richard Davison")

(license MIT)

(documentation https://github.com/rdavison/typocide)

(package
 (name typocide)
 (synopsis "A typing tutor")
 (description "Where Typos Meet Their Demise!")
 (depends ocaml dune notty_async core_unix)
 (tags
  (typing tutor keyboard)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
