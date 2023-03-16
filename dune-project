(lang dune 2.7)

(name gluten)

(formatting
 (enabled_for ocaml))

(generate_opam_files true)

(source
 (github anmonteiro/gluten))

(authors "Antonio Nuno Monteiro <anmonteiro@gmail.com>")

(maintainers "Antonio Nuno Monteiro <anmonteiro@gmail.com>")

(homepage "https://github.com/anmonteiro/gluten")

(bug_reports "https://github.com/anmonteiro/gluten/issues")

(license "BSD-3-clause")

(package
 (name gluten)
 (synopsis "A reusable runtime library for network protocols")
 (description
  "gluten implements platform specific runtime code for driving network libraries based on state machines, such as http/af, h2 and websocketaf.")
 (depends
  (ocaml
   (>= "4.08.0"))
  (bigstringaf
   (>= "0.4.0"))
  (faraday
   (>= "0.7.3"))
  (ke
   (>= "0.5"))))

(package
 (name gluten-lwt)
 (synopsis "Lwt-specific runtime for gluten")
 (depends
  (ocaml
   (>= "4.08.0"))
  (gluten
   (= :version))
  (lwt
   (>= "5.1.1"))))

(package
 (name gluten-lwt-unix)
 (synopsis "Lwt + Unix support for gluten")
 (depends
  (ocaml
   (>= "4.08.0"))
  (gluten-lwt
   (= :version))
  (faraday-lwt-unix
   (>= "0.7.3")))
 (depopts
  (lwt_ssl
   (>= "1.2.0"))
  tls-lwt))

(package
 (name gluten-mirage)
 (synopsis "Mirage support for gluten")
 (depends
  (ocaml
   (>= "4.08.0"))
  (gluten-lwt
   (= :version))
  (faraday-lwt
   (>= "0.7.3"))
  (conduit-mirage
   (>= "2.0.2"))
  (mirage-flow
   (>= "2.0.0"))
  (cstruct
   (>= "6.0.0"))))

(package
 (name gluten-async)
 (synopsis "Async support for gluten")
 (depends
  (ocaml
   (>= "4.08.0"))
  (gluten
   (= :version))
  (faraday-async
   (>= "0.7.3"))
  (async
   (>= "v0.15.0"))
  (core
   (>= "v0.15.0")))
 (depopts async_ssl tls-async))

(package
 (name gluten-eio)
 (synopsis "EIO runtime for gluten")
 (depends
  (ocaml
   (>= "4.08.0"))
  (gluten
   (= :version))
  (eio
   (>= 0.7))))
