0.5.2 2024-09-04
--------------

- gluten-async: Silence async deprecation warnings
  ([#78)(https://github.com/anmonteiro/gluten/pull/78))
- migrate to tls 1.0.0 without cstruct
  ([#80](https://github.com/anmonteiro/gluten/pull/80))

0.5.1 2024-06-04
--------------

- gluten-lwt,gluten-eio: remove `Client.socket`
  ([#75](https://github.com/anmonteiro/gluten/pull/75))
- gluten-eio: don't require `Eio_unix.stream_socket_ty`, allowing the use of
  mock sockets ([#74](https://github.com/anmonteiro/gluten/pull/74))

0.5.0 2023-10-25
--------------

- gluten-eio: require `~sw:Eio.Switch.t` argument and fail the switch when loop
  raises an exception ([#61](https://github.com/anmonteiro/gluten/pull/61))
- gluten-eio: handle peer disconnects
  ([#60](https://github.com/anmonteiro/gluten/pull/60))
- gluten: replace `Gluten.Buffer` implementation and drop the `Ke` dependency
  ([#67](https://github.com/anmonteiro/gluten/pull/67))
- gluten-eio: adapt to Eio v0.12 ([#66](https://github.com/anmonteiro/gluten/pull/66)
- gluten-eio: don't fail switches in the I/O loop([#70](https://github.com/anmonteiro/gluten/pull/70)
- gluten-eio: open up #Eio.Flow.two_way ([#62](https://github.com/anmonteiro/gluten/pull/62)
- gluten-async: async-tls compilation error when using tls-async package ([#63](https://github.com/anmonteiro/gluten/pull/63))
- gluten-eio, gluten-async, gluten-lwt, gluten-lwt-unix: refactor: unwrap Gluten.Buffer.put in favor of exceptions ([#58](https://github.com/anmonteiro/gluten/pull/58))

0.4.1 2023-03-16
--------------

- gluten-lwt-unix: require tls-lwt `>= 0.16`
  ([#53](https://github.com/anmonteiro/gluten/pull/53))
- gluten-eio: adapt to `Eio.Io` errors
  ([#54](https://github.com/anmonteiro/gluten/pull/54))
- gluten-eio: return a `Eio.Promise.t` from `Gluten_eio.Client.shutdown`
  ([f8b88c485](https://github.com/anmonteiro/gluten/commit/f8b88c485beb473af97de7b39461fb60a56cff3f))

0.4.0 2023-02-11
--------------

- gluten-eio: Add `gluten-eio` package, a gluten backend for
  [eio](https://github.com/ocaml-multicore/eio)
  ([#35](https://github.com/anmonteiro/gluten/pull/35))
- gluten-async: Allow connecting to a UNIX domain socket
  ([#40](https://github.com/anmonteiro/gluten/pull/40))
- gluten-async: Fix memory leak in the SSL / TLS implementations
  ([#48](https://github.com/anmonteiro/gluten/pull/48))


0.3.0 2022-08-08
--------------

- gluten-lwt, gluten-lwt-unix: in the OpenSSL backend, select the ALPN protocol
  properly ([#20](https://github.com/anmonteiro/gluten/pull/20))
- Make `(select ...)` compatible with Dune 2.0
  ([#21](https://github.com/anmonteiro/gluten/pull/21))
- Adapt to newer conduit versions by removing `Server_with_conduit`
  ([#22](https://github.com/anmonteiro/gluten/pull/22))
- gluten-async: Depend on `core` and `async` >= v.0.15.0
  ([#30](https://github.com/anmonteiro/gluten/pull/30))
- gluten-async: Add `tls-async` I/O support on the client
  ([#31](https://github.com/anmonteiro/gluten/pull/31))
- gluten-mirage: Fix `read` and `writev`
  ([#32](https://github.com/anmonteiro/gluten/pull/32)):
    - `read` now respects the length parameter of the receiving buffer and
      buffers extra bytes until the next time `read` is called
    - `writev` now copies the underlying IOVecs, as `Flow.writev` takes
      ownership of the buffers, which can't be reused

0.2.1 2020-05-16
--------------

- gluten-lwt, gluten-async, gluten-lwt-unix, gluten-mirage: never call
  `shutdown` with `SHUTDOWN_SEND`. This is especially important on the client,
  where sending a `FIN` packet might cause the other end to shutdown the
  connection without sending back a response
  ([#10](https://github.com/anmonteiro/gluten/pull/10))

0.2.1 2020-05-16
--------------

- gluten-mirage: Add a Mirage runtime
  ([#5](https://github.com/anmonteiro/gluten/pull/5))
- gluten: Remove dependency on httpaf
  ([#6](https://github.com/anmonteiro/gluten/pull/6))
- gluten-lwt-unix: Allow configuring accepted ALPN protocols in the SSL / TLS
  runtimes ([#7](https://github.com/anmonteiro/gluten/pull/7))
- gluten-async: Add an Async runtime
  ([#8](https://github.com/anmonteiro/gluten/pull/8))

0.2.0 2020-04-29
--------------

- gluten-lwt: Refactor the runtime to reuse more code
  ([#3](https://github.com/anmonteiro/gluten/pull/3))
- gluten, gluten-lwt: Change the API, support runtimes that aren't upgradable
  ([#4](https://github.com/anmonteiro/gluten/pull/4))

0.1.0 2020-04-27
--------------

- Initial public release

