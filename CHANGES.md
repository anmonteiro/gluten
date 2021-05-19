Unreleased
--------------

- gluten-lwt, gluten-lwt-unix: in the OpenSSL backend, select the ALPN protocol
  properly ([#20](https://github.com/anmonteiro/gluten/pull/20))
- Make `(select ...)` compatible with Dune 2.0
  ([#21](https://github.com/anmonteiro/gluten/pull/21))
- Adapt to newer conduit versions by removing `Server_with_conduit`
  ([#22](https://github.com/anmonteiro/gluten/pull/22))

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

