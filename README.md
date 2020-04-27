# gluten

gluten implements platform specific runtime code for driving network libraries
based on state machines, such as
[http/af](https://github.com/anmonteiro/httpaf),
[h2](https://github.com/anmonteiro/ocaml-h2) and
[websocketaf](https://github.com/anmonteiro/websocketaf).

It also additionally provides a self-contained solution for upgrading a
connection to a different protocol, which can easily be used to implement
HTTP/1.1 upgrades to Websockets, or to start HTTP/2 communication over the
`h2c` (HTTP/2 over plaintext TCP) upgrade mechanism.

## License

gluten is distributed under the 3-Clause BSD License, see [LICENSE](./LICENSE).

This source distribution includes work based on
[http/af](https://github.com/inhabitedtype/httpaf). http/af's license file is
included in [httpaf.LICENSE](./httpaf.LICENSE).
