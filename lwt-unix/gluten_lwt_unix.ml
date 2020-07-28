(*----------------------------------------------------------------------------
 * Copyright (c) 2019-2020, António Nuno Monteiro
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

open Lwt.Infix

module Io :
  Gluten_lwt.IO
    with type socket = Lwt_unix.file_descr
     and type addr = Unix.sockaddr = struct
  type socket = Lwt_unix.file_descr

  type addr = Unix.sockaddr

  let close socket =
    match Lwt_unix.state socket with
    | Closed ->
      Lwt.return_unit
    | _ ->
      Lwt.catch
        (fun () ->
          Lwt_unix.shutdown socket SHUTDOWN_ALL;
          Lwt_unix.close socket)
        (fun _exn -> Lwt.return_unit)

  let read socket bigstring ~off ~len =
    Lwt.catch
      (fun () ->
        Lwt_bytes.read socket bigstring off len >|= function
        | 0 ->
          `Eof
        | n ->
          `Ok n)
      (function
        | Unix.Unix_error (Unix.EBADF, _, _) ->
          (* If the socket is closed we need to feed EOF to the state machine. *)
          Lwt.return `Eof
        | exn ->
          Lwt.async (fun () -> close socket);
          Lwt.fail exn)

  let writev socket = Faraday_lwt_unix.writev_of_fd socket

  let shutdown socket command =
    if Lwt_unix.state socket <> Lwt_unix.Closed then
      try Lwt_unix.shutdown socket command with
      | Unix.Unix_error (Unix.ENOTCONN, _, _) ->
        ()

  let shutdown_receive socket = shutdown socket Unix.SHUTDOWN_RECEIVE
end

module Server = struct
  include Gluten_lwt.Server (Io)

  module TLS = struct
    include Gluten_lwt.Server (Tls_io.Io)

    let create_default ?alpn_protocols ~certfile ~keyfile =
      let make_tls_server =
        Tls_io.make_server ?alpn_protocols ~certfile ~keyfile
      in
      fun _client_addr socket -> make_tls_server socket
  end

  module SSL = struct
    include Gluten_lwt.Server (Ssl_io.Io)

    let create_default ?alpn_protocols ~certfile ~keyfile =
      let make_ssl_server =
        Ssl_io.make_server ?alpn_protocols ~certfile ~keyfile
      in
      fun _client_addr socket -> make_ssl_server socket
  end
end

module Client = struct
  include Gluten_lwt.Client (Io)

  module TLS = struct
    include Gluten_lwt.Client (Tls_io.Io)

    let create_default ?alpn_protocols socket =
      Tls_io.make_client ?alpn_protocols socket
  end

  module SSL = struct
    include Gluten_lwt.Client (Ssl_io.Io)

    let create_default ?alpn_protocols socket =
      Ssl_io.make_default_client ?alpn_protocols socket
  end
end
