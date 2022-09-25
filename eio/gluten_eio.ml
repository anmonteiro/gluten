(*----------------------------------------------------------------------------
 *  Copyright (c) 2022 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

open Eio.Std
module Buffer = Gluten.Buffer

module IO_loop = struct
  let start
      : type t fd.
        (module Gluten_eio_intf.IO with type socket = fd)
        -> (module Gluten.RUNTIME with type t = t)
        -> read_buffer_size:int
        -> cancel:unit Promise.t
        -> t
        -> fd
        -> unit
    =
   fun (module Io) (module Runtime) ~read_buffer_size ~cancel t socket ->
    let read_buffer = Buffer.create read_buffer_size in
    let rec read_loop () =
      let rec read_loop_step () =
        match Runtime.next_read_operation t with
        | `Read ->
          let read_result =
            Fiber.first
              (fun () ->
                let p, u = Promise.create () in
                try
                  Buffer.put
                    ~f:(fun buf ~off ~len k -> k (Io.read socket buf ~off ~len))
                    read_buffer
                    (Promise.resolve u);

                  Promise.await p
                with
                | Eio.Cancel.Cancelled Eio__core__Fiber.Not_first ->
                  (* TODO(anmonteiro): consider raising End_of_file instead *)
                  `Eof)
              (fun () ->
                (* https://github.com/ocaml-multicore/eio/issues/214
                 * TODO(anmonteiro): Investigate using [shutdown flow `Send]
                 * instead on the writer. *)
                Promise.await cancel;
                `Eof)
          in
          (match read_result with
          | `Eof ->
            let (_ : int) = Buffer.get read_buffer ~f:(Runtime.read_eof t) in
            read_loop_step ()
          | `Ok _n ->
            let (_ : int) = Buffer.get read_buffer ~f:(Runtime.read t) in
            read_loop_step ())
        | `Yield ->
          let p, u = Promise.create () in
          Runtime.yield_reader t (Promise.resolve u);
          Promise.await p;
          read_loop ()
        | `Close -> Io.shutdown_receive socket
      in
      match read_loop_step () with
      | () -> ()
      | exception exn -> Runtime.report_exn t exn
    in
    let rec write_loop () =
      let rec write_loop_step () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          let write_result = Io.writev socket io_vectors in
          Runtime.report_write_result t write_result;
          write_loop_step ()
        | `Yield ->
          let p, u = Promise.create () in
          Runtime.yield_writer t (Promise.resolve u);
          Promise.await p;
          write_loop ()
        | `Close _ -> ()
      in
      match write_loop_step () with
      | () -> ()
      | exception exn -> Runtime.report_exn t exn
    in
    Fiber.both read_loop write_loop
end

module Io : Gluten_eio_intf.IO with type socket = Eio.Net.stream_socket = struct
  type socket = Eio.Net.stream_socket

  let shutdown socket cmd =
    try Eio.Flow.shutdown socket cmd with
    | Unix.Unix_error (ENOTCONN, _, _) -> ()

  let shutdown_receive socket = shutdown socket `Receive
  let close socket = shutdown socket `All

  let read socket buf ~off ~len =
    match Eio.Flow.read socket (Cstruct.of_bigarray buf ~off ~len) with
    | n -> `Ok n
    | exception (End_of_file | Unix.Unix_error (ENOTCONN, _, _)) ->
      (* TODO(anmonteiro): logging? *)
      `Eof

  let writev socket iovecs =
    let lenv, cstructs =
      List.fold_left_map
        (fun acc { Faraday.buffer; off; len } ->
          acc + len, Cstruct.of_bigarray buffer ~off ~len)
        0
        iovecs
    in
    let iovec_source = Eio.Flow.cstruct_source cstructs in
    match Eio.Flow.copy iovec_source socket with
    | () -> `Ok lenv
    | exception _ -> `Closed
end

module MakeServer (Io : Gluten_eio_intf.IO) = struct
  module Server = Gluten.Server

  type socket = Io.socket
  type addr = Eio.Net.Sockaddr.stream

  let create_connection_handler
      ~read_buffer_size
      ~protocol
      connection
      _client_addr
      socket
    =
    let connection = Server.create ~protocol connection in
    let never, _ = Promise.create () in
    IO_loop.start
      (module Io)
      (module Server)
      connection
      ~cancel:never
      ~read_buffer_size
      socket

  let create_upgradable_connection_handler
      ~read_buffer_size
      ~protocol
      ~create_protocol
      ~request_handler
      (client_addr : addr)
      socket
    =
    let never, _ = Promise.create () in
    let connection =
      Server.create_upgradable
        ~protocol
        ~create:create_protocol
        (request_handler client_addr)
    in
    IO_loop.start
      (module Io)
      (module Server)
      ~read_buffer_size
      ~cancel:never
      connection
      socket
end

module Server = struct
  module type S = Gluten_eio_intf.Server

  include MakeServer (Io)

  module SSL = struct
    include MakeServer (Ssl_io.Io)

    let create_default ?alpn_protocols ~certfile ~keyfile =
      let make_ssl_server =
        Ssl_io.make_server ?alpn_protocols ~certfile ~keyfile
      in
      fun _client_addr socket -> make_ssl_server socket
  end
end

module MakeClient (Io : Gluten_eio_intf.IO) = struct
  module Client_connection = Gluten.Client

  type socket = Io.socket

  type t =
    { connection : Client_connection.t
    ; socket : socket
    ; shutdown_reader : unit -> unit
    ; shutdown_complete : unit Promise.t
    }

  let create ~sw ~read_buffer_size ~protocol t socket =
    let connection = Client_connection.create ~protocol t in
    let shutdown_p, shutdown_u = Promise.create () in
    let cancel_reader, resolve_cancel_reader = Promise.create () in
    Fiber.fork ~sw (fun () ->
        Fun.protect ~finally:(Promise.resolve shutdown_u) (fun () ->
            Switch.run (fun sw ->
                Fiber.fork ~sw (fun () ->
                    IO_loop.start
                      (module Io)
                      (module Client_connection)
                      ~cancel:cancel_reader
                      ~read_buffer_size
                      connection
                      socket))));
    { connection
    ; socket
    ; shutdown_reader = Promise.resolve resolve_cancel_reader
    ; shutdown_complete = shutdown_p
    }

  let upgrade t protocol =
    Client_connection.upgrade_protocol t.connection protocol

  let shutdown t =
    t.shutdown_reader ();
    Client_connection.shutdown t.connection;
    Promise.await t.shutdown_complete

  let is_closed t = Client_connection.is_closed t.connection
  let socket t = t.socket
end

module Client = struct
  module type S = Gluten_eio_intf.Client

  include MakeClient (Io)

  module SSL = struct
    include MakeClient (Ssl_io.Io)

    let create_default ?alpn_protocols socket =
      Ssl_io.make_default_client ?alpn_protocols socket
  end
end
