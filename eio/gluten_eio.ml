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
  let writev socket iovecs =
    let lenv, cstructs =
      List.fold_left_map
        (fun acc { Faraday.buffer; off; len } ->
          acc + len, Cstruct.of_bigarray buffer ~off ~len)
        0
        iovecs
    in
    match Eio.Flow.write socket cstructs with
    | () -> `Ok lenv
    | exception End_of_file -> `Closed

  let read_inner flow buffer =
    let p, u = Promise.create () in
    Buffer.put
      ~f:(fun buf ~off ~len k ->
        match Eio.Flow.single_read flow (Cstruct.of_bigarray buf ~off ~len) with
        | n -> k (`Ok n)
        | exception
            ( End_of_file
            | Unix.Unix_error (ENOTCONN, _, _)
            | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (_, _, _)), _)
            | Eio.Io (Eio.Net.E (Connection_reset _), _) ) ->
          (* TODO(anmonteiro): logging? *)
          k `Eof)
      buffer
      (Promise.resolve u);
    Promise.await p

  let rec read flow buffer =
    match read_inner flow buffer with
    | r -> r
    | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
      Fiber.yield ();
      read flow buffer

  let shutdown flow cmd =
    try Eio.Flow.shutdown flow cmd with
    | Unix.Unix_error (ENOTCONN, _, _)
    | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENOTCONN, _, _)), _) ->
      ()

  let start :
      type t.
      (module Gluten.RUNTIME with type t = t)
      -> read_buffer_size:int
      -> cancel:unit Promise.t
      -> t
      -> #Eio.Flow.two_way
      -> unit
    =
   fun (module Runtime) ~read_buffer_size ~cancel t socket ->
    let read_buffer = Buffer.create read_buffer_size in
    let rec read_loop () =
      let rec read_loop_step () =
        match Runtime.next_read_operation t with
        | `Read ->
          let read_result =
            Fiber.first
              (fun () -> read socket read_buffer)
              (fun () ->
                Promise.await cancel;
                `Eof)
          in
          let (_ : int) =
            Buffer.get read_buffer ~f:(fun buf ~off ~len ->
                match read_result with
                | `Eof -> Runtime.read_eof t buf ~off ~len
                | `Ok _n -> Runtime.read t buf ~off ~len)
          in
          read_loop_step ()
        | `Yield ->
          let p, u = Promise.create () in
          Runtime.yield_reader t (Promise.resolve u);
          Promise.await p;
          read_loop ()
        | `Close -> shutdown socket `Receive
      in
      match read_loop_step () with
      | () -> ()
      | exception exn -> Runtime.report_exn t exn
    in
    let rec write_loop () =
      let rec write_loop_step () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          let write_result = writev socket io_vectors in
          Runtime.report_write_result t write_result;
          write_loop_step ()
        | `Yield ->
          let p, u = Promise.create () in
          Runtime.yield_writer t (Promise.resolve u);
          Promise.await p;
          write_loop ()
        | `Close _ -> shutdown socket `Send
      in
      match write_loop_step () with
      | () -> ()
      | exception exn -> Runtime.report_exn t exn
    in
    Fiber.both read_loop write_loop
end

module Server = struct
  type addr = Eio.Net.Sockaddr.stream

  let create_connection_handler
      ~read_buffer_size
      ~protocol
      connection
      _client_addr
      socket
    =
    let connection = Gluten.Server.create ~protocol connection in
    let never, _ = Promise.create () in
    IO_loop.start
      (module Gluten.Server)
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
      Gluten.Server.create_upgradable
        ~protocol
        ~create:create_protocol
        (request_handler client_addr)
    in
    IO_loop.start
      (module Gluten.Server)
      ~read_buffer_size
      ~cancel:never
      connection
      socket
end

module Client = struct
  type t =
    { connection : Gluten.Client.t
    ; socket : Eio.Flow.two_way
    ; shutdown_reader : unit -> unit
    ; shutdown_complete : unit Promise.t
    }

  let create ~sw ~read_buffer_size ~protocol t socket =
    let connection = Gluten.Client.create ~protocol t in
    let shutdown_p, shutdown_u = Promise.create () in
    let cancel_reader, resolve_cancel_reader = Promise.create () in
    Fiber.fork ~sw (fun () ->
        Fun.protect ~finally:(Promise.resolve shutdown_u) (fun () ->
            Switch.run (fun sw ->
                Fiber.fork ~sw (fun () ->
                    IO_loop.start
                      (module Gluten.Client)
                      ~cancel:cancel_reader
                      ~read_buffer_size
                      connection
                      socket))));
    { connection
    ; socket
    ; shutdown_reader = Promise.resolve resolve_cancel_reader
    ; shutdown_complete = shutdown_p
    }

  let upgrade t protocol = Gluten.Client.upgrade_protocol t.connection protocol

  let shutdown t =
    t.shutdown_reader ();
    Gluten.Client.shutdown t.connection;
    t.shutdown_complete

  let is_closed t = Gluten.Client.is_closed t.connection
  let socket t = t.socket
end
