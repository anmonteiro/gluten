(*----------------------------------------------------------------------------
 *  Copyright (c) 2018 Inhabited Type LLC.
 *  Copyright (c) 2018 Anton Bachin
 *  Copyright (c) 2019-2020 Antonio N. Monteiro.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the author nor the names of his contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
 *  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 *  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

open Lwt.Infix
module Buffer = Gluten.Buffer
include Gluten_lwt_intf

module IO_loop = struct
  let start :
      type t fd.
      (module IO with type socket = fd)
      -> (module Gluten.RUNTIME with type t = t)
      -> t
      -> read_buffer_size:int
      -> fd
      -> unit Lwt.t
    =
   fun (module Io) (module Runtime) t ~read_buffer_size socket ->
    let read_buffer = Buffer.create read_buffer_size in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in
    let rec read_loop () =
      let rec read_loop_step () =
        match Runtime.next_read_operation t with
        | `Read ->
          Buffer.put
            ~f:(fun buf ~off ~len k ->
              Lwt.on_success (Io.read socket buf ~off ~len) k)
            read_buffer
            (function
              | `Eof ->
                let (_ : int) =
                  Buffer.get read_buffer ~f:(Runtime.read_eof t)
                in
                read_loop_step ()
              | `Ok _ ->
                let (_ : int) = Buffer.get read_buffer ~f:(Runtime.read t) in
                read_loop_step ())
        | `Yield -> Runtime.yield_reader t read_loop
        | `Close ->
          Lwt.wakeup_later notify_read_loop_exited ();
          Io.shutdown_receive socket
      in
      Lwt.async (fun () ->
          Lwt.catch (Lwt.wrap1 read_loop_step) (fun exn ->
              Runtime.report_exn t exn;
              Lwt.return_unit))
    in
    let writev = Io.writev socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in
    let rec write_loop () =
      let rec write_loop_step () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Runtime.report_write_result t result;
          write_loop_step ()
        | `Yield ->
          Runtime.yield_writer t write_loop;
          Lwt.return_unit
        | `Close _ ->
          Lwt.wakeup_later notify_write_loop_exited ();
          Lwt.return_unit
      in
      Lwt.async (fun () ->
          Lwt.catch write_loop_step (fun exn ->
              Runtime.report_exn t exn;
              Lwt.return_unit))
    in
    read_loop ();
    write_loop ();
    Lwt.join [ read_loop_exited; write_loop_exited ] >>= fun () ->
    Io.close socket
end

module Server (Io : IO) = struct
  module Server = Gluten.Server

  type socket = Io.socket
  type addr = Io.addr

  let create_connection_handler
      ~read_buffer_size
      ~protocol
      connection
      _client_addr
      socket
    =
    let connection = Server.create ~protocol connection in
    IO_loop.start
      (module Io)
      (module Server)
      connection
      ~read_buffer_size
      socket

  let create_upgradable_connection_handler
      ~read_buffer_size
      ~protocol
      ~create_protocol
      ~request_handler
      client_addr
      socket
    =
    let connection =
      Server.create_upgradable
        ~protocol
        ~create:create_protocol
        (request_handler client_addr)
    in
    IO_loop.start
      (module Io)
      (module Server)
      connection
      ~read_buffer_size
      socket
end

module Client (Io : IO) = struct
  module Client_connection = Gluten.Client

  type socket = Io.socket

  type t =
    { connection : Client_connection.t
    ; socket : socket
    }

  let create ~read_buffer_size ~protocol t socket =
    let connection = Client_connection.create ~protocol t in
    Lwt.async (fun () ->
        IO_loop.start
          (module Io)
          (module Client_connection)
          connection
          ~read_buffer_size
          socket);
    Lwt.return { connection; socket }

  let upgrade t protocol =
    Client_connection.upgrade_protocol t.connection protocol

  let shutdown t =
    Client_connection.shutdown t.connection;
    Io.close t.socket

  let is_closed t = Client_connection.is_closed t.connection
  let socket t = t.socket
end
