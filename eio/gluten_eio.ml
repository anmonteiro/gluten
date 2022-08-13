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

module Buffer = Gluten.Buffer

module IO_loop = struct
  let start
      : type t.
        (module Gluten.RUNTIME with type t = t)
        -> t
        -> domain_mgr:Eio.Domain_manager.t
        -> read_buffer_size:int
        -> Eio.Flow.two_way
        -> unit
    =
   fun (module Runtime) t ~domain_mgr ~read_buffer_size socket ->
    let read_buffer = Buffer.create read_buffer_size in
    let rec read_loop () =
      let rec read_loop_step () =
        match Runtime.next_read_operation t with
        | `Read ->
          Buffer.put
            ~f:(fun buf ~off ~len k ->
              k
              @@
              match
                Eio.Flow.read socket (Cstruct.of_bigarray buf ~off ~len)
              with
              | n -> `Ok n
              | exception End_of_file -> `Eof)
            read_buffer
            (function
              | `Eof ->
                Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
                    Runtime.read_eof t bigstring ~off ~len)
                |> ignore;
                read_loop_step ()
              | `Ok _ ->
                Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
                    Runtime.read t bigstring ~off ~len)
                |> ignore;
                read_loop_step ())
        | `Yield -> Runtime.yield_reader t read_loop
        | `Close -> Eio.Flow.shutdown socket `Receive
      in
      match read_loop_step () with
      | () -> ()
      | exception exn -> Runtime.report_exn t exn
    in
    let rec write_loop () =
      let rec write_loop_step () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          let lenv, cstructs =
            List.fold_left_map
              (fun acc { Faraday.buffer; off; len } ->
                acc + len, Cstruct.of_bigarray buffer ~off ~len)
              0
              io_vectors
          in
          let iovec_source = Eio.Flow.cstruct_source cstructs in

          (match Eio.Flow.copy iovec_source socket with
          | () -> Runtime.report_write_result t (`Ok lenv)
          | exception _ -> Runtime.report_write_result t `Closed);
          write_loop_step ()
        | `Yield -> Runtime.yield_writer t write_loop
        | `Close _ -> ()
      in
      match write_loop_step () with
      | () -> ()
      | exception exn -> Runtime.report_exn t exn
    in
    let read_loop_domain () = Eio.Domain_manager.run domain_mgr read_loop in
    let write_loop_domain () = Eio.Domain_manager.run domain_mgr write_loop in
    Eio.Fiber.both read_loop_domain write_loop_domain;
    Eio.Flow.shutdown socket `All
end

module Server = struct
  module Server = Gluten.Server

  type socket = Eio.Flow.two_way
  type addr = Eio.Net.Sockaddr.stream

  let create_connection_handler
      ~read_buffer_size
      ~domain_mgr
      ~protocol
      connection
      _client_addr
      socket
    =
    let connection = Server.create ~protocol connection in
    IO_loop.start
      (module Server)
      connection
      ~domain_mgr
      ~read_buffer_size
      socket

  let create_upgradable_connection_handler
      ~read_buffer_size
      ~protocol
      ~create_protocol
      ~request_handler
      ~domain_mgr
      (client_addr : addr)
      socket
    =
    let connection =
      Server.create_upgradable
        ~protocol
        ~create:create_protocol
        (request_handler client_addr)
    in
    IO_loop.start
      (module Server)
      connection
      ~domain_mgr
      ~read_buffer_size
      socket
end

module Client = struct
  module Client_connection = Gluten.Client

  type socket = Eio.Flow.two_way

  type t =
    { connection : Client_connection.t
    ; socket : socket
    }

  let create ~domain_mgr ~read_buffer_size ~protocol t socket =
    let connection = Client_connection.create ~protocol t in
    let _domain =
      Domain.spawn (fun () ->
          IO_loop.start
            (module Client_connection)
            connection
            ~domain_mgr
            ~read_buffer_size
            socket)
    in
    { connection; socket }

  let upgrade t protocol =
    Client_connection.upgrade_protocol t.connection protocol

  let shutdown t =
    Client_connection.shutdown t.connection;
    Eio.Flow.shutdown t.socket `All

  let is_closed t = Client_connection.is_closed t.connection
  let socket t = t.socket
end
