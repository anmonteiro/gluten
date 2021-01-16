(*----------------------------------------------------------------------------
 *  Copyright (c) 2018 Inhabited Type LLC.
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

open Core
open Async

module Buffer : sig
  type t

  val create : int -> t

  val get : t -> f:(Bigstring.t -> off:int -> len:int -> int) -> int

  val put
    :  t
    -> f:(Bigstring.t -> off:int -> len:int -> [ `Eof | `Ok of int ] Deferred.t)
    -> [ `Eof | `Ok of int ] Deferred.t
end = struct
  type t =
    { buffer : Bigstring.t
    ; mutable off : int
    ; mutable len : int
    }

  let create size =
    let buffer = Bigstring.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0 then (
      t.off <- 0;
      t.len <- 0)
    else if t.off > 0 then (
      Bigstring.blit
        ~src:t.buffer
        ~src_pos:t.off
        ~dst:t.buffer
        ~dst_pos:0
        ~len:t.len;
      t.off <- 0)

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0 then
      t.off <- 0;
    n

  let put t ~f =
    compress t;
    f t.buffer ~off:(t.off + t.len) ~len:(Bigstring.length t.buffer - t.len)
    >>| function
    | `Eof ->
      `Eof
    | `Ok n as ret ->
      t.len <- t.len + n;
      ret
end

module IO_loop = struct
  let start
      : type t fd.
        (module Gluten_async_intf.IO with type socket = fd)
        -> (module Gluten.RUNTIME with type t = t)
        -> t
        -> read_buffer_size:int
        -> fd
        -> unit Deferred.t
    =
   fun (module Io) (module Runtime) t ~read_buffer_size socket ->
    let read_buffer = Buffer.create read_buffer_size in
    let read_complete = Ivar.create () in
    let rec reader_thread () =
      match Runtime.next_read_operation t with
      | `Read ->
        Buffer.put ~f:(Io.read socket) read_buffer >>> ( function
        | `Eof ->
          Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Runtime.read_eof t bigstring ~off ~len)
          |> ignore;
          reader_thread ()
        | `Ok _ ->
          Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Runtime.read t bigstring ~off ~len)
          |> ignore;
          reader_thread () )
      | `Yield ->
        Runtime.yield_reader t reader_thread
      | `Close ->
        Ivar.fill read_complete ();
        Io.shutdown_receive socket
    in
    let writev = Io.writev socket in
    let write_complete = Ivar.create () in
    let rec writer_thread () =
      match Runtime.next_write_operation t with
      | `Write iovecs ->
        writev iovecs >>> fun result ->
        Runtime.report_write_result t result;
        writer_thread ()
      | `Yield ->
        Runtime.yield_writer t writer_thread
      | `Close _ ->
        Ivar.fill write_complete ()
    in
    let conn_monitor = Monitor.create () in
    Scheduler.within ~monitor:conn_monitor reader_thread;
    Scheduler.within ~monitor:conn_monitor writer_thread;
    Monitor.detach_and_iter_errors conn_monitor ~f:(fun exn ->
        Runtime.report_exn t exn);
    (* The Tcp module will close the file descriptor once this becomes
       determined. *)
    Deferred.all_unit [ Ivar.read read_complete; Ivar.read write_complete ]
    >>= fun () -> Io.close socket
end

module Make_server (Io : Gluten_async_intf.IO) = struct
  type socket = Io.socket

  let create_connection_handler
      ~read_buffer_size ~protocol connection _client_addr socket
    =
    let connection = Gluten.Server.create ~protocol connection in
    IO_loop.start
      (module Io)
      (module Gluten.Server)
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
      Gluten.Server.create_upgradable
        ~protocol
        ~create:create_protocol
        (request_handler client_addr)
    in
    IO_loop.start
      (module Io)
      (module Gluten.Server)
      connection
      ~read_buffer_size
      socket
end

module Unix_io :
  Gluten_async_intf.IO
    with type socket = ([ `Active ], Socket.Address.Inet.t) Socket.t
     and type addr = Socket.Address.Inet.t = struct
  type socket = ([ `Active ], Socket.Address.Inet.t) Socket.t

  type addr = Socket.Address.Inet.t

  let read socket bigstring ~off ~len =
    let fd = Socket.fd socket in
    let badfd fd = failwithf "read got back fd: %s" (Fd.to_string fd) () in
    let rec finish fd buffer result =
      let open Unix.Error in
      match result with
      | `Already_closed | `Ok 0 ->
        return `Eof
      | `Ok n ->
        return (`Ok n)
      | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
        Fd.ready_to fd `Read >>= ( function
        | `Bad_fd ->
          badfd fd
        | `Closed ->
          return `Eof
        | `Ready ->
          go fd buffer )
      | `Error (Unix.Unix_error (EBADF, _, _)) ->
        badfd fd
      | `Error exn ->
        Deferred.don't_wait_for (Fd.close fd);
        raise exn
    and go fd buffer =
      if Fd.supports_nonblock fd then
        finish
          fd
          buffer
          (Fd.syscall fd ~nonblocking:true (fun file_descr ->
               Unix.Syscall_result.Int.ok_or_unix_error_exn
                 ~syscall_name:"read"
                 (Bigstring_unix.read_assume_fd_is_nonblocking
                    file_descr
                    bigstring
                    ~pos:off
                    ~len)))
      else
        Fd.syscall_in_thread fd ~name:"read" (fun file_descr ->
            Bigstring_unix.read file_descr bigstring ~pos:off ~len)
        >>= fun result -> finish fd buffer result
    in
    go fd bigstring

  let writev socket = Faraday_async.writev_of_fd (Socket.fd socket)

  let shutdown_receive socket =
    let fd = Socket.fd socket in
    if not (Fd.is_closed fd) then
      Socket.shutdown socket `Receive

  let close socket =
    let fd = Socket.fd socket in
    if not (Fd.is_closed fd) then (
      Socket.shutdown socket `Both;
      Fd.close fd)
    else
      Deferred.unit
end

module Server = struct
  include Make_server (Unix_io)

  module SSL = struct
    include Make_server (Ssl_io.Io)

    let create_default ?alpn_protocols ~certfile ~keyfile =
      let make_ssl_server =
        Ssl_io.make_server ?alpn_protocols ~certfile ~keyfile
      in
      fun _client_addr socket -> make_ssl_server socket
  end
end

module Make_client (Io : Gluten_async_intf.IO) = struct
  module Client_connection = Gluten.Client

  type socket = Io.socket

  type t =
    { connection : Client_connection.t
    ; socket : socket
    }

  let create ~read_buffer_size ~protocol t socket =
    let connection = Client_connection.create ~protocol t in
    don't_wait_for
      (IO_loop.start
         (module Io)
         (module Client_connection)
         connection
         ~read_buffer_size
         socket);
    Deferred.return { connection; socket }

  let upgrade t protocol =
    Client_connection.upgrade_protocol t.connection protocol

  let shutdown t =
    Client_connection.shutdown t.connection;
    Io.close t.socket

  let is_closed t = Client_connection.is_closed t.connection
end

module Client = struct
  include Make_client (Unix_io)

  module SSL = struct
    include Make_client (Ssl_io.Io)

    let create_default ?alpn_protocols socket =
      Ssl_io.make_default_client ?alpn_protocols socket
  end
end
