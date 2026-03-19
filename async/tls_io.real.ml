(*----------------------------------------------------------------------------
 *  Copyright (c) 2019-2020 António Nuno Monteiro
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

open Core
open Async
module IOVec = Core_unix.IOVec

(* This is now a tuple instead of a nominative record so we can provide a public
   interface that can be shared with ssl_io.dummy.ml. reader, writer, closed
   ivar *)
type 'a descriptor = Reader.t * Writer.t * unit Deferred.t
  constraint 'a = [< Socket.Address.t ]

module Io : Gluten_async_intf.IO with type 'a socket = 'a descriptor = struct
  type 'a socket = 'a descriptor

  let read (reader, _, _) bigstring ~off ~len =
    let bigsubstr = Bigsubstring.create ~pos:off ~len bigstring in
    Reader.read_bigsubstring reader bigsubstr >>| function
    | `Eof -> raise End_of_file
    | `Ok n -> n

  let writev (_, writer, _) iovecs =
    match Writer.is_closed writer with
    | true -> Deferred.return `Closed
    | false ->
      let iovecs_q = Queue.create ~capacity:(List.length iovecs) () in
      let len =
        List.fold
          ~init:0
          ~f:(fun acc { Faraday.buffer; off = pos; len } ->
            Queue.enqueue iovecs_q (IOVec.of_bigstring ~pos ~len buffer);
            acc + len)
          iovecs
      in
      Writer.schedule_iovecs writer iovecs_q;
      Writer.flushed writer >>| fun () -> `Ok len

  (* From RFC8446§6.1:
   *   The client and the server must share knowledge that the connection is
   *   ending in order to avoid a truncation attack.
   *
   * Note: In the SSL / TLS runtimes we can't just shutdown one part of the
   * full-duplex connection, as both sides must know that the underlying TLS
   * conection is closing. *)
  let shutdown_receive _ = ()

  let close (reader, writer, closed) =
    Writer.flushed writer >>= fun () ->
    Deferred.all_unit [ Writer.close writer; Reader.close reader ] >>= fun () ->
    closed
end

let connect :
     config:Tls.Config.client
    -> socket:([ `Unconnected ], ([< Socket.Address.t ] as 'a)) Socket.t
    -> where_to_connect:'a Tcp.Where_to_connect.t
    -> host:[ `host ] Domain_name.t option
    -> 'a descriptor Deferred.t
  =
 fun ~config ~socket ~where_to_connect ~host ->
  Tls_async.connect ~socket config where_to_connect ~host >>= fun res ->
  match res with
  | Error e ->
    failwithf "Gluten_async.Tls_io.connect: %s" (Error.to_string_hum e) ()
  | Ok (_session, reader, writer) ->
    let closed = Ivar.create () in
    don't_wait_for
      ( Deferred.all_unit
          [ Reader.close_finished reader; Writer.close_finished writer ]
      >>| fun () -> (Ivar.fill [@ocaml.alert "-deprecated"]) closed () );
    return (reader, writer, Ivar.read closed)

let null_auth ?ip:_ ~host:_ _ = Ok None

let make_default_client :
     ?alpn_protocols:string list
    -> ?host:[ `host ] Domain_name.t
    -> ([ `Unconnected ], ([< Socket.Address.t ] as 'b)) Socket.t
    -> 'b Tcp.Where_to_connect.t
    -> 'b descriptor Deferred.t
  =
 fun ?alpn_protocols ?host socket where_to_connect ->
  let config =
    Tls.Config.client ?alpn_protocols ~authenticator:null_auth ()
    |> Result.map_error ~f:(fun (`Msg msg) -> msg)
    |> Result.ok_or_failwith
  in
  connect ~config ~socket ~where_to_connect ~host

let reader_writer_of_sock
    ?buffer_age_limit
    ?reader_buffer_size
    ?writer_buffer_size
    s
  =
  let fd = Socket.fd s in
  ( Reader.create ?buf_len:reader_buffer_size fd
  , Writer.create ?buffer_age_limit ?buf_len:writer_buffer_size fd )

let make_server :
     ?alpn_protocols:string list
  -> certfile:string
  -> keyfile:string
  -> ([ `Active ], ([< Socket.Address.t ] as 'a)) Socket.t
  -> 'a descriptor Deferred.t
  =
 fun ?alpn_protocols ~certfile ~keyfile socket ->
  let outer_reader, outer_writer = reader_writer_of_sock socket in
  Tls_async.X509_async.Certificate.of_pem_file certfile
  |> Deferred.Or_error.ok_exn
  >>= fun certificate ->
  Tls_async.X509_async.Private_key.of_pem_file keyfile
  |> Deferred.Or_error.ok_exn
  >>= fun priv_key ->
  let config =
    Tls.Config.server
      ?alpn_protocols
      ~certificates:(`Single (certificate, priv_key))
      ()
    |> Result.map_error ~f:(fun (`Msg msg) -> msg)
    |> Result.ok_or_failwith
  in
  let descriptor_ivar = Ivar.create () in
  don't_wait_for
    (Tls_async.upgrade_server_handler
       ~config
       (fun _session inner_reader inner_writer ->
         let closed = Ivar.create () in
         don't_wait_for
           (Deferred.all_unit
              [ Reader.close_finished inner_reader
              ; Writer.close_finished inner_writer
              ]
           >>| fun () ->
           (Ivar.fill [@ocaml.alert "-deprecated"]) closed ());
         (Ivar.fill [@ocaml.alert "-deprecated"]) descriptor_ivar
           (inner_reader, inner_writer, Ivar.read closed);
         Ivar.read closed)
       outer_reader
       outer_writer);
  Ivar.read descriptor_ivar
