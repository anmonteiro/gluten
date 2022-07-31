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

open Lwt.Infix

module type Flow = sig
  module Mirage : Mirage_flow.S

  type data = (Cstruct.t Mirage_flow.or_eof, Mirage.error) result
  type t

  val create : Mirage.flow -> t
  val read : t -> int -> data Lwt.t
  val writev : t -> Cstruct.t list -> (unit, Mirage.write_error) result Lwt.t
  val close : t -> unit Lwt.t
end

module Make_flow (Flow : Mirage_flow.S) : Flow with module Mirage = Flow =
struct
  module Mirage = Flow

  type data = (Cstruct.t Mirage_flow.or_eof, Mirage.error) result

  type t = {
    flow : Flow.flow;
    mutable buf : Cstruct.t;
  }

  let create flow = { flow; buf = Cstruct.empty }

  let writev v bufs =
    let len = Cstruct.lenv bufs in
    let data = Cstruct.create_unsafe len in
    let _, _ = Cstruct.fillv ~src:bufs ~dst:data in
    Mirage.write v.flow data

  let close v = Mirage.close v.flow

  let read v len =
    let trunc buf =
      match Cstruct.length buf > len with
      | false ->
        buf
      | true ->
        let head, rest = Cstruct.split buf len in
        v.buf <- rest;
        head
    in
    let buffered_data =
      match Cstruct.is_empty v.buf with
      | true ->
        None
      | false ->
        let buf = v.buf in
        v.buf <- Cstruct.empty;
        Some (Ok (`Data (trunc buf)))
    in
    match buffered_data with
    | Some data ->
      Lwt.return data
    | None -> (
      Flow.read v.flow >|= fun data ->
      assert (Cstruct.is_empty v.buf);
      match data with Ok (`Data buf) -> Ok (`Data (trunc buf)) | x -> x)
end

module Make_IO (Flow : Flow) :
  Gluten_lwt.IO with type socket = Flow.t and type addr = unit = struct
  type socket = Flow.t
  type addr = unit

  let shutdown flow = Flow.close flow
  let shutdown_receive flow = Lwt.async (fun () -> shutdown flow)
  let close = shutdown

  let read flow bigstring ~off ~len =
    Lwt.catch
      (fun () ->
        Flow.read flow len >|= function
        | Ok (`Data buf) ->
          Bigstringaf.blit buf.buffer ~src_off:buf.off bigstring ~dst_off:off
            ~len:buf.len;
          `Ok buf.len
        | Ok `Eof ->
          `Eof
        | Error error ->
          failwith (Format.asprintf "%a" Flow.Mirage.pp_error error))
      (fun exn -> shutdown flow >>= fun () -> Lwt.fail exn)

  let writev flow iovecs =
    let cstruct_iovecs =
      List.map
        (fun { Faraday.buffer; off; len } ->
          Cstruct.of_bigarray ~off ~len buffer)
        iovecs
    in
    Lwt.catch
      (fun () ->
        Flow.writev flow cstruct_iovecs >|= fun x ->
        match x with
        | Ok () ->
          `Ok (Cstruct.lenv cstruct_iovecs)
        | Error `Closed ->
          `Closed
        | Error other_error ->
          raise
            (Failure
               (Format.asprintf "%a" Flow.Mirage.pp_write_error other_error)))
      (fun exn -> shutdown flow >>= fun () -> Lwt.fail exn)
end

module Server (F : Flow) = Gluten_lwt.Server (Make_IO (F))
module Client (F : Flow) = Gluten_lwt.Client (Make_IO (F))
