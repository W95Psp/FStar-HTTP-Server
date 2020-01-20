module FStar.Tactics.TcpI

open FStar.Tactics
open FStar.String
// open FStar.Bytes
// open FStar.Error

unfold let networkStream
  = int

unfold let tcpListener
  = int

// val set_nonblock: networkStream -> unit
// val clear_nonblock: networkStream -> unit

(* Server side *)

val listen: string -> int -> Tot tcpListener
val acceptTimeout: int -> tcpListener -> Tot networkStream
val accept: tcpListener -> Tot networkStream
val stop: tcpListener -> Tot unit

(* Client side *)

val connectTimeout: int -> string -> int -> Tot networkStream
val connect: string -> int -> Tot networkStream

(* Input/Output *)

// adding support for (potentially) non-blocking I/O
// NB for now, send *fails* on partial writes, and *loops* on EAGAIN/EWOULDBLOCK.

unfold let bytes = string
unfold let optResult a b = v: (option a * option b)
  {(Some? (fst v) /\ None? (snd v)) \/ (None? (fst v) /\ Some? (snd v))}

// type recv_result (max:int) = 
//   | RecvWouldBlock
//   | RecvError of string
//   | Received of b:bytes {length b <= max}
// val recv_async: networkStream -> max:int -> Tot (recv_result max)

val recv: networkStream -> max:int -> Tot (optResult string (b:bytes {length b <= max}))
val send: networkStream -> bytes -> Tot (optResult string unit)
val close: networkStream -> Tot unit

(* Create a network stream from a given stream.
   Only used by the application interface TLSharp. *)
(* assume val create: System.IO.Stream -> NetworkStream*)

 
