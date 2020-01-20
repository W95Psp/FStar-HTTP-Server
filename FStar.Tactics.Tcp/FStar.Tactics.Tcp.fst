module FStar.Tactics.Tcp

open FStar.Tactics.TcpI
open FStar.Tactics
open FStar.String

[@plugin]
val listen: string -> nat -> Tac tcpListener
let listen s m = listen s m

[@plugin]
val acceptTimeout: nat -> tcpListener -> Tac networkStream
let acceptTimeout n s = acceptTimeout n s

[@plugin]
val accept: tcpListener -> Tac networkStream
let accept s = accept s 

[@plugin]
val stop: tcpListener -> Tac unit
let stop s = stop s

[@plugin]
val connectTimeout: nat -> string -> nat -> Tac networkStream
let connectTimeout n s nn = connectTimeout n s nn

[@plugin]
val connect: string -> nat -> Tac networkStream
let connect s n = connect s n 

[@plugin]
val recv: networkStream -> max:nat -> Tac (optResult string (b:bytes {length b <= max}))
let recv s max = recv s max

[@plugin]
val send: networkStream -> bytes -> Tac (optResult string unit)
let send s b = send s b

[@plugin]
val close: networkStream -> Tac unit
let close s = close s

