open FStar_Bytes
open FStar_Error
open Unix
(* open Map *)

type networkStream = file_descr
type tcpListener = file_descr               

module IntMap = Map.Make(struct type t = int let compare = compare end)
                 
let opened_files = ref IntMap.empty;;
let opened_files_last_id = ref 0;;

exception FileDescriptor_not_found

let register_file_desc fd =
  opened_files_last_id := !opened_files_last_id + 1;
  opened_files := IntMap.add !opened_files_last_id fd !opened_files;
  Prims.of_int (!opened_files_last_id)

let find_file_desc fd_id =
  match IntMap.find_opt (Z.to_int fd_id) !opened_files with
    Some x -> x
  | None   -> raise FileDescriptor_not_found

let listen s (i: Prims.int) =
  let i = Z.to_int i in
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  let fd = (setsockopt server_sock SO_REUSEADDR true ;
   let address = inet_addr_of_string s in
   bind server_sock (ADDR_INET (address, i)) ;
   listen server_sock 10;
   server_sock) in
  register_file_desc fd

let accept s =
  let s = find_file_desc s in
  let (client_sock, client_addr) = accept s in
  register_file_desc client_sock

let acceptTimeout t s = accept s

let stop s =
  let s = find_file_desc s in
  shutdown s SHUTDOWN_ALL

let connect s i =
  let i = Z.to_int i in
  let client_sock = socket PF_INET SOCK_STREAM 0 in
  let hentry = gethostbyname s in
  connect client_sock (ADDR_INET (hentry.h_addr_list.(0), i)) ;
  register_file_desc client_sock

let connectTimeout t s i = connect s i

let sock_send sock str =
  let str = get_cbytes str in
  let len = String.length str in
  send_substring sock str 0 len []

let sock_recv sock maxlen =
  let str = Bytes.create maxlen in
  let recvlen = recv sock str 0 maxlen [] in
  let str = Bytes.sub_string str 0 recvlen in
  abytes str

type 'a recv_result = 
  | RecvWouldBlock
  | RecvError of string
  | Received of bytes

let recv_async s i =
  let i = Z.to_int i in
  try Received (sock_recv s i) with
  | Unix_error ((EAGAIN | EWOULDBLOCK),_,_) -> RecvWouldBlock
  | Unix_error (e,s1,s2) -> RecvError (Printf.sprintf "%s: %s(%s)" (error_message e) s1 s2)

(* let set_nonblock s = 
 *   let s = find_file_desc s in
 *   set_nonblock s
 * let clear_nonblock s = 
 *   let s = find_file_desc s in
 *   clear_nonblock s *)

let recv s i =
  let s = find_file_desc s in
  let i = Z.to_int i in
  try (None, Some (sock_recv s i))
  with Unix_error (e,s1,s2) ->
    (Some (Printf.sprintf "%s: %s(%s)" (error_message e) s1 s2), None)

let rec send s_id b =
  let s = find_file_desc s_id in
  try (
    let n = sock_send s b in 
    let m = String.length b in 
    if n < m
    then 
      (* send s (String.sub str n (m - n) *)
      (Some (Printf.sprintf "Network error, wrote %d bytes" n), None)
    else (None, Some ()))
  with 
  | Unix_error ((EAGAIN | EWOULDBLOCK),_,_) -> send s_id b
  | Unix_error (e,s1,s2) -> (Some (Printf.sprintf "%s: %s(%s)" (error_message e) s1 s2), None)

let close s =
  let s = find_file_desc s in
  close s
