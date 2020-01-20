module HTTP.Server

open FStar.Tactics.Tcp
open FStar.Tactics
open FStar.String

module Str = FStar.String
module L = FStar.List.Tot

open HTTP.Server.Helpers
open HTTP.Server.Types
open HTTP.Server.Types.Parse
open HTTP.Server.Types.Print
open HTTP.Server.Types.Helpers

module DM = DependentMap
module Map = FStar.OrdMap

module JSON = Data.JSON

module JS = FStar.Tactics.JavaScript
module JSN = FStar.Tactics.JavaScript.Natives

// unfold let setHD
//   #has
//   (x: handlerDataRef has)
//   (#t: Type0)
//   (key: string) (value: t)
//   : handlerDataRef (has +: (key, t))
//   = let data = setHD' x.otherData #t key value in
//     let n = {x with otherData = data} in
//     admit (); n


let (@.) = DM.get

let (<~) #x data (k, v) = DM.set #x data k v

let handlerR s exit = s * either exit httpRes
let handlerT s exit (constraints: DM.constraints) =
  s -> (m: DM.t string) -> (_: unit {normalize (DM.has constraints m)}) -> handlerR s exit

let (+:) #x = DM.op_Plus_Colon #x
let (++) #x = DM.op_Plus_Plus #x

let (>>) = DM.op_Greater_Greater

let weaken #s #exit
  #constraints1
  (#constraints2: DM.constraints {constraints2 >> constraints1})
  (h: handlerT s exit constraints1)
  : handlerT s exit constraints2
  = fun state m -> h state (admit (); m)

let applyHandlerT #s #exit #c1 #c2 = weaken #s #exit #c1 #c2


let basicReqs: DM.constraints = [
          "raw-body", string;
          "method", httpMethod;
          "uri", string;
          "headers", (string -> option string);
          "Content-Type", contentType;
        ]

unfold let liftHttpReq (req: httpReq): (r: DM.t string {
       normalize (DM.has basicReqs r)
  }) =  
  let h = getHeader req.httpReqHeaders in
  let line = req.httpReqLine in
  DM.empty <~ ("raw-body", req.httpReqBody)
           <~ ("method", line.httpRequestLineMethod)
           <~ ("uri", line.httpRequestLineRequestURI)
           <~ ("headers", h)
           <~ ("Content-Type", parseContentType (fromOption (h "Content-Type") ""))
           
let (!) #s #e #c1 #c2 = weaken #s #e #c1 #c2

let rec loop
  #exit #s
  (handler: handlerT s exit basicReqs)
  (on_error: s -> (error: string) -> s * either exit httpRes)
  (state: s)
  socket: Tac exit =
  let socket' = accept socket in
  let continue (v: handlerR _ _): Tac _ =
    let state', res = v in
    match res with
    | Inl exit -> close socket'; close socket; exit
    | Inr res -> let _ = send socket' (printHttpRes res) in 
                loop handler on_error state' socket
  in
  match recv socket' 2048 with
  | (Some err, None) -> continue (on_error state err)
  | (None, Some body) -> (
    ( match parseHttpReq body with
    | Some req -> continue (handler state (liftHttpReq req) (
      ()// assert_norm (DM.has basicReqs (liftHttpReq req))
    ))
    | None     -> 
        let res = mkHttpRes200Html "Error" in
        let _ = send socket' (printHttpRes res) in 
        loop handler on_error  state socket
    )
  )

let split_in_2 (cut: char) (s: string): string * string = 
  let cut = [cut] in
  match String.split cut s with
  | hd::tl -> hd, String.concat (string_of_list cut) tl
  | [hd] -> hd, ""
  | _ -> "", ""

let normalize_same
  c1 map
  : Lemma
    (requires DM.has c1 map)
    (ensures normalize (DM.has c1 map))
  = assert_norm (normalize (DM.has c1 map) == DM.has c1 map)

let makeHandlerTransformer
  required brings
  (prog: (data: DM.t string)
       -> (_: unit {DM.has required data})
       -> (data': DM.t string {
                DM.only_changes_are data data' brings
         })
       )
  #s #exit extra
  (h: handlerT s exit (normalize_term (extra ++ required ++ brings)))
  : Tot (handlerT s exit (normalize_term (extra ++ required)))
  = fun (state: s) data (proof: unit {normalize (DM.has (extra ++ required) data)
  })
    -> DM.normalize_same (extra ++ required) data;
      let data' = prog data (DM.append_lemma extra required data) in
      DM.only_changes_implies data data'
             (extra ++ required) brings;
      assert (DM.has (extra ++ required ++ brings) data');
      normalize_same (extra ++ required ++ brings) data';
      h state data' ()

let hasJSON = "post-json", option (either JSON.jsonValue string)
let parseJSON
  = makeHandlerTransformer basicReqs [hasJSON]
    (fun data proof
      ->
      data <~ 
        ( "post-json"
        , ( if Application_Json? (data @. "Content-Type")
            then Some (JSON.parse (data @. "raw-body"))
            else None
          )
        )
    )


let hasUrlencoded = "post-url-encoded", option (list (string * string))
let parseUrlencoded
  = makeHandlerTransformer basicReqs [hasUrlencoded]
    (fun data proof
      ->
      data <~ 
        ( "post-url-encoded"
        , ( if Application_XWwwFormUrlencoded? (data @. "Content-Type")
            then Some (L.map (split_in_2 '=')
              (split ['&'] (data @. "raw-body")))
            else None
          )
        )
    )


























(*
// let solver (): Tac unit =
//   compute ()

let sillyForm = "
<form method=post enctype='application/json'>
  <input name='name' value='Bender'>
  <select name='hind'>
    <option selected>Bitable</option>
    <option>Kickable</option>
  </select>
  <input type='checkbox' name='shiny' checked>
</form>
        
        
        "

module N = FStar.Tactics.JavaScript

let rec mkList (n: nat): list int 
  = if n = 0
    then []
    else n::mkList (n-1)

let max n m = if n > m then n else m

module HTML = FStar.Tactics.JavaScript.HTML
module VDom = FStar.Tactics.JavaScript.VirtualDOM

type message =
  | ChangeColor: string -> message
  | ChangeText: string -> message
  | RequestRandom

let test (): JS.JS unit =
  let open VDom in
  let open HTML in
  let open N in
  startElm
    #(string * string)
    #message
    (fun _ -> ("initial value", "red"), [])
    (fun msg (text, color) -> 
      ( match msg with
      | ChangeText text -> (text, color), []
      | ChangeColor color -> (text, color), []
      | RequestRandom -> (text, color), [
          RandomNumber 0 3 (fun i -> 
            let Some color = admit (); L.nth ["orange"; "pink"; "green"] i in
            ChangeColor color
          )
        ]
      )
      )
    (fun (text, color) f -> 
      
      let style s = TextAttribute "style" (Some s) in
      let value s = TextAttribute "value" (Some s) in
      div
        [style "background: #EEE; padding: 20px;"]
        [ input [ oninput (fun text -> f (ChangeText text))
                ; value text
                ] []
        ; br []
        ; input [ style ("color: "^color^"; font-size: 20px;")
                ; oninput (fun color -> f (ChangeColor color))
                ; value color
                ] []
        ; br []
        ; begin
          let n i
                = div []
                [ span [] [ HtmlRaw (string_of_int i ^ ". ") ]
                ; a [ style "color: red; font-size: 20px;"
                ; onclick (fun () -> f (ChangeText (text ^ "x")))
                ]
                [ HtmlRaw (if text = "" then "empty :(" else text) ]
                ; br []
                ]
            in
            div [] (L.map n (mkList (max 1 (String.length text))))
         end
         ; br []
         ; button [
             onclick (fun () -> f RequestRandom)
           ] []
         ]
      )
    []

let testJS: string = _ by (
  let s = JS.jsCodeOf
      VDom.additional_cons
      VDom.additional_funs
      VDom.extra
      (`test) in
  exact (quote s)
)

open DependentMap

let socket: Tactics.TcpI.tcpListener = _ by (
    let t = listen "0.0.0.0" 8085 in
    exact (quote t)
  )



let example () =
  let handle = fun s req (_: unit {
      normalize (DM.has (basicReqs ++ [hasUrlencoded;hasJSON]) req)
    }) -> 
    let uri = req @. "uri" in
    let json: option (either JSON.jsonValue string) = req @. "post-json" in
    let urlencoded: option (list (string * string)) = req @. "post-url-encoded" in 
    let res = mkHttpRes200Html (
          "<script src=\"//wzrd.in/standalone/virtual-dom@latest\"></script>"
        ^ "<script type=\"text/javascript\">" ^ testJS ^ "</script>"
        ^ "I've just received: '"
        ^ (req @. "raw-body")
        ^ "', from '"
        ^ uri
        ^ "' ["
        ^ string_of_int s
        ^ "] " ^ sillyForm ^ " ^<br/>JSON=<pre>" ^ (
          match json with
          | Some (Inl x) -> JSON.stringify x "  "
          | _ -> "NO JSON FOUND"
        ) ^ "</pre><br/>URLENCODED=<pre>"^
        (String.concat "\n" (L.map (fun (x,y) -> x ^ " => " ^ y) (fromOption urlencoded []))
        )
        ^ "</pre> <div id=render></div>"
    ) in
    s+1, Inr res
  in
  loop
    (parseUrlencoded [] (parseJSON [hasUrlencoded] handle))
    (fun s e -> s, Inl e) 0 socket

let _ = assert (true) by (
    let x = example () in
    dump x
  )


// *)





