module Example

open FStar.Tactics
open HTTP.Server
module JS = FStar.Tactics.JavaScript
module L = FStar.List.Tot

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
          RandomNumber 0 7 (fun i -> 
            let Some color = admit (); 
              L.nth
                ["orange"; "pink"; "green";"gray";"yellow";"brown";"black";"yellow"]
                i
            in
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
           ] [ HtmlRaw "Change color randomly" ]
         ]
      )
    []

let testJS: string = _ by (
  let s = JS.jsCodeOf
      VDom.additional_cons
      VDom.additional_funs
      VDom.extra
      (`(N.setTimeout test 2000)) in
  exact (quote s)
)

module DM = DependentMap
open DependentMap
open FStar.Tactics.Tcp
module JSON = Data.JSON

open HTTP.Server.Types.Helpers
open HTTP.Server.Helpers

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
