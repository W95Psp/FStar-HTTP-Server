module TodoList.Task

open FStar.Tactics
open HTTP.Server

module JS = FStar.Tactics.JavaScript
module L = FStar.List.Tot
module N = FStar.Tactics.JavaScript
module HTML = FStar.Tactics.JavaScript.HTML
module VDom = FStar.Tactics.JavaScript.VirtualDOM
module H = HTTP.Server.Helpers

let notSpace c = match c with|"\t"|" "->false|_->true
let mkListFunStrFun f s = String.concat ""
    (f (L.map String.string_of_list [String.list_of_string s]))
let trimRight s = mkListFunStrFun (H.trimRightList notSpace) s
let trimLeft s = mkListFunStrFun (H.trimLeftList notSpace) s
let trim s = mkListFunStrFun (H.trimList notSpace) s

let onfinish (enter escape: unit -> JS.JS unit) = HTML.onkeydown (fun _ c -> 
  if c = 13 then enter () else escape ()
)

type model
=  { description: string
   ; completed: bool
   ; edits: option string
   ; id: int
   }

type message =
  | Focus : string -> message
  | Edit : string -> message
  | Cancel
  | Commit
  | Completed : bool -> message
  | Delete

let update msg model
  = match msg with
  | Focus elementId -> Some ({ model with edits = Some model.description })
  | Edit description -> Some ({ model with edits = Some description })
  | Cancel -> Some ({model with edits = None })
  | Commit -> begin 
               match model.edits with
               | None -> Some model
               | Some rawDescription ->
                      let description = trim rawDescription in
                      if description = ""
                      then None
                      else Some ({model with edits = None; description = description})
             end
  | Completed bool -> Some ({model with completed = bool})
  | Delete -> None
  | _ -> None

let fromOption d o =
  match o with
  | Some o -> o
  | None   -> d

let class_ c = HTML.TextAttribute "className" (Some c)
let type_ c = HTML.TextAttribute "type" (Some c)
let checked c = HTML.TextAttribute "checked" (Some (string_of_bool c))
let value c = HTML.TextAttribute "value" (Some c)
let name c = HTML.TextAttribute "name" (Some c)
let id c = HTML.TextAttribute "id" (Some c)

let init (desc: string) (id: int): model
  = {description = desc; completed = false; edits = None; id = id}

let view model (f: message -> JS.JS unit) =
    let open HTML in
    let open VDom in
    let className =
            (if model.completed then "completed "
             else "") ^ 
            (match model.edits with
            | Some _ -> "editing"
            | None -> ""
            ) in
    let description = fromOption model.description model.edits in
    let elementId = "todo-" ^ string_of_int model.id
    in
        li
            [ class_ className ]
            [ div
                [ class_ "view" ]
                [ input
                    ([ class_ "toggle"
                    ; type_ "checkbox"
                    // ; checked model.completed
                    ; onclick (fun _ -> f (Completed (not model.completed)))
                    ] @ (if model.completed then [checked true] else []))
                    []
                ; label
                    [ ondblclick (fun _ ->  f (Focus elementId)) ]
                    [ HtmlRaw description ]
                ; button
                    [ class_ "destroy"
                    ; onclick (fun _ -> f Delete)
                    ]
                    []
                ]
            ; input
                [ class_ "edit"
                ; value description
                ; name "title"
                ; id (elementId)
                ; oninput (fun v -> f (Edit v))
                ; onblur (fun _ -> f Commit)
                // ; onfinish (fun _ -> f Commit) (fun _ -> f Cancel)
                ]
                []
            ]

