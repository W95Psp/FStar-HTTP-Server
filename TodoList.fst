module TodoList

open FStar.Tactics
open HTTP.Server

module JS = FStar.Tactics.JavaScript
module L = FStar.List.Tot
module N = FStar.Tactics.JavaScript
module HTML = FStar.Tactics.JavaScript.HTML
module VDom = FStar.Tactics.JavaScript.VirtualDOM

module Task = TodoList.Task
module H = HTTP.Server.Helpers


type model
= { tasks: list Task.model
  ; field: string
  ; uid: nat
  ; visibility: string
  }

type flags = option model

let emptyModel () = {
    tasks = []
  ; visibility = "All"
  ; field = ""
  ; uid = 0
}

type message =
  | NoOp
  | UpdateField: string -> message
  | Add
  | UpdateTask: int * Task.message -> message
  | DeleteComplete
  | CheckAll : bool -> message
  | ChangeVisibility: string -> message

// TODO these two
let save _ = []
let focusTask elementId = []
    // Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus elementId)

let rec filterMap (f: 'a -> option 'b) (l: list 'a): list 'b =
  match l with
  | [] -> []
  | hd::tl -> match f hd with
            | Some hd -> hd::filterMap f tl
            | None    -> filterMap f tl

let update (msg: message) (mo: model): model * VDom.command message
  = match msg with
  | NoOp -> (mo, [])
  | UpdateField str -> ({mo with field = str}), save mo // TODO storage
  | Add -> let d = Task.trim mo.field in
          let mo_
            = if d = ""
              then mo
              else ({ mo with uid = mo.uid + 1
                    ; field = ""
                    ; tasks = mo.tasks @ [ Task.init d mo.uid ]})
          in (mo_, save mo_) // TODO 
  | UpdateTask (id, taskMsg) ->
          let updateTask t = if t.Task.id = id
                             then Task.update taskMsg t
                             else Some t in
          let newMo = {mo with tasks = filterMap updateTask mo.tasks } in
          ( match taskMsg with
          | Task.Focus elementId ->
              newMo, (save newMo @ focusTask elementId)
          | _ -> newMo, save newMo
          )
  | DeleteComplete ->
          let newMo = {mo with tasks = L.filter (fun t -> t.Task.completed = false) mo.tasks} in
          newMo, save newMo
  | CheckAll bool ->
          let updateTask t = {t with Task.completed = bool } in
          let newMo = { mo with tasks = L.map updateTask mo.tasks} in
          newMo, save newMo
  | ChangeVisibility visibility ->
          let newMo = { mo with visibility = visibility } in
          newMo, save mo








open HTML
let style (l: list (string * string)) = HTML.TextAttribute
    "style" 
    (Some (String.concat "" (L.map (fun (k,v) -> k ^ ":" ^ v ^ ";") l)))

let text = HTML.HtmlRaw


let class_ c = HTML.TextAttribute "className" (Some c)
let type_ c = HTML.TextAttribute "type" (Some c)
let checked c = HTML.TextAttribute "checked" (Some (string_of_bool c))
let value c = HTML.TextAttribute "value" (Some c)
let name c = HTML.TextAttribute "name" (Some c)
let id c = HTML.TextAttribute "id" (Some c)
let placeholder c = HTML.TextAttribute "placeholder" (Some c)
let autofocus c = HTML.TextAttribute "autofocus" (Some (string_of_bool c))
let for c = HTML.TextAttribute "for" (Some ( c))
let href c = HTML.TextAttribute "for" (Some ( c))
let hidden c = HTML.TextAttribute "for" (Some (string_of_bool c))



// let onkeydown (f: string -> nat -> JS unit) = EventAttribute "onkeydown" (fun e -> f 
//     (unsafe_pure_jsVal_cast #string (e @. "code"))
//     (unsafe_pure_jsVal_cast #nat (e @. "keyCode"))
//   )

// taskEntry : String -> Html Msg
let taskEntry (task: string) (f: message -> JS.JS unit) =
    header
        [ class_ "header" ]
        [ h1 [] [ text "todos" ]
        ; input
            [ class_ "new-todo"
            ; placeholder "What needs to be done?"
            ; autofocus true
            ; value task
            ; name "newTodo"
            ; oninput (fun v -> f (UpdateField v))
            ; Task.onfinish (fun _ -> f Add) (fun _ -> f NoOp)
            ]
            []
        ]

let all f l = L.fold_left (&&) true (L.map f l)

let completedGetter x = x.Task.completed

let taskList (visibility: string) (tasks: list Task.model) (f: message -> JS.JS unit) =
    let isVisible todo =
            match visibility with
            | "Completed" -> todo.Task.completed
            | "Active" -> not todo.Task.completed
            |  _ -> true in
    let allCompleted = all (fun t -> t.Task.completed) tasks in
    let cssVisibility = if tasks = [] then "hidden" else "visible" in
    section
        [ class_ "main"
        ; style [ ( "visibility", cssVisibility ) ]
        ]
        [ input
            [ class_ "toggle-all"
            ; id "toggle-all"
            ; type_ "checkbox"
            ; name "toggle"
            ; checked allCompleted
            ; onclick (fun _ -> f (CheckAll (not allCompleted)))
            ]
            []
        ; label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        ; ul
            [ class_ "todo-list" ]
            (L.map
                (fun task ->
                    let id = task.Task.id in
                    let taskView = Task.view task in
                    taskView (fun msg -> f (UpdateTask ( id, msg )))
                )
                (L.filter isVisible tasks)
            )
        ]



let visibilitySwap (uri visibility actualVisibility: string) (f: message -> JS.JS unit) =
    let className =
         if visibility = actualVisibility
         then "selected" else "" in
    li
        [ onclick (fun _ -> f (ChangeVisibility visibility)) ]
        [ a [ class_ className; href uri ] [ text visibility ] ]

let controls (visibility: string) (tasks: list Task.model) (f: message -> JS.JS unit) =
    let tasksCompleted = L.length (L.filter (fun t -> t.Task.completed) tasks) in
    let tasksLeft = L.length tasks - tasksCompleted in
    let item_ = if tasksLeft = 1 then " item"
                else " items" in
    footer
        [ class_ "footer"
        ; hidden (L.isEmpty tasks)
        ]
        [ span
            [ class_ "todo-count" ]
            [ strong [] [ text (string_of_int tasksLeft) ]
            ; text (item_ ^ " left")
            ]
        ; ul
            [ class_ "filters" ]
            [ visibilitySwap "#/" "All" visibility f
            ; text " "
            ; visibilitySwap "#/active" "Active" visibility f
            ; text " "
            ; visibilitySwap "#/completed" "Completed" visibility f
            ]
        ; button
            [ class_ "clear-completed"
            ; hidden (tasksCompleted = 0)
            ; onclick (fun _ -> f DeleteComplete)
            ]
            [ text ("Clear completed (" ^ string_of_int tasksCompleted ^ ")") ]
        ]


let view (mo: model) (f: message -> JS.JS unit) =
    div
        [ class_ "todomvc-wrapper"
        // ; style [ "visibility", "hidden" ]
        ]
        [ section
            [ class_ "todoapp" ]
            [ taskEntry mo.field f
            ; taskList mo.visibility mo.tasks f
            ; controls mo.visibility mo.tasks f
            ]
        ]












open FStar.Tactics.Tcp
// let socket: Tactics.TcpI.tcpListener = _ by (
//     let t = listen "0.0.0.0" 8090 in
//     exact (quote t)
//   )
  
module DM = DependentMap
module JSON = Data.JSON


let app (): JS.JS unit =
  let open VDom in
  let open HTML in
  let open N in
  startElm
    #model
    #message
    (fun () -> emptyModel (), [])
    update
    view
    []


let js_of_app: string = _ by (
  let s = JS.jsCodeOf
      VDom.additional_cons
      VDom.additional_funs
      VDom.extra
      (`(N.setTimeout app 2000)) in
  exact (quote s)
)


let writeToFile file content
  = let _ = launch_process "sh" ["-c"; "cat - > " ^ file] content in
    ()

let _: unit = _ by (
  let content =  "
     <link rel=\"stylesheet\" href=\"http://todomvc.com/examples/elm/node_modules/todomvc-app-css/index.css\">
  <script src=\"http://wzrd.in/standalone/virtual-dom@latest\"></script>"
        ^ "<script type=\"text/javascript\">" ^ js_of_app ^ "</script> <div id=render></div>" in
  writeToFile "todo-app.html" content;
  exact (`())
)


// open HTTP.Server.Types.Helpers
// let example () =
//   let handle = fun s req (_: unit {
//       normalize (DM.has (basicReqs ++ [hasUrlencoded;hasJSON]) req)
//     }) -> 
//     let res = mkHttpRes200Html (
//           "<script src=\"//wzrd.in/standalone/virtual-dom@latest\"></script>"
//         ^ "<script type=\"text/javascript\">" ^ js_of_app ^ "</script> <div id=render></div>"
//     ) in
//     s+1, Inr res
//   in
//   loop
//     (parseUrlencoded [] (parseJSON [hasUrlencoded] handle))
//     (fun s e -> s, Inl e) 0 socket

// let _ = assert (true) by (
//     let x = example () in
//     dump x
//   )








