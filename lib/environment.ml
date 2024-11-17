open Core

type t = (string, Value.t) Hashtbl.t list

let global =
  Hashtbl.of_alist_exn
    (module String)
    [ ( "clock"
      , Value.NativeCallable
          ( 0
          , fun _ ->
              Time_float.(now () |> to_span_since_epoch |> Span.to_sec) |> Value.Number )
      )
    ]
;;

let create (parent : t option) : t =
  Hashtbl.create (module String) :: Option.value ~default:[ global ] parent
;;

let rec get (env : t) (name : string) : Value.t option =
  match env with
  | [] -> None
  | tbl :: rest ->
    (match Hashtbl.find tbl name with
     | Some value -> Some value
     | None -> get rest name)
;;

let define (env : t) (name : string) (value : Value.t) : unit =
  match env with
  | [] -> failwith "Environment.define: No table to define in."
  | tbl :: _ -> Hashtbl.set tbl ~key:name ~data:value
;;

let set (env : t) (name : string) (value : Value.t) : unit =
  let f tbl =
    let f _ = Hashtbl.set tbl ~key:name ~data:value in
    Hashtbl.find tbl name |> Option.map ~f
  in
  if List.find_map env ~f |> Option.is_none
  then failwith "Environment.set: No table to set in."
;;
