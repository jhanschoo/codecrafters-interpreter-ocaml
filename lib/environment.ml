open Core

type t = (string, Value.t) Hashtbl.t list

let create (parent : t option) : t =
  let tbl = Hashtbl.create (module String) in
  match parent with
  | Some parent -> tbl :: parent
  | None -> tbl :: []
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
