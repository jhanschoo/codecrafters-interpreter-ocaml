open Core

type 'a t = (string, 'a) Hashtbl.t list

let initialize (clock : 'a) =
  Hashtbl.of_alist_exn (module String) [ "clock", clock ] :: []
;;

let create (parent : 'a t) : 'a t = Hashtbl.create (module String) :: parent

let rec get (env : 'a t) (name : string) : 'a option =
  match env with
  | [] -> None
  | tbl :: rest ->
    (match Hashtbl.find tbl name with
     | Some value -> Some value
     | None -> get rest name)
;;

let define (env : 'a t) (key : string) (data : 'a) : unit =
  match env with
  | [] -> failwith "Environment.define: No table to define in."
  | tbl :: _ -> Hashtbl.set tbl ~key ~data
;;

let set (env : 'a t) (key : string) (data : 'a) : unit =
  let f tbl =
    let f _ = Hashtbl.set tbl ~key ~data in
    Hashtbl.find tbl key |> Option.map ~f
  in
  if List.find_map env ~f |> Option.is_none
  then failwith "Environment.set: No table to set in."
;;
