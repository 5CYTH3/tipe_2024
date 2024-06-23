type t =
    | TVar of string
    (* Primitives *)
    | Bool
    | Str
    | Int
    (* Builtins *)
    | List of t
    | Func of t list * t
    [@@deriving show];;

type scheme = Forall of string list * t;;

exception TypeError of string

module TypeMap = Map.Make(String);;

(* String : Type *)
type env = t TypeMap.t;;
let empty_type_env = TypeMap.empty;;

let extend env k v = TypeMap.add k v env;;

let lookup env k =
    try TypeMap.find k env 
    with Not_found -> failwith "the variable is not in scope: " ^ k
;;

(* String : Type *)
type subst_set = t TypeMap.t;;
let empty_subst_set = TypeMap.empty;;

let rec substitute s t =
    match t with
    | TVar v -> begin 
        match TypeMap.find_opt v s with
        | Some x -> x
        | None -> TVar v
    end
    | Func (args, ret) -> Func (List.map (substitute s) args, substitute s ret)
    | t -> t
;;

let rec compose_subst s1 s2 =
    TypeMap.union (fun _ t _ -> Some t) (TypeMap.map (substitute s1) s2) s1
;;

let rec unify t1 t2 =
    match (t1, t2) with
    | (Func (args1, ret1), Func (args2, ret2)) when List.length args1 = List.length args2 ->
        let s1 = List.fold_left2 (fun s arg1 arg2 -> 
            compose_subst s (unify (substitute s arg1) (substitute s arg2))
        ) empty_subst_set args1 args2 in
        let s2 = unify (substitute s1 ret1) (substitute s1 ret2) in
        compose_subst s2 s1
    | TVar v, t | t, TVar v -> v |-> t
    | Int, Int | Bool, Bool | Str, Str -> empty_subst_set
    | _ -> raise (TypeError "Cannot unify types")

and (|->) v t =
    (* Binding operation from a type variable to a type (or type variable) *)
    if t = TVar v then empty_subst_set
    else if occurs_check v t then raise (TypeError "Occurs check failed")
    else TypeMap.singleton v t

and occurs_check v t =
    (* Check if the type variable is used anywhere in a function type to prevent infinite function type *)
    match t with
    | TVar u -> u = v
    | Func (args, ret) -> List.exists (occurs_check v) args || occurs_check v ret
    | _ -> false
;;

(* Instantiation is like the final substitution to replace all monotypes type variables *)
