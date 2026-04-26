open Parser_plaf.Ast
open Parser_plaf.Parser
open ReM
open Unif

let agreement_equations ctx1 ctx2 ctx3 =
  let ctx_arr = [|ctx1;ctx2;ctx3|]
  in
  (* place largest at the beginning of the array *)
  Array.sort (fun c1 c2 -> SubsMap.cardinal c2-SubsMap.cardinal c1)
    ctx_arr;
  (* now compute conflict equations *)
  SubsMap.fold
    (fun id ty ac ->
       match SubsMap.find_opt id ctx_arr.(1),SubsMap.find_opt id ctx_arr.(2)  with
       | Some ty',Some ty'' -> EqSet.(add (ty',ty'') (add (ty,ty') ac))
       | Some ty',None -> EqSet.add (ty,ty') ac
       | None,Some ty'-> EqSet.add (ty,ty') ac
       | None,None -> ac)
    ctx_arr.(0)
    EqSet.empty

let rec infer_expr : expr -> (texpr SubsMap.t*texpr) result =
  fun e ->
  match e with
  | Int(n) -> returnE (SubsMap.empty, IntType)
  | Var(id) ->
    let tname = TypeVar (fresh_string "a") in
    let ctx = SubsMap.singleton id tname in
    returnE (ctx, tname)
  | IsZero(e) ->
    infer_expr e >>>= fun (ctx, ty) ->
    let eqs = EqSet.singleton (ty, IntType)
    in unify eqs >>>= fun mgu ->
    returnE (apply_tsubs_to_context ctx mgu, BoolType)
  | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) ->
    infer_expr e1 >>>= fun (ctx1, ty1) ->
    infer_expr e2 >>>= fun (ctx2, ty2) ->
    let agreeEqs = agreement_equations ctx1 ctx2 SubsMap.empty in
    let intEqs = EqSet.of_list [(ty1, IntType); (ty2, IntType)] in
    let unionEqs = EqSet.union agreeEqs intEqs
    in unify unionEqs >>>= fun mgu ->
    let ctx = subs_union "Binary Operation Error" (apply_tsubs_to_context ctx1 mgu) (apply_tsubs_to_context ctx2 mgu)
    in returnE (ctx, IntType)
  | ITE(e1, e2, e3) ->
    infer_expr e1 >>>= fun (ctx1, ty1) ->
    infer_expr e2 >>>= fun (ctx2, ty2) ->
    infer_expr e3 >>>= fun (ctx3, ty3) ->
    let agreeEqs = agreement_equations ctx1 ctx2 ctx3 in
    let iteEqs = EqSet.of_list [(ty2, ty3); (ty1, BoolType)] in
    let unionEqs = EqSet.union agreeEqs iteEqs
    in unify unionEqs >>>= fun mgu ->
    let ctx0 = subs_union "ITE error" (apply_tsubs_to_context ctx1 mgu) (apply_tsubs_to_context ctx2 mgu) in
    let ctx = subs_union "ITE error" ctx0 (apply_tsubs_to_context ctx3 mgu) in
    let ss = apply_tsubs_to_type ty2 mgu in
    returnE (ctx,ss)
  | App(e1, e2) ->
    infer_expr e1 >>>= fun (ctx1, ty1) ->
    infer_expr e2 >>>= fun (ctx2, ty2) ->
    let agreeEqs = agreement_equations ctx1 ctx2 SubsMap.empty in
    let tname = TypeVar (fresh_string "a") in
    let appEqs = EqSet.of_list [(ty1, FuncType(ty2, tname))] in
    let unionEqs = EqSet.union agreeEqs appEqs
    in unify unionEqs >>>= fun mgu ->
    let ctx = subs_union "App error" (apply_tsubs_to_context ctx1 mgu) (apply_tsubs_to_context ctx2 mgu) in
    let ss = apply_tsubs_to_type tname mgu in
    returnE (ctx, ss)
  | Let(id, e1, e2) ->
    infer_expr e1 >>>= fun (ctx1, ty1) ->
    infer_expr e2 >>>= fun (ctx2, ty2) ->
    let s = find_or_fresh id ctx2 in
    let agreeEqs = agreement_equations ctx1 ctx2 SubsMap.empty in
    let letEqs = EqSet.singleton (s, ty1) in
    let unionEqs = EqSet.union agreeEqs letEqs
    in unify unionEqs >>>= fun mgu ->
    let ctx = subs_union "Let error" (apply_tsubs_to_context ctx1 mgu) (SubsMap.remove id (apply_tsubs_to_context ctx2 mgu)) in
    let sr = apply_tsubs_to_type ty2 mgu in
    returnE (ctx, sr)
  | Proc(id, None, e) ->
    infer_expr e >>>= fun (ctx, ty) ->
    let s = find_or_fresh id ctx in
    let ctx0 = SubsMap.remove id ctx in
    returnE (ctx0, s)
  | Proc(id, Some tPar, e) ->
    infer_expr e >>>= fun (ctx, ty) ->
    let s = find_or_fresh id ctx in
    let eqs = EqSet.singleton (s, tPar)
    in unify eqs >>>= fun mgu ->
    let ctx0 = SubsMap.remove id ctx in
    let ssr = apply_tsubs_to_type (FuncType(s, ty)) mgu in
    returnE (ctx0, ssr)
  (*| _ -> failwith @@ "infer_expr: not implemented yet: "^string_of_expr e*)

let infer_prog (AProg(_,e)) : (texpr SubsMap.t*texpr) result =
  infer_expr e

let infer : string -> (texpr SubsMap.t*texpr) result =
  fun e ->
  e |> parse |> infer_prog

let string_of_infer e =
  match infer e with
  | Error s -> "error: "^s
  | Ok (ctx,typ) -> "("^string_of_texpr typ^","^string_of_subs ctx^")"
