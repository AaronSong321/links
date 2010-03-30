open Utility

module A = Algebra

(* Table information node (q, cs, itbls, _) *)


module Cs = struct

  type offset = int
  type cs = csentry list
  and csentry =
    | Offset of offset
    | Mapping of string * cs

  (* return all columns *)	
  let rec leafs cs =
    List.rev
      (List.fold_left
	 (fun leaf_list cs_entry ->
	    match cs_entry with
	      | Offset o -> o :: leaf_list
	      | Mapping (_, cs) -> (List.rev (leafs cs)) @ leaf_list)
	 []
	 cs)

  let cardinality = List.length

  (* increase all column names by i *)
  let rec shift cs i =
    List.map
      (function
	 | Offset o -> Offset (o + i)
	 | Mapping (key, cs) -> Mapping (key, (shift cs i)))
      cs

  (* append to cs components *)
  let append cs1 cs2 =
    cs1 @ (shift cs2 (cardinality cs1))

  (* fuse two cs's by choosing the larger one *)
  let fuse cs1 cs2 =
    if (List.length cs1) > (List.length cs2) then
      cs1
    else
      cs2

  (* true iff cs has exactly one flat column *)
  let is_operand cs =
    if List.length cs <> 1 then
      false
    else
      match (List.hd cs) with
	| Offset _ -> true
	| _ -> false

  (* look up the column corresponding to a record field *)
  let lookup_record_field cs field =
    let rec loop = function
      | (Offset _) :: tl ->
	  loop tl
      | (Mapping (key, cs)) :: tl ->
	  if key = field then
	    cs
	  else
	    loop tl
      | [] ->
	  failwith "Cs.get_mapping: unknown field name"
    in
      loop cs

  (* remove all mappings with keys in fields *)
  let filter_record_fields cs fields =
    List.filter
      (function 
	 | Mapping (key, _) when StringSet.mem key fields -> false 
	 | _ -> true)
      cs

  (* return all top-level record fields *)
  let record_fields cs =
    List.fold_left
      (fun l c ->
	 match c with
	   | Mapping (key, _) -> key :: l
	   | Offset _ -> l)
      []
      cs
end

type tblinfo = Ti of (A.Dag.dag ref * Cs.cs * ((int * tblinfo) list) * unit)

module Itbls = struct
  let empty = []
  let keys itbls = List.map fst itbls
  let incr_keys itbls i = List.map (fun (offset, ti) -> (offset + i, ti)) itbls
  let decr_keys itbls i =  List.map (fun (offset, ti) -> (offset - i, ti)) itbls
  let append = List.append

  (* remove all mappings whose offsets are not in keys *)
  let retain_by_keys itbls keys = 
    let p i j = i = j in
    let f l (offset, _ti) = List.exists (p offset) l in
      List.filter (f keys) itbls
end

module AEnv = Env.Int
type aenv = tblinfo AEnv.t

let dummy = ()

let incr l i = List.map (fun j -> j + i) l
let items_of_offsets = List.map (fun i -> A.Item i)

let proj1 col = (col, col)

let proj_list = List.map proj1

let proj_list_map new_cols old_cols = 
  List.map2 (fun a b -> (a, b)) new_cols old_cols

let proj_list_single new_cols old_col =
  List.map (fun a -> (a, old_col)) new_cols

let wrap_1to1 f res c c' algexpr =
  A.Dag.mk_fun1to1
    (f, res, [c; c'])
    algexpr

let wrap_eq res c c' algexpr =
  A.Dag.mk_funnumeq
    (res, (c, c'))
    algexpr

let incr_col = function
  | A.Iter i -> A.Iter (i + 1)
  | A.Pos i -> A.Pos (i + 1)
  | A.Item i -> A.Item (i + 1)

let wrap_ne res c c' algexpr =
  let res' = incr_col res in
    A.Dag.mk_funboolnot
      (res, res')
      (A.Dag.mk_funnumeq
	 (res', (c, c'))
	 algexpr)

let wrap_gt res c c' algexpr =
  A.Dag.mk_funnumgt
    (res, (c, c'))
    algexpr

let wrap_not res op_attr algexpr =
  A.Dag.mk_funboolnot
    (res, op_attr)
    algexpr

(* Naming conventions for Iter and Pos columns. Use only these names! *)
let iter = A.Iter 0 
let iter' = A.Iter 1
let inner = A.Iter 2
let outer = A.Iter 3 
let pos = A.Pos 0 
let pos' = A.Pos 1
let ord = A.Pos 2
let ord' = A.Pos 3
let item' = A.Pos 4
let item'' = A.Pos 5
let c' = A.Pos 6
let res = A.Pos 7

let rec suap q_paap it1 it2 : (int * tblinfo) list =
  match (it1, it2) with
    | (c1, Ti (q_1, cs1, subs_1, _)) :: subs_hat, ((_, Ti (q_2, cs2, subs_2, _)) :: subs_tilde) ->
	let q =
	  (A.Dag.mk_rownum 
	     (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
	     (A.Dag.mk_disjunion
		(A.Dag.mk_attach
		   (ord, A.Nat 1n)
		   q_1)
		(A.Dag.mk_attach
		   (ord, A.Nat 2n)
		   q_2)))
	in
	let q'_projlist = [(iter, item''); proj1 pos] in
	let q'_projlist = q'_projlist @ (proj_list (items_of_offsets (difference (Cs.leafs cs1) (Itbls.keys subs_1)))) in
	let q'_projlist = q'_projlist @ (proj_list_single (items_of_offsets (Itbls.keys subs_1)) item') in
	let q' =
	  (A.Dag.mk_project
	     q'_projlist
	     (A.Dag.mk_thetajoin
		[(A.Eq, (ord, ord')); (A.Eq, (iter, c'))]
		q
		(A.Dag.mk_project
		   [(ord', ord); (item'', item'); (c', A.Item c1)]
		   q_paap)))
	in
	  [(c1, (Ti (q', (Cs.fuse cs1 cs2), (suap q subs_1 subs_2), dummy)))] @ (suap q_paap subs_hat subs_tilde)
    | [], [] -> []
    | [], subs -> subs
    | subs, [] -> subs

let rec suse q_pase subs : ((int * tblinfo) list) =
  match subs with
    | (offset, (Ti(q, cs, itbls, _))) :: subs ->
	let q' = 
	  A.Dag.mk_project
	    ([proj1 iter; proj1 pos] @ (proj_list (items_of_offsets (Cs.leafs cs))))
	    (A.Dag.mk_eqjoin
	       (iter, iter')
	       q
	       (A.Dag.mk_project
		  [(iter', A.Item offset)]
		  q_pase))
	in
	  [(offset, (Ti(q', cs, (suse q' itbls), dummy)))] @ (suse q_pase subs)
	    
    | [] ->
	[]


let wrap_agg loop q attachment =
  A.Dag.mk_attach
    (pos, A.Nat 1n)
    (A.Dag.mk_disjunion
       q
       (A.Dag.mk_attach
	  (A.Item 1, attachment)
	  (A.Dag.mk_difference
	     loop
	     (A.Dag.mk_project
		[proj1 iter]
		q))))

(* the empty list *)
let nil = A.Dag.mk_emptytbl [(A.Iter 0, A.NatType); (A.Pos 0, A.NatType)]

(* loop-lift q by map *)
let lift map (Ti (q, cs, _, _)) =
  let q' =
    (A.Dag.mk_project
       ([(iter, inner); proj1 pos] @ (proj_list (items_of_offsets (Cs.leafs cs))))
       (A.Dag.mk_eqjoin
	  (iter, outer)
	  q
	  map))
  in
    Ti (q', cs, Itbls.empty, dummy)

(* construct the ordering map of a for-loop *)
let rec omap map sort_criteria sort_cols =
  match (sort_criteria, sort_cols) with
    | (o :: os), (col :: cols) ->
	A.Dag.mk_project
	  ([proj1 outer; proj1 inner; (col, A.Item 1)] @ (proj_list cols))
	  (A.Dag.mk_eqjoin
	     (inner, iter)
	     (omap map os cols)
	     o)
    | [], [] ->
	map
    | _ -> assert false

(* compute absolute positions for q *)
let abspos q cols =
  A.Dag.mk_project
    ([proj1 iter; proj1 pos] @ (proj_list cols))
    (A.Dag.mk_rownum
       (pos, [(pos', A.Ascending)], Some iter)
       (A.Dag.mk_project
	  ([proj1 iter; (pos', pos)] @ (proj_list cols))
	  q))

let rec compile_box env loop e =
  let ti_e = compile_expression env loop e in
  let q_o = 
    A.Dag.mk_attach
      (pos, A.Nat 1n)
      (A.Dag.mk_project 
	 [(proj1 iter); (A.Item 1, iter)]
	 loop)
  in
    Ti(q_o, [Cs.Offset 1], [(1, ti_e)], dummy)

and compile_unbox env loop e =
  let Ti(_, cs, itbls, _) = compile_expression env loop e in
    assert ((Cs.cardinality cs) = 1);
    assert ((List.length itbls) = 1);
    snd (List.hd itbls)

and compile_append env loop l =
  match l with
    | e :: [] ->
	compile_expression env loop e
    | hd_e :: tl_e ->
	let hd = compile_expression env loop hd_e in
	let tl = compile_append env loop tl_e in
	  compile_list hd tl
    | [] ->
	Ti (nil, [], Itbls.empty, dummy)

and compile_list (Ti (hd_q, hd_cs, hd_itbls, _)) (Ti (tl_q, tl_cs, tl_itbls, _)) =
  let fused_cs = Cs.fuse hd_cs tl_cs in
  let q =
    A.Dag.mk_rownum
      (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
      (A.Dag.mk_rank
	 (pos', [(ord, A.Ascending); (pos, A.Ascending)])
	 (A.Dag.mk_disjunion
	    (A.Dag.mk_attach
	       (ord, A.Nat 1n)
	       hd_q)
	    (A.Dag.mk_attach
	       (ord, A.Nat 2n)
	       tl_q)))
  in
  let q'_projlist = [proj1 iter; (pos, pos')] in
  let q'_projlist = q'_projlist @ (proj_list (items_of_offsets (difference (Cs.leafs hd_cs) (Itbls.keys hd_itbls)))) in
  let q'_projlist = q'_projlist @ (proj_list_single (items_of_offsets (Itbls.keys hd_itbls)) item') in
  let q' = 
    A.Dag.mk_project
      q'_projlist
      q
  in
  let itbls' = suap q hd_itbls tl_itbls in
    Ti (q', fused_cs, itbls', dummy)

and compile_length env loop args =
  assert ((List.length args) = 1);
  let e = List.hd args in
  let Ti (q_e, _, _, _) = compile_expression env loop e in
  let q = 
    A.Dag.mk_funaggrcount
      (A.Item 1, Some iter)
      q_e
  in
  let q' = wrap_agg loop q (A.Nat 1n) in
    Ti (q', [Cs.Offset 1], Itbls.empty, dummy)

and compile_aggr env loop aggr_fun args =
  assert ((List.length args) = 1);
  let c = A.Item 1 in
  let e = List.hd args in
  let Ti (q_e, cs_e, _, _) = compile_expression env loop e in
    assert (Cs.is_operand cs_e);
    let q = 
      (A.Dag.mk_funaggr
	 (aggr_fun, (c, c), Some iter)
	 q_e)
    in
    let q' = wrap_agg loop q (A.String "error") in
      Ti (q', [Cs.Offset 1], Itbls.empty, dummy)

and compile_nth env loop operands =
  assert ((List.length operands) = 2);
  let Ti (q1, _, _, _) = compile_expression env loop (List.hd operands) in
  let Ti (q2, cs2, itbls_2, _) = compile_expression env loop (List.nth operands 1) in
  let q2' = abspos q2 (items_of_offsets (Cs.leafs cs2)) in
  let q =
    (A.Dag.mk_project
       ([proj1 iter; proj1 pos] @ proj_list (items_of_offsets (Cs.leafs cs2)))
       (A.Dag.mk_select
	  res
	  (A.Dag.mk_funnumeq
	     (res, (pos', c'))
	     (A.Dag.mk_eqjoin
		(iter, iter')
		(A.Dag.mk_cast
		   (pos', pos, A.IntType)
		   q2')
		(A.Dag.mk_project
		   [(iter', iter); (c', A.Item 1)]
		   q1)))))
  in
  let itbls' = suse q itbls_2 in
    Ti (q, cs2, itbls', dummy)

and compile_binop env loop wrapper operands =
  assert ((List.length operands) = 2);
  let Ti (op1_q, op1_cs, _, _) = compile_expression env loop (List.hd operands) in
  let Ti (op2_q, op2_cs, _, _) = compile_expression env loop (List.nth operands 1) in
    assert (Cs.is_operand op1_cs);
    assert (Cs.is_operand op2_cs);
    let c = A.Item 1 in
    let c' = A.Item 2 in
    let res = A.Item 3 in
    let q = 
      A.Dag.mk_project
	[(proj1 iter); (proj1 pos); (c, res)]
	(wrapper 
	   res c c'
	   (A.Dag.mk_eqjoin
	      (iter, iter')
	      op1_q
	      (A.Dag.mk_project
		 [(iter', iter); (c', c)]
		 op2_q)))
    in
      Ti (q, op1_cs, Itbls.empty, dummy)

and compile_unop env loop wrapper operands =
  assert ((List.length operands) = 1);
  let Ti (op_q, op_cs, _, _) = compile_expression env loop (List.hd operands) in
    assert (Cs.is_operand op_cs);
    let c = A.Item 1 in
    let res = A.Item 2 in
    let q = 
      A.Dag.mk_project
	[proj1 iter; proj1 pos; (c, res)]
	(wrapper
	   res c
	   op_q)
    in
      Ti (q, op_cs, Itbls.empty, dummy)

and compile_take env loop args =
  assert ((List.length args) = 2);
  let Ti(q1, _cs1, _, _) = compile_expression env loop (List.hd args) in
  let Ti(q2, cs2, itbls2, _) = compile_expression env loop (List.nth args 1) in
  let cols = (items_of_offsets (Cs.leafs cs2)) in
  let q2' = abspos q2 cols in
  let c = A.Item 1 in
  let one = A.Item 2 in
  let q' = 
    A.Dag.mk_project
      ([proj1 iter; proj1 pos] @ (proj_list cols))
      (A.Dag.mk_select
	 res
	 (A.Dag.mk_funnumgt
	    (res, (c', pos'))
	    (A.Dag.mk_eqjoin
	       (iter, iter')
	       (A.Dag.mk_cast
		  (pos', pos, A.IntType)
		  q2')
	       (A.Dag.mk_project
		  [(iter', iter); (c', res)]
		  (A.Dag.mk_fun1to1
		     (A.Add, res, [c; one])
		     (A.Dag.mk_attach
			(one, A.Int (Num.Int 1))
			q1))))))
       in
    let itbls' = suse q' itbls2 in
	Ti(q', cs2, itbls', dummy)

and compile_drop env loop args =
  assert ((List.length args) = 2);
  let Ti(q1, _cs1, _, _) = compile_expression env loop (List.hd args) in
  let Ti(q2, cs2, itbls2, _) = compile_expression env loop (List.nth args 1) in
  let cols = (items_of_offsets (Cs.leafs cs2)) in
  let q2' = abspos q2 cols in
  let c = A.Item 1 in
  let q' =
    A.Dag.mk_project
      ([proj1 iter; proj1 pos] @ (proj_list cols))
      (A.Dag.mk_select
	 res
	 (A.Dag.mk_funnumgt
	    (res, (pos', c'))
	    (A.Dag.mk_eqjoin
	       (iter, iter')
	       (A.Dag.mk_cast
		  (pos', pos, A.IntType)
		  q2')
	       (A.Dag.mk_project
		  [(iter', iter); (c', c)]
		  q1))))
  in
  let itbls' = suse q' itbls2 in
    Ti(q', cs2, itbls', dummy)

and compile_apply env loop f args =
  match f with
    | "+" 
    | "+." -> compile_binop env loop (wrap_1to1 A.Add) args
    | "-" 
    | "-." -> compile_binop env loop (wrap_1to1 A.Subtract) args
    | "*"
    | "*." -> compile_binop env loop (wrap_1to1 A.Multiply) args
    | "/" 
    | "/." -> compile_binop env loop (wrap_1to1 A.Divide) args
    | "==" -> compile_binop env loop wrap_eq args
    | ">" -> compile_binop env loop wrap_gt args
    | "<>" -> compile_binop env loop wrap_ne args
    | "not" -> compile_unop env loop wrap_not args
    | "nth" -> compile_nth env loop args
    | "length" -> compile_length env loop args
    | "maxf" -> compile_aggr env loop A.Max args
	(*    | "min" -> compile_aggr env loop A.Min args
	      | "avg" -> compile_aggr env loop A.Avg args
	      | "sum" -> compile_aggr env loop A.Sum args *)
    | "take" -> compile_take env loop args
    | "drop" -> compile_drop env loop args
    | "<" | "<=" | ">=" ->
	failwith ("CompileQuery.compile_apply: </<=/>= should have been rewritten in query2")
    | s ->
	failwith ("CompileQuery.op_dispatch: " ^ s ^ " not implemented")
	  (*
	    | `PrimitiveFunction "max" ->
	    | `PrimitiveFunction "min" ->
	    | `PrimitiveFunction "hd" ->
	    | `PrimitiveFunction "tl" ->
	  *)

and compile_for env loop v e1 e2 order_criteria =
  let Ti (q1, cs1, _, _) = compile_expression env loop e1 in
  let q_v = 
    A.Dag.mk_rownum
      (inner, [(iter, A.Ascending); (pos, A.Ascending)], None)
      q1
  in
  let map =
    A.Dag.mk_project
      [(outer, iter); proj1 inner]
      q_v
  in
  let loop_v =
    A.Dag.mk_project
      [(iter, inner)]
      q_v
  in
  let q_v' =
    A.Dag.mk_attach
      (pos, A.Nat 1n)
      (A.Dag.mk_project
	 ([(iter, inner)] @ (proj_list (items_of_offsets (Cs.leafs cs1))))
	 q_v)
  in
  let env = AEnv.map (lift map) env in
  let env_v = AEnv.bind env (v, Ti (q_v', cs1, Itbls.empty, dummy)) in
  let Ti (q2, cs2, itbls2, _) = compile_expression env_v loop_v e2 in
  let (order_cols, map') =
    match order_criteria with
      | _ :: _ ->
	  (* compile orderby expressions *)
	  let q_os = List.map (compile_expression env_v loop_v) order_criteria in
	  let q_os = List.map (fun (Ti (q, _, _, _)) -> q) q_os in
	  let offset = (List.length (Cs.leafs cs2)) + 1 in
	  let cols = mapIndex (fun _ i -> A.Item (i + offset)) q_os in
	  let order_cols = List.map (fun c -> (c, A.Ascending)) (cols @ [pos]) in
	    (order_cols, omap map q_os cols)
	      
      | [] ->
	  ([(iter, A.Ascending); (pos, A.Ascending)], map)
  in
  let q =
    A.Dag.mk_project
      ([(iter, outer); (pos, pos')] @ (proj_list (items_of_offsets (Cs.leafs cs2))))
      (A.Dag.mk_rank
	 (pos', order_cols)
	 (A.Dag.mk_eqjoin
	    (inner, iter)
	    map'
	    q2))
  in
    Ti (q, cs2, itbls2, dummy)

and singleton_record env loop (name, e) =
  let Ti (q, cs, itbls, _) = compile_expression env loop e in
    Ti (q, [Cs.Mapping (name, cs)], itbls, dummy)

and extend_record env loop ext_fields r =
  assert (match ext_fields with [] -> false | _ -> true);
  match ext_fields with
    | (name, e) :: [] -> 
	(match r with 
	   | Some record ->
	       merge_records (singleton_record env loop (name, e)) record
	   | None ->
	       singleton_record env loop (name, e))
    | (name, e) :: tl ->
	let new_field = singleton_record env loop (name, e) in
	let record = extend_record env loop tl r in
	  merge_records new_field record
    | [] ->
	failwith "CompileQuery.extend_record: empty ext_fields"

and merge_records (Ti (r1_q, r1_cs, r1_itbls, _)) (Ti (r2_q, r2_cs, r2_itbls, _)) =
  let r2_leafs = Cs.leafs r2_cs in
  let new_names_r2 = items_of_offsets (incr r2_leafs (Cs.cardinality r1_cs)) in
  let old_names_r2 = items_of_offsets r2_leafs in
  let names_r1 = items_of_offsets (Cs.leafs r1_cs) in
  let r2_itbls' = Itbls.incr_keys r2_itbls (Cs.cardinality r1_cs) in
  let q =
    A.Dag.mk_project
      (proj_list ([A.Iter 0; A.Pos 0] @ names_r1 @ new_names_r2))
      (A.Dag.mk_eqjoin
	 (iter, iter')
	 r1_q
	 ((A.Dag.mk_project
	     ((iter', iter) :: (proj_list_map new_names_r2 old_names_r2))
	     r2_q)))
  in
  let cs = Cs.append r1_cs r2_cs in
  let itbls = Itbls.append r1_itbls r2_itbls' in
    Ti (q, cs, itbls, dummy)

and compile_project env loop field r =
  let Ti (q_r, cs_r, itbls_r, _) = compile_expression env loop r in
  let field_cs' = Cs.lookup_record_field cs_r field in
  let c_old = Cs.leafs field_cs' in
  let offset = List.hd c_old in
  let c_new = incr c_old (-offset + 1) in
  let field_cs = Cs.shift field_cs' (-offset + 1) in
  let field_itbls = Itbls.decr_keys (Itbls.retain_by_keys itbls_r c_old) (offset - 1) in 
  let q =
    A.Dag.mk_project
      ([proj1 iter; proj1 pos] @ proj_list_map (items_of_offsets c_new) (items_of_offsets c_old))
      q_r
  in
    Ti (q, field_cs, field_itbls, dummy)

and compile_erase env loop erase_fields r =
  let Ti (q_r, cs_r, itbls_r, _) = compile_expression env loop r in
  let remaining_cs = Cs.filter_record_fields cs_r erase_fields in
  let remaining_cols = items_of_offsets (Cs.leafs remaining_cs) in
  let remaining_itbls = Itbls.retain_by_keys itbls_r (Cs.leafs remaining_cs) in
  let q =
    A.Dag.mk_project
      ([proj1 iter; proj1 pos] @ (proj_list remaining_cols))
      q_r
  in
    Ti(q, remaining_cs, remaining_itbls, dummy)

and compile_record env loop r =
  match r with
    | (name, value) :: [] ->
	singleton_record env loop (name, value)
    | (name, value) :: tl ->
	let f = singleton_record env loop (name, value) in
	  merge_records f (compile_record env loop tl)
    | [] ->
	failwith "CompileQuery.compile_record_value: empty record"

(* HACK HACK HACK: rewrite this when Value.table stores key information *)
and compile_table loop ((_db, _params), tblname, _row) =
  Printf.printf "tblname = %s\n" tblname;
  flush stdout;
  let (key_infos, columns) = 
    match tblname with
      | "test1" ->
	  ([[A.Item 1]], ["foo"; "bar"]) 
      | "test2" ->
	  ([[A.Item 1]], ["id"; "name"; "value"])
      | "players" ->
	  ([[A.Item 1]], ["name"; "age"; "team"])
      | "teams" ->
	  ([[A.Item 1]], ["name"])
      | _ -> failwith "table not known"
  in
  let col_pos = mapIndex (fun c i -> (c, (i + 1))) columns in
  let items = List.map (fun (c, i) -> (c, A.Item i)) col_pos in
  let cs = List.map (fun (c, i) -> Cs.Mapping (c, [Cs.Offset i])) col_pos in
  let pos = A.Pos 0 in
  let attr_infos = List.map (fun (tname, name) -> (name, tname, A.IntType)) items in
  let q =
    A.Dag.mk_cross
      loop
      (A.Dag.mk_rank
	 (pos, (List.map (fun (_, name) -> (name, A.Ascending)) items))
	 (A.Dag.mk_tblref
	    (tblname, attr_infos, key_infos)))
  in
    Ti (q, cs, Itbls.empty, dummy)

and compile_constant loop (c : Constant.constant) =
  let cs = [Cs.Offset 1] in
  let q =
    (A.Dag.mk_attach
       (A.Item 1, A.const c)
       (A.Dag.mk_attach
	  (A.Pos 0, A.Nat 1n)
	  loop))
  in
    Ti (q, cs, Itbls.empty, dummy)

and compile_if env loop e1 e2 e3 =
  let c = A.Item 1 in
  let res = A.Item 2 in
    
  let select loop (Ti (q, cs, itbls, _)) =
    let cols = items_of_offsets (Cs.leafs cs) in
    let q' =
      A.Dag.mk_project
	([proj1 iter; proj1 pos] @ (proj_list cols))
	(A.Dag.mk_eqjoin
	   (iter, iter')
	   q
	   (A.Dag.mk_project
	      [(iter', iter)]
	      loop))
    in
    let itbls' = suse q itbls in
      Ti (q', cs, itbls', dummy)
  in
    (* condition *)
  let Ti (q_e1, cs_e1, _, _) = compile_expression env loop e1 in
    assert (Cs.is_operand cs_e1);
    let loop_then =
      A.Dag.mk_project
	[proj1 iter]
	(A.Dag.mk_select
	   c
	   q_e1)
    in
    let loop_else =
      A.Dag.mk_project
	[proj1 iter]
	(A.Dag.mk_select
	   res
	   (A.Dag.mk_funboolnot
	      (res, c)
	      q_e1))
    in
    let env_then = AEnv.map (select loop_then) env in
    let env_else = AEnv.map (select loop_else) env in
    let Ti (q_e2, cs_e2, itbls_e2, _) = compile_expression env_then loop_then e2 in
    let Ti (q_e3, _cs_e3, itbls_e3, _) = compile_expression env_else loop_else e3 in
    let q =
      A.Dag.mk_rownum
	(item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
	(A.Dag.mk_disjunion
	   (A.Dag.mk_attach
	      (ord, A.Nat 1n)
	      q_e2)
	   (A.Dag.mk_attach
	      (ord, A.Nat 2n)
	      q_e3))
    in
    let cols = Cs.leafs cs_e2 in
    let keys = Itbls.keys itbls_e2 in
    let proj = [proj1 iter; proj1 pos] in
    let proj = proj @ (proj_list (items_of_offsets (difference cols keys))) in
    let proj = proj @ (proj_list_single (items_of_offsets keys) item') in
    let q' = 
      A.Dag.mk_project
	proj
	q
    in
    let itbls' = suap q itbls_e2 itbls_e3 in
      Ti (q', cs_e2, itbls', dummy)

and compile_expression env loop e : tblinfo =
  match e with
    | `Constant c -> compile_constant loop c
    | `Apply (f, args) -> compile_apply env loop f args
    | `Var x -> AEnv.lookup env x
    | `Project (r, field) -> compile_project env loop field r
    | `Record r -> compile_record env loop (StringMap.to_alist r)
    | `Extend (r, ext_fields) ->
	let ext_fields = StringMap.to_alist ext_fields in
	  extend_record env loop ext_fields (opt_map (compile_expression env loop) r)
    | `Erase (r, erase_fields) -> compile_erase env loop erase_fields r
    | `Singleton e -> compile_expression env loop e
    | `Append l -> compile_append env loop l
    | `Table t -> compile_table loop t
    | `If (c, t, e) -> compile_if env loop c t e
    | `For ([x, l], os, body) -> compile_for env loop x l body os
    | `For _ -> failwith "compile_expression: multi-generator for-expression not implemented"
    | `Box e -> compile_box env loop e
    | `Unbox e -> compile_unbox env loop e
    | `Closure _
    | `Variant _
    | `XML _ -> failwith "compile_expression: not implemented"
    | `Primitive _ -> failwith "compile_expression: eval error"

let wrap_serialize (Ti (q,cs,_,_)) =
  A.Dag.mk_serializerel 
    (A.Iter 0, A.Pos 0, items_of_offsets (Cs.leafs cs))
    (A.Dag.mk_nil)
    q

let rec collect_itbls (plan_id, ref_id) itbls collected =
  match itbls with
    | (offset, (Ti(_, _, [], _) as ti)) :: remaining_itbls ->
	let l = ((plan_id, ref_id, offset), (wrap_serialize ti)) :: collected in
	  collect_itbls (plan_id + 1, ref_id) remaining_itbls l
    | (offset, (Ti(_, _, itbls, _) as ti)) :: remaining_itbls ->
	let (next_id, l) = collect_itbls (plan_id + 1, plan_id) itbls [] in
	let l = ((plan_id, ref_id, offset), (wrap_serialize ti)) :: (l @ collected) in
	  collect_itbls (next_id, plan_id) remaining_itbls l
    | [] ->
	(plan_id, collected)
      
let compile e =
  let loop = 
    (A.Dag.mk_littbl
       ([[A.Nat 1n]], [(A.Iter 0, A.NatType)]))
  in
  let Ti (_, _, itbls, _) as ti = compile_expression AEnv.empty loop e in
  let plan_bundle = (wrap_serialize ti), snd (collect_itbls (1, 0) itbls []) in
    A.Dag.export_plan_bundle "plan.xml" plan_bundle
