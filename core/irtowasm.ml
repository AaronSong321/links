
open Var
open Ir
open Lib
open Types
open CommonTypes
open Utility

exception Not_implemented of string
exception Unreachable of string

module ListExt = struct
  include List 
  let select = map
  let select_many: ('a -> 'b list) -> 'a list -> 'b list = fun collection_selector source -> 
    source 
    |> select collection_selector
    |> flatten
  let select_many_result: ('a -> 'b list) -> ('a -> 'b -> 'c) -> 'a list -> 'c list = fun collection_selector result_selector source ->
    source 
    |> select (fun a -> a, collection_selector a)
    |> select (fun (a,b) -> select (fun b' -> result_selector a b') b)
    |> flatten
  let rec last = function
    | [] -> raise Not_found
    | [v] -> v
    | _::v -> last v
  let rec skip_while filter container =
    match container with
    | [] -> []
    | elem::v -> 
      if filter elem then skip_while filter v 
      else container
end
module SysExt = struct
  let get_file_name_without_extension file =
    let dot_position = Str.search_backward (Str.regexp "\\.") file ((String.length file) - 1) in
    String.sub file 0 dot_position
  let get_extension file =
    let dot_position = Str.search_backward (Str.regexp "\\.") file ((String.length file) - 1) in
    String.sub file dot_position ((String.length file) - dot_position)
end
module IntMapExt = struct
  type 'a __some_private_type_dont_use = {
    mutable __ugly_field_what_do_i_want: 'a list
  }
  let __new_ugly_value_haha () = {
    __ugly_field_what_do_i_want = []
  }
  let map map_func container =
    let r = __new_ugly_value_haha () in
    IntMap.iter (fun key value -> r.__ugly_field_what_do_i_want <- r.__ugly_field_what_do_i_want @ [map_func key value]) container;
    r.__ugly_field_what_do_i_want
end
module StringMapExt = struct
  type 'a __some_private_type_dont_use = {
    mutable __ugly_field_what_do_i_want: 'a list
  }
  let __new_ugly_value_haha () = {
    __ugly_field_what_do_i_want = []
  }
  let map map_func container =
    let r = __new_ugly_value_haha () in
    StringMap.iter (fun key value -> r.__ugly_field_what_do_i_want <- r.__ugly_field_what_do_i_want @ [map_func key value]) container;
    r.__ugly_field_what_do_i_want
end
    
let (>>): ('a->'b) -> ('b->'c) -> ('a->'c) = fun a b ->
  fun m -> m |> a |> b
let (?|>): 'a option -> ('a -> 'b) -> 'b option = fun a b ->
  match a with
  | Some c -> Some (b c)
  | None -> None
let non_null t =
  match t with
  | Some v -> v 
  | None -> assert false
let non_null_list t =
  let rec g f acc =
    match f with
    | v::t -> (match v with 
      | Some k -> g t (acc @ [k])
      | None -> g t acc
    )
    | _ -> acc
  in
  g t []

module Wasm = struct
  module Grammar = struct
    type number_type_length = L32|L64
    type memory_number_length = ML8|ML16|ML32
    type number_type_sign = Signed|Unsigned
    type number_int_or_float = NInt|NFloat
    type wasm_type =
      | Heap_type of heap_type
      | Val_type of val_type
      | Func_type of func_type
      | Memory_type of memory_type
      | Table_type of table_type
      | Global_type of global_type
      | Error_type
      | Unit_type
    and ref_type =
      | Func_ref
      | Extern_ref of extern_type
    and heap_type =
      | Func_heap_ref
      | Extern_heap_ref
    and val_type =
      | Number_type of number_int_or_float * number_type_length
      | Ref_type of ref_type
    and func_type = {
      p: param_annotator;
      r: result_annotator
    }
    and result_annotator = val_type list 
    and param_annotator = (string option * val_type) list
    and limit_type = {
      mutable min: int;
      mutable max: int option
    }
    and memory_type = { mutable memory_type_limit: limit_type }
    and table_type = {
      table_id: string option;
      mutable table_type_limit: limit_type;
      table_type_type: ref_type
    }
    and global_type = {
      global_var_type: val_type;
      is_mutable: bool
    }
    and extern_type = 
      | Extern_func of func_type
      | Extern_table of table_type
      | Extern_memory of memory_type
      | Extern_global of global_type
    
    let increment_limit f = 
      let min = f.min + 1 in
      f.min <- min;
      f.max <- match f.max with
      | Some m -> Some (if m = min - 1 then min else m)
      | None -> None
    let wasm_func_pointer_type = Number_type(NInt, L32)
    let rec to_string: wasm_type -> string = function
      | Val_type v -> to_string_val_type v
      | _ -> raise (Not_implemented __LOC__)
    and to_string_val_type: val_type -> string = function
      | Number_type (a,b) -> (
        match a with
        | NInt -> "i"
        | NFloat -> "f"
      ) ^ (
        match b with
        | L32 -> "32"
        | L64 -> "64"
      )
      | Ref_type _ -> to_string_val_type wasm_func_pointer_type
          
    type mono_index = 
      | Mni_int of int
      | Mni_name of string
    type func_index = mono_index
    type local_index = mono_index
    type type_index = mono_index
    type table_index = mono_index
    type memory_index = mono_index
    type global_index = mono_index
    type label_index = mono_index
    type element_index = mono_index
    type data_index = mono_index
      
    type type_def = {
      id: string option;
      ft: func_type
    }
    type type_use =
      | TU_index of type_index
      | TU_def of func_type
      | TU_index_def of type_index * func_type
    type instruction =
      | Control_insc of control_insc
      | Ref_insc of ref_insc
      | Param_insc of param_insc
      | Var_insc of var_insc
      | Table_insc of table_insc
      | Memory_insc of memory_insc
      | Numeric_insc of numeric_insc
    and insc_semantical_param = instruction
    and nii_relop_type = number_int_or_float * number_type_length * insc_semantical_param * insc_semantical_param
    and nii_iunop_type = number_type_length * insc_semantical_param
    and nii_ibinop_signed_type = number_type_length * number_type_sign * insc_semantical_param * insc_semantical_param
    and nii_ibinop_unsigned_type = number_type_length * insc_semantical_param * insc_semantical_param
    and nii_fbinop_type = number_type_length * insc_semantical_param * insc_semantical_param
    and nii_funop_type = number_type_length * insc_semantical_param
    and nii_trunc_type = number_type_length * number_type_length * number_type_sign * insc_semantical_param
    and nii_extend_from_type =
      | Nii_extend_from_8
      | Nii_extend_from_16
      | Nii_extend_from_32
    and numeric_insc =
      | NInsc_consti of number_type_length * int
      | NInsc_constf of number_type_length * float
      | NInsc_relop_eq of nii_relop_type
      | NInsc_relop_ne of nii_relop_type
      | NInsc_relop_lt of nii_relop_type
      | NInsc_relop_gt of nii_relop_type
      | NInsc_relop_le of nii_relop_type
      | NInsc_relop_ge of nii_relop_type
      | NInsc_iunop_clz of nii_iunop_type
      | NInsc_iunop_ctz of nii_iunop_type
      | NInsc_iunop_popcnt of nii_iunop_type
      | NInsc_itestop_eqz of nii_iunop_type
      | NInsc_ibinop_add of nii_ibinop_unsigned_type
      | NInsc_ibinop_sub of nii_ibinop_unsigned_type
      | NInsc_ibinop_mul of nii_ibinop_unsigned_type
      | NInsc_ibinop_div of nii_ibinop_unsigned_type
      | NInsc_ibinop_rem of nii_ibinop_unsigned_type
      | NInsc_ibinop_and of nii_ibinop_unsigned_type
      | NInsc_ibinop_or of nii_ibinop_unsigned_type
      | NInsc_ibinop_xor of nii_ibinop_unsigned_type
      | NInsc_ibinop_shl of nii_ibinop_unsigned_type
      | NInsc_ibinop_shr of nii_ibinop_unsigned_type
      | NInsc_ibinop_roti of nii_ibinop_unsigned_type
      | NInsc_ibinop_rotr of nii_ibinop_unsigned_type
      | NInsc_fbinop_add of nii_fbinop_type
      | NInsc_fbinop_sub of nii_fbinop_type
      | NInsc_fbinop_mul of nii_fbinop_type
      | NInsc_fbinop_div of nii_fbinop_type
      | NInsc_fbinop_min of nii_fbinop_type
      | NInsc_fbinop_max of nii_fbinop_type
      | NInsc_fbinop_copysign of nii_fbinop_type
      | NInsc_funop_abs of nii_funop_type
      | NInsc_funop_neg of nii_funop_type
      | NInsc_funop_sqrt of nii_funop_type
      | NInsc_funop_ceil of nii_funop_type
      | NInsc_funop_floor of nii_funop_type
      | NInsc_funop_trunc of nii_funop_type
      | NInsc_funop_nearest of nii_funop_type
      | NInsc_extend of number_type_length * nii_extend_from_type * insc_semantical_param
      | NInsc_wrap of insc_semantical_param
      | NInsc_trunc of nii_trunc_type
      | NInsc_trunc_sat of nii_trunc_type
      | NInsc_promote of insc_semantical_param
      | NInsc_demote of insc_semantical_param
      | NInsc_convert of nii_trunc_type
      | NInsc_reinterpret of number_int_or_float * number_type_length * number_type_length * number_type_sign * insc_semantical_param
    and control_insc =
      | CInsc_nop 
      | CInsc_if of type_use * insc_semantical_param * insc_semantical_param list * insc_semantical_param list
      | CInsc_return of insc_semantical_param
      | CInsc_call of func_index * insc_semantical_param list
      | CInsc_unreachable
      | CInsc_block of type_use * instruction list
      | CInsc_br of label_index
      | CInsc_brif of label_index
      | CInsc_brtable of label_index list * label_index
      | CInsc_loop of type_use * instruction list
      | CInsc_call_indirect of local_index * bool * type_use * insc_semantical_param list
    and ref_insc = 
      | RInsc_null of heap_type
      | RInsc_is_null of insc_semantical_param
      | RInsc_func of func_index
    and param_insc =
      | PInsc_drop of insc_semantical_param
      | PInsc_select of result_annotator * insc_semantical_param * insc_semantical_param * insc_semantical_param
    and var_insc =
      | VInsc_lget of local_index
      | VInsc_lset of local_index * insc_semantical_param
      | VInsc_ltee of local_index * insc_semantical_param
      | VInsc_gget of global_index
      | VInsc_gset of global_index * insc_semantical_param
    and table_insc = (* table instructions are experimental; table.set is not allowed *)
      | TInsc_get of table_index
    and memory_insc = 
      | MInsc_load_i of number_type_length
      | MInsc_load_small_i of number_type_length * number_type_sign * memory_number_length 
    and expr = instruction list
    and func_def = {
      func_id: string option;
      func_wasm_type: func_type;
      mutable locals: (string option * val_type) list;
      mutable body: expr
    }
    let name_of_func: func_def -> string = fun f -> non_null f.func_id
    
    (* let fold_left: wasm_type -> wasm_type -> wasm_type = fun old_type result_type ->
      let to_combine_type: wasm_type -> wasm_type option = fun ot ->
        match ot with 
        | Error_type -> Some Error_type
        | Unit_type -> None
        | _ -> Some ot
      in
      let params = match old_type with 
        | Func_type old_func_type -> (
            let p = old_func_type.p in
            let r = old_func_type.r in
            let nr = match r with
              | [] -> None
              | single::[] -> to_combine_type (Val_type single)
              | _ -> failwith "right recursive function types"
            in
            match nr with
            | Some nr' -> (
                match nr' with 
                | Error_type -> None
                | Val_type t -> Some (p @ [None, t])
                | _ -> None(* only val_type can be used as function parameters in wasm*)
              )
            | None -> Some p
          )
        | Error_type -> None
        | Val_type t -> Some [None, t]
        | _ -> None
      in
      match params, result_type with
      | None, _
      | _, Error_type -> Error_type
      | Some params', Unit_type -> Func_type { p = params'; r = [] }
      | Some params', Val_type t -> Func_type { p = params'; r = [t] }
      | _, _ -> Error_type *)

    type mem_def = {
      memory_id: string option;
      memory_type: memory_type
    }
    type global_def = {
      global_id: string option;
      global_type: global_type;
      init_value: expr
    }
    type export_desc =
      | ED_func of func_index
      | ED_table of table_index
      | ED_mem of memory_index
      | ED_global of global_index
    type export_def = {
      export_id: string;
      desc: export_desc;
    }
    type table_def = table_type
    type elem_def = insc_semantical_param * func_index list

     
    type function_pointer_map = int IntMap.t
    type module_def = {
      module_id: string;
      mutable ty: type_def list;
      mutable fn: func_def list;
      mutable fp_dic: function_pointer_map;
      mutable fp_num: int;
      mutable me: mem_def list;
      mutable gl: global_def list;
      mutable ex: export_def list;
      mutable st: func_index option;
      module_tb: table_def list;
      mutable module_el: elem_def list;
    }
    let add_local: func_def -> string option -> val_type -> unit = fun f id v ->
      f.locals <- f.locals @ [id, v]
    let new_func_def id func_type = {
      func_id = id;
      func_wasm_type = func_type;
      locals = [];
      body = []
    }
    let add_global (m: module_def) (id: string option) (v: val_type): unit =
      let init_value_insc =
        match v with
        | Number_type (iof, length) -> Numeric_insc (
          match iof with
          | NInt -> NInsc_consti (length, 0)
          | NFloat -> NInsc_constf (length, 0.0)
        )
        | Ref_type h -> (
          match h with
          | Func_ref -> Ref_insc (RInsc_null Func_heap_ref)
          | Extern_ref _ -> Ref_insc (RInsc_null Extern_heap_ref)
        )
      in
      let global_value_type = {
        global_var_type = v;
        is_mutable = true
      } in
      let global_def = {
        global_id = id;
        global_type = global_value_type;
        init_value = [init_value_insc]
      } in
      m.gl <- m.gl @ [global_def]
    let declare_fun: module_def -> string option -> param_annotator -> result_annotator -> func_def = fun module_ id p r ->
      let func_def = new_func_def id ({ p = p; r = r }) in
      module_.fn <- module_.fn @ [func_def];
      func_def

    let new_module id = 
      let links_file_func = new_func_def (Some "_links_wasm_file_func") ({ p = []; r = [] }) in {
        module_id = id; 
        ty = [];
        fn = [links_file_func];
        fp_dic = IntMap.empty;
        fp_num = 0;
        me = [];
        gl = [];
        ex = [];
        st = Some (Mni_name (name_of_func links_file_func));
        module_tb = [{
          table_id = Some "$$table";
          table_type_limit = {
            min = 1;
            max = Some 1
          };
          table_type_type = Func_ref
        }];
        module_el = [];
      }
  end
  
  module BinderMap = Utility.IntMap
  type var_binder_map = Ir.binder BinderMap.t

  module Pretty_printing = struct
    open Grammar
    type pretty_print_element =
      | IncIndent
      | DecIndent
      | Line
      | LineIndent
      | IncWithLineIndent
      | DecWithLineIndent
      | Paren of pretty_print_element list
      | Literal of string
      | Empty
      | IdSep
    type instruction_print_style =
      | Plain_style
      | Folded_style
    type argument_print_style =
      | Stack_style 
      | Arg_style

    type printer = {
      mutable indent: int;
      indent_wSNum: int;
      plain_insc_style: instruction_print_style;
      ctrl_insc_style: instruction_print_style;
      mutable argument_print: argument_print_style;
      abbreviate_single_module_def: bool
    }
    let default_printer () = { 
      indent = 0; 
      indent_wSNum = 2; 
      plain_insc_style = Plain_style;
      ctrl_insc_style = Plain_style;
      argument_print = Stack_style;
      abbreviate_single_module_def = false
    }
    let reverse_style_printer () = {
      indent = 0;
      indent_wSNum = 2;
      plain_insc_style = Folded_style;
      ctrl_insc_style = Folded_style;
      argument_print = Arg_style;
      abbreviate_single_module_def = true
    }
  
    let links_int_value_type = Number_type (NInt, L64)
    let links_int_type = Val_type links_int_value_type
    let links_float_value_type = Number_type (NFloat, L64)
    let links_float_type = Val_type links_float_value_type
    let links_unit_type = Unit_type
    (* let wasm_func_pointer_size = 4 *)
    let rec int_or_float_of_val_type value =
      match value with
      | Number_type (a, _) -> a 
      | Ref_type _ -> int_or_float_of_val_type wasm_func_pointer_type
    let rec length_of_val_type value =
      match value with
      | Number_type (_, b) -> b
      | Ref_type _ -> length_of_val_type wasm_func_pointer_type

    let ir_primitive_type2Wasm: CommonTypes.Primitive.t -> val_type = fun p ->
      let open CommonTypes.Primitive in
      match p with
      | Bool   
      | Int    
      | Char    -> links_int_value_type
      | Float   -> links_float_value_type
      | XmlItem 
      | DB      
      | String  -> raise (Not_implemented __LOC__)
    let to_val_type t =
      match t with
      | Val_type v -> v 
      | Heap_type _ -> raise (Not_implemented __LOC__)
      | Func_type _ -> raise (Not_implemented __LOC__)
      | Memory_type _ -> raise (Not_implemented __LOC__)
      | Table_type _ -> raise (Not_implemented __LOC__)
      | Global_type _ -> raise (Not_implemented __LOC__)
      | Error_type -> raise (Not_implemented __LOC__)
      | Unit_type -> raise (Not_implemented __LOC__)

    module PPInstruction = struct
      let prepend_dollar p = "$" ^ p
      let get_binder_name: Ir.binder -> string = fun binder ->
        let mangle_ir_local_name p = "_loc_" ^ string_of_int p in
        let mangle_ir_global_name p = "_glob_" ^ string_of_int p in
        let mangle_ir_var_name: Var.var -> bool -> string = fun var is_local ->
          if is_local then mangle_ir_local_name var
          else mangle_ir_global_name var in
        let mangle_ir_binder_name: Ir.binder -> string = fun binder ->
          mangle_ir_var_name (var_of_binder binder) (Scope.is_local (scope_of_binder binder)) 
        in
        let name = name_of_binder binder in
        let name =
          if name = "" then mangle_ir_binder_name binder
          else name ^ "_" ^ (binder |> var_of_binder |> string_of_int)
        in prepend_dollar name
      let get_function_binder_name (binder: Ir.binder): string =
        let name = name_of_binder binder in
        let h = string_of_int (var_of_binder binder) in
        let name =
          if name = "" then "_fun_" ^ h
          else name ^ "_" ^ h
        in prepend_dollar name
      (* let flatten_args: pretty_print_element list list -> pretty_print_element list = fun args ->
        List.fold_left (fun l r -> 
            match l with 
            | [] -> r
            | _ -> l @ r
          ) [] args *)
      let flatten_defs: pretty_print_element list -> pretty_print_element list = fun args ->
        List.fold_left (fun l r ->
            match l with
            | [] -> [r]
            | _ -> l @ [LineIndent; r]
          ) [] args
      let flatten_parts: pretty_print_element list list -> pretty_print_element list = fun args ->
        List.fold_left (fun l r ->
          match l, r with
          | [], r -> r
          | l, [] -> l
          | l, r -> l @ [LineIndent] @ r
        ) [] args

      (* let insc_args: printer -> pretty_print_element list -> pretty_print_element list list -> pretty_print_element list = fun printer insc args ->          
        let insc_args_inner: printer -> pretty_print_element list -> pretty_print_element list -> pretty_print_element list = fun printer insc args ->
          match printer.argument_print with
          | Stack_style -> (
            match args with 
            | [] -> LineIndent::insc
            | _ -> args @ [LineIndent] @ insc
          )
          | Arg_style -> 
            [LineIndent; Paren (insc @ IncIndent::args @ [DecIndent])]
        in
        insc_args_inner printer insc (flatten_args args)
      let insc_literal_args: printer -> string -> pretty_print_element list list -> pretty_print_element list = fun printer insc_name args ->
        insc_args printer [Literal insc_name] args
      let insc_literal_arg0: printer -> string -> pretty_print_element list = fun printer insc_name ->
        insc_args printer [Literal insc_name] []
      let const: printer -> CommonTypes.Constant.t -> pretty_print_element list = fun printer c ->
        let const1: printer -> val_type -> string -> pretty_print_element list = fun printer value_type string_value_of_constant ->
          insc_args printer [Literal (to_string_val_type value_type); Literal ".const"; IdSep; Literal string_value_of_constant] []
        in
        let string_value_of_constant = match c with
          | Bool value  -> string_of_bool value
          | Int value   -> string_of_int value
          | Char c      -> "\"" ^ Char.escaped c ^ "\""
          | String s    -> "\"" ^ escape_string s ^ "\""
          | Float value -> Utility.string_of_float' value
        in
        let value_type = c |> type_of |> ir_primitive_type2Wasm in
        const1 printer value_type string_value_of_constant
      let read_var: printer -> Ir.binder -> pretty_print_element list = fun printer binder ->
        insc_args printer [Literal (match scope_of_binder binder with | Local -> "local.get" | Global -> "global.get"); IdSep; Literal (ir_var_name2Wasm binder)] []
      let write_var: printer -> Ir.binder -> pretty_print_element list -> pretty_print_element list = fun printer binder arg1 ->
        insc_args printer [Literal (match scope_of_binder binder with | Local -> "local.set" | Global -> "global.set"); IdSep; Literal (ir_var_name2Wasm binder)] [arg1] *)

      let stringify name =
        Printf.sprintf "\"%s\"" name
      (* let insc_inline_import outer_name inner_name =
        Paren [Literal "import"; IdSep; Literal (stringify outer_name); IdSep; Literal (stringify inner_name)] *)
    end

    let give_indent_string pt = 
      String.make (pt.indent * pt.indent_wSNum) ' '
    let increment_indent pt = 
      pt.indent <- pt.indent + 1
    let decrement_indent pt =
      pt.indent <- pt.indent - 1
    let give: printer -> string -> string = fun _pt some ->
      some
    let give_line: printer -> string option -> string = fun pt some ->
      match some with
      | Some alp -> alp ^ (give pt (alp ^ "\n"))
      | None -> give pt "\n"
    let give_line_indent pt =
      (give_line pt None) ^ (give_indent_string pt)
    let give_id_sep: printer -> string = fun _ ->
      " "
    
                              
    let rec to_string pt element =
      match element with
      | IncIndent -> increment_indent pt; ""
      | DecIndent -> decrement_indent pt; ""
      | IdSep -> give_id_sep pt
      | Line -> give_line pt None
      | LineIndent -> give_line_indent pt
      | IncWithLineIndent -> 
        let _ = to_string pt IncIndent in
        to_string pt LineIndent
      | DecWithLineIndent -> 
        let _ = to_string pt DecIndent in
        to_string pt LineIndent
      | Paren es -> 
        "(" ^ (es |> List.map (fun e -> to_string pt e) |> String.concat "") ^ ")"
      | Empty -> ""
      | Literal s -> give pt s
        
    module OrderedBinder: Set.OrderedType with type t = Ir.binder = struct
      type t = Ir.binder
      let compare (a: binder) (b: binder) =
        (var_of_binder a) - (var_of_binder b)
      let show _ = raise (Not_implemented __LOC__)
      let pp _ _ = raise (Not_implemented __LOC__)
    end
    module type BINDERSET = Set with type elt = binder
    module BinderSet : BINDERSET = Set.Make(OrderedBinder)

    type wasm_writer = {
      printer: printer;
      writer: out_channel;
      wasm_module: module_def;
      mutable func_map: (func_def * Ir.fun_def) list;
      mutable func_map2: (binder, func_def) Hashtbl.t;
      mutable var_map: var_binder_map;
      mutable func_var: BinderSet.t;
      primitive_functions: Var.var Env.String.t
    }
    let new_wasm_writer printer out_channel module_def = {
      printer = printer;
      writer = out_channel;
      wasm_module = module_def;
      func_map = [];
      func_map2 = Hashtbl.create 2000;
      var_map = BinderMap.empty;
      func_var = BinderSet.empty;
      primitive_functions = Lib.nenv
    }
    let add_binder writer binder = 
      let var = Var.var_of_binder binder in
      writer.var_map <- BinderMap.add var binder writer.var_map
    let find_binder: wasm_writer -> Var.var -> Ir.binder option = fun writer var ->
      BinderMap.find_opt var writer.var_map
    let is_func_binder writer binder =
      match BinderSet.find_opt binder writer.func_var with
      | Some _ -> true
      | None -> false
    let is_func_var writer var =
      let binder = find_binder writer var in
      match binder with
      | Some b -> is_func_binder writer b 
      | None -> false
    let add_func_var writer binder = 
      writer.func_var <- BinderSet.add binder writer.func_var
    (* let wasm_table_name = "$$table" *)
    let rec get_function_pointer writer func_var =
      let dic = writer.wasm_module.fp_dic in
      match IntMap.find_opt func_var dic with
      | Some r -> r 
      | None -> 
        let p = func_pointer_number writer + 1 in (* function index starts from 1 and 0 is left blank, because decompiled wat's do so *)
        writer.wasm_module.fp_dic <- IntMap.add func_var p dic;
        writer.wasm_module.fp_num <- p;
        p
    and func_pointer_number writer =
      writer.wasm_module.fp_num

    let rec ir_type2Wasm: Types.datatype -> wasm_type = fun ir_type ->
      let ir_meta_type2Wasm = fun point ->
        (match Unionfind.find point with
         | Var _ -> raise (Not_implemented __LOC__)
         | Closed -> raise (Not_implemented __LOC__)
         | Recursive _ -> raise (Not_implemented __LOC__)
         | t -> ir_type2Wasm t
        ) in
      let ir_record_type2Wasm row = ir_type2Wasm row in
      let rec get_result (f: typ) =
        match f with
        | Function (_, _, out_type) -> get_result out_type
        | Primitive p -> ir_primitive_type2Wasm p 
        | _ -> raise (Not_implemented __LOC__)
      in
      let get_ir_func_params in_type =
        let rec ir_type_to_wasm_private ir_type =
          match ir_type with
          | Present t -> ir_type_to_wasm_private t 
          | _ -> ir_type2Wasm ir_type 
        in
        let extract_from_row r =
          let (field_map, _, _) = r in
          let types = StringMapExt.map (fun field_name field_type -> Some field_name, field_type |> ir_type_to_wasm_private) field_map in
          let types1 = match types with
          | [t] -> (
            match t with
            | (_, Unit_type) -> []
            | _ -> [t]
          )
          | _ -> types
          in
          List.map (fun (a, b) -> (a, to_val_type b)) types1
        in
        let rec extract_from_in_type = function
          | Row r -> extract_from_row r 
          | Record r -> extract_from_in_type r 
          | _ -> raise (Not_implemented __LOC__)
        in
        extract_from_in_type in_type
      in

      if string_of_datatype ir_type = "()" then Unit_type
      else
      match ir_type with
      | Primitive p -> Val_type (ir_primitive_type2Wasm p)
      | Function (in_type, _, out_type) ->
        let result_type = get_result ir_type in
        Func_type {
          p = get_ir_func_params in_type; r = [result_type]
        }
      | Effect _ -> raise (Not_implemented __LOC__)
      | Var _ -> raise (Not_implemented __LOC__)
      | Recursive _ -> raise (Not_implemented __LOC__)
      | Not_typed -> raise (Not_implemented __LOC__)
      | Alias _ -> raise (Not_implemented __LOC__)
      | Application _ -> raise (Not_implemented __LOC__)
      | RecursiveApplication _ -> raise (Not_implemented __LOC__)
      | Meta point -> ir_meta_type2Wasm point
      | Lolli _ -> raise (Not_implemented __LOC__)
      | Record row -> ir_record_type2Wasm row
      | Variant _ -> raise (Not_implemented __LOC__)
      | Table _ -> raise (Not_implemented __LOC__)
      | Lens _ -> raise (Not_implemented __LOC__)
      | ForAll (_quantifiers, underlying_type) -> ir_type2Wasm underlying_type
      | Row _ -> raise (Not_implemented (string_of_datatype ir_type))
      | Closed -> raise (Not_implemented __LOC__)
      (* Presence *)
      | Absent -> raise (Not_implemented __LOC__)
      | Present a -> raise (Not_implemented (string_of_datatype a))
      (* Session *)
      | Input _ -> raise (Not_implemented __LOC__)
      | Output _ -> raise (Not_implemented __LOC__)
      | Select _ -> raise (Not_implemented __LOC__)
      | Choice _ -> raise (Not_implemented __LOC__)
      | Dual _ -> raise (Not_implemented __LOC__)
      | End -> raise (Not_implemented __LOC__)
    let rec get_result_type = function
      | Function (_, _, result_type) -> ir_type2Wasm result_type 
      | Primitive _ -> raise (Not_implemented __LOC__)
      | Effect _ -> raise (Not_implemented __LOC__)
      | Var _ -> raise (Not_implemented __LOC__)
      | Recursive _ -> raise (Not_implemented __LOC__)
      | Not_typed -> raise (Not_implemented __LOC__)
      | Alias _ -> raise (Not_implemented __LOC__)
      | Application _ -> raise (Not_implemented __LOC__)
      | RecursiveApplication _ -> raise (Not_implemented __LOC__)
      | Meta _ -> raise (Not_implemented __LOC__)
      | Lolli _ -> raise (Not_implemented __LOC__)
      | Record _ -> raise (Not_implemented __LOC__)
      | Variant _ -> raise (Not_implemented __LOC__)
      | Table _ -> raise (Not_implemented __LOC__)
      | Lens _ -> raise (Not_implemented __LOC__)
      | ForAll (_quantifiers, underlying_type) -> get_result_type underlying_type
      | Row _ -> raise (Not_implemented __LOC__)
      | Closed -> raise (Not_implemented __LOC__)
      (* Presence *)
      | Absent -> raise (Not_implemented __LOC__)
      | Present _ -> raise (Not_implemented __LOC__)
      (* Session *)
      | Input _ -> raise (Not_implemented __LOC__)
      | Output _ -> raise (Not_implemented __LOC__)
      | Select _ -> raise (Not_implemented __LOC__)
      | Choice _ -> raise (Not_implemented __LOC__)
      | Dual _ -> raise (Not_implemented __LOC__)
      | End -> raise (Not_implemented __LOC__)
    let ir_func2wasm: module_def -> Ir.fun_def -> func_def = fun wasm_module ir_func ->
      let function_binder = ir_func.fn_binder in
      let params = 
        ir_func.fn_params 
        |> List.map (fun binder -> type_of_binder binder, binder)
        |> List.map (fun (ir_type, binder) -> 
          let wasm_type = ir_type2Wasm ir_type in 
          (wasm_type, binder)
        )
      in
      let params = 
        if List.length params = 1 then
          match List.hd params with
          | Unit_type, _ -> []
          | t, binder -> [Some (PPInstruction.get_binder_name binder), to_val_type t]
        else List.map (fun (t, binder) -> Some (PPInstruction.get_binder_name binder), to_val_type t) params
      in
      let result = get_result_type (type_of_binder function_binder) in
      let result = match result with
        | Unit_type -> []
        | t -> [to_val_type t]
      in
      let new_fun = declare_fun wasm_module (Some (PPInstruction.get_function_binder_name function_binder)) params result in
      new_fun
      
    let ir_value_is_unit: Ir.value -> bool = fun t ->
      match t with
      | Extend (name_map, value) -> 
        if Utility.StringMap.is_empty name_map then (
          match value with 
          | None -> true
          | _ -> false
        )
        else false
      | _ -> false
      
    (* type primitive_wasm_func_arity =
      | Zero of string
      | One of string
      | Two of string
      | Three of string
      | Any of string *)

    (* open PPInstruction *)
    (* let primitive_ir_func_table =
      let table = [
        "+", Two "add";
        "-", Two "sub";
        "*", Two "mul";
        "/", Two "div_s";
        "mod", Two "rem_s";
        "+.", Two "add";
        "-.", Two "sub";
        "*.", Two "mul";
        "/.", Two "div";
        "negatef", Two "neg";
        "==", Two "eq";
        "!=", Two "ne";
        ">=", Two "lt";
        "<=", Two "gt";
        "floor", One "floor";
        "ceiling", One "ceil";
        "sqrt", One "sqrt";
        "int_to_float", Zero ((to_string_val_type links_float_value_type) ^ ".convert_" ^ (to_string_val_type links_int_value_type) ^ "_s");
        "float_to_int", Zero ((to_string_val_type links_int_value_type) ^ ".trunc_sat_" ^ (to_string_val_type links_float_value_type) ^ "_s");
      ] in
      Utility.StringMap.from_alist table
    let find_primitive_func_name (v: Var.var): primitive_wasm_func_arity option = Utility.StringMap.find_opt (primitive_name v) primitive_ir_func_table *)
    let nenv' = Env.String.fold (fun key value acc -> Env.Int.bind value key acc) Lib.nenv Env.Int.empty
    let find_ir_var_type v = Env.String.find (Env.Int.find v nenv') Lib.type_env

    let rec type_of_apply writer applied_value _args =
      match applied_value with
      | Variable v -> 
        let ret_type1 = 
          if is_primitive_var v then find_ir_var_type v
          else v |> find_binder writer |> non_null |> type_of_binder 
        in 
        get_result_type ret_type1
      | Extend _ -> 
        if ir_value_is_unit applied_value then links_unit_type 
        else raise (Not_implemented __LOC__)
      | ApplyPure (applied_value, args) -> (
        match applied_value with
        | TApp (applied_value1, _) -> type_of_apply writer applied_value1 args
        | _ -> raise (Not_implemented __LOC__)
      )
      | _ -> raise (Not_implemented __LOC__)
    and type_of_value writer value =
      let type_of_primitive_value var = find_ir_var_type var |> ir_type2Wasm in
      let open Constant in
      match value with
      | Constant t -> (match t with
        | Bool _
        | Int _
        | Char _ -> links_int_type
        | String _ -> raise (Not_implemented __LOC__)
        | Float _ -> links_float_type
      )
      | Variable v -> 
        if is_primitive_var v then type_of_primitive_value v
        else find_binder writer v |> (fun t -> match t with|Some s->s|None->raise (Not_implemented (__LOC__ ^ " ... " ^ (string_of_int v)))) |> type_of_binder |> ir_type2Wasm
      | Extend _ -> if ir_value_is_unit value then links_unit_type else raise (Not_implemented __LOC__)
      | Project _ -> raise (Not_implemented __LOC__)
      | Erase _ -> raise (Not_implemented __LOC__)
      | Inject _ -> raise (Not_implemented __LOC__)
      | TAbs _ -> raise (Not_implemented __LOC__)
      | TApp _ -> raise (Not_implemented __LOC__)
      | XmlNode _ -> raise (Not_implemented __LOC__)
      | ApplyPure (applied_value, args) -> (
        match applied_value with
        | TApp (applied_value1, _) -> type_of_apply writer applied_value1 args
        | _ -> raise (Not_implemented __LOC__)
      )
      | Closure _ -> raise (Not_implemented __LOC__)
      | Coerce _ -> raise (Not_implemented __LOC__)
    (* let type_of_tail_computation (writer: wasm_writer) (comp: tail_computation) =
      match comp with
      | Return v -> type_of_value writer v
      | Apply (func, value) -> (
        match func with
        | TApp (func_value, _) -> type_of_apply writer func_value value
        | _ -> raise (Not_implemented __LOC__)
      )
      | Special _ -> links_int_type
      | Case _ -> links_int_type
      | If _ -> links_int_type *)

    (* let is_constant0 (v: Ir.value) =
      match v with
      | Constant c -> (
        match c with
        | Char f -> Char.code f = 0
        | Int i -> i = 0
        | Bool b -> b = false
        | _ -> false
      )
      | _ -> false *)
    (* let primitive_ir_func_to_wasm_insc (v: var) (operands: Ir.value list) (operator_types: val_type list) =
      let open Lib in
      if is_primitive_var v then 
        let operator_name = find_primitive_func_name v in
        match operator_name with 
        | Some op -> (
          let op_type_name = List.hd operator_types |> to_string_val_type in
          let operator_name1 = 
            match List.hd operator_types with
            | Number_type (n1, _) -> (
              match n1 with
              | NInt -> (
                let op1 = 
                  match op with
                  | Two op3 -> 
                    if op3 = "lt" || op3 = "gt" || op3 = "le" || op3 = "ge" then Two (op3 ^ "_s") 
                    else if op3 = "eq" && List.exists is_constant0 operands then One "eqz" 
                    else op
                  | _ -> op
                in op1
              )
              | _ -> op
            )
            | _ -> op
          in
          let temp t = Some (op_type_name ^ "." ^ t) in
          match operator_name1 with
          | Zero t -> Some t
          | One t -> temp t
          | Two t -> temp t
          | Three t -> temp t
          | Any t -> temp t
        )
        | None -> None
      else None *)

    (* let rec write_primitive_ir_func_call writer wasm_func v operands =
      let operator_types = operands |> List.map (fun t -> type_of_value writer t |> to_val_type) in
      let insc_name = primitive_ir_func_to_wasm_insc v operands operator_types in
      let write_arg = write_value writer wasm_func in
      let elements = match insc_name with
        | Some name -> 
          if SysExt.get_extension name = ".eqz" then
            let arg0 = List.nth operands 0 in
            let arg1 = List.nth operands 1 in
            if is_constant0 arg0 then Some (insc_literal_args writer.printer name [arg1 |> write_arg])
            else Some (insc_literal_args writer.printer name [arg0 |> write_arg])
          else
            Some (insc_literal_args writer.printer name (List.map write_arg operands))
        | None -> None
      in elements
    and write_normal_ir_func_call writer wasm_func v operands =
      BinderMap.iter (fun t _ -> Printf.printf "known binder: %i\n%!" t) writer.var_map;
      Printf.printf "finished%!";
      let binder = find_binder writer v |> non_null in
      let insc_lit = get_function_binder_name binder in
      insc_args writer.printer [Literal "call"; IdSep; Literal (prepend_dollar insc_lit)] (List.map (write_value writer wasm_func) operands)
    and write_func_pointer_call writer (v: Var.var) operands =
      let write_wasm_func_sig func =
        let process f = List.map to_string_val_type f |> List.map (fun t -> [IdSep; Literal t]) |> List.flatten in
        let params = Paren ([Literal "param"] @ (func.p |> List.map (fun (_name, t) -> t) |> process)) in
        let result = Paren ([Literal "result"] @ (process func.r)) in
        [params; IdSep; result]
      in
      let binder = v |> find_binder writer |> non_null in
      let read_func_var = binder |> write_read_var writer in
      let func_type = binder |> type_of_binder |> ir_type2Wasm in
      let type_insc = match func_type with
        | Func_type t -> write_wasm_func_sig t
        | _ -> raise (Not_implemented __LOC__)
      in
      insc_args writer.printer (Literal "call_indirect"::IdSep::type_insc) (read_func_var::operands)
    and write_ir_func_call writer wasm_func (v: Var.var) operands =
      let try_fp = if is_func_var writer v then write_func_pointer_call writer v (List.map (write_value writer wasm_func) operands)
      else (
        let try1 = write_primitive_ir_func_call writer wasm_func v operands in
        let try2 = match try1 with
        | Some s -> s 
        | None -> write_normal_ir_func_call writer wasm_func v operands
        in try2
      )
      in try_fp
    and write_computation: wasm_writer -> func_def -> computation -> pretty_print_element list = fun writer wasm_func comp ->
      let (bindings, tail) = comp in
      let b1 = bindings |> List.map (fun t -> write_binding writer wasm_func t) |> List.flatten in
      let t1 = write_tail_computation writer wasm_func tail in
      b1 @ t1
    and write_tail_computation: wasm_writer -> func_def -> tail_computation -> pretty_print_element list = fun writer wasm_func ir_insc ->
      let g1 = match ir_insc with
        | Return v -> 
            if ir_value_is_unit v then []
            else write_value writer wasm_func v
        | Apply (f, args) -> write_apply writer wasm_func f args
        | Special s -> write_special writer wasm_func s 
        | Case _ -> raise (Not_implemented __LOC__)
        | If (value, if_ins, else_ins) -> write_if_expression writer wasm_func value if_ins else_ins
      in g1
    and write_if_expression writer wasm_func value if_ins else_ins =
      let value_elements =
      match writer.printer.ctrl_insc_style with
      | Folded_style ->
        let old_style = writer.printer.argument_print in
        writer.printer.argument_print <- Arg_style;
        let a = write_value writer wasm_func value in
        writer.printer.argument_print <- old_style;
        a
      | Plain_style -> write_value writer wasm_func value
      in
      let if_elements = write_computation writer wasm_func if_ins in
      let else_elements = write_computation writer wasm_func else_ins in
      let (_, if_result) = if_ins in
      let if_result_type = type_of_tail_computation writer if_result in
      let (_, else_result) = else_ins in
      let else_result_type = type_of_tail_computation writer else_result in
      let _ = if if_result_type = else_result_type then ()
        else raise (Not_implemented "if else type mismatch")
      in
      let result_type_element = Paren [Literal "result"; IdSep; Literal (Grammar.to_string if_result_type)] in
      match writer.printer.ctrl_insc_style with
      | Plain_style -> (
        let all = value_elements @ [LineIndent; Literal "if"; IdSep; result_type_element; IncIndent] @ if_elements @ [DecWithLineIndent; Literal "else"; IncIndent] @ else_elements @ [DecWithLineIndent; Literal "end"] in
        all
      )
      | Folded_style -> (
        let connect keyword_name elements =
          if List.length elements <= 1 then Paren ([Literal keyword_name; IdSep] @ elements)
          else Paren ([Literal keyword_name; IncIndent] @ elements @ [DecWithLineIndent])
        in
        let then_element = connect "then" if_elements in
        let else_element = connect "else" else_elements in
        let all = Paren ([Literal "if"; IdSep; result_type_element; IncIndent] @ value_elements @ [LineIndent; then_element; LineIndent; else_element; DecWithLineIndent]) in
        [LineIndent; all]
      )
    and write_apply: wasm_writer -> func_def -> Ir.value -> Ir.value list -> pretty_print_element list = fun writer wasm_func f args ->
      write_apply_pure writer wasm_func f args
    and write_apply_pure: wasm_writer -> func_def -> Ir.value -> Ir.value list -> pretty_print_element list = fun writer wasm_func value args ->
      match value with
      | TApp (applied_value, _) -> (
        match applied_value with
        | Variable v -> write_ir_func_call writer wasm_func v args
        | _ -> raise (Not_implemented __LOC__)
      )
      | _ -> raise (Not_implemented __LOC__)
    and write_special: wasm_writer -> func_def -> Ir.special -> pretty_print_element list = fun _writer _wasm_func _special ->
      raise (Not_implemented __LOC__)
    and write_value: wasm_writer -> func_def -> Ir.value -> pretty_print_element list = fun writer wasm_func ir_value ->
      let write_func_value var =
        let fp = var |> get_function_pointer writer in
        insc_args writer.printer [Literal "i32.load"; IdSep; Literal "offset="; Literal (string_of_int (wasm_func_pointer_size * fp))] []
      in
      match ir_value with
      | Constant c -> write_constant writer c
      | Variable v -> write_read_var writer (find_binder writer v |> (fun t -> match t with | Some t -> t | None -> raise (Not_implemented (string_of_int v))))
      | ApplyPure (f, args) -> write_apply_pure writer wasm_func f args
      | Project  _ -> raise (Not_implemented __LOC__)
      | Extend _ -> raise (Not_implemented __LOC__)
      | Erase  _ -> raise (Not_implemented __LOC__)
      | Inject  _ -> raise (Not_implemented __LOC__)
      | TAbs  _ -> raise (Not_implemented __LOC__)
      (* TApp is probably function value, treat it as is *)
      | TApp (var, _tyargs) -> (
        match var with
        | Variable v -> write_func_value v
        | _ -> raise (Not_implemented __LOC__)
      )
      | XmlNode _ -> raise (Not_implemented __LOC__)
      | Closure _ -> raise (Not_implemented __LOC__)
      | Coerce _ -> raise (Not_implemented __LOC__)
    and write_constant: wasm_writer -> CommonTypes.Constant.t -> pretty_print_element list = fun writer constant ->
      const writer.printer constant
    and write_read_var: wasm_writer -> Ir.binder -> pretty_print_element list = fun writer binder ->
      read_var writer.printer binder
    and write_let_binding: wasm_writer -> func_def -> Ir.binder -> Ir.tyvar list -> Ir.tail_computation -> pretty_print_element list = fun writer wasm_func binder _type_var_list tail_computation ->
      let write_value_let_binding v =
        let local_name = get_binder_name binder in
        if scope_of_binder binder |> Scope.is_local then add_local wasm_func (Some local_name) v 
        else add_global writer.wasm_module (Some local_name) v;
        let comp = write_tail_computation writer wasm_func tail_computation in
        write_var writer.printer binder comp
      in
      let write_func_let_binding _f =
        let local_name = get_binder_name binder in
        if scope_of_binder binder |> Scope.is_local then add_local wasm_func (Some local_name) (Ref_type (Func_ref))
        else add_global writer.wasm_module (Some local_name) (Ref_type (Func_ref));
        let comp = write_tail_computation writer wasm_func tail_computation in
        add_func_var writer binder;
        write_var writer.printer binder comp
      in
      add_binder writer binder;
      let local_type = ir_type2Wasm (type_of_binder binder) in
      let local_type1 = match local_type with
        | Val_type v -> write_value_let_binding v 
        | Func_type f -> write_func_let_binding f
      in local_type1
    and write_binding: wasm_writer -> func_def -> Ir.binding -> pretty_print_element list = fun writer wasm_func binding ->
      let binding' = match binding with
        | Let (binder, (type_var_list, tail_computation)) -> write_let_binding writer wasm_func binder type_var_list tail_computation
        | Fun fun_def -> 
          collect_ir_fun writer fun_def; []
        | Rec fun_defs ->
          List.iter (collect_ir_fun writer) fun_defs; []
        | _ -> assert false
      in binding'
    and collect_ir_fun: wasm_writer -> Ir.fun_def -> unit = fun writer ir_func ->
      let wasm_func = ir_func2wasm writer.wasm_module ir_func in
      ir_func.fn_params |> List.iter (add_binder writer);
      writer.func_map <- writer.func_map @ [wasm_func, ir_func];
      add_binder writer ir_func.fn_binder

    and write_local: wasm_writer -> string option * val_type -> pretty_print_element list = fun _writer local ->
      [IdSep] @ (
        let (possible_name, _) = local in
        match possible_name with
        | Some s -> [Literal (prepend_dollar s); IdSep]
        | None -> []
      ) @ [Literal (let (_, val_type) = local in to_string_val_type val_type)]
    and write_global (_writer: wasm_writer) global_name val_type is_mutable =
      let name_literals = match global_name with
      | Some s -> [Literal (prepend_dollar s); IdSep]
      | None -> [] in
      let type_literals = Literal (to_string_val_type val_type) in
      let type_literals = if is_mutable then Paren [Literal "mut"; IdSep; type_literals] else type_literals in
      let init_literals = Paren [Literal (to_string_val_type val_type ^ ".const"); IdSep; Literal "0"] in
      let elements = [Literal "global"; IdSep] @ name_literals @ [type_literals; IdSep; init_literals] in
      Paren elements
      
    and write_wasm_func_params writer (wasm_func: func_def) =
      let func_type = wasm_func.wasm_type in
      func_type.p
      |> List.map (fun t -> [Literal "param"] @ (write_local writer t))
      |> List.map (fun t -> [IdSep; Paren t])
      |> List.flatten
    and write_wasm_func_result _writer (wasm_func: func_def) =
      let result_type = List.map to_string_val_type wasm_func.wasm_type.r |> List.map (fun t -> [IdSep; Literal t]) |> List.flatten in
      match result_type with
        | [] -> []
        | _ -> [IdSep; Paren ([Literal "result"] @ result_type)]
    and write_ir_func_sig writer ir_func =
      let wasm_func = ir_func2wasm writer.wasm_module ir_func in
      (write_wasm_func_params writer wasm_func) @ (write_wasm_func_result writer wasm_func)
    and write_wasm_func_title_params writer wasm_func =
      let title = [Literal "func"; IdSep; Literal (prepend_dollar (name_of_func wasm_func))] in
      let params = write_wasm_func_params writer wasm_func in
      title @ params
    let write_wasm_fun_locals writer wasm_func =
      let locals = wasm_func.locals 
        |> List.map (fun t -> Paren (Literal "local"::(write_local writer t)))
        |> List.fold_left (fun l r -> match l with
          | [] -> [r]
          | _ -> l @ [IdSep; r]
        ) [] in
      let locals = match locals with
        | [] -> [IncIndent]
        | _ -> IncWithLineIndent::locals in
      locals
    let write_wasm_fun_with: wasm_writer -> func_def -> pretty_print_element list -> pretty_print_element = fun writer wasm_func body ->
      let title_params = write_wasm_func_title_params writer wasm_func in
      let results = write_wasm_func_result writer wasm_func in
      let locals = write_wasm_fun_locals writer wasm_func in
      let fun_body = title_params @ results @ locals @ body @ [DecWithLineIndent] in
      Paren fun_body
    let write_main_func writer wasm_func body =
      let (bindings, tail) = body in
      let bindings = bindings |> List.map (write_binding writer wasm_func) |> List.flatten in
      let tail_computation = tail |> write_tail_computation writer wasm_func in
      let pop = if type_of_tail_computation writer tail = links_unit_type then [] else insc_literal_arg0 writer.printer "drop" in
      let body = bindings @ tail_computation @ pop in
      let title_params = write_wasm_func_title_params writer wasm_func in
      let result_type1 = [] in
      let locals = write_wasm_fun_locals writer wasm_func in
      let fun_body = title_params @ result_type1 @ locals @ body @ [DecWithLineIndent] in
      Paren fun_body
    let write_function_computation: wasm_writer -> func_def -> computation -> pretty_print_element = fun writer wasm_func body ->
      let (bindings, tail) = body in
      let bindings = bindings |> List.map (write_binding writer wasm_func) |> List.flatten in
      let tail_computation = tail |> write_tail_computation writer wasm_func in
      write_wasm_fun_with writer wasm_func (bindings @ tail_computation)
    let write_wasm_fun: wasm_writer -> func_def -> Ir.fun_def -> pretty_print_element = fun writer wasm_func ir_func ->
      write_function_computation writer wasm_func ir_func.fn_body
    let write_globals writer =
      let globals = writer.wasm_module.gl in
      let global_elements = List.map (fun (t: global_def) -> write_global writer t.global_id t.global_type.global_var_type t.global_type.is_mutable) globals in
      flatten_defs global_elements
    let write_tables writer =
      let fp = func_pointer_number writer in
      Paren [Literal "table"; IdSep; Literal wasm_table_name; IdSep; Literal (string_of_int fp); IdSep; Literal "funcref"]
    let write_elements writer =
      
      let r = writer.wasm_module.fp_dic 
        |> IntMapExt.map (fun key _ -> key |> find_binder writer |> non_null |> get_function_binder_name |> prepend_dollar)
        |> List.map (fun name -> [IdSep; Literal name])
        |> List.flatten in
      Paren ([Literal "elem"; IdSep; Paren [Literal "i32.const"; IdSep; Literal "1"]; IdSep; Literal "func"] @ r)
    let write_program: wasm_writer -> Ir.program -> pretty_print_element = fun writer program ->
      let wasm_module = writer.wasm_module in
      let main_func = List.hd wasm_module.fn in
      let main_func1 = write_main_func writer main_func program in
      let functions = writer.func_map |> List.map (fun t -> let (wasm_func, ir_func) = t in write_wasm_fun writer wasm_func ir_func) in
      let functions = functions @ [main_func1] |> flatten_defs in
      let globals = write_globals writer in
      let tables = [write_tables writer] in
      let elements = [write_elements writer] in
      let module_defs = flatten_parts [globals; functions; tables; elements] in
      if writer.printer.abbreviate_single_module_def then Paren module_defs
      else (
        let g1 = [Literal "module"; IdSep] in
        let g2 = g1 @ [Literal (prepend_dollar wasm_module.module_id)] in
        let g3 = g2 @ [IncWithLineIndent] @ module_defs @ [DecWithLineIndent] in
        let g4 = Paren g3 in
        g4
      )

    let write_ir2Wasm writer program =
      let ppe = write_program writer program in
      to_string writer.printer ppe |> Printf.fprintf writer.writer "%s";
      close_out writer.writer *)
  end

  module Ir2WasmAst = struct
    open Grammar
    open Pretty_printing
    open PPInstruction

    let new_module id = {
      module_id = id; 
      ty = [];
      fn = [];
      fp_dic = IntMap.empty;
      fp_num = 0;
      me = [{
        memory_id = Some "$$memory";
        memory_type = {
          memory_type_limit = {
            min = 0;
            max = None;
          }
        }
      }];
      gl = [];
      ex = [];
      st = None;
      module_tb = [{
        table_id = Some "$$table";
        table_type_limit = {
          min = 1;
          max = Some 1
        };
        table_type_type = Func_ref
      }];
      module_el = [];
    }

    let links_main_func_name = "$_links_wasm_file_func"
    let rec collect_program writer program =
      let (bindings, _) = program in
      let main_func = declare_fun writer.wasm_module (Some links_main_func_name) [] [] in
      bindings |> List.iter (collect_binding writer main_func);
      (* main func must have type [] -> [] *)
      (* let main_func_type = type_of_tail_computation writer tail_computation in
      let main_func_type = match main_func_type with
        | Val_type t -> [t]
        | Unit_type -> []
        | _ -> assert false
      in
      main_func.func_wasm_type <- { p = []; r = main_func_type }; *)
      writer.wasm_module.st <- Some (Mni_name (name_of_func main_func));
    and collect_binding writer func binding =
      match binding with
      | Let (binder, (type_var_list, tail_computation)) -> collect_let_binding writer func binder type_var_list tail_computation
      | Fun fun_def -> 
        collect_ir_fun writer fun_def
      | Rec fun_defs ->
        List.iter (collect_ir_fun writer) fun_defs
      | Alien _ -> raise (Not_implemented __LOC__)
      | Module _ -> raise (Not_implemented __LOC__)
    and collect_let_binding writer func binder _tylist tail_computation = 
      collect_tail_computation writer func tail_computation;
      let local_name = get_binder_name binder in
      let collect_value_let_binding v =
        if scope_of_binder binder |> Scope.is_local then add_local func (Some local_name) v 
        else add_global writer.wasm_module (Some local_name) v
      in
      let collect_fp_let_binding _f =
        if scope_of_binder binder |> Scope.is_local then add_local func (Some local_name) (Ref_type (Func_ref))
        else add_global writer.wasm_module (Some local_name) (Ref_type (Func_ref));
        add_func_var writer binder
      in
      add_binder writer binder;
      let local_type = ir_type2Wasm (type_of_binder binder) in
      match local_type with
      | Val_type v -> collect_value_let_binding v 
      | Func_type f -> collect_fp_let_binding f
    and collect_ir_fun writer (ir_func: Ir.fun_def) =
      let wasm_func = ir_func2wasm writer.wasm_module ir_func in
      ir_func.fn_params |> List.iter (fun t -> add_binder writer t);
      Hashtbl.add writer.func_map2 ir_func.fn_binder wasm_func;
      add_binder writer ir_func.fn_binder;
      let comp = ir_func.fn_body in
      collect_computation writer wasm_func comp
    and collect_computation writer func comp =
      let (bindings, tail) = comp in
      List.iter (collect_binding writer func) bindings;
      collect_tail_computation writer func tail
    and collect_tail_computation writer func tail = 
      match tail with
      | If (_, a, b) -> collect_computation writer func a; collect_computation writer func b
      | Case _ -> raise (Not_implemented __LOC__)
      | _ -> ()
    

    let find_func writer fn_binder =
      Hashtbl.find writer.func_map2 fn_binder


    let rec conv_program writer (program: Ir.program) =
      conv_main_func writer program    
    and type_of_tail_computation writer comp =
      match comp with
      | Return v -> type_of_value writer v
      | Apply (func, value) -> (
        match func with
        | TApp (func_value, _) -> type_of_apply writer func_value value
        | _ -> raise (Not_implemented __LOC__)
      )
      | Special _ -> links_int_type
      | Case _ -> links_int_type
      | If (_, if_tail, else_tail) -> 
        type_of_comp_group writer [if_tail; else_tail]
    and type_of_comp_group writer (comps: computation list) =
      let types = List.map (fun t -> let (_, tail) = t in type_of_tail_computation writer tail) comps in
      let hd = List.hd types in
      if List.for_all (fun t -> t = hd) types then hd
      else raise (Not_implemented __LOC__)
    and conv_tail_computation writer func tail_computation =
      match tail_computation with
      | Return v -> 
        if ir_value_is_unit v then Control_insc CInsc_nop
        else conv_value writer func v
      | Apply (f, args) -> conv_apply writer func f args
      | Special _ -> raise (Not_implemented __LOC__)
      | Case _ -> raise (Not_implemented __LOC__)
      | If (value, if_comp, else_comp) -> conv_if writer func value if_comp else_comp
    and conv_if writer func value if_comp else_comp =
      let value1 = conv_value writer func value in
      let if1: instruction list = conv_computation writer func if_comp in
      let else1: instruction list = conv_computation writer func else_comp in
      let if_type: wasm_type = type_of_comp_group writer [if_comp; else_comp] in
      let insc = CInsc_if (TU_def { p = []; r = [to_val_type if_type] }, value1, if1, else1) in
      Control_insc insc
    and conv_main_func writer program =
      let (bindings, tail_computation) = program in
      let main_func = writer.wasm_module.fn |> List.hd in
      let bindings = bindings |> conv_bindings writer main_func in
      let tail = conv_tail_computation writer main_func tail_computation in
      let tail = match type_of_tail_computation writer tail_computation with
        | Unit_type -> [tail]
        | _ -> [Param_insc (PInsc_drop tail)]
      in
      main_func.body <- bindings @ tail
    and conv_let_binding writer func binder _type_var_list tail_computation =
      let comp_insc = conv_tail_computation writer func tail_computation in
      conv_write_var binder comp_insc
    and conv_bindings writer func bindings =
      List.map (conv_binding writer func) bindings |> non_null_list
    and conv_binding writer func binding =
      let process_func fun_def =
        conv_func writer (find_func writer fun_def.fn_binder) fun_def
      in
      match binding with
      | Let (binder, (type_var_list, tail_computation)) -> Some (conv_let_binding writer func binder type_var_list tail_computation)
      | Fun fun_def -> process_func fun_def; None
      | Rec fun_defs -> List.iter process_func fun_defs; None
      | Alien _ -> raise (Not_implemented __LOC__)
      | Module _ -> raise (Not_implemented __LOC__)
    and conv_func writer func ir_func =
      func.body <- conv_computation writer func ir_func.fn_body
    and conv_read_var binder =
      let var_index = Mni_name (get_binder_name binder) in
      let insc = match scope_of_binder binder with
        | Local -> VInsc_lget var_index
        | Global -> VInsc_gget var_index
      in Var_insc insc
    and conv_write_var binder comp_insc =
      let var_index = Mni_name (get_binder_name binder) in
      let insc = match scope_of_binder binder with
        | Local -> VInsc_lset (var_index, comp_insc)
        | Global -> VInsc_gset (var_index, comp_insc)
      in Var_insc insc
    and conv_value writer func ir_value =
      let conv_const (const_value: Constant.t) =
        let insc = match const_value with
          | Bool value -> NInsc_consti (length_of_val_type links_int_value_type, if value then 1 else 0)
          | Char value -> NInsc_consti (length_of_val_type links_int_value_type, int_of_char value)
          | Int value -> NInsc_consti (length_of_val_type links_int_value_type, value)
          | Float value -> NInsc_constf (length_of_val_type links_float_value_type, value)
          | _ -> raise (Not_implemented __LOC__)
        in
        Numeric_insc insc
      in
      let conv_func_value var =
        let fp = var |> get_function_pointer writer in
        let insc = NInsc_consti (length_of_val_type wasm_func_pointer_type, fp) in
        Numeric_insc insc
      in
      match ir_value with
      | Constant c -> conv_const c
      | Variable v -> 
        conv_read_var (find_binder writer v |> (fun t -> match t with | Some t -> t | None -> raise (Not_implemented (string_of_int v))))
      | ApplyPure (f, args) -> conv_apply_pure writer func f args
      | Project  _ -> raise (Not_implemented __LOC__)
      | Extend _ -> raise (Not_implemented __LOC__)
      | Erase  _ -> raise (Not_implemented __LOC__)
      | Inject  _ -> raise (Not_implemented __LOC__)
      | TAbs  _ -> raise (Not_implemented __LOC__)
      (* TApp is probably function value, treat it as is *)
      | TApp (var, _tyargs) -> (
        match var with
        | Variable v -> conv_func_value v
        | _ -> raise (Not_implemented __LOC__)
      )
      | XmlNode _ -> raise (Not_implemented __LOC__)
      | Closure _ -> raise (Not_implemented __LOC__)
      | Coerce _ -> raise (Not_implemented __LOC__)
    and conv_apply_pure writer func value args =
      match value with
      | TApp (applied_value, _) -> (
        match applied_value with
        | Variable v -> conv_ir_func_call writer func v args
        | _ -> raise (Not_implemented __LOC__)
      )
      | _ -> raise (Not_implemented __LOC__)
    and conv_apply writer func value args = conv_apply_pure writer func value args 
    and conv_primitive_ir_func_call writer func v operands =
      let operator_types = operands |> List.map (fun t -> type_of_value writer t |> to_val_type) in
      let op1_type = List.nth operator_types 0 in
      let op1 = List.nth operands 0 |> conv_value writer func in
      let op2 = match List.nth_opt operands 1 with
        | Some t -> Some (conv_value writer func t)
        | None -> None
      in
          
      let insc = match v |> primitive_name with
        | "+" -> NInsc_ibinop_add (op1_type |> length_of_val_type, op1, non_null op2)
        | "-" -> NInsc_ibinop_sub (op1_type |> length_of_val_type, op1, non_null op2)
        | "*" -> NInsc_ibinop_mul (op1_type |> length_of_val_type, op1, non_null op2)
        | "/" -> NInsc_ibinop_div (op1_type |> length_of_val_type, op1, non_null op2)
        | "mod" -> NInsc_ibinop_rem (op1_type |> length_of_val_type, op1, non_null op2)
        | "+." -> NInsc_fbinop_add (op1_type |> length_of_val_type, op1, non_null op2)
        | "-." -> NInsc_fbinop_sub (op1_type |> length_of_val_type, op1, non_null op2)
        | "*." -> NInsc_fbinop_mul (op1_type |> length_of_val_type, op1, non_null op2)
        | "/." -> NInsc_fbinop_div (op1_type |> length_of_val_type, op1, non_null op2)
        | "negatef" -> NInsc_funop_neg (op1_type |> length_of_val_type, op1)
        | "==" -> NInsc_relop_eq (op1_type |> int_or_float_of_val_type, op1_type |> length_of_val_type, op1, non_null op2)
        | "!=" -> NInsc_relop_ne (op1_type |> int_or_float_of_val_type, op1_type |> length_of_val_type, op1, non_null op2)
        | ">=" -> NInsc_relop_ge (op1_type |> int_or_float_of_val_type, op1_type |> length_of_val_type, op1, non_null op2)
        | "<=" -> NInsc_relop_le (op1_type |> int_or_float_of_val_type, op1_type |> length_of_val_type, op1, non_null op2)
        | ">" -> NInsc_relop_gt (op1_type |> int_or_float_of_val_type, op1_type |> length_of_val_type, op1, non_null op2)
        | "<" -> NInsc_relop_lt (op1_type |> int_or_float_of_val_type, op1_type |> length_of_val_type, op1, non_null op2)
        | "floor" -> NInsc_funop_floor (op1_type |> length_of_val_type, op1)
        | "ceiling" -> NInsc_funop_ceil (op1_type |> length_of_val_type, op1)
        | "sqrt" -> NInsc_funop_sqrt (op1_type |> length_of_val_type, op1)
        | "int_to_float" -> NInsc_convert (op1_type |> length_of_val_type, links_float_value_type |> length_of_val_type, Signed, op1)
        | "float_to_int" -> NInsc_trunc_sat (op1_type |> length_of_val_type, links_int_value_type |> length_of_val_type, Signed, op1)

        | "abs" -> NInsc_funop_abs (op1_type |> length_of_val_type, op1)
        | "maximum" -> NInsc_fbinop_max (op1_type |> length_of_val_type, op1, non_null op2)
        | name -> raise (Not_implemented ("primitive value " ^ name ^ "not recognised"))
      in
      Numeric_insc insc
    and conv_normal_ir_func_call writer func v operands =
      let ops = List.map (conv_value writer func) operands in
          
      let func_index = Mni_name (find_binder writer v |> non_null |> get_function_binder_name) in
      let insc = CInsc_call (func_index, ops) in
      Control_insc insc
    and conv_func_pointer_call writer func (v: Var.var) operands =
      let binder = v |> find_binder writer |> non_null in
      let func_type = binder |> type_of_binder |> ir_type2Wasm in
      let type_use = match func_type with
        | Func_type t -> TU_def t
        | _ -> raise (Not_implemented __LOC__)
      in
      let insc = CInsc_call_indirect (Mni_name (binder |> get_binder_name), Scope.is_local (scope_of_binder binder), type_use, (operands |> List.map (conv_value writer func))) in
      Control_insc insc
    and conv_ir_func_call writer func v operands =
      let try_fp = 
        if is_func_var writer v then conv_func_pointer_call writer func v operands
        else if is_primitive_var v then conv_primitive_ir_func_call writer func v operands
        else conv_normal_ir_func_call writer func v operands
      in try_fp    
    and conv_computation writer func comp =
      let (bindings, tail) = comp in
      let not_nop = function
        | Control_insc c -> (
          match c with
          | CInsc_nop -> false
          | _ -> true
        )
        | _ -> true
      in
      let b1 = bindings |> List.map (conv_binding writer func) |> non_null_list |> List.filter not_nop in
      let t1 = conv_tail_computation writer func tail in
      b1 @ [t1]


    let string_of_length = function
      | L32 -> "32"
      | L64 -> "64"
    let string_of_iof = function
      | NInt -> "i"
      | NFloat -> "f"
    let string_of_memory_length = function
      | Nii_extend_from_8 -> "8"
      | Nii_extend_from_16 -> "16"
      | Nii_extend_from_32 -> "32"
    let string_of_sign = function
      | Signed -> "s"
      | Unsigned -> "u"
    let write_integer i =
      string_of_int i 
    let write_float f =
      string_of_float f

    let rec write_program writer =
      let mo = writer.wasm_module in
      let printer = writer.printer in
      let id = Literal mo.module_id in
      let fn = List.map (write_func writer) mo.fn |> flatten_defs in
      let me = List.map write_memory mo.me |> flatten_defs in
      let gl = List.map (write_global printer) mo.gl |> flatten_defs in
      let ex = List.map write_export mo.ex |> flatten_defs in
      let st = write_start_point mo.st in
      let tb = List.map (write_table writer) mo.module_tb |> flatten_defs in
      let el = write_element writer in
      let parts = match st with
        | Empty -> [fn; me; gl; ex; tb; [el]]
        | _ -> [fn; me; gl; ex; [st]; tb; [el]]
      in
      let parts = [Literal "module"; IdSep; id; IncWithLineIndent] @ (flatten_parts parts) @ [DecWithLineIndent] in
      Paren parts
    and write_memory memory =
      let limit = write_limit memory.memory_type.memory_type_limit in
      let p = [Literal "memory"; IdSep] @ (match memory.memory_id with | Some s -> [Literal s; IdSep] | None -> []) @ limit in
      Paren p 
    and write_limit limit =
      match limit.max with
      | Some max -> [Literal (string_of_int limit.min); IdSep; Literal (string_of_int max)]
      | None -> [Literal (string_of_int limit.min)]
    and write_global printer global =
      let global_type = global.global_type in
      let t = [Literal "global"; IdSep] in
      let t = match global.global_id with
        | Some id -> t @ [Literal id; IdSep]
        | None -> t
      in
      let type_node = Literal (to_string_val_type global_type.global_var_type) in
      let type_node = if global_type.is_mutable then Paren [Literal "mut"; IdSep; type_node] else type_node in
      let init_node = write_instructions_inline printer global.init_value in
      Paren (t @ [type_node] @ [IdSep; Paren init_node])
    and write_mono_index index =
      match index with
      | Mni_int number -> Literal (string_of_int number)
      | Mni_name name -> Literal name
    and write_export export =
      let write_export_desc desc =
        match desc with
        | ED_func i -> Paren [Literal "func"; IdSep; write_mono_index i]
        | ED_global i -> Paren [Literal "global"; IdSep; write_mono_index i]
        | ED_mem i -> Paren [Literal "memory"; IdSep; write_mono_index i]
        | ED_table i -> Paren [Literal "table"; IdSep; write_mono_index i]
      in
      let ex = write_export_desc export.desc in
      Paren [Literal "export"; IdSep; Literal (stringify export.export_id); IdSep; ex]
    and write_start_point st =
      match st with 
      | Some st -> Paren [Literal "start"; IdSep; write_mono_index st]
      | None -> Empty
    and write_table writer tb =
      let func_num =  writer.wasm_module.fp_num + 1 in
      tb.table_type_limit.min <- func_num;
      (match tb.table_type_limit.max with
      | Some _ -> tb.table_type_limit.max <- Some func_num
      | None -> ());
      Paren ([Literal "table"; IdSep] @ (write_limit tb.table_type_limit) @ [IdSep; Literal "funcref"])
    and write_element writer =
      let elems = IntMapExt.map (fun var _ -> [IdSep; Literal (var |> find_binder writer |> non_null |> get_function_binder_name)]) writer.wasm_module.fp_dic |> List.flatten in
      Paren ([Literal "elem"; IdSep; Paren [Literal "i32.const 1"]; IdSep; Literal "func"] @ elems)
    and flatten_params params =
      List.fold_left (fun acc p -> match acc with | [] -> [p] | _ -> acc @ [IdSep; p]) [] params
    and try_write_name name =
      match name with
      | Some n -> [IdSep; Literal n]
      | None -> []
    and write_func writer func =
      let func_type_nodes = write_func_type func.func_wasm_type in
      let name_nodes = try_write_name func.func_id in
      let write_locals locals =
        let write_local local =
          let (n, t) = local in
          let local_name_nodes = try_write_name n in
          Paren ([Literal "local"] @ local_name_nodes @ [IdSep] @ [write_val_type t])
        in
        List.map write_local locals |> flatten_params 
      in
      let local_nodes = match write_locals func.locals with
      | [] -> [] 
      | a -> a @ [LineIndent] in
      let insc_nodes = func.body |> List.map (write_instruction writer.printer) |> List.flatten |> ListExt.skip_while (function | LineIndent -> true | _ -> false) in
      let nodes = [Literal "func"] @ name_nodes @ func_type_nodes @ [IncWithLineIndent] @ local_nodes @ insc_nodes @ [DecWithLineIndent] in
      Paren nodes
    and write_func_type func_type =
      let write_param param =
        let (n, t) = param in
        let name_nodes = try_write_name n in
        Paren ([Literal "param"] @ name_nodes @ [IdSep] @ [write_val_type t])
      in
      let param_nodes = List.map write_param func_type.p |> List.map (fun t -> [IdSep; t]) |> List.flatten in
      let result_nodes = List.map (fun t -> Paren [Literal "result"; IdSep; write_val_type t]) func_type.r |> List.map (fun t -> [IdSep; t]) |> List.flatten in
      param_nodes @ result_nodes
    and write_val_type val_type =
      Literal (to_string_val_type val_type)
    and insc_args printer insc_nodes args =
      let insc_nodes = LineIndent::insc_nodes in
      let connected_args = List.fold_left (fun acc arg -> match acc with | [] -> arg | t -> t @ [IdSep] @ arg) [] args in
      match printer.argument_print with
      | Arg_style -> [Paren ([IncIndent] @ insc_nodes @ connected_args @ [DecIndent])]
      | Stack_style -> connected_args @ insc_nodes
    and insc_arg0 insc_nodes =
      LineIndent::insc_nodes
    and write_instruction printer instruction =
      let node = match instruction with
        | Control_insc c -> write_control_insc printer c
        | Numeric_insc c -> write_numeric_insc printer c
        | Ref_insc c -> write_ref_insc printer c 
        | Var_insc c -> write_var_insc printer c
        | Param_insc c -> write_param_insc printer c
      in 
      node
    and write_insc_name_with_number insc number =
      [LineIndent; Literal insc; IdSep; Literal (write_integer number)]
    and write_insc_name_with_arg insc arg_nodes =
      [Literal insc; IdSep] @ arg_nodes
    and write_insc_name_with_index insc index =
      [Literal insc; IdSep] @ [write_mono_index index]
    and write_numeric_insc printer instruction =
      let write_numeric_relop name t =
        let (iof, length, left, right) = t in
        let name =
          match iof with
          | NInt -> (
            match name with
            | "eq" | "ne" -> name
            | _ -> name ^ "_s"
          )
          | NFloat -> name
        in
        let left_nodes = write_instruction printer left in
        let right_nodes = write_instruction printer right in
        let insc_name = Literal ((string_of_iof iof) ^ (string_of_length length) ^ "." ^ name) in
        let nodes = insc_args printer [insc_name] [left_nodes; right_nodes] in
        nodes
      in
      let write_unop name iof t =
        let (length, arg) = t in
        let args = [write_instruction printer arg] in
        let insc_name = Literal ((string_of_iof iof) ^ (string_of_length length) ^ "." ^ name) in
        insc_args printer [insc_name] args
      in
      let write_binop name iof length args =
        let args = List.map (write_instruction printer) args in
        let insc_name = Literal ((string_of_iof iof) ^ (string_of_length length) ^ "." ^ name) in
        insc_args printer [insc_name] args
      in
      let write_binop_unsigned name iof t =
        let (length, arg1, arg2) = t in
        write_binop name iof length [arg1; arg2]
      in
      let write_name_arg name arg =
        insc_args printer [Literal name] [write_instruction printer arg]
      in
      let write_trunc name t =
        let (to_t, from_t, sign, arg) = t in
        write_name_arg ((string_of_iof NInt) ^ (string_of_length to_t) ^ name ^ "_" ^ (string_of_iof NInt) ^ (string_of_length from_t) ^ "_" ^ (string_of_sign sign)) arg
      in
      let write_convert t =
        let (to_t, from_t, sign, arg) = t in
        write_name_arg ((string_of_iof NFloat) ^ (string_of_length to_t) ^ "convert_" ^ (string_of_iof NFloat) ^ (string_of_length from_t) ^ "_" ^ (string_of_sign sign)) arg
      in
      
      match instruction with
      | NInsc_consti (length, i) -> write_insc_name_with_number ((string_of_iof NInt) ^ (string_of_length length) ^ ".const") i
      | NInsc_constf (length, f) -> write_insc_name_with_arg ((string_of_iof NFloat) ^ (string_of_length length) ^ ".const") ([Literal (write_float f)])
      | NInsc_relop_eq t -> write_numeric_relop "eq" t 
      | NInsc_relop_ne t -> write_numeric_relop "ne" t 
      | NInsc_relop_ge t -> write_numeric_relop "ge" t 
      | NInsc_relop_le t -> write_numeric_relop "le" t 
      | NInsc_relop_gt t -> write_numeric_relop "gt" t 
      | NInsc_relop_lt t -> write_numeric_relop "lt" t 
      (* | NInsc_iunop_clz t -> write_unop "clz" NInt t 
      | NInsc_iunop_ctz t -> write_unop "ctz" NInt t 
      | NInsc_iunop_popcnt t -> write_unop "popcnt" NInt t  *)
      | NInsc_iunop_clz _ -> raise (Unreachable __LOC__)
      | NInsc_iunop_ctz _ -> raise (Unreachable __LOC__)
      | NInsc_iunop_popcnt _ -> raise (Unreachable __LOC__)
      | NInsc_itestop_eqz t -> write_unop "eqz" NInt t
      | NInsc_ibinop_add t -> write_binop_unsigned "add" NInt t 
      | NInsc_ibinop_sub t -> write_binop_unsigned "sub" NInt t 
      | NInsc_ibinop_mul t -> write_binop_unsigned "mul" NInt t 
      | NInsc_ibinop_div t -> write_binop_unsigned "div_s" NInt t 
      | NInsc_ibinop_rem t -> write_binop_unsigned "rem_s" NInt t 
      | NInsc_ibinop_and _ -> raise (Unreachable __LOC__)
      | NInsc_ibinop_or _ -> raise (Unreachable __LOC__)
      | NInsc_ibinop_xor _ -> raise (Unreachable __LOC__)
      | NInsc_ibinop_shl _ -> raise (Unreachable __LOC__)
      | NInsc_ibinop_shr _ -> raise (Unreachable __LOC__)
      | NInsc_ibinop_roti _ -> raise (Unreachable __LOC__)
      | NInsc_ibinop_rotr _ -> raise (Unreachable __LOC__)
      | NInsc_fbinop_add t -> write_binop_unsigned "add" NFloat t 
      | NInsc_fbinop_sub t -> write_binop_unsigned "sub" NFloat t 
      | NInsc_fbinop_mul t -> write_binop_unsigned "mul" NFloat t 
      | NInsc_fbinop_div t -> write_binop_unsigned "div" NFloat t 
      | NInsc_fbinop_min t -> write_binop_unsigned "min" NFloat t 
      | NInsc_fbinop_max t -> write_binop_unsigned "max" NFloat t 
      | NInsc_fbinop_copysign t -> write_binop_unsigned "copysign" NFloat t 
      | NInsc_funop_abs t -> write_unop "abs" NFloat t 
      | NInsc_funop_neg t -> write_unop "neg" NFloat t 
      | NInsc_funop_sqrt t -> write_unop "sqrt" NFloat t 
      | NInsc_funop_ceil t -> write_unop "ceil" NFloat t 
      | NInsc_funop_floor t -> write_unop "floor" NFloat t 
      | NInsc_funop_trunc t -> write_unop "trunc" NFloat t 
      | NInsc_funop_nearest t -> write_unop "nearest" NFloat t 
      | NInsc_extend (to_t, from_t, arg) -> insc_args printer [Literal ((string_of_iof NInt) ^ (string_of_length to_t) ^ ".extend_" ^ (string_of_iof NInt) ^ (string_of_memory_length from_t) ^ "_s")] [write_instruction printer arg]
      | NInsc_wrap arg -> write_name_arg "i32.wrap_i64" arg
      | NInsc_trunc t -> write_trunc "trunc" t 
      | NInsc_trunc_sat t -> write_trunc "trunc_sat" t 
      | NInsc_promote _ -> raise (Unreachable __LOC__)
      | NInsc_demote _ -> raise (Unreachable __LOC__)
      | NInsc_convert t -> write_convert t 
      | NInsc_reinterpret _ -> raise (Unreachable __LOC__)

    and write_control_insc printer insc =
      match insc with
      | CInsc_nop -> [Empty]
      | CInsc_if (type_use, if_value, inscs1, inscs2) -> (      
        let value_elements =
          match printer.ctrl_insc_style with
          | Folded_style ->
            let old_style = printer.argument_print in
            printer.argument_print <- Arg_style;
            let a = write_instruction printer if_value in
            printer.argument_print <- old_style;
            a
          | Plain_style -> write_instruction printer if_value
        in
        let in1 = List.map (write_instruction printer) inscs1 |> List.flatten in
        let in2 = List.map (write_instruction printer) inscs2 |> List.flatten in
        let tn = write_type_use printer type_use in
        match printer.ctrl_insc_style with
        | Folded_style -> 
          let nodes = [LineIndent; Literal "if"; IdSep] @ tn @ [IncIndent; Paren value_elements; DecIndent] @ [Paren ([Literal "then"; IncIndent] @ in1 @ [DecIndent])] @ [Paren ([Literal "else"; IncIndent] @ in2 @ [DecIndent])] in
          [Paren nodes]
        | Plain_style ->
          value_elements @ [LineIndent] @ (write_insc_name_with_arg "if" tn) @ [IncIndent] @ in1 @ [DecWithLineIndent; Literal "else"; IncIndent] @ in2 @ [DecWithLineIndent; Literal "end"]
      )
      | CInsc_call (func_index, args) -> (
        let call_insc = write_insc_name_with_index "call" func_index in
        let args = List.map (write_instruction printer) args in
        insc_args printer call_insc args
      )
      | CInsc_call_indirect (local_index, var_is_local, type_use, args) -> (
        let call_insc = write_insc_name_with_arg "call_indirect" (write_type_use printer type_use) in
        let get_var_insc = LineIndent::(if var_is_local then write_insc_name_with_index "local.get" local_index else write_insc_name_with_index "global.get" local_index) in
        insc_args printer call_insc ((List.map (write_instruction printer) args) @ [get_var_insc])
      )
      | CInsc_unreachable -> [LineIndent; Literal "unreachable"]
      | CInsc_nop -> [LineIndent; Literal "nop"]
      | _ -> assert false
    and write_type_use _printer type_use =
      let use_index index =
        Paren (write_insc_name_with_index "type" index)
      in
      let use_type func_type =
        let result_nodes = (List.map write_val_type func_type.r |> List.map (fun t -> [IdSep; t]) |> List.flatten) in
        let param_nodes = func_type.p |> List.map (fun t -> let (_, b) = t in b) |> List.map write_val_type |> List.map (fun t -> [IdSep; t]) |> List.flatten in
        let process name a = [Paren ([Literal name] @ a)] in
        match param_nodes, result_nodes with
        | [], [] -> []
        | p, [] -> process "param" p
        | [], r -> process "result" r
        | p, r -> (process "param" p) @ [IdSep] @ (process "result" r)
      in
      match type_use with
      | TU_index index -> [use_index index]
      | TU_def func_type -> use_type func_type
      | TU_index_def (index, def) -> [use_index index] @ [IdSep] @ use_type def
    and write_ref_insc printer insc =
      match insc with
      | RInsc_null h -> 
        let immediate_param = (
          match h with
          | Func_heap_ref -> "funcref"
          | Extern_heap_ref -> "externref"
        )
        in
        [LineIndent] @ write_insc_name_with_arg "ref.null" [Literal immediate_param]
      | RInsc_is_null arg ->
        insc_args printer [Literal "ref.is_null"] [write_instruction printer arg]
      | RInsc_func func_index -> write_insc_name_with_index "ref.func" func_index
    and write_var_insc printer insc =
      match insc with
      | VInsc_lget index -> insc_arg0 (write_insc_name_with_index "local.get" index) 
      | VInsc_lset (index, arg) -> insc_args printer (write_insc_name_with_index "local.set" index) [write_instruction printer arg]
      | VInsc_ltee (index, arg) -> insc_args printer (write_insc_name_with_index "local.tee" index) [write_instruction printer arg]
      | VInsc_gget index -> insc_arg0 (write_insc_name_with_index "global.get" index)
      | VInsc_gset (index, arg) -> insc_args printer (write_insc_name_with_index "global.set" index) [write_instruction printer arg]
    and write_param_insc printer insc =
      match insc with
      | PInsc_drop arg -> insc_args printer [Literal "drop"] [write_instruction printer arg]
      | PInsc_select _ -> raise (Unreachable __LOC__)
    and write_instructions_inline printer instructions =
      let i = List.map (write_instruction printer) instructions |> List.flatten in
      match i with
      | p::t -> (
        match p with
        | LineIndent -> t 
        | _ -> i
      ) 
      | _ -> i


    let compile_links_ir_to_wasm writer program =
      collect_program writer program;
      conv_program writer program;
      let parts = write_program writer in
      to_string writer.printer parts |> Printf.fprintf writer.writer "%s";
      close_out writer.writer

  end
end


open Wasm
let run (result: Backend.result) output_wat =
  let program = result.Backend.program in
  (* let output_stream = open_out output_wat in
  let writer = Wasm.Pretty_printing.new_wasm_writer (Wasm.Pretty_printing.default_printer ()) output_stream (Wasm.Grammar.new_module "default_module") in
  let _ = write_ir2Wasm writer program in *)
  (* let output_stream2 = open_out ((SysExt.get_file_name_without_extension output_wat) ^ "-reverse-style.wat") in
  let writer2 = Wasm.Pretty_printing.new_wasm_writer (Wasm.Pretty_printing.reverse_style_printer ()) output_stream2 (Wasm.Grammar.new_module "default_module") in
  let _ = write_ir2Wasm writer2 program in *)
  let output_stream2 = open_out ((SysExt.get_file_name_without_extension output_wat) ^ ".wat") in
  let writer2 = Wasm.Pretty_printing.new_wasm_writer (Wasm.Pretty_printing.default_printer ()) output_stream2 (Ir2WasmAst.new_module "$default_module") in
  Ir2WasmAst.compile_links_ir_to_wasm writer2 program
