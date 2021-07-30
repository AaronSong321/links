
open Var
open Ir
open Lib
open Types
open CommonTypes
open Utility

exception NotImplemented of string
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
  let concat_with elem container =
    let g = List.map (fun t -> [elem; t]) container |> List.flatten in
    match g with
    | [] -> []
    | _::v -> v
end
module SysExt = struct
  let get_filename_without_extension file =
    let dot_position = Str.search_backward (Str.regexp "\\.") file ((String.length file) - 1) in
    String.sub file 0 dot_position
  let get_extension file =
    let dot_position = Str.search_backward (Str.regexp "\\.") file ((String.length file) - 1) in
    String.sub file dot_position ((String.length file) - dot_position)
  let get_filename file =
    let file = 
      try 
        let dot_position = Str.search_backward (Str.regexp "/") file ((String.length file) - 1) in
        String.sub file (dot_position + 1) ((String.length file) - dot_position - 1)
      with
        Not_found -> file
    in file
end
module type FILE_EXT = sig
  val read_all_text: string -> string
  val read_all_lines: string -> string list
end
module FileExt: FILE_EXT = struct
  let read_all_lines filename =
    let lines = ref [] in
    let channel = open_in filename in
    try 
      while true; do
        lines := input_line channel::!lines
      done;
      !lines
    with End_of_file ->
      close_in channel;
      List.rev !lines

  let read_all_text filename =
    let text = ref "" in
    let channel = open_in filename in
    try 
      while true; do
        text := !text ^ (input_line channel) ^ "\n"
      done;
      !text
    with End_of_file ->
      close_in channel;
      !text
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
      | _ -> raise (NotImplemented __LOC__)
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
    (* and nii_ibinop_signed_type = number_type_length * number_type_sign * insc_semantical_param * insc_semantical_param *)
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
      | CInsc_return_call_indirect of local_index * bool * type_use * insc_semantical_param list
      | CInsc_return_call of func_index * insc_semantical_param list
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
      mutable body: expr;
      func_export_anno: export_annotation option
    }
    and export_annotation = string


    let name_of_func: func_def -> string = fun f -> non_null f.func_id

    type mem_def = {
      memory_id: string option;
      memory_type: memory_type;
      memory_export_anno: export_annotation option
    }
    type global_def = {
      global_id: string option;
      global_type: global_type;
      init_value: expr;
      global_export_anno: export_annotation option
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
    type table_def = table_type * export_annotation option
    type elem_def = insc_semantical_param * func_index list
    type import_def = 
      | ImportFunc of import_js_name_t * import_wasm_name_t * type_use
      (* | ImportTable of import_js_name_t * import_wasm_name_t * table_type
      | ImportMemory of import_js_name_t * import_wasm_name_t * memory_type *)
      | ImportGlobal of import_js_name_t * import_wasm_name_t * global_type
    and import_wasm_name_t = string option
    and import_js_name_t = string * string
     
    type function_pointer_map = int IntMap.t
    type module_def = {
      module_id: string;
      mutable ty: type_def list;
      mutable im: import_def list;
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
    let new_func_def id func_type export = {
      func_id = id;
      func_wasm_type = func_type;
      locals = [];
      body = [];
      func_export_anno = export
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
      let remove_dollar name = 
        if String.length name >= 1 && String.get name 0 = '$' then String.sub name 1 ((String.length name) - 1)
        else name
      in
      let global_def = {
        global_id = id;
        global_type = global_value_type;
        init_value = [init_value_insc];
        global_export_anno = (match id with | Some i -> Some (remove_dollar i) | None -> None)
      } in
      m.gl <- m.gl @ [global_def]
    let declare_fun: module_def -> string option -> param_annotator -> result_annotator -> export_annotation option -> func_def = fun module_ id p r export_anno ->
      let func_def = new_func_def id ({ p = p; r = r }) export_anno in
      module_.fn <- module_.fn @ [func_def];
      func_def

    let links_main_func_name = "$$links_wasm_file_func"
  end
  
  module BinderMap = Utility.IntMap

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
      indent_ws_num: int;
      plain_insc_style: instruction_print_style;
      ctrl_insc_style: instruction_print_style;
      mutable argument_print: argument_print_style;
      abbreviate_single_module_def: bool
    }
    let default_printer () = { 
      indent = 0; 
      indent_ws_num = 2; 
      plain_insc_style = Plain_style;
      ctrl_insc_style = Plain_style;
      argument_print = Stack_style;
      abbreviate_single_module_def = false
    }
    let reverse_style_printer () = {
      indent = 0;
      indent_ws_num = 2;
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

    let ir_primitive_type_to_wasm: CommonTypes.Primitive.t -> val_type = fun p ->
      let open CommonTypes.Primitive in
      match p with
      | Bool   
      | Int    
      | Char    -> links_int_value_type
      | Float   -> links_float_value_type
      | XmlItem 
      | DB      
      | String  -> raise (NotImplemented __LOC__)
    let to_val_type t =
      match t with
      | Val_type v -> v 
      | Heap_type _ -> raise (NotImplemented __LOC__)
      | Func_type _ -> wasm_func_pointer_type
      | Memory_type _ -> raise (NotImplemented __LOC__)
      | Table_type _ -> raise (NotImplemented __LOC__)
      | Global_type _ -> raise (NotImplemented __LOC__)
      | Error_type -> raise (NotImplemented __LOC__)
      | Unit_type -> raise (NotImplemented __LOC__)

    module PPInstruction = struct
      let prepend_dollar p = "$" ^ p
      let get_export_name binder =
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
        in name
      let get_binder_name: Ir.binder -> string = fun binder ->
        prepend_dollar (get_export_name binder)
      let get_function_export_name binder =
        let name = name_of_binder binder in
        let h = string_of_int (var_of_binder binder) in
        if name = "" then "_fun_" ^ h
        else name ^ "_" ^ h
      let get_function_binder_name (binder: Ir.binder): string =
        binder |> get_function_export_name |> prepend_dollar
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

      let stringify name =
        Printf.sprintf "\"%s\"" name
    end

    let give_indent_string pt = 
      String.make (pt.indent * pt.indent_ws_num) ' '
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
      let show _ = raise (NotImplemented __LOC__)
      let pp _ _ = raise (NotImplemented __LOC__)
    end
    module type BINDERSET = Set with type elt = binder
    module BinderSet : BINDERSET = Set.Make(OrderedBinder)

    type wasm_optimiser = {
      opt_tail_call: bool
    }
    let default_optimiser () = {
      opt_tail_call = true
    }
    
    type wasm_writer = {
      wasm_out_filename: string;
      optimiser: wasm_optimiser;
      printer: printer;
      writer: out_channel;
      wasm_module: module_def;
      use_cps: bool;
      mutable func_map: (func_def * Ir.fun_def) list;
      mutable func_map2: (binder, func_def) Hashtbl.t;
      mutable var_map: Ir.binder BinderMap.t;
      mutable func_var: BinderSet.t;
      mutable unit_var: BinderSet.t;
      mutable import_jslib_func: IntSet.t;
      mutable tail_call_set: (value, int) Hashtbl.t;
      primitive_functions: Var.var Env.String.t
    }
    let new_wasm_writer wasm_out_filename use_cps printer out_channel module_def = {
      wasm_out_filename = wasm_out_filename;
      optimiser = default_optimiser ();
      printer = printer;
      writer = out_channel;
      wasm_module = module_def;
      use_cps = use_cps;
      func_map = [];
      func_map2 = Hashtbl.create 2000;
      var_map = BinderMap.empty;
      func_var = BinderSet.empty;
      unit_var = BinderSet.empty;
      import_jslib_func = IntSet.empty;
      tail_call_set = Hashtbl.create 1;
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
    let add_unit_var writer binder =
      writer.unit_var <- BinderSet.add binder writer.unit_var
      
    let can_use_tail_call writer value =
      match Hashtbl.find_opt writer.tail_call_set value with
      | Some _ -> true
      | _ -> false
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
          | Var _ -> raise (NotImplemented __LOC__)
          | Closed -> raise (NotImplemented __LOC__)
          | Recursive _ -> raise (NotImplemented __LOC__)
          | t -> ir_type2Wasm t
        ) in
      let ir_record_type2Wasm row = ir_type2Wasm row in
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
        | _ -> raise (NotImplemented __LOC__)
      in
      let rec get_result (f: typ) =
        match f with
        | Function (_, _, out_type) -> get_result out_type
        | Primitive p -> Some (ir_primitive_type_to_wasm p)
        | Lolli _ -> raise (NotImplemented __LOC__)
        | Record _ -> None
        | Variant _ -> raise (NotImplemented __LOC__)
        | Table _ -> raise (NotImplemented __LOC__)
        | Lens _ -> raise (NotImplemented __LOC__)
        | ForAll (_quantifiers, underlying_type) -> get_result underlying_type
        | _ -> raise (NotImplemented __LOC__)
      in
      let get_ir_func_params in_type =
        extract_from_in_type in_type
      in

      if string_of_datatype ir_type = "()" then Unit_type
      else
        match ir_type with
        | Primitive p -> Val_type (ir_primitive_type_to_wasm p)
        | Function (in_type, _, _) ->
            let result_type = get_result ir_type in
            let result_type = match result_type with
              | None -> []
              | Some a -> [a]
            in
            Func_type {
              p = get_ir_func_params in_type; r = result_type
            }
        | Effect _ -> raise (NotImplemented __LOC__)
        | Var _ -> raise (NotImplemented __LOC__)
        | Recursive _ -> raise (NotImplemented __LOC__)
        | Not_typed -> raise (NotImplemented __LOC__)
        | Alias _ -> raise (NotImplemented __LOC__)
        | Application _ -> raise (NotImplemented __LOC__)
        | RecursiveApplication _ -> raise (NotImplemented __LOC__)
        | Meta point -> ir_meta_type2Wasm point
        | Lolli _ -> raise (NotImplemented __LOC__)
        | Record row -> ir_record_type2Wasm row
        | Variant _ -> raise (NotImplemented __LOC__)
        | Table _ -> raise (NotImplemented __LOC__)
        | Lens _ -> raise (NotImplemented __LOC__)
        | ForAll (_quantifiers, underlying_type) -> ir_type2Wasm underlying_type
        | Row _ -> raise (NotImplemented (string_of_datatype ir_type))
        | Closed -> raise (NotImplemented __LOC__)
      (* Presence *)
        | Absent -> raise (NotImplemented __LOC__)
        | Present a -> raise (NotImplemented (string_of_datatype a))
      (* Session *)
        | Input _ -> raise (NotImplemented __LOC__)
        | Output _ -> raise (NotImplemented __LOC__)
        | Select _ -> raise (NotImplemented __LOC__)
        | Choice _ -> raise (NotImplemented __LOC__)
        | Dual _ -> raise (NotImplemented __LOC__)
        | End -> raise (NotImplemented __LOC__)
    let rec get_result_type = function
      | Function (_, _, result_type) -> ir_type2Wasm result_type 
      | Primitive _ -> raise (NotImplemented __LOC__)
      | Effect _ -> raise (NotImplemented __LOC__)
      | Var _ -> raise (NotImplemented __LOC__)
      | Recursive _ -> raise (NotImplemented __LOC__)
      | Not_typed -> raise (NotImplemented __LOC__)
      | Alias _ -> raise (NotImplemented __LOC__)
      | Application _ -> raise (NotImplemented __LOC__)
      | RecursiveApplication _ -> raise (NotImplemented __LOC__)
      | Meta _ -> raise (NotImplemented __LOC__)
      | Lolli _ -> raise (NotImplemented __LOC__)
      | Record _ -> raise (NotImplemented __LOC__)
      | Variant _ -> raise (NotImplemented __LOC__)
      | Table _ -> raise (NotImplemented __LOC__)
      | Lens _ -> raise (NotImplemented __LOC__)
      | ForAll (_quantifiers, underlying_type) -> get_result_type underlying_type
      | Row _ -> raise (NotImplemented __LOC__)
      | Closed -> raise (NotImplemented __LOC__)
      (* Presence *)
      | Absent -> raise (NotImplemented __LOC__)
      | Present _ -> raise (NotImplemented __LOC__)
      (* Session *)
      | Input _ -> raise (NotImplemented __LOC__)
      | Output _ -> raise (NotImplemented __LOC__)
      | Select _ -> raise (NotImplemented __LOC__)
      | Choice _ -> raise (NotImplemented __LOC__)
      | Dual _ -> raise (NotImplemented __LOC__)
      | End -> raise (NotImplemented __LOC__)
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
      let export_anno = if Scope.is_global (scope_of_binder ir_func.fn_binder) then Some (PPInstruction.get_export_name function_binder) else None in
      let new_fun = declare_fun wasm_module (Some (PPInstruction.get_function_binder_name function_binder)) params result export_anno in
      new_fun
      
    let import_func writer binder module_name =
      let module_def = writer.wasm_module in
      let import_name = name_of_binder binder in
      let func_type = match binder |> type_of_binder |> ir_type2Wasm with
        | Func_type f -> TU_def f 
        | _ -> assert false
      in
      module_def.im <- module_def.im @ [ImportFunc ((module_name, import_name), Some (PPInstruction.((get_function_binder_name binder))), func_type)]
    let import_global writer binder module_name =
      let module_def = writer.wasm_module in
      let import_name = name_of_binder binder in
      module_def.im <- module_def.im @ [ImportGlobal ((module_name, import_name), Some (PPInstruction.((get_binder_name binder))), {
        global_var_type = binder |> type_of_binder |> ir_type2Wasm |> to_val_type;
        is_mutable = false
      })]

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
        else raise (NotImplemented __LOC__)
      | ApplyPure (applied_value, args) -> (
        match applied_value with
        | TApp (applied_value1, _) -> type_of_apply writer applied_value1 args
        | _ -> raise (NotImplemented __LOC__)
      )
      | _ -> raise (NotImplemented __LOC__)
    and type_of_value writer value =
      let type_of_primitive_value var = find_ir_var_type var |> ir_type2Wasm in
      let open Constant in
      match value with
      | Constant t -> (match t with
        | Bool _
        | Int _
        | Char _ -> links_int_type
        | String _ -> raise (NotImplemented __LOC__)
        | Float _ -> links_float_type
      )
      | Variable v -> 
        if is_primitive_var v then type_of_primitive_value v
        else find_binder writer v |> (fun t -> match t with|Some s->s|None->raise (NotImplemented (__LOC__ ^ " ... " ^ (string_of_int v)))) |> type_of_binder |> ir_type2Wasm
      | Extend _ -> if ir_value_is_unit value then links_unit_type else raise (NotImplemented __LOC__)
      | Project _ -> raise (NotImplemented __LOC__)
      | Erase _ -> raise (NotImplemented __LOC__)
      | Inject _ -> raise (NotImplemented __LOC__)
      | TAbs _ -> raise (NotImplemented __LOC__)
      | TApp _ -> raise (NotImplemented __LOC__)
      | XmlNode _ -> raise (NotImplemented __LOC__)
      | ApplyPure (applied_value, args) -> (
          match applied_value with
          | TApp (applied_value1, _) -> type_of_apply writer applied_value1 args
          | _ -> raise (NotImplemented __LOC__)
        )
      | Closure _ -> raise (NotImplemented __LOC__)
      | Coerce _ -> raise (NotImplemented __LOC__)

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
  end

  module Ir2WasmAst = struct
    open Grammar
    open Pretty_printing
    open PPInstruction

    let new_module id = {
      module_id = id; 
      ty = [];
      im = [];
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
        };
        memory_export_anno = Some "wasm_memory"
      }];
      gl = [];
      ex = [];
      st = None;
      module_tb = [({
        table_id = Some "$$table";
        table_type_limit = {
          min = 1;
          max = Some 1
        };
        table_type_type = Func_ref
      }, Some "wasm_table")];
      module_el = [];
    }

    module OptimiseTailCall = struct
      let rec check_computation tail_call_set func_v computation =
        let (bindings, tail) = computation in
        List.iter (check_binding tail_call_set) bindings;
        check_tail_computation tail_call_set func_v tail
      and check_binding tail_call_set binding =
        match binding with
        | Fun f -> check_func tail_call_set f 
        | Rec fs -> List.iter (check_func tail_call_set) fs
        | _ -> ()
      and check_func tail_call_set func =
        let (bindings, computation) = func.fn_body in 
        List.iter (check_binding tail_call_set) bindings;
        check_tail_computation tail_call_set (var_of_binder func.fn_binder) computation
      and check_tail_computation tail_call_set func_v tail =
        match tail with
        | Return _ -> ()
        | Apply (value, _) -> check_value tail_call_set func_v value
        | Special _ -> ()
        | Case (_, values, default_case) -> (
          StringMap.iter (fun _ case_pair ->
            let (_, tail) = case_pair in
            check_computation tail_call_set func_v tail
          ) values;
          match default_case with
          | Some case -> let (_, computation) = case in check_computation tail_call_set func_v computation
          | None -> ()
        )
        | If (_, if_tail, else_tail) ->
          check_computation tail_call_set func_v if_tail;
          check_computation tail_call_set func_v else_tail
      and check_value tail_call_set func_v value =
        match value with
        | TApp (applied_value, _) -> (
          match applied_value with
          | Variable _ -> Hashtbl.add tail_call_set value 1
          | _ -> ()
        )
        | ApplyPure (applied_value, _) -> check_value tail_call_set func_v applied_value
        | _ -> ()

      let opt_tail_call program =
        let tail_calls = Hashtbl.create 1000 in
        let (high_level_bindings, _) = program in
        List.iter (check_binding tail_calls) high_level_bindings;
        tail_calls
    end

    let rec collect_program writer program =
      let (bindings, tail_computation) = program in
      let main_func = declare_fun writer.wasm_module (Some links_main_func_name) [] [] (Some links_main_func_name) in
      collect_tail_computation writer main_func tail_computation;
      bindings |> List.iter (collect_binding writer main_func);
      writer.wasm_module.st <- Some (Mni_name (name_of_func main_func))
    and collect_binding writer func binding =
      let collect_alien_var writer binder module_name = 
        add_binder writer binder;
        let local_name = get_binder_name binder in
        add_global writer.wasm_module (Some local_name) (ir_type2Wasm (type_of_binder binder) |> to_val_type);
        import_global writer binder module_name
      in
      let collect_alien_fun writer binder module_name =
        add_binder writer binder;
        import_func writer binder module_name
      in
      match binding with
      | Let (binder, (type_var_list, tail_computation)) -> collect_let_binding writer func binder type_var_list tail_computation
      | Fun fun_def -> 
        collect_ir_fun writer fun_def
      | Rec fun_defs ->
        List.iter (collect_ir_fun writer) fun_defs
      | Alien alien_info -> (
        let binder = alien_info.binder in
        let type_of_alien = type_of_binder binder |> ir_type2Wasm in
        match type_of_alien with
        | Func_type _ -> collect_alien_fun writer binder alien_info.object_name
        | Val_type _ -> collect_alien_var writer binder alien_info.object_name
        | _ -> assert false
      )
      | Module _ -> raise (NotImplemented __LOC__)
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
      let collect_unit_let_binding () =
        if scope_of_binder binder |> Scope.is_local then add_local func (Some local_name) (Ref_type (Func_ref))
        else add_global writer.wasm_module (Some local_name) (Ref_type (Func_ref));
        add_unit_var writer binder
      in
      add_binder writer binder;
      let local_type = ir_type2Wasm (type_of_binder binder) in
      match local_type with
      | Val_type v -> collect_value_let_binding v 
      | Func_type f -> collect_fp_let_binding f
      | Memory_type _ -> raise (NotImplemented __LOC__)
      | Table_type _ -> raise (NotImplemented __LOC__)
      | Global_type _ -> raise (NotImplemented __LOC__)
      | Error_type -> raise (NotImplemented __LOC__)
      | Heap_type _ -> raise (NotImplemented __LOC__)
      | Unit_type -> collect_unit_let_binding ()
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
      | Case _ -> raise (NotImplemented __LOC__)
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
          | _ -> raise (NotImplemented __LOC__)
        )
      | Special _ -> links_int_type
      | Case _ -> links_int_type
      | If (_, if_tail, else_tail) -> 
        type_of_comp_group writer [if_tail; else_tail]
    and type_of_comp_group writer (comps: computation list) =
      let types = List.map (fun t -> let (_, tail) = t in type_of_tail_computation writer tail) comps in
      let hd = List.hd types in
      if List.for_all (fun t -> t = hd) types then hd
      else raise (NotImplemented __LOC__)
    and conv_tail_computation writer func tail_computation =
      match tail_computation with
      | Return v -> 
        if ir_value_is_unit v then Control_insc CInsc_nop
        else conv_value writer func v
      | Apply (f, args) -> conv_apply writer func f args
      | Special _ -> raise (NotImplemented __LOC__)
      | Case _ -> raise (NotImplemented __LOC__)
      | If (value, if_comp, else_comp) -> conv_if writer func value if_comp else_comp
    and conv_if writer func value if_comp else_comp =
      let value1 = conv_value writer func value in
      let if1 = conv_computation writer func if_comp in
      let else1 = conv_computation writer func else_comp in
      let if_type = type_of_comp_group writer [if_comp; else_comp] in
      let if_type = match if_type with
        | Unit_type -> []
        | _ -> [to_val_type if_type]
      in
      let insc = CInsc_if (TU_def { p = []; r = if_type }, value1, if1, else1) in
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
      match BinderSet.find_opt binder writer.unit_var with
      | Some _ -> comp_insc
      | None -> conv_write_var binder comp_insc
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
      | Alien _ -> None
      | Module _ -> raise (NotImplemented __LOC__)
    and conv_func writer func ir_func =
      func.body <- conv_computation writer func ir_func.fn_body
    and conv_read_var binder =
      let var_index = Mni_name (get_binder_name binder) in
      let open Var.Scope in
      let insc = match scope_of_binder binder with
        | Local -> VInsc_lget var_index
        | Global -> VInsc_gget var_index
      in Var_insc insc
    and conv_write_var binder comp_insc =
      let var_index = Mni_name (get_binder_name binder) in
      let open Var.Scope in
      let insc = match scope_of_binder binder with
        | Local -> VInsc_lset (var_index, comp_insc)
        | Global -> VInsc_gset (var_index, comp_insc)
      in Var_insc insc
    and conv_value writer func ir_value =
      let conv_const (const_value: Constant.t) =
        let open CommonTypes.Constant in
        let insc = match const_value with
          | Bool value -> NInsc_consti (length_of_val_type links_int_value_type, if value then 1 else 0)
          | Char value -> NInsc_consti (length_of_val_type links_int_value_type, int_of_char value)
          | Int value -> NInsc_consti (length_of_val_type links_int_value_type, value)
          | Float value -> NInsc_constf (length_of_val_type links_float_value_type, value)
          | _ -> raise (NotImplemented __LOC__)
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
        conv_read_var (find_binder writer v |> (fun t -> match t with | Some t -> t | None -> raise (NotImplemented (string_of_int v))))
      | ApplyPure (f, args) -> conv_apply_pure writer func f args
      | Project  _ -> raise (NotImplemented __LOC__)
      | Extend _ -> raise (NotImplemented __LOC__)
      | Erase  _ -> raise (NotImplemented __LOC__)
      | Inject  _ -> raise (NotImplemented __LOC__)
      | TAbs  _ -> raise (NotImplemented __LOC__)
      (* TApp is probably function value, treat it as is *)
      | TApp (var, _tyargs) -> (
          match var with
          | Variable v -> conv_func_value v
          | _ -> raise (NotImplemented __LOC__)
        )
      | XmlNode _ -> raise (NotImplemented __LOC__)
      | Closure _ -> raise (NotImplemented __LOC__)
      | Coerce _ -> raise (NotImplemented __LOC__)
    and conv_apply_pure writer func value args =
      match value with
      | TApp (applied_value, _) -> (
          let call_insc = match applied_value with
            | Variable v -> conv_ir_func_call writer func v args
            | _ -> raise (NotImplemented __LOC__)
          in
          if can_use_tail_call writer value then (
            match call_insc with
            | Control_insc c -> (
                match c with
                | CInsc_call (a, b) -> Control_insc (CInsc_return_call (a, b))
                | CInsc_call_indirect (a, b, c, d) -> Control_insc (CInsc_return_call_indirect (a, b, c, d))
                | _ -> call_insc
              )
            | _ -> call_insc
          )
          else call_insc
        )
      | _ -> raise (NotImplemented __LOC__)
    and conv_apply writer func value args = conv_apply_pure writer func value args 
    and conv_primitive_ir_func_call writer func v operands =
      let operator_types = operands |> List.map (fun t -> type_of_value writer t |> to_val_type) in
      let import_jslib_func v =
        let module_def = writer.wasm_module in
        let import_name = primitive_name v in
        let import_name = if writer.use_cps then import_name else "_" ^ import_name in
        let module_name = "jslib" in
        let func_type = match v |> find_ir_var_type |> ir_type2Wasm with
          | Func_type f -> TU_def f 
          | _ -> assert false
        in
        module_def.im <- module_def.im @ [ImportFunc ((module_name, import_name), Some (import_name ^ "_" ^ (string_of_int v)), func_type)];
        writer.import_jslib_func <- IntSet.add v writer.import_jslib_func
      in
      let use_jslib_func v =
        let import_name = primitive_name v in
        let import_name = if writer.use_cps then import_name else "_" ^ import_name in
        Control_insc (CInsc_call (Mni_name import_name, (List.map (conv_value writer func) operands)))
      in
      let op1 = match List.nth_opt operands 0 with
        | Some t -> Some (conv_value writer func t)
        | None -> None
      in
      let op1_type = match List.nth_opt operator_types 0 with
        | Some t -> Some (length_of_val_type t) 
        | None -> None
      in
      let op1_length = match List.nth_opt operator_types 0 with
        | Some t -> Some (int_or_float_of_val_type t)
        | None -> None
      in
      let op2 = match List.nth_opt operands 1 with
        | Some t -> Some (conv_value writer func t)
        | None -> None
      in
          
      let insc = match v |> primitive_name with
        | "+" -> Numeric_insc (NInsc_ibinop_add (non_null op1_type, non_null op1, non_null op2))
        | "-" -> Numeric_insc (NInsc_ibinop_sub (non_null op1_type, non_null op1, non_null op2))
        | "*" -> Numeric_insc (NInsc_ibinop_mul (non_null op1_type, non_null op1, non_null op2))
        | "/" -> Numeric_insc (NInsc_ibinop_div (non_null op1_type, non_null op1, non_null op2))
        | "mod" -> Numeric_insc (NInsc_ibinop_rem (non_null op1_type, non_null op1, non_null op2))
        | "+." -> Numeric_insc (NInsc_fbinop_add (non_null op1_type, non_null op1, non_null op2))
        | "-." -> Numeric_insc (NInsc_fbinop_sub (non_null op1_type, non_null op1, non_null op2))
        | "*." -> Numeric_insc (NInsc_fbinop_mul (non_null op1_type, non_null op1, non_null op2))
        | "/." -> Numeric_insc (NInsc_fbinop_div (non_null op1_type, non_null op1, non_null op2))
        | "negatef" -> Numeric_insc (NInsc_funop_neg (non_null op1_type, non_null op1))
        | "==" -> Numeric_insc (NInsc_relop_eq (non_null op1_length, non_null op1_type, non_null op1, non_null op2))
        | "!=" -> Numeric_insc (NInsc_relop_ne (non_null op1_length, non_null op1_type, non_null op1, non_null op2))
        | ">=" -> Numeric_insc (NInsc_relop_ge (non_null op1_length, non_null op1_type, non_null op1, non_null op2))
        | "<=" -> Numeric_insc (NInsc_relop_le (non_null op1_length, non_null op1_type, non_null op1, non_null op2))
        | ">" -> Numeric_insc (NInsc_relop_gt (non_null op1_length, non_null op1_type, non_null op1, non_null op2))
        | "<" -> Numeric_insc (NInsc_relop_lt (non_null op1_length, non_null op1_type, non_null op1, non_null op2))
        | "floor" -> Numeric_insc (NInsc_funop_floor (non_null op1_type, non_null op1))
        | "ceiling" -> Numeric_insc (NInsc_funop_ceil (non_null op1_type, non_null op1))
        | "sqrt" -> Numeric_insc (NInsc_funop_sqrt (non_null op1_type, non_null op1))
        | "int_to_float" -> Numeric_insc (NInsc_convert (non_null op1_type, links_float_value_type |> length_of_val_type, Signed, non_null op1))
        | "float_to_int" -> Numeric_insc (NInsc_trunc_sat (non_null op1_type, links_int_value_type |> length_of_val_type, Signed, non_null op1))
        | "abs" -> Numeric_insc (NInsc_funop_abs (non_null op1_type, non_null op1))
        | "maximum" -> Numeric_insc (NInsc_fbinop_max (non_null op1_type, non_null op1, non_null op2))
        | _ -> if IntSet.mem v writer.import_jslib_func then () else import_jslib_func v;
          use_jslib_func v
      in
      insc
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
        | _ -> raise (Unreachable __LOC__)
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
      let im = List.map (write_import writer) mo.im |> flatten_defs in
      let fn = List.map (write_func writer) mo.fn |> flatten_defs in
      let me = List.map write_memory mo.me |> flatten_defs in
      let gl = List.map (write_global printer) mo.gl |> flatten_defs in
      let ex = List.map write_export mo.ex |> flatten_defs in
      let st = write_start_point mo.st in
      let tb = List.map (write_table writer) mo.module_tb |> flatten_defs in
      let el = write_element writer in
      let parts = match st with
        | Empty -> [im; fn; me; gl; ex; tb; [el]]
        | _ -> [im; fn; me; gl; ex; [st]; tb; [el]]
      in
      let parts = [Literal "module"; IdSep; id; IncWithLineIndent] @ (flatten_parts parts) @ [DecWithLineIndent] in
      Paren parts
    and write_memory memory =
      let limit = write_limit memory.memory_type.memory_type_limit in
      let p = [Literal "memory"] @ (match memory.memory_id with | Some s -> [IdSep; Literal s] | None -> []) @ (write_export_annotation memory.memory_export_anno) @ [IdSep] @ limit in
      Paren p 
    and write_limit limit =
      match limit.max with
      | Some max -> [Literal (string_of_int limit.min); IdSep; Literal (string_of_int max)]
      | None -> [Literal (string_of_int limit.min)]
    and write_global printer global =
      let global_type = global.global_type in
      let t = [Literal "global"] in
      let t = match global.global_id with
        | Some id -> t @ [IdSep; Literal id]
        | None -> t
      in
      let t = t @ (write_export_annotation (global.global_export_anno)) in
      let type_node = write_global_type global_type in
      let init_node = write_instructions_inline printer global.init_value in
      Paren (t @ [IdSep; type_node] @ [IdSep; Paren init_node])
    and write_global_type global_type =
      let type_node = Literal (to_string_val_type global_type.global_var_type) in
      let type_node = if global_type.is_mutable then Paren [Literal "mut"; IdSep; type_node] else type_node in
      type_node
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
      let (table_type, export_anno) = tb in
      table_type.table_type_limit.min <- func_num;
      (match table_type.table_type_limit.max with
       | Some _ -> table_type.table_type_limit.max <- Some func_num
       | None -> ());
      Paren ([Literal "table"] @ (write_export_annotation export_anno) @ [IdSep] @ (write_limit table_type.table_type_limit) @ [IdSep; Literal "funcref"])
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
      let nodes = [Literal "func"] @ name_nodes @ (write_export_annotation func.func_export_anno) @ func_type_nodes @ [IncWithLineIndent] @ local_nodes @ insc_nodes @ [DecWithLineIndent] in
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
        | _ -> raise (Unreachable __LOC__)
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
      let write_call insc_name func_index args =
        let call_insc = write_insc_name_with_index insc_name func_index in
        let args = List.map (write_instruction printer) args in
        insc_args printer call_insc args
      in
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
      | CInsc_call (func_index, args) -> write_call "call" func_index args
      | CInsc_call_indirect (local_index, var_is_local, type_use, args) -> write_call_indirect printer local_index var_is_local type_use args "call_indirect"
      | CInsc_return_call_indirect (local_index, var_is_local, type_use, args) -> write_call_indirect printer local_index var_is_local type_use args "return_call_indirect"
      | CInsc_unreachable -> [LineIndent; Literal "unreachable"]
      | CInsc_return_call (func_index, args) -> write_call "return_call" func_index args
      | _ -> assert false
    and write_call_indirect printer local_index var_is_local type_use args insc_name =
      let call_insc = write_insc_name_with_arg insc_name (write_type_use printer type_use) in
      let get_var_insc = LineIndent::(if var_is_local then write_insc_name_with_index "local.get" local_index else write_insc_name_with_index "global.get" local_index) in
      insc_args printer call_insc ((List.map (write_instruction printer) args) @ [get_var_insc])
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
    and write_export_annotation export_anno =
      match export_anno with
      | Some name -> [IdSep; Paren [Literal "export"; IdSep; Literal (stringify name)]]
      | None -> []
    and write_import writer import =
      match import with
      | ImportGlobal ((import_module_name, import_name), wasm_name, global_type) ->
        Paren ([Literal "import"; IdSep; Literal (stringify import_module_name); IdSep; Literal (stringify import_name); IdSep] @ [Paren ([Literal "global"] @ (try_write_name wasm_name) @ [IdSep] @ [write_global_type global_type])])
      | ImportFunc ((import_module_name, import_name), wasm_name, type_use) ->
        Paren ([Literal "import"; IdSep; Literal (stringify import_module_name); IdSep; Literal (stringify import_name); IdSep] @ [Paren ([Literal "func"] @ (try_write_name wasm_name) @ [IdSep] @ (write_type_use writer.printer type_use))])

    let write_js_file writer context =
      let index_js_file_contents =
        let p1 = {efgh|"use strict"
function ToArrayBuffer(buffer) {
    var a = new ArrayBuffer(buffer.length)
    var v = new Uint8Array(a)
    for (var i=0; i<buffer.length; ++i) {
        v[i] = buffer[i]
    }
    return a
}
const fs = require("fs")
async function InstantiateWasmFile(filename, importObject) {
    var data = fs.readFileSync(filename)
    var _wasmArrayBuffer = ToArrayBuffer(data)
    const {_, instance} = await WebAssembly.instantiate(_wasmArrayBuffer, importObject)
    return instance
}

InstantiateWasmFile(|efgh}
        in
        let p2 = {efgh|, {imports: {
|efgh} in
        let p3 =
          if Str.string_match (Str.regexp ".*-client-wasm") writer.wasm_out_filename 0 then {efgh|
} }).then(module => {
  const main = module.exports["$$links_wasm_file_func"]
  const timeBeforeRun = performance.now()
  main()
  const timeAfterRun = performance.now()
  console.log("client-wasm: " + (timeAfterRun - timeBeforeRun))
})
|efgh} 
          else {efgh|
} }).then(module => {
  const main = module.exports["$$links_wasm_file_func"]
  main()
})
|efgh} 
        in
        let gather_import_js_names =
          (* let jslib = (SysExt.get_filename_without_extension writer.wasm_out_filename |> SysExt.get_filename) ^ "-jslib" in *)
          let import_files = (Context.ffi_files context) in
          let import_contents = import_files |> List.map (fun filename -> Printf.sprintf "    %s : %s" (stringify filename) ("require(" ^ (stringify ("./" ^ filename)) ^ ")")) |> String.concat ",\n" in
          p1 ^ (stringify ((writer.wasm_out_filename |> SysExt.get_filename |> SysExt.get_filename_without_extension) ^ ".wasm")) ^ p2 ^ import_contents ^ p3
        in
        gather_import_js_names
      in
      let output_js_filename = (SysExt.get_filename_without_extension writer.wasm_out_filename) ^ "--index.js" in
      let output_js_stream = open_out output_js_filename in
      Printf.fprintf output_js_stream "%s" index_js_file_contents;
      close_out output_js_stream
    let copy_js_lib writer =
      let target_filename = writer.wasm_out_filename |> SysExt.get_filename_without_extension in
      let target_filename = target_filename ^ "-jslib.js" in
      let source_jslib = Sys.getcwd () ^ "/lib/js/jslib.js" in
      let contents = source_jslib |> FileExt.read_all_text in
      let output_stream = open_out target_filename in
      Printf.fprintf output_stream "%s" contents;
      close_out output_stream

    let compile_links_ir_to_wasm writer ir_result =
      let program = ir_result.Backend.program in
      if writer.optimiser.opt_tail_call then writer.tail_call_set <- OptimiseTailCall.opt_tail_call program else ();
      collect_program writer program;
      conv_program writer program;
      let parts = write_program writer in
      to_string writer.printer parts |> Printf.fprintf writer.writer "%s";
      close_out writer.writer;
      write_js_file writer ir_result.Backend.context;
      copy_js_lib writer;

  end
end

module NotWasm = struct 
  module Grammar = struct
    type notwasm_type = 
      | Any
      | ValueType of notwasm_value_type
      | ReferenceType of notwasm_pointer_type
      | InternalUnit
    and notwasm_param_list_type = notwasm_type list
    and return_type = 
      | ReturnT of notwasm_type
      | Void
    and notwasm_value_type =
      | I32
      | F64
      | Bool
    and notwasm_pointer_type =
      | Str
      | Array of notwasm_type
      | DynObject
      | HT
      | Function of notwasm_param_list_type * return_type
      | Closure of notwasm_param_list_type * return_type
      | Ref of notwasm_type
      | Env
      | InternalRecord of notwasm_type StringMap.t
      | InternalVariant of string * notwasm_type StringMap.t
    
    type bop =
      | IntAdd
      | IntSub 
      | IntMul
      | IntGT 
      | IntLT 
      | IntGE 
      | IntLE 
      | IntEqual
      | IntNeq
      | PointerEqual
      | FloatAdd
      | FloatSub
      | FloatMul
      | FloatDiv
      (* | FloatLE *)
      | FloatGE
    type boolean_literal = bool
    type int_literal = int
    type float_literal = float
    type string_literal = string
    type atom =
      | IntLiteral of int_literal
      | FloatLiteral of float_literal
      | StringLiteral of string_literal
      | NullLiteral
      | BoundIdentifier of identifier
      | ApplyPrimitive of identifier * atom list
      | PointerDereference of notwasm_pointer_type
      | ReadField of atom * string
      | BinaryExpression of atom * bop * atom
      | AnyLiteral of atom
      | BoolLiteral of boolean_literal
      | Cast of atom * notwasm_type
      | EnvGet of int_literal * notwasm_type
    (* and primitive_function = identifier *)
    and identifier = string

    type expression =
      | SingleAtom of atom
      | ApplyPrimitiveMemory of identifier * atom list
      | FunctionApplication of identifier * identifier list
      | ClosureApplication of identifier * identifier list
      | DynObjectLiteral
      | RefLiteral of atom * notwasm_type
      | ArrayPush of atom * atom
      | ClosureLiteral of identifier * (identifier * notwasm_type) list

    type statement =
      | VarDeclare of var_declare
      | If of atom * block * block
      | Loop of block
      | Return of atom
      | Break 
      | UpdateLocalVariable of identifier * expression
      | HTSet of atom
      | WriteField of atom * identifier * atom
      | SingleExpression of expression
    and block = statement list
    and var_declare = identifier * notwasm_type * expression
    type notwasm_func = {
      func_name: identifier;
      func_params: (identifier * notwasm_type) list;
      func_ret: return_type;
      func_is_closure: bool;
      mutable func_body: block
    }

    type program = {
      mutable notwasm_globals: var_declare list;
      mutable notwasm_functions: notwasm_func list;
      mutable main_func: notwasm_func;
    }
    type writer = {
      w_program: program;
      w_result: Backend.result;
      w_out_channel: out_channel;
      w_wat_output: string;
      w_func_map: (var, notwasm_func) Hashtbl.t;
      w_binder_map: (var, binder) Hashtbl.t;
      mutable w_var_index: int;
      w_variant_name_map: (string, int) Hashtbl.t;
      w_closure_values: (var, identifier) Hashtbl.t;
      w_primitive_vars: identifier Env.Int.t;
      w_env_values: (var, int) Hashtbl.t;
      w_closure_capture_field_map: ((var * var), int) Hashtbl.t;
    }
    let new_notwasm_writer result wat_output =
      let main_func = {
        func_name = "links_notwasm_main";
        func_params = [];
        func_ret = Void;
        func_body = [];
        func_is_closure = false;
      } in
      {
        w_program = {
          notwasm_globals = [];
          notwasm_functions = [main_func];
          main_func = main_func
        };
        w_result = result;
        w_out_channel = open_out wat_output;
        w_wat_output = wat_output;
        w_func_map = Hashtbl.create 100;
        w_binder_map = Hashtbl.create 100;
        w_var_index = 0;
        w_variant_name_map = Hashtbl.create 30;
        w_closure_values = Hashtbl.create 30;
        w_primitive_vars = Env.String.fold (fun key value acc -> Env.Int.bind value key acc) Lib.nenv Env.Int.empty;
        w_env_values = Hashtbl.create 30;
        w_closure_capture_field_map = Hashtbl.create 100;
      }
    
    let add_binder writer binder =
      let var = var_of_binder binder in
      Hashtbl.add writer.w_binder_map var binder;
      if writer.w_var_index < var then writer.w_var_index <- var else ()
    let find_binder writer var =
      Hashtbl.find writer.w_binder_map var
    let next_var writer =
      let v = writer.w_var_index + 1 in
      writer.w_var_index <- v;
      v
    let notwasm_temp_var_name var =
      "__notwasm_tmp_" ^ (string_of_int var)
    let next_var_name writer =
      let var = next_var writer in
      notwasm_temp_var_name var
    let add_closure writer var =
      Hashtbl.add writer.w_closure_values var (next_var_name writer)
    let is_closure writer var =
      Hashtbl.mem writer.w_closure_values var
    let closure_var_name writer var =
      Hashtbl.find writer.w_closure_values var
    let add_env writer var =
      Hashtbl.add writer.w_env_values var 1
    let is_env writer var =
      Hashtbl.mem writer.w_env_values var
    let find_ir_var_type writer v =
      Env.String.find (Env.Int.find v writer.w_primitive_vars) Lib.type_env
    let add_closure_captured_fields writer binder =
      let closure_info_var = var_of_binder binder in
      match type_of_binder binder with
      | Record row -> (
        match row with
        | Row (field_map, _, _) ->
          StringMapExt.map (fun name _ -> int_of_string name) field_map 
          |> List.iteri (fun index name -> Hashtbl.add writer.w_closure_capture_field_map (closure_info_var, name) index)
        | _ -> ()
      )
      | _ -> ()
    let closure_capture_field_name writer closure_info_var name_of_var =
      Hashtbl.find writer.w_closure_capture_field_map (closure_info_var, int_of_string name_of_var)

    let variant_kind writer variant_name =
      match Hashtbl.find_opt writer.w_variant_name_map variant_name with
      | Some index -> index
      | None -> let index = Hashtbl.length writer.w_variant_name_map in
        Hashtbl.add writer.w_variant_name_map variant_name index;
        index

    let dummy_variant_type = ReferenceType (InternalVariant ("a", StringMap.empty))
    let rec ir_type_to_notwasm ir_type =
      if string_of_datatype ir_type = "()" then InternalUnit
      else
        match ir_type with
        | Primitive p -> ir_primitive_type_to_notwasm p
        | Function (in_type, _, result_type) ->
          let param_types = get_ir_func_params in_type |> List.map (fun t ->
            let (_, b) = t in b 
          ) in
          let result_type = (match result_type with
            | Primitive p -> ir_primitive_type_to_notwasm p
            | ForAll (_quantifiers, underlying_type) -> ir_type_to_notwasm underlying_type
            | f -> ir_type_to_notwasm f
          ) in
          let result_type = (match result_type with
            | InternalUnit -> Void
            | t -> ReturnT t
          ) in
          ReferenceType (Function (param_types, result_type))
        | Effect _ -> raise (NotImplemented __LOC__)
        | Var _ -> raise (NotImplemented __LOC__)
        | Recursive _ -> raise (NotImplemented __LOC__)
        | Not_typed -> raise (NotImplemented __LOC__)
        | Alias _ -> raise (NotImplemented __LOC__)
        | Application _ -> raise (NotImplemented __LOC__)
        | RecursiveApplication _ -> raise (NotImplemented __LOC__)
        | Meta point -> ir_meta_type_to_notwasm point
        | Lolli _ -> raise (NotImplemented __LOC__)
        | Record row -> ir_row_type_to_notwasm row
        | Variant _ -> dummy_variant_type
        | Table _ -> raise (NotImplemented __LOC__)
        | Lens _ -> raise (NotImplemented __LOC__)
        | ForAll (_quantifiers, underlying_type) -> ir_type_to_notwasm underlying_type
        | Row _ -> raise (NotImplemented ("row type: " ^ (string_of_datatype ir_type)))
        | Closed -> raise (NotImplemented __LOC__)
      (* Presence *)
        | Absent -> raise (NotImplemented __LOC__)
        | Present a -> raise (NotImplemented ("present type: " ^ (string_of_datatype a)))
      (* Session *)
        | Input _ -> raise (NotImplemented __LOC__)
        | Output _ -> raise (NotImplemented __LOC__)
        | Select _ -> raise (NotImplemented __LOC__)
        | Choice _ -> raise (NotImplemented __LOC__)
        | Dual _ -> raise (NotImplemented __LOC__)
        | End -> raise (NotImplemented __LOC__)
    and ir_meta_type_to_notwasm = fun point ->
      (match Unionfind.find point with
       | Var _ -> dummy_variant_type
       | Closed -> raise (NotImplemented __LOC__)
       | Recursive _ -> raise (NotImplemented __LOC__)
       | t -> ir_type_to_notwasm t
      )
    and ir_row_type_to_notwasm row =
      match row with
      | Row (field_map, _, _) -> ReferenceType (InternalRecord (StringMap.map (fun ir_type -> ir_field_type_to_notwasm ir_type) field_map))
      | _ -> raise (Unreachable __LOC__)
    and ir_field_type_to_notwasm field_type =
      match field_type with
      | Present t -> ir_type_to_notwasm t 
      | _ -> ir_type_to_notwasm field_type
    and extract_from_row r =
      let (field_map, _, _) = r in
      let types = StringMapExt.map (fun field_name field_type -> field_name, field_type |> ir_field_type_to_notwasm) field_map in
      types
    and extract_from_in_type = function
      | Row r -> extract_from_row r 
      | Record r -> extract_from_in_type r 
      | _ -> raise (NotImplemented __LOC__)
    and get_result_type f =
      let rec get_ir_func_result_type = function 
        | Types.Function (_, _, out_type) -> get_ir_func_result_type out_type
        | Primitive p -> ir_primitive_type_to_notwasm p
        | ForAll (_quantifiers, underlying_type) -> get_ir_func_result_type underlying_type
        | f -> ir_type_to_notwasm f
      in
      match get_ir_func_result_type f with
      | InternalUnit -> Void
      | t -> ReturnT t
    and get_ir_func_params in_type =
      let origin_types = extract_from_in_type in_type in
      reduce_unit_type_in_params origin_types
    and reduce_unit_type_in_params params =
      let params = if List.length params = 1 then 
        let (_, t) = List.nth params 0 in
          if is_unit_type t then []
          else params
        else params
      in
      params |> List.map (fun (a, t) ->
        if is_unit_type t then (a, ReferenceType (DynObject))
        else (a, t)
      )
    and is_unit_type t =
      match t with
      | InternalUnit -> true
      | ReferenceType r -> (
        match r with
        | InternalRecord field_map -> StringMap.cardinal field_map = 0
        | _ -> false
      )
      | _ -> false
    and ir_param_to_notwasm param =
      let name = "_" ^ (string_of_int (var_of_binder param)) in (* in links ir params don't have a name*)
      let t = type_of_binder param |> ir_type_to_notwasm in
      (name, t)
    and ir_func_to_notwasm writer func =
      let params = reduce_unit_type_in_params (List.map ir_param_to_notwasm func.fn_params) in
      let result_type = get_result_type (type_of_binder func.fn_binder) in
      let name = get_binder_name func.fn_binder in
      let is_closure = match func.fn_closure with
        | Some binder -> 
          add_binder writer binder; 
          add_closure writer (var_of_binder func.fn_binder);
          add_env writer (var_of_binder binder);
          add_closure_captured_fields writer binder;
          true
        | None -> false
      in
      let notwasm_func = declare_func writer name params result_type is_closure in
      func.fn_params |> List.iter (add_binder writer);
      Hashtbl.add writer.w_func_map (var_of_binder func.fn_binder) notwasm_func;
      notwasm_func
    and declare_func writer name params ret is_closure =
      let new_fun = {
        func_name = name;
        func_params = params;
        func_ret = ret;
        func_body = [];
        func_is_closure = is_closure;
      } in
      writer.w_program.notwasm_functions <- new_fun::writer.w_program.notwasm_functions;
      new_fun
    and get_binder_name binder =
      let name = name_of_binder binder in
      let var = var_of_binder binder in
      name ^ "_" ^ (string_of_int var)
    and get_var_name writer v =
      let binder = find_binder writer v in
      get_binder_name binder
    and ir_primitive_type_to_notwasm t =
      let open CommonTypes.Primitive in
      match t with
      | Bool   -> links_bool_type
      | Int    
      | Char    -> links_int_type
      | Float   -> links_float_type
      | XmlItem 
      | DB      -> raise (NotImplemented __LOC__)
      | String  -> links_string_type
    and links_bool_type = ValueType Bool
    and links_int_type = ValueType I32
    and links_float_type = ValueType F64
    and links_string_type = ReferenceType Str
    let links_record_type = ReferenceType (InternalRecord (StringMap.empty))
    let links_variant_type = ReferenceType (InternalRecord (StringMap.empty))
    let links_unit_type = links_record_type
    let links_variant_kind_type = ValueType (I32)
    let links_variant_kind_field_name = "_variant_kind"
    let links_record_field_name int_name = "_record_field_" ^ int_name

    let find_func writer var =
      Hashtbl.find writer.w_func_map var
    let raise_unit_type () = raise (Unreachable "unit type should be detonted by DynObject type")

    let default_value_expression notwasm_type =
      let default_value_atom = function
        | I32 -> IntLiteral 0
        | F64 -> (FloatLiteral 0.0)
        | Bool -> (BoolLiteral false)
      in
      match notwasm_type with
      | Any -> raise (NotImplemented "cannot create a value of type any without underlying type")
      | ValueType v -> SingleAtom (default_value_atom v)
      | ReferenceType r -> (
        match r with
        | Str -> SingleAtom (StringLiteral "")
        | Array _ -> ApplyPrimitiveMemory ("array_new", [])
        | DynObject -> DynObjectLiteral
        | HT -> ApplyPrimitiveMemory ("ht_new", [])
        | Function _ -> SingleAtom NullLiteral
        | Closure _ -> SingleAtom NullLiteral
        | Ref underlying_type -> RefLiteral ((match underlying_type with
          | ValueType v -> default_value_atom v
          | _ -> raise (Unreachable "underlying type of ref must be value type")
        ), underlying_type)
        | Env -> SingleAtom NullLiteral
        | InternalRecord _ -> DynObjectLiteral
        | InternalVariant _ -> DynObjectLiteral
      )
      | InternalUnit -> raise_unit_type ()

    let declare_var_with _writer name nt_type exp =
      VarDeclare (name, nt_type, exp)
    let declare_global_with writer name nt_type exp =
      let var_decl = (name, nt_type, exp) in
      let var_stat = VarDeclare var_decl in
      let program = writer.w_program in
      program.notwasm_globals <- program.notwasm_globals @ [var_decl];
      var_stat
    let declare_var name nt_type =
      let var_decl = (name, nt_type, default_value_expression nt_type) in
      VarDeclare var_decl
    let declare_global writer name nt_type =
      declare_global_with writer name nt_type (default_value_expression nt_type)
    
    let rec type_of_tail_computation writer tail_computation =
      match tail_computation with
      | Ir.Return value -> type_of_value writer value
      | Apply (f, args) -> type_of_apply writer f args
      | Special _ -> raise (NotImplemented __LOC__)
      | Case (_value, cases, default_case) ->
        let case_computations = (StringMapExt.map (fun _ (_, (_bindings, tail)) -> tail) cases) @ (
          match default_case with
          | Some a -> let (_binder, (_bindings, tail)) = a in [tail]
          | None -> []
        )
        in
        let tail_types = case_computations |> List.map (type_of_tail_computation writer) in
        common_type tail_types
      | If (_value, if_case, else_case) ->
        [if_case; else_case]
        |> List.map (fun (_bindings, tail) -> tail)
        |> List.map (type_of_tail_computation writer)
        |> common_type
    and assert_record_field_map notwasm_type =
      match notwasm_type with
      | ReferenceType r -> (match r with
        | InternalRecord old_map -> old_map
        | _ -> raise (Unreachable "not record")
      )
      | ValueType _ 
      | Any -> raise (Unreachable "not reference type")
      | InternalUnit -> StringMap.empty
    and extend_record_type writer new_fields old_value =
      let old_map: notwasm_type StringMap.t = match old_value with
        | Some value -> type_of_value writer value |> assert_record_field_map
        | None -> StringMap.empty
      in
      let fields: (string * notwasm_type) list = new_fields |> StringMap.map (type_of_value writer) |> StringMapExt.map (fun name field_type -> (name, field_type)) in
      let result_map = List.fold_left (fun (old_map: notwasm_type StringMap.t) (name, field_type) ->
        StringMap.add name field_type old_map
      ) old_map fields
      in ReferenceType (InternalRecord result_map)
    and erase_record_type writer delete_fields old_value =
      let old_map = type_of_value writer old_value |> assert_record_field_map in
      let result_map = StringSet.fold (fun field old_map ->
        StringMap.remove field old_map
      ) delete_fields old_map
      in ReferenceType (InternalRecord result_map)
    and type_of_constant c = Constant.type_of c |> ir_primitive_type_to_notwasm
    and type_of_value writer value =
      match value with
      | Constant c -> type_of_constant c 
      | Variable v -> type_of_var writer v 
      | ApplyPure (f, args) -> type_of_apply writer f args
      | Extend (new_fields, old_value) -> extend_record_type writer new_fields old_value
      | Project (name, record_value) -> (
        type_of_value writer record_value |> assert_record_field_map |> StringMap.find name
      )
      | Erase (delete_fields, old_value) -> erase_record_type writer delete_fields old_value
      | Inject (variant_name, old_value, _t) -> (* the variant_name provides a more specific type than the type *)
        ReferenceType (InternalVariant (variant_name, type_of_value writer old_value |> assert_record_field_map))
      | TAbs  _ -> raise (NotImplemented __LOC__)
      (* TApp is probably function value, treat it as is *)
      | TApp (value, tyargs) -> type_of_type_apply writer value tyargs
      | XmlNode _ -> raise (NotImplemented __LOC__)
      | Closure (var, _, _) -> type_of_var writer var
      | Coerce (_value, to_type) -> ir_type_to_notwasm to_type
    and type_of_type_apply writer value _tyargs =
      type_of_value writer value
    and function_type_to_closure t1 =
      match t1 with
      | ReferenceType r -> (match r with
        | Function (params, ret) -> ReferenceType (Closure (params, ret))
        | _ -> t1
      )
      | _ -> t1
    and type_of_var writer var =
      let t1 = (if is_primitive_var var then find_ir_var_type writer var
      else find_binder writer var |> type_of_binder) |> ir_type_to_notwasm in
      if is_closure writer var then function_type_to_closure t1
      else t1
    and type_of_apply writer f args =
      let applied_type = get_applied_value f |> type_of_value writer in 
      let rec reduce_func_type func_type arg_num =
        let reduce_function param_list return_type =
          let param_num = List.length param_list in
          let return_type = match return_type with
            | ReturnT t -> t 
            | Void -> links_unit_type
          in
          if arg_num >= param_num then reduce_func_type return_type (arg_num - param_num)
          else if arg_num = 0 then applied_type
          else raise (Unreachable "param number less than argument number")
        in
        match func_type with
        | ReferenceType r -> (
          match r with
          | Function (param_list, return_type) -> reduce_function param_list return_type 
          | Closure (param_list, return_type) -> reduce_function param_list return_type
          | _ -> func_type
        )
        | _ -> func_type
      in
      reduce_func_type applied_type (List.length args)
    and get_applied_value f =
      match f with
      | TApp (applied_value, _ty) -> applied_value
      | _ -> assert false
    and common_type types =
      match types with
      | [] -> raise (Unreachable "empty type list to compute a common type")
      | first::_v -> first
  end

  module Collect = struct
    open Grammar
    let rec collect_program writer =
      let (bindings, tail_computation) = writer.w_result.Backend.program in
      let main_func = writer.w_program.main_func in
      bindings |> List.iter (collect_binding writer main_func);
      tail_computation |> collect_tail_computation writer main_func
    and collect_binding writer func binding =
      (* let collect_alien_var writer binder module_name = 
        add_binder writer binder;
        let local_name = get_binder_name binder in
        add_global writer.wasm_module (Some local_name) (ir_type2Wasm (type_of_binder binder) |> to_val_type);
        import_global writer binder module_name
      in
      let collect_alien_fun writer binder module_name =
        add_binder writer binder;
        import_func writer binder module_name
      in *)
      match binding with
      | Let (binder, (type_var_list, tail_computation)) -> collect_let_binding writer func binder type_var_list tail_computation
      | Fun fun_def -> 
        collect_ir_fun writer fun_def
      | Rec fun_defs ->
        List.iter (collect_ir_fun writer) fun_defs
      (* | Alien alien_info -> (
        let binder = alien_info.binder in
        let type_of_alien = type_of_binder binder |> ir_type2Wasm in
        match type_of_alien with
        | Func_type _ -> collect_alien_fun writer binder alien_info.object_name
        | Val_type _ -> collect_alien_var writer binder alien_info.object_name
        | _ -> assert false
      ) *)
      | Alien _ -> raise (NotImplemented "alien javascript not implemented in NotWasm")
      | Module _ -> raise (NotImplemented __LOC__)
    and collect_let_binding writer func binder _ty_var_list tail_computation =
      add_binder writer binder;
      collect_tail_computation writer func tail_computation
    and collect_tail_computation writer func tail_computation =
      match tail_computation with
      | Return _ -> ()
      | Apply _ -> ()
      | Special _ -> raise (NotImplemented __LOC__)
      | Case (_, cases, default_case) -> (
        StringMap.iter (fun _ (binder, computation) ->
          add_binder writer binder;
          collect_computation writer func computation
        ) cases;
        match default_case with
        | Some case -> 
          let (binder, computation) = case in
          add_binder writer binder;
          collect_computation writer func computation
        | None -> ()
      )
      | If (_, if_tail, else_tail) ->
        collect_computation writer func if_tail;
        collect_computation writer func else_tail
    and collect_computation writer func computation =
      let (bindings, tail_computation) = computation in
      bindings |> List.iter (collect_binding writer func);
      tail_computation |> collect_tail_computation writer func
    and collect_ir_fun writer ir_func =
      let notwasm_func = ir_func_to_notwasm writer ir_func in
      add_binder writer ir_func.fn_binder;
      collect_computation writer notwasm_func ir_func.fn_body
  end

  module Convert = struct
    open Grammar
    let rec conv_program writer =
      let (bindings, tail) = writer.w_result.Backend.program in
      let main_func = writer.w_program.main_func in
      let bindings = bindings |> List.map (conv_binding writer main_func true) |> List.flatten in
      let tail_computation_type = type_of_tail_computation writer tail in
      let (_tail_temp_var_name, tail_stats) = match tail_computation_type with
        | InternalUnit -> conv_tail_computation_to_temp_var writer main_func links_unit_type tail
        | _ -> conv_tail_computation_to_temp_var writer main_func tail_computation_type tail 
      in
      main_func.func_body <- bindings @ tail_stats
    and conv_binding writer func is_global binding: statement list =
      match binding with
      | Let (binder, (type_var_list, tail_computation)) -> 
        conv_let_binding writer func binder type_var_list is_global tail_computation
      | Fun fun_def -> 
        conv_ir_fun writer fun_def; []
      | Rec fun_defs ->
        List.iter (conv_ir_fun writer) fun_defs; []
      | Alien _ -> raise (NotImplemented "alien javascript not implemented in NotWasm")
      | Module _ -> raise (NotImplemented __LOC__)
    and conv_var_with writer name notwasm_type var_exp is_global =
      if is_global then declare_global_with writer name notwasm_type var_exp
      else declare_var_with writer name notwasm_type var_exp
    and conv_var writer name notwasm_type is_global =
      conv_var_with writer name notwasm_type (default_value_expression notwasm_type) is_global
    and copy_record_fields src dst field_set =
      let field_set = field_set |> List.map (fun field ->
          match int_of_string_opt field with
          | Some _ -> links_record_field_name field
          | None -> field) 
      in
      List.map (fun t -> WriteField (BoundIdentifier dst, t, (ReadField (BoundIdentifier src, t)))) field_set
    and conv_set_value writer func var_name value =
      let set_var exp =
        [UpdateLocalVariable (var_name, exp)]
      in
      let field_set_of_record_value writer value =
        let (fields, _) = type_of_value writer value |> assert_record_field_map |> StringMap.bindings |> List.split in
        fields
      in
      let copy_record writer record_value_option =
        let src_var_name = next_var_name writer in
        let field_map = match record_value_option with
          | Some record_value -> type_of_value writer record_value |> assert_record_field_map
          | None -> StringMap.empty
        in
        let record_type = ReferenceType (InternalRecord field_map) in
        let new_record_stat = declare_var src_var_name record_type in
        let set_field_stats = copy_record_fields src_var_name var_name (StringMapExt.map (fun name _ -> name) field_map) in
        new_record_stat::set_field_stats
      in
      match value with
      | Constant c -> 
        let exp = (SingleAtom (conv_constant c)) in
        set_var exp
      | Variable v ->
        if is_closure writer v then set_var (SingleAtom (BoundIdentifier (closure_var_name writer v)))
        else set_var (SingleAtom (BoundIdentifier (get_var_name writer v)))
      | ApplyPure (f, args) -> 
        let (arg_stats, exp) = conv_apply_pure writer func f args in
        arg_stats @ (set_var exp)
      | Extend (field_map, record_value) ->
        let copy_stats = copy_record writer record_value in
        let new_stats = StringMapExt.map (fun field_name value ->
          let value_var_name = next_var_name writer in
          let value_declare_stat = declare_var value_var_name links_record_type in
          let value_stats = conv_set_value writer func value_var_name value in
          let set_stat = WriteField (BoundIdentifier var_name, field_name, BoundIdentifier value_var_name) in
          value_declare_stat::value_stats @ [set_stat]
        ) field_map |> List.flatten
        in
        copy_stats @ new_stats
      | Project (field_name, record_value) ->
        let old_record_value_var_name = next_var_name writer in
        let is_env value =
          match value with
          | Variable v -> is_env writer v 
          | _ -> false
        in
        let is_env_project = is_env record_value in
        let field_type = 
          type_of_value writer record_value |> assert_record_field_map |> StringMap.find field_name 
        in
        let declare_stat = declare_var old_record_value_var_name links_record_type in
        let record_stats = conv_set_value writer func old_record_value_var_name record_value in
        let compose atom = 
          declare_stat::record_stats @ (set_var (SingleAtom atom))
        in
        if is_env_project then compose (EnvGet (closure_capture_field_name writer (match record_value with
          | Variable v -> v
          | _ -> assert false) field_name, field_type))
        else compose (Cast ((ReadField (BoundIdentifier old_record_value_var_name, field_name)), field_type))
      | Erase (field_set, record_value) ->
        let copy_stats =
          let src_var_name = next_var_name writer in
          let new_record_stat = declare_var src_var_name links_record_type in
          let fields = field_set_of_record_value writer record_value in
          let fields = List.filter (fun t -> not (StringSet.mem t field_set)) fields in
          let set_field_stats = List.map (fun t -> WriteField (BoundIdentifier var_name, t, ReadField (BoundIdentifier src_var_name, t))) fields in
          new_record_stat::set_field_stats
        in
        copy_stats
      | Inject (variant_name, value, _t) -> 
        let copy_stats = copy_record writer (Some value) in
        let variant_kind_stat = WriteField ((BoundIdentifier var_name), links_variant_kind_field_name, (IntLiteral (variant_kind writer variant_name))) in
        variant_kind_stat::copy_stats
      | TAbs _ -> raise (NotImplemented __LOC__)
      (* TApp is probably function value, treat it as is *)
      | TApp (value, _tyargs) -> (
        let applied_var_name = next_var_name writer in
        let assign_stats = conv_set_value writer func applied_var_name value in
        (declare_var applied_var_name (type_of_value writer value))::assign_stats @ set_var (SingleAtom (BoundIdentifier applied_var_name))
      )
      | XmlNode _ -> raise (NotImplemented __LOC__)
      | Closure (func_var, _ty_var_list, record_value) ->
        let field_map =
          match record_value with
          | Extend (field_map, _) -> StringMapExt.map (fun name_of_captured_var _ ->
              let var = int_of_string name_of_captured_var in
              (get_var_name writer var, type_of_var writer var)
            ) field_map
          | Variable v -> 
            let field_map = v |> type_of_var writer |> assert_record_field_map in
            StringMapExt.map (fun name_of_captured_var _ ->
              let var = int_of_string name_of_captured_var in
              (get_var_name writer var, type_of_var writer var)
            ) field_map
          | _ -> Printf.printf "k: %s\n%!" (string_of_value (record_value)); raise (Unreachable __LOC__)
        in
        set_var (ClosureLiteral (get_var_name writer func_var, field_map))
      | Coerce (value, ir_type) ->
        let notwasm_type = ir_type_to_notwasm ir_type in
        let applied_var_name = next_var_name writer in
        let assign_stats = conv_set_value writer func applied_var_name value in
        (declare_var applied_var_name (type_of_value writer value))::assign_stats @ set_var (SingleAtom (Cast (BoundIdentifier applied_var_name, notwasm_type)))
    and conv_let_binding writer (func: notwasm_func) (binder: binder) (_ty_var_list: tyvar list) (is_global: bool) tail_computation =
      let name = get_binder_name binder in
      let notwasm_type = ir_type_to_notwasm (type_of_binder binder) in
      conv_let_binding_to_var_name writer func _ty_var_list is_global name notwasm_type tail_computation
    and conv_let_binding_to_var_name writer (func: notwasm_func) _ty_var_list (is_global: bool) (var_name: identifier) (notwasm_type: notwasm_type) tail_computation: statement list =
      let conv_var_with_init_value writer var_name notwasm_type is_global value =
        (conv_var writer var_name notwasm_type is_global)::(conv_set_value writer func var_name value)
      in
      let conv_computation computation =
        let (bindings, if_tail) = computation in
        let if_bindings_stats = List.map (conv_binding writer func false) bindings |> List.flatten in
        let (if_var_name, if_tail_stats) = conv_tail_computation_to_temp_var writer func notwasm_type if_tail in
        if_bindings_stats @ if_tail_stats @ [UpdateLocalVariable (var_name, SingleAtom (BoundIdentifier if_var_name))]
      in
      let init_value_exp = match tail_computation with
        | Ir.Return value -> (
          match value with
          | Constant c -> 
            let exp = SingleAtom (conv_constant c) in
            [conv_var_with writer var_name notwasm_type exp is_global]
          | Variable v ->
            let var_exp = SingleAtom (BoundIdentifier (get_var_name writer v)) in
            [conv_var_with writer var_name notwasm_type var_exp is_global]
          | _ -> conv_var_with_init_value writer var_name notwasm_type is_global value 
        )
        | Apply (f, args) ->
          let (arg_stats, value_exp) = conv_apply writer func f args in
          arg_stats @ [conv_var_with writer var_name notwasm_type value_exp is_global]
        | Special _ -> raise (NotImplemented __LOC__)
        | Case (value, cases, default_case) -> (
          let init_var_stat = conv_var writer var_name notwasm_type is_global in
          let case_value_temp_var_name = next_var_name writer in
          let predicate_value_type = links_variant_type in
          let case_value_declare_stat = declare_var case_value_temp_var_name predicate_value_type in
          let predicate_stats = conv_var_with_init_value writer case_value_temp_var_name predicate_value_type false value in
          let (variant_kind_temp_var_name, variant_kind_stat) = get_variant_kind writer case_value_temp_var_name in
          let one_case variant_name (binder, computation) =
            let (match_result_var, variant_match_stats) = conv_match_variant writer variant_kind_temp_var_name variant_name in
            let field_map = type_of_binder binder |> ir_type_to_notwasm |> assert_record_field_map
              |> StringMap.filter (fun field_name _ -> field_name <> links_variant_kind_field_name)
            in
            let case_binder_var_name = get_binder_name binder in
            let bind_stat = declare_var case_binder_var_name (ReferenceType (InternalRecord field_map)) in
            let bind_case_var_stats = copy_record_fields case_value_temp_var_name case_binder_var_name (field_map |> StringMapExt.map (fun field_name _ -> (links_record_field_name field_name))) in
            let computation_stats = conv_computation computation in
            (variant_match_stats, match_result_var, bind_stat::bind_case_var_stats @ computation_stats)
          in
          let convert_all_cases = StringMapExt.map one_case cases in
          let init_var_stats = [init_var_stat; case_value_declare_stat] @ predicate_stats @ variant_kind_stat @ (List.map (fun (a, _, _) -> a) convert_all_cases |> List.flatten) in
          let default_case_stats = match default_case with
            | Some (binder, computation) ->
              let bind_case_var_stats = VarDeclare ((get_binder_name binder), predicate_value_type, (SingleAtom (BoundIdentifier case_value_temp_var_name))) in
              let computation_stats = conv_computation computation in
              ([bind_case_var_stats] @ computation_stats)
            | None -> []
          in
          let case_stats = if StringMap.cardinal cases = 0 then default_case_stats
            else List.fold_right (fun (_, match_result_var, if_stats) else_stats -> [Grammar.If ((BoundIdentifier match_result_var), if_stats, else_stats)]) convert_all_cases default_case_stats 
          in
          init_var_stats @ case_stats
        )
        | If (value, if_computation, else_computation) -> 
          let predicate_var_name = next_var_name writer in
          let predicate_stats = conv_var_with_init_value writer predicate_var_name (ValueType Bool) false value in
          let init_var_stat = conv_var writer var_name notwasm_type is_global in
          let init_var_stats = predicate_stats @ [init_var_stat] in
          let if_stats = conv_computation if_computation in
          let else_stats = conv_computation else_computation in
          init_var_stats @ [Grammar.If ((BoundIdentifier predicate_var_name), if_stats, else_stats)]
      in init_value_exp
    and conv_tail_computation_to_temp_var writer func notwasm_type tail_computation =
      let var_name = next_var_name writer in
      let stats = conv_let_binding_to_var_name writer func [] false var_name notwasm_type tail_computation in
      (var_name, stats)
    and get_variant_kind writer variant_var =
      let variant_kind_temp_var_name = next_var_name writer in
      let variant_kind_stat = VarDeclare (variant_kind_temp_var_name, links_variant_kind_type, SingleAtom (Cast ((ReadField ((BoundIdentifier variant_var), links_variant_kind_field_name)), (ValueType I32)))) in
      (variant_kind_temp_var_name, [variant_kind_stat])
    and conv_match_variant writer variant_kind_temp_var_name variant_name =
      let match_result_var = next_var_name writer in
      let variant_match_stat = VarDeclare (match_result_var, (ValueType Bool), SingleAtom (BinaryExpression (BoundIdentifier variant_kind_temp_var_name, IntEqual, IntLiteral (variant_kind writer variant_name)))) in
      (match_result_var, [variant_match_stat])
    and conv_ir_fun writer ir_fun =
      let notwasm_func = find_func writer (var_of_binder ir_fun.fn_binder) in
      let (bindings, tail) = ir_fun.fn_body in
      let binding_stats = List.map (conv_binding writer notwasm_func false) bindings |> List.flatten in
      let tail_computation_type = type_of_tail_computation writer tail in
      let (tail_temp_var_name, tail_stats) = match tail_computation_type with
        | InternalUnit -> conv_tail_computation_to_temp_var writer notwasm_func links_unit_type tail
        | _ -> conv_tail_computation_to_temp_var writer notwasm_func tail_computation_type tail 
      in
      let return_stat = match tail_computation_type with
        | InternalUnit -> []
        | _ -> [Grammar.Return (BoundIdentifier tail_temp_var_name)]
      in
      let stats = binding_stats @ tail_stats @ return_stat in
      notwasm_func.func_body <- stats
    and conv_arg writer func value =
      let arg_var_name = next_var_name writer in
      let arg_stats = conv_set_value writer func arg_var_name value in
      ((declare_var arg_var_name (type_of_value writer value))::arg_stats, arg_var_name)
    and conv_args writer func values =
      let (stats, names) = List.map (conv_arg writer func) values |> List.split in
      (List.flatten stats, names)
    and conv_constant (const_value: Constant.t) =
      let open CommonTypes.Constant in
      match const_value with
      | Bool value -> BoolLiteral value
      | Char value -> StringLiteral (string_of_char value)
      | Int value -> IntLiteral value
      | Float value -> FloatLiteral value
      | String value -> StringLiteral value
    and conv_apply writer func f args =
      conv_apply_pure writer func f args
    and conv_apply_pure writer func f args =
      match f with
      | TApp (applied_value, _) -> (
        let call_insc = match applied_value with
          | Variable v -> conv_ir_func_call writer func v args
          | Closure (func_var, _, _) ->
            let func_var_name = next_var_name writer in
            let init_closure_stats = [declare_var func_var_name (type_of_var writer func_var)] in
            let conv_closure_stats = conv_set_value writer func func_var_name f in
            let (call_stats, call_exp) = conv_closure_call writer func func_var_name args in
            (init_closure_stats @ conv_closure_stats @ call_stats, call_exp)
          | _ -> raise (NotImplemented __LOC__)
        in call_insc
      )
      | Variable v -> conv_ir_func_call writer func v args (* probably a closure here*)
      | _ -> raise (Unreachable __LOC__)
    and conv_ir_func_call writer func v args =
      if is_primitive_var v then conv_primitive_ir_func_call writer func v args
      else conv_normal_ir_func_call writer func v args
    and conv_primitive_ir_func_call writer func v args =
      let name = primitive_name v in
      let (arg_stats, arg_names) = conv_args writer func args in
      let op1 = match List.nth_opt arg_names 0 with
        | Some op1 -> Some (BoundIdentifier op1)
        | None -> None
      in
      let op2 = match List.nth_opt arg_names 1 with
        | Some op2 -> Some (BoundIdentifier op2)
        | None -> None
      in
      let op1_type = match List.nth_opt args 0 with
        | Some o -> Some (type_of_value writer o)
        | None -> None
      in
      let try_compose_int_binary operator =
        SingleAtom (BinaryExpression (non_null op1, operator, non_null op2))
      in
      let try_compose_int_operator operator =
        let try_compare_pointer () = 
          match operator with
          | "==" -> SingleAtom (BinaryExpression (non_null op1, PointerEqual, non_null op2))
          | _ -> raise (NotImplemented "pointer only supports equality test")
        in
        (match (non_null op1_type) with
          | Any -> raise (Unreachable "cannot compare any value")
          | ValueType v -> (
            match v with
              | I32 -> (
                let binaryExp = match operator with 
                | "==" -> BinaryExpression (non_null op1, IntEqual, non_null op2)
                | "!=" -> BinaryExpression (non_null op1, IntNeq, non_null op2)
                | ">=" -> BinaryExpression (non_null op1, IntGE, non_null op2)
                | ">" -> BinaryExpression (non_null op1, IntGT, non_null op2)
                | "<=" -> BinaryExpression (non_null op1, IntLE, non_null op2)
                | "<" -> BinaryExpression (non_null op1, IntLT, non_null op2)
                | _ -> raise (Unreachable __LOC__)
                in SingleAtom(binaryExp)
              )
              | F64 -> (
                let binaryExp = match operator with 
                (* | "==" -> BinaryExpression (non_null op1, FloatLE, non_null op2) *)
                | ">=" | ">" -> BinaryExpression (non_null op2, FloatGE, non_null op1)
                | "<=" | "<" -> BinaryExpression (non_null op1, FloatGE, non_null op2)
                | _ -> raise (Unreachable __LOC__)
                in SingleAtom(binaryExp)
              )
              | _ -> raise (NotImplemented "float comparison")
            )
          | ReferenceType _ -> try_compare_pointer ()
          | InternalUnit -> raise_unit_type ()
        )
      in
      let try_compose_float_unary_operator name =
        let op1 = non_null op1 in
        ApplyPrimitiveMemory (name, [NullLiteral; NullLiteral; op1])
      in
      let exp = match name with
        | "+" -> try_compose_int_binary IntAdd
        | "-" -> try_compose_int_binary IntSub
        | "*" -> try_compose_int_binary IntMul
        | "+." -> try_compose_int_binary FloatAdd
        | "-." -> try_compose_int_binary FloatSub
        | "*." -> try_compose_int_binary FloatMul
        | "/." -> try_compose_int_binary FloatDiv
        | "==" -> try_compose_int_operator "=="
        | "!=" -> try_compose_int_operator "!="
        | ">=" -> try_compose_int_operator ">="
        | "<=" -> try_compose_int_operator "<="
        | ">" -> try_compose_int_operator ">"
        | "<" -> try_compose_int_operator "<"
        | "sqrt" -> try_compose_float_unary_operator "math_sqrt"
        | _ -> raise (NotImplemented ("primitive links function\"" ^ name ^ "\"not implemented in notwasm backend"))
      in
      (arg_stats, exp)
    and conv_normal_ir_func_call writer func v args =
      let (arg_stats, arg_names) = conv_args writer func args in
      let name = get_var_name writer v in
      let exp = if is_closure writer v then ClosureApplication (name, arg_names)
        else FunctionApplication (name, arg_names)
      in
      (arg_stats, exp)
    and conv_closure_call writer func name args =
      let (stats, names) = conv_args writer func args in
      let exp = ClosureApplication(name, names) in
      (stats, exp)
  end

  module Write = struct
    open Grammar
    type pretty_print_element =
    | IncIndent
    | DecIndent
    | LineIndent
    | IncWithLineIndent
    | DecWithLineIndent
    | L of string
    | S
    | Seq of pretty_print_element list

    type printer = {
      mutable indent: int;
      indent_ws_num: int;
    }
    let new_printer indent_ws_num = {
      indent = 0;
      indent_ws_num = indent_ws_num;
    }

    let give_indent_string pt = 
      String.make (pt.indent * pt.indent_ws_num) ' '
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

    let rec to_string pt element =
      match element with
      | IncIndent -> increment_indent pt; ""
      | DecIndent -> decrement_indent pt; ""
      | S -> " "
      | LineIndent -> give_line_indent pt
      | IncWithLineIndent -> 
        let _ = to_string pt IncIndent in
        to_string pt LineIndent
      | DecWithLineIndent -> 
        let _ = to_string pt DecIndent in
        to_string pt LineIndent
      | L s -> give pt s
      | Seq elements -> List.map (to_string pt) elements |> String.concat "" 

    let reduce_type notwasm_type =
      match notwasm_type with
      | ValueType _ -> notwasm_type
      | Any -> notwasm_type
      | InternalUnit -> ReferenceType DynObject
      | ReferenceType r -> (
        match r with
        | InternalRecord _ -> ReferenceType DynObject
        | InternalVariant _ -> ReferenceType DynObject
        | _ -> notwasm_type
      )

    let merge_parts parts =
      ListExt.concat_with LineIndent parts

    let rec write_program writer =
      let program = writer.w_program in
      let globals = program.notwasm_globals |> List.map write_global |> merge_parts in
      let functions = program.notwasm_functions |> List.map (fun t -> Seq (write_function t)) |> merge_parts in
      let parts = Seq (globals @ [LineIndent] @ functions) in
      let filename = writer.w_wat_output |> SysExt.get_filename_without_extension |> (fun t -> t ^ ".notwasm") in
      let output = open_out filename in
      Printf.fprintf output "%s" (to_string (new_printer 4) parts);
      close_out output
    and write_global var_declare =
      Seq (write_var_declare var_declare)
    and write_var_declare (name, t, init_exp) =
      [L "var "; L name; L ": "] @ [write_notwasm_type t] @ [L " = "] @ (write_exp init_exp) @ [L ";"]
    and write_exp exp =
      match exp with
      | SingleAtom atom -> [write_atom atom]
      | ApplyPrimitiveMemory (name, args) -> [L "!"; L name; L "("] @ (write_atoms args) @ [L ")"]
      | FunctionApplication (name, names) -> [L name; L "("] @ (write_names names) @ [L ")"]
      | ClosureApplication (name, names) -> [L name; L "!("] @ (write_names names) @ [L ")"]
      | DynObjectLiteral -> [L "{}"]
      | RefLiteral (atom, t) -> [L "newRef("] @ [write_atom atom] @ [L ", "] @ [write_notwasm_type t]
      | ArrayPush (atom, elem) -> [L "arrayPush("] @ [write_atom atom] @ [L ", "] @ [write_atom elem]
      | ClosureLiteral (func_name, field_type_list) -> 
        let fields = field_type_list 
          |> List.map (fun (name, t) ->
            Seq ([L name; L ": "] @ [write_notwasm_type t])
          ) 
          |> (fun t -> (L func_name)::t)
          |> ListExt.concat_with (L ", ")
        in
        [L "clos"; L "("] @ fields @ [L ")"]
    and write_notwasm_type notwasm_type =
      match reduce_type notwasm_type with
      | Any -> L "any"
      | ValueType v -> (
        match v with
        | I32 -> L "i32"
        | F64 -> L "f64"
        | Bool -> L "bool"
      )
      | ReferenceType r -> (
        match r with
        | Array _ -> raise (Unreachable "array is not represented in links")
        | DynObject -> L "DynObject"
        | Function (params, return) -> write_function_type params return
        | Closure (params, return) -> Seq [L "clos "; write_function_type params return]
        | HT -> L "HT"
        | Ref underlying_type -> Seq [L "Ref("; write_notwasm_type underlying_type; L ")"]
        | Env -> L "env"
        | _ -> raise (Unreachable "should be reduced")
      )
      | _ -> raise (Unreachable "should be reduced")
    and write_function_type params return =
      let params_elem = params |> List.map write_notwasm_type |> ListExt.concat_with (L ", ") in
      let ret_elem = write_return_type return in
      Seq ([L "("] @ params_elem @ [L ") -> "] @ ret_elem)
    and write_statement = function
      | VarDeclare v -> write_var_declare v
      | If (atom, if_block, else_block) ->
        [L "if ("] @ [write_atom atom] @ [L ") "] @ (write_block if_block) @ [L " else "] @ (write_block else_block)
      | Loop block -> [L "loop "] @ (write_block block)
      | Return atom -> [L "return "] @ [write_atom atom] @ [L ";"]
      | Break -> [L "break;"]
      | UpdateLocalVariable (name, exp) -> [L name; L " = "] @ (write_exp exp) @ [L ";"]
      | HTSet _ -> raise (Unreachable "hashtable not used in links")
      | WriteField (src, field, dst) -> [write_atom src] @ [L "."; L field; L " = "] @ [write_atom dst] @ [L ";"]
      | SingleExpression exp -> (write_exp exp) @ [L ";"]
    and write_atom = function
      | IntLiteral v -> L (string_of_int v)
      | FloatLiteral v -> L (notwasm_float v)
      | StringLiteral v -> L v
      | NullLiteral -> L "null"
      | BoundIdentifier id -> L id
      | ApplyPrimitive (id, atoms) -> Seq ([L "$"; L id; L "("] @ (write_atoms atoms) @ [L ")"])
      | PointerDereference _ -> raise (Unreachable "deref not used in links")
      | ReadField (atom, field) -> Seq ([write_atom atom] @ [L "."; L field])
      | BinaryExpression (l, op, r) ->
        let op_name = match op with
        | IntAdd -> "+"
        | IntSub -> "-"
        | IntMul -> "*"
        | IntGT -> ">"
        | IntLT -> "<"
        | IntGE -> ">="
        | IntLE -> "<="
        | IntEqual -> "=="
        | IntNeq -> "!="
        | PointerEqual -> "==="
        | FloatAdd -> "+."
        | FloatSub -> "-."
        | FloatMul -> "*."
        | FloatDiv -> "/."
        | FloatGE -> ">."
        in
        Seq ([write_atom l] @ [S; L op_name; S] @ [write_atom r])
      | AnyLiteral atom -> Seq ([L "any("] @ [write_atom atom] @ [L ")"])
      | BoolLiteral v -> L (string_of_bool v)
      | Cast (atom, t) -> Seq ([write_atom atom] @ [L " as "] @ [write_notwasm_type t])
      | EnvGet (field, t) -> Seq ([L "env."; L (string_of_int field); L ": "] @ [write_notwasm_type t])
    and notwasm_float number =
      let res = string_of_float number in
      let res = if res.[(String.length res) - 1] = '.' then res ^ "0" else res in
      res ^ "f"
    and write_atoms atoms =
      List.map write_atom atoms |> ListExt.concat_with (L ", ")
    and write_names names =
      List.map (fun t -> L t) names |> ListExt.concat_with (L ", ")
    and write_block block =
      let body = List.map (fun t -> Seq (write_statement t)) block |> ListExt.concat_with (LineIndent) in
      [L "{"; IncWithLineIndent] @ body @ [DecWithLineIndent; L "}"]
    and write_function func =
      let title = [L "function "; L (func.func_name)] in
      let params = func.func_params 
        |> List.map (fun (name, t) -> Seq ([L name; L ": "] @ [write_notwasm_type t]))
        |> ListExt.concat_with (L ", ") in
      let signature = [L "("] @ (
        if func.func_is_closure then [L "_: env"] else []
      ) @ (
        if func.func_is_closure && List.length params > 0 then [L ", "] else []
      ) @ params @ [L ")"] @ (match func.func_ret with
        | ReturnT t -> [L " : "; write_notwasm_type t]
        | Void -> []
      )
      in
      title @ signature @ [S] @ (write_block func.func_body)
    and write_return_type = function
      | ReturnT t -> [write_notwasm_type t]
      | Void -> []
  end

  let compile_ir_to_notwasm result wat_output =
    let writer = Grammar.new_notwasm_writer result wat_output in
    (* Ir.string_of_program result.Backend.program |> Debug.print; *)
    Collect.collect_program writer;
    Convert.conv_program writer;
    Write.write_program writer;
    ()
end

module type WASM_PERFORMANCE = sig
  val measure_wasm_performance: bool Settings.setting
  val trans: string -> unit
end
module Wasm_performance = struct
  let measure_wasm_performance = Settings.(
    flag "measure_wasm_performance"
    |> convert parse_bool
    |> CLI.(add (long "wasm-performance"))
    |> sync
  )

  let trans filename =
    let output_filename = (SysExt.get_filename_without_extension filename) ^ "-client-js.links" in
    let text = FileExt.read_all_text filename in
    let performance_func = {efgh|
alien javascript "/impl.js" printInteger: (Int) ~> ();
alien javascript "/impl.js" printFloat: (Float) ~> ();
alien javascript "/impl.js" changeInnerHtml_hidden: (String, String) ~> ();
fun changeInnerHtml(a, b) {
  changeInnerHtml_hidden(a, b)
}
fun measure() client {
    var timeBeforeRun = clientTimeMilliseconds();
    var mainResult = main();
    var timeAfterRun = clientTimeMilliseconds();
    timeAfterRun - timeBeforeRun
}|efgh}
    in
    let output_text = "fun main() {\n" ^ text ^ "\n}\n" ^ performance_func ^ {efgh|
fun mainPage(_) {
    var l1 = "label1";
    page
    <html>
    <head/>
    <body>
    <p id="label1">unchanged</p>
    <button l:onclick="{changeInnerHtml(l1, intToString(measure()))}">Click me</button>
    </body>
    </html>
}
fun main1() {
    addRoute("/", mainPage);
    servePages()
}
main1()
|efgh}
    in 
    let output_stream = open_out output_filename in
    Printf.fprintf output_stream "%s" output_text;
    close_out output_stream;
    let output_filename = (SysExt.get_filename_without_extension filename) ^ "-client-wasm.links" in
    let output_text = text in
    let output_stream = open_out output_filename in
    Printf.fprintf output_stream "%s" output_text;
    close_out output_stream
end

open Wasm
open Wasm_performance
let run (result: Backend.result) backend output_wat =
  if backend = "wasm" then
    let output_stream2 = open_out ((SysExt.get_filename_without_extension output_wat) ^ ".wat") in
    let writer2 = Wasm.Pretty_printing.new_wasm_writer output_wat false (Wasm.Pretty_printing.default_printer ()) output_stream2 (Ir2WasmAst.new_module "$$default_module") in
    Ir2WasmAst.compile_links_ir_to_wasm writer2 result;
    if Settings.get (Wasm_performance.measure_wasm_performance) then trans ((SysExt.get_filename_without_extension output_wat) ^ ".links") else ()
  else if backend = "notwasm" then NotWasm.compile_ir_to_notwasm result output_wat
  else failwith (Printf.sprintf "Unrecognised client backend %s" backend)
