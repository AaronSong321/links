
open Var
open Ir
open Lib
open Types
open CommonTypes
open Types

exception NotImplemented of string

module ListExt = struct
  include List 
  let select = map
  let selectMany: ('a -> 'b list) -> 'a list -> 'b list = fun collectionSelector source -> 
    source 
    |> select collectionSelector
    |> flatten
  let selectManyResult: ('a -> 'b list) -> ('a -> 'b -> 'c) -> 'a list -> 'c list = fun collectionSelector resultSelector source ->
    source 
    |> select (fun a -> a, collectionSelector a)
    |> select (fun (a,b) -> select (fun b' -> resultSelector a b') b)
    |> flatten
  let rec last = function
    | [] -> raise Not_found
    | [v] -> v
    | x::v -> last v
end
module SysExt = struct
  let getFileNameWithoutExtension file =
    let dotPosition = Str.search_backward (Str.regexp "\\.") file ((String.length file) - 1) in
    String.sub file 0 dotPosition
  let getExtension file =
    let dotPosition = Str.search_backward (Str.regexp "\\.") file ((String.length file) - 1) in
    String.sub file dotPosition ((String.length file) - dotPosition)
end
    
let (>>): ('a->'b) -> ('b->'c) -> ('a->'c) = fun a b ->
  fun m -> m |> a |> b
let nonNull t =
  match t with
  | Some v -> v 
  | None -> assert false

module Wasm = struct
  
  module Grammar = struct
    type numberTypeLength = L32|L64
    type memoryNumberLength = ML8|ML16|ML32
    type numberTypeSign = Signed|Unsigned
    type numberIntOrFloat = NInt|NFloat
    type type_ =
      | HeapType of heapType
      | ValType of valType
      | FuncType of funcType
      | MemoryType of memoryType
      | TableType of tableType
      | GlobalType of globalType
      | ErrorType
      | UnitType
    and refType =
      | FuncRef
      | ExternRef of externType
    and heapType =
      | FuncHeapRef
      | ExternHeapRef
    and valType =
      | NumberType of numberIntOrFloat * numberTypeLength
      | RefType of refType
    and funcType = {
      p: paramAnnotator;
      r: resultAnnotator
    }
    and resultAnnotator = valType list 
    and paramAnnotator = (string option * valType) list
    and limitType = {
      min: int32;
      max: int32 option
    }
    and memoryType = { limit:limitType }
    and tableType = {
      limit:limitType;
      t:refType
    }
    and globalType = {
      t: valType;
      isMutable: bool
    }
    and externType = 
      | ExternFunc of funcType
      | ExternTable of tableType
      | ExternMemory of memoryType
      | ExternGlobal of globalType

          
    let rec toString: type_ -> string = function
      | ValType v -> toStringValType v
      | _ -> raise (NotImplemented __LOC__)
    and toStringValType: valType -> string = function
      | NumberType (a,b) -> (
        match a with
        | NInt -> "i"
        | NFloat -> "f"
      ) ^ (
        match b with
        | L32 -> "32"
        | L64 -> "64"
      )
      | _ -> raise (NotImplemented __LOC__)
          
    type monoIndex = 
      | I_I of int
      | I_Id of string
    type funcIndex = monoIndex
    type localIndex = monoIndex
    type typeIndex = monoIndex
    type tableIndex = monoIndex
    type memoryIndex = monoIndex
    type globalIndex = monoIndex
    type labelIndex = monoIndex
    type elementIndex = monoIndex
    type dataIndex = monoIndex
      
    type typeDef = {
      id: string option;
      ft: funcType
    }
    type typeUse =
      | TU_Index of typeIndex
      | TU_Def of funcType
      | TU_IndexDef of typeIndex * funcType
      
    type instruction =
      | PlainInsc of plainInsc
      | BlockInsc of blockInsc
    and labelIdentifier = {
      id: string option;
      insc: instruction
    }
    and blockType =
      | BT_Result of resultAnnotator
      | BT_TypeUse of typeUse
    and plainInsc =
      | ControlInsc of controlInsc
      | RefInsc of refInsc
      | ParamInsc of paramInsc
      | VarInsc of varInsc
      | TableInsc of tableInsc
      | MemoryInsc of memoryInsc
      | NumericInsc of numericInsc
    and blockInsc =
      | CI_Block of {
          insc: labelIdentifier;
          bt: blockType;
          ins: instruction list;
          id: string option
        }
      | CI_Loop of {
          insc: labelIdentifier;
          bt: blockType;
          ins: instruction list;
          id: string option
        }
      | CI_If of { 
          insc: labelIdentifier;
          bt: blockType;
          ifIns: instruction list;
          id1: string option;
          elseIns: instruction list;
          id2: string option
        } 
    and numericInsc =
      | InnConst of numberTypeLength * int
      | FnnConst of numberTypeLength * float
      | InnRrelop of irelop 
    and irelop =
      | NII_Eq
      | NII_Ne
      | NII_Lt of numberTypeSign
      | NII_Gt of numberTypeSign
      | NII_Le of numberTypeSign
      | NII_Ge of numberTypeSign
    and controlInsc =
      | CInsc_Nop 
      | CInsc_If of cInsc_If
      | CInsc_Return
      | CInsc_Call of funcDef
    and cInsc_If = {
      bt: blockType ;
      mutable ifIns: instruction list;
      mutable elseIns: instruction list
    }
    and refInsc = 
      | RInsc_Null of heapType
      | RInsc_IsNull
      | RInsc_Func of funcDef
    and paramInsc =
      | PInsc_Drop
      | PInsc_Select of resultAnnotator
    and varInsc =
      | VInsc_LGet of localIndex
      | VInsc_LSet of localIndex
      | VInsc_LTee of localIndex
      | VInsc_GGet of globalIndex
      | Vinsc_GSet of globalIndex
    and tableInsc =
      | TInsc_Get of tableIndex
    and memoryInsc = 
      | MInsc_LoadI of numberTypeLength
      | MInsc_LoadSmallI of numberTypeLength * numberTypeSign * memoryNumberLength 
    and expr = instruction list
    and funcDef = {
      id: string option;
      (*type_: typeUse;*) (* it's like this in desugared wat, but it's better to write the other way*)
      type_: funcType;
      mutable locals: (string option * valType) list;
      mutable body: expr
    }
    let nameOfFunc: funcDef -> string = fun f -> nonNull f.id
    
    let foldLeft: type_ -> type_ -> type_ = fun oldType resultType ->
      let toCombineType: type_ -> type_ option = fun ot ->
        match ot with 
        | ErrorType -> Some ErrorType
        | UnitType -> None
        | _ -> Some ot
      in
      let params = match oldType with 
        | FuncType oldFuncType -> (
            let p = oldFuncType.p in
            let r = oldFuncType.r in
            let nr = match r with
              | [] -> None
              | single::[] -> toCombineType (ValType single)
              | _ -> failwith "right recursive function types"
            in
            match nr with
            | Some nr' -> (
                match nr' with 
                | ErrorType -> None
                | ValType t -> Some (p@[None, t])
                | _ -> None(* only valType can be used as function parameters in wasm*)
              )
            | None -> Some p
          )
        | ErrorType -> None
        | ValType t -> Some [None, t]
        | _ -> None
      in
      match params, resultType with
      | None, _
      | _, ErrorType -> ErrorType
      | Some params', UnitType -> FuncType { p = params'; r = [] }
      | Some params', ValType t -> FuncType { p = params'; r = [t] }
      | _, _ -> ErrorType

    type memDef = {
      id: string option;
      type_: memoryType
    }
    type globalDef = {
      id: string option;
      type_: globalType;
      initValue: expr
    }
    type exportDesc =
      | ED_Func of funcIndex
      | ED_Table of tableIndex
      | ED_Mem of memoryIndex
      | ED_Global of globalIndex
    type exportDef = {
      name: string;
      desc: exportDesc;
    }
    type tableDef = {
      id: string option;
      tt: tableType
    }
    
    type moduleDef = {
      id: string;
      mutable ty: typeDef list;
      mutable fn: funcDef list;
      mutable tb: tableDef list;
      mutable me: memDef list;
      mutable gl: globalDef list;
      mutable ex: exportDef list;
      mutable st: funcDef option;
    }
    let addLocal: funcDef -> string option -> valType -> unit = fun f id v ->
      f.locals <- f.locals@[id, v]
    let newFuncDef id funcType = {
      id = id;
      type_ = funcType;
      locals = [];
      body = []
    }
    let addGlobal (m: moduleDef) (id: string option) (v: valType): unit =
      let globalValueType = {
        t = v;
        isMutable = true
      } in
      let globalDef = {
        id = id;
        type_ = globalValueType;
        initValue = []
      } in
      m.gl <- m.gl@[globalDef]
    let declareFun: moduleDef -> string option -> paramAnnotator -> resultAnnotator -> funcDef = fun module_ id p r ->
      let funcDef_ = newFuncDef id ({ p = p; r = r }) in
      let _ = module_.fn = module_.fn@[funcDef_] in
      funcDef_
    let newModule id = 
      let linksFileFunc = newFuncDef (Some "_links_wasm_fileFunc") ({ p = []; r = [] }) in {
        id = id; 
        ty = [];
        fn = [linksFileFunc];
        tb = [];
        me = [];
        gl = [];
        ex = [];
        st = Some linksFileFunc
      }
  end
  
  module BinderMap = Utility.IntMap
  type varBinderMap = Ir.binder BinderMap.t

  module PrettyPrinting = struct
    open Grammar
    type prettyPrintElement =
      | IncIndent
      | DecIndent
      | Line
      | LineIndent
      | IncWithLineIndent
      | DecWithLineIndent
      | Paren of prettyPrintElement list
      | Literal of string
      (* | PPE_Par of string option * string
      | PPE_RetType of string *)
      | IdSep
    type instructionPrintStyle =
      | PlainStyle
      | FoldedStyle
    type argumentPrintStyle =
      | StackStyle 
      | ArgStyle

    type printer = {
      mutable indent: int;
      indentWSNum: int;
      plainInscStyle: instructionPrintStyle;
      ctrlInscStyle: instructionPrintStyle;
      mutable argumentPrint: argumentPrintStyle;
      abbreviateSingleModuleDef: bool
    }
    let defaultPrinter () = { 
      indent = 0; 
      indentWSNum = 2; 
      plainInscStyle = PlainStyle;
      ctrlInscStyle = PlainStyle;
      argumentPrint = StackStyle;
      abbreviateSingleModuleDef = false
    }
    let reverseStylePrinter () = {
      indent = 0;
      indentWSNum = 2;
      plainInscStyle = FoldedStyle;
      ctrlInscStyle = FoldedStyle;
      argumentPrint = ArgStyle;
      abbreviateSingleModuleDef = true
    }
  
    let linksIntValueType = NumberType (NInt, L64)
    let linksIntType = ValType linksIntValueType
    let linksFloatValueType = NumberType (NFloat, L64)
    let linksFloatType = ValType linksFloatValueType
    let linksUnitType = ValType (NumberType (NInt, L32))
    let irPrimitiveType2Wasm: CommonTypes.Primitive.t -> valType = fun p ->
      let open CommonTypes.Primitive in
      match p with
      | Bool   
      | Int    
      | Char    -> linksIntValueType
      | Float   -> linksFloatValueType
      | XmlItem 
      | DB      
      | String  -> raise (NotImplemented __LOC__)
    let toValType t =
      match t with
      | ValType v -> v 
      | HeapType _ -> raise (NotImplemented __LOC__)
      | FuncType _ -> raise (NotImplemented __LOC__)
      | MemoryType _ -> raise (NotImplemented __LOC__)
      | TableType _ -> raise (NotImplemented __LOC__)
      | GlobalType _ -> raise (NotImplemented __LOC__)
      | ErrorType -> raise (NotImplemented __LOC__)
      | UnitType -> raise (NotImplemented __LOC__)
      | _ -> assert false

    module PPInstruction = struct
      open CommonTypes.Constant
      open Var.Scope
      let getBinderName: Ir.binder -> string = fun binder ->
        let mangleIrLocalName p = "_loc_" ^ string_of_int p in
        let mangleIrGlobalName p = "_glob_" ^ string_of_int p in
        let mangleIrVarName: Var.var -> bool -> string = fun var isLocal ->
          if isLocal then mangleIrLocalName var
          else mangleIrGlobalName var in
        let mangleIrBinderName: Ir.binder -> string = fun binder ->
          mangleIrVarName (var_of_binder binder) (Scope.is_local (scope_of_binder binder)) 
        in
        let name = name_of_binder binder in
        if name = "" then mangleIrBinderName binder
        else name ^ "_" ^ (binder |> var_of_binder |> string_of_int)
      let getFunctionBinderName (binder: Ir.binder): string =
        let name = name_of_binder binder in
        let h = string_of_int (var_of_binder binder) in
        if name = "" then "_fun_" ^ h
        else name ^ "_" ^ h
      let prependDollar p = "$" ^ p
      let irVarName2Wasm: Ir.binder -> string = fun p -> prependDollar (getBinderName p)
      let flattenArgs: prettyPrintElement list list -> prettyPrintElement list = fun args ->
        List.fold_left (fun l r -> 
            match l with 
            | [] -> r
            | _ -> l@r
          ) [] args
      let flattenDefs: prettyPrintElement list -> prettyPrintElement list = fun args ->
        List.fold_left (fun l r ->
            match l with
            | [] -> [r]
            | _ -> l@[LineIndent; r]
          ) [] args
      let flattenParts: prettyPrintElement list list -> prettyPrintElement list = fun args ->
        List.fold_left (fun l r ->
          match l with
          | [] -> r
          | _ -> l@[LineIndent]@r
        ) [] args
      let inscArgs: printer -> prettyPrintElement list -> prettyPrintElement list list -> prettyPrintElement list = fun printer insc args ->          
        let inscArgs_inner: printer -> prettyPrintElement list -> prettyPrintElement list -> prettyPrintElement list = fun printer insc args ->
          match printer.argumentPrint with
          | StackStyle -> (
            match args with 
            | [] -> LineIndent::insc
            | _ -> args@[LineIndent]@insc
          )
          | ArgStyle -> 
            [LineIndent; Paren (insc@IncIndent::args@[DecIndent])]
        in
        inscArgs_inner printer insc (flattenArgs args)
      let inscLiteralArgs: printer -> string -> prettyPrintElement list list -> prettyPrintElement list = fun printer inscName args ->
        inscArgs printer [Literal inscName] args
      let inscLiteralArg0: printer -> string -> prettyPrintElement list = fun printer inscName ->
        inscArgs printer [Literal inscName] []
      let const: printer -> CommonTypes.Constant.t -> prettyPrintElement list = fun printer c ->
        let const1: printer -> valType -> string -> prettyPrintElement list = fun printer valueType stringValueOfConstant ->
          inscArgs printer [Literal (toStringValType valueType); Literal ".const"; IdSep; Literal stringValueOfConstant] []
        in
        let stringValueOfConstant = match c with
          | Bool value  -> string_of_bool value
          | Int value   -> string_of_int value
          | Char c      -> "\"" ^ Char.escaped c ^ "\""
          | String s    -> "\"" ^ escape_string s ^ "\""
          | Float value -> Utility.string_of_float' value
        in
        let valueType = c |> type_of |> irPrimitiveType2Wasm in
        const1 printer valueType stringValueOfConstant
      let readVar: printer -> Ir.binder -> prettyPrintElement list = fun printer binder ->
        inscArgs printer [Literal (match scope_of_binder binder with | Local -> "local.get" | Global -> "global.get"); IdSep; Literal (irVarName2Wasm binder)] []
      let writeVar: printer -> Ir.binder -> prettyPrintElement list -> prettyPrintElement list = fun printer binder arg1 ->
        inscArgs printer [Literal (match scope_of_binder binder with | Local -> "local.set" | Global -> "global.set"); IdSep; Literal (irVarName2Wasm binder)] [arg1]
    end

    let giveIndentString pt = 
      String.make (pt.indent * pt.indentWSNum) ' '
    let incrementIndent pt = 
      pt.indent <- pt.indent + 1
    let decrementIndent pt =
      pt.indent <- pt.indent - 1
    let give: printer -> string -> string = fun _pt some ->
      some
    let giveLine: printer -> string option -> string = fun pt some ->
      match some with
      | Some alp -> alp ^ (give pt (alp ^ "\n"))
      | None -> give pt "\n"
    let giveLineIndent pt =
      (giveLine pt None) ^ (giveIndentString pt)
    let giveIdSep: printer -> string = fun _ ->
      " "
    
                              
    let rec toString pt element =
      match element with
      | IncIndent -> incrementIndent pt; ""
      | DecIndent -> decrementIndent pt; ""
      | IdSep -> giveIdSep pt
      | Line -> giveLine pt None
      | LineIndent -> giveLineIndent pt
      | IncWithLineIndent -> 
          let _ = toString pt IncIndent in
          toString pt LineIndent
      | DecWithLineIndent -> 
          let _ = toString pt DecIndent in
          toString pt LineIndent
      | Paren es -> 
          "(" ^ (es |> List.map (fun e -> toString pt e) |> String.concat "") ^ ")"
      | Literal s -> give pt s
        
    type wasmWriter = {
      printer: printer;
      writer: out_channel;
      wasmModule: moduleDef;
      mutable funcMap: (funcDef * Ir.fun_def) list;
      mutable varMap: varBinderMap;
      primitiveFunctions: Var.var Env.String.t
    }
    let newWasmWriter printer outChannel moduleDef = {
      printer = printer;
      writer = outChannel;
      wasmModule = moduleDef;
      funcMap = [];
      varMap = BinderMap.empty;
      primitiveFunctions = Lib.nenv
    }
    let addBinder writer binder = 
      let var = Var.var_of_binder binder in
      writer.varMap <- BinderMap.add var binder writer.varMap
    let findBinder: wasmWriter -> Var.var -> Ir.binder option = fun writer var ->
      BinderMap.find_opt var writer.varMap

    let rec irType2Wasm: Types.datatype -> type_ = fun irType ->
      let irMetaType2Wasm = fun point ->
        (match Unionfind.find point with
         | Var _ -> raise (NotImplemented __LOC__)
         | Closed -> raise (NotImplemented __LOC__)
         | Recursive _ -> raise (NotImplemented __LOC__)
         | t -> Printf.printf "%s\n%!" __LOC__; irType2Wasm t
        ) in
      let irRecordType2Wasm row = Printf.printf "%s\n%!" __LOC__; irType2Wasm row in
      let rec getResult (f: typ) =
        match f with
        | Function (_, _, outType) -> getResult outType
        | Primitive p -> irPrimitiveType2Wasm p 
        | _ -> raise (NotImplemented __LOC__)
      in
      match irType with
      | Primitive p -> ValType (irPrimitiveType2Wasm p)
      | Function _ ->
        (* let inType' = Printf.printf "%s\n%!" __LOC__; irType2Wasm inType in
        let outType' = Printf.printf "%s\n%!" __LOC__; irType2Wasm outType in
        foldLeft inType' outType' *)
        Printf.printf "%s\n%!" __LOC__;
        let resultType = getResult irType in
        FuncType {
          p = []; r = [resultType]
        }
      | Effect _ -> raise (NotImplemented __LOC__)
      | Var _ -> raise (NotImplemented __LOC__)
      | Recursive _ -> raise (NotImplemented __LOC__)
      | Not_typed -> raise (NotImplemented __LOC__)
      | Alias _ -> raise (NotImplemented __LOC__)
      | Application _ -> raise (NotImplemented __LOC__)
      | RecursiveApplication _ -> raise (NotImplemented __LOC__)
      | Meta point -> irMetaType2Wasm point
      | Lolli _ -> raise (NotImplemented __LOC__)
      | Record row -> irRecordType2Wasm row
      | Variant _ -> raise (NotImplemented __LOC__)
      | Table _ -> raise (NotImplemented __LOC__)
      | Lens _ -> raise (NotImplemented __LOC__)
      | ForAll (_quantifiers, underlyingType) -> Printf.printf "%s\n%!" __LOC__; irType2Wasm underlyingType
      | Row _ -> Printexc.print_backtrace stdout; raise (NotImplemented __LOC__)
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
    let rec getResultType = function
      | Function (_, _, resultType) -> Printf.printf "%s\n%!" __LOC__; irType2Wasm resultType 
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
      | ForAll (_quantifiers, underlyingType) -> getResultType underlyingType
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
    let irFunc2Wasm: moduleDef -> Ir.fun_def -> funcDef = fun wasmModule irFunc ->
      let functionBinder = irFunc.fn_binder in
      let params = 
        irFunc.fn_params 
        |> List.map (fun binder -> type_of_binder binder, binder)
        |> List.map (fun (irType, binder) -> 
          let wasmType = Printf.printf "%s\n%!" __LOC__; irType2Wasm irType in Some (PPInstruction.getBinderName binder), toValType wasmType
        )
      in
      let result = getResultType (type_of_binder functionBinder) |> toValType in
      let newFun = declareFun wasmModule (Some (PPInstruction.getFunctionBinderName functionBinder)) params [result] in
      newFun
      
    let irValueIsUnit: Ir.value -> bool = fun t ->
      match t with
      | Extend (nameMap, value) -> 
        if Utility.StringMap.is_empty nameMap then (
          match value with 
          | None -> true
          | _ -> false
        )
        else false
      | _ -> false
      
    type primitiveWasmFuncArity =
      | Zero of string
      | One of string
      | Two of string
      | Three of string
      | Any of string

    open PPInstruction
    let primitiveIrFuncTable =
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
        "intToFloat", Zero ((toStringValType linksFloatValueType) ^ ".convert_" ^ (toStringValType linksIntValueType) ^ "_s");
        "floatToInt", Zero ((toStringValType linksIntValueType) ^ ".trunc_sat_" ^ (toStringValType linksFloatValueType) ^ "_s");
      ] in
      Utility.StringMap.from_alist table
    let findPrimitiveFuncName (v: Var.var): primitiveWasmFuncArity option = Utility.StringMap.find_opt (primitive_name v) primitiveIrFuncTable
    let nenv' = Env.String.fold (fun key value acc -> Env.Int.bind value key acc) Lib.nenv Env.Int.empty

    let rec typeOfApply writer appliedValue _args =
      match appliedValue with
      | Variable v -> 
        let retType1 = 
        if is_primitive_var v then Env.String.find (Env.Int.find v nenv') Lib.type_env
        else v |> findBinder writer |> nonNull |> type_of_binder 
        in getResultType retType1
      | Extend _ -> linksUnitType
      | ApplyPure (appliedValue, args) -> (
        match appliedValue with
        | TApp (appliedValue1, _) -> typeOfApply writer appliedValue1 args
        | _ -> raise (NotImplemented __LOC__)
      )
      | _ -> raise (NotImplemented __LOC__)
    and typeOfValue writer value =
      let typeOfPrimitiveValue var = Env.String.find (Env.Int.find var nenv') Lib.type_env |> (let _ = Printf.printf "%i %s\n%!" var __LOC__ in irType2Wasm )in
      match value with
      | Constant t -> (match t with
        | Bool _
        | Int _
        | Char _ -> linksIntType
        | String _ -> raise (NotImplemented __LOC__)
        | Float _ -> linksFloatType
      )
      | Variable v -> 
        if is_primitive_var v then typeOfPrimitiveValue v
        else findBinder writer v |> nonNull |> type_of_binder |> (let _ = Printf.printf "%s\n%!" __LOC__ in irType2Wasm)
      | Extend _ -> if irValueIsUnit value then linksUnitType else raise (NotImplemented __LOC__)
      | Project _ -> raise (NotImplemented __LOC__)
      | Erase _ -> raise (NotImplemented __LOC__)
      | Inject _ -> raise (NotImplemented __LOC__)
      | TAbs _ -> raise (NotImplemented __LOC__)
      | TApp _ -> raise (NotImplemented __LOC__)
      | XmlNode _ -> raise (NotImplemented __LOC__)
      | ApplyPure (appliedValue, args) -> (
        match appliedValue with
        | TApp (appliedValue1, _) -> typeOfApply writer appliedValue1 args
        | _ -> raise (NotImplemented __LOC__)
      )
      | Closure _ -> raise (NotImplemented __LOC__)
      | Coerce _ -> raise (NotImplemented __LOC__)

    let isConstant0 (v: Ir.value) =
      match v with
      | Constant c -> (
        match c with
        | Char f -> Char.code f = 0
        | Int i -> i = 0
        | Bool b -> b = false
        | _ -> false
      )
      | _ -> false
    let primitiveIrFuncToWasmInsc (v: var) (operands: Ir.value list) (operatorTypes: valType list) =
      let open Lib in
      if is_primitive_var v then 
        let operatorName = findPrimitiveFuncName v in
        match operatorName with 
        | Some op -> (
          let opTypeName = List.hd operatorTypes |> toStringValType in
          let operatorName1 = 
            match List.hd operatorTypes with
            | NumberType (n1, _) -> (
              match n1 with
              | NInt -> (
                let op1 = 
                  match op with
                  | Two op3 -> 
                    if op3 = "lt" || op3 = "gt" || op3 = "le" || op3 = "ge" then Two (op3 ^ "_s") 
                    else if op3 = "eq" && List.exists isConstant0 operands then One "eqz" 
                    else op
                  | _ -> op
                in op1
              )
              | _ -> op
            )
            | _ -> op
          in
          let temp t = Some (opTypeName ^ "." ^ t) in
          match operatorName1 with
          | Zero t -> Some t
          | One t -> temp t
          | Two t -> temp t
          | Three t -> temp t
          | Any t -> temp t
        )
        | None -> None
      else None

    let rec writePrimitiveIrFuncCall writer wasmFunc v operands =
      let operatorTypes = operands |> List.map (fun t -> typeOfValue writer t |> toValType) in
      (* let operatorTypes = operands |> List.map (fun t -> linksIntValueType) in *)
      let inscName = primitiveIrFuncToWasmInsc v operands operatorTypes in
      let writeArg = writeValue writer wasmFunc in
      let elements = match inscName with
        | Some name -> 
          if SysExt.getExtension name = ".eqz" then
            let arg0 = List.nth operands 0 in
            let arg1 = List.nth operands 1 in
            if isConstant0 arg0 then Some (inscLiteralArgs writer.printer name [arg1 |> writeArg])
            else Some (inscLiteralArgs writer.printer name [arg0 |> writeArg])
          else
            Some (inscLiteralArgs writer.printer name (List.map writeArg operands))
        | None -> None
      in elements
    and writeNormalIrFuncCall writer wasmFunc v operands =
      let binder = findBinder writer v |> (fun t -> match t with | Some t -> t | None -> raise (NotImplemented (string_of_var v))) in
      let inscLit = getFunctionBinderName binder in
      inscArgs writer.printer [Literal "call"; IdSep; Literal (prependDollar inscLit)] (List.map (writeValue writer wasmFunc) operands)
    and writeIrFuncCall writer wasmFunc (v: Var.var) operands =
      let try1 = writePrimitiveIrFuncCall writer wasmFunc v operands in
      let try2 = match try1 with
      | Some s -> s 
      | None -> writeNormalIrFuncCall writer wasmFunc v operands
      in try2
    and writeComputation: wasmWriter -> funcDef -> computation -> prettyPrintElement list = fun writer wasmFunc comp ->
      let (bindings, tail) = comp in
      let b1 = bindings |> List.map (fun t -> writeBinding writer wasmFunc t) |> List.flatten in
      let t1 = writeTailComputation writer wasmFunc tail in
      b1@t1
    and writeTailComputation: wasmWriter -> funcDef -> tail_computation -> prettyPrintElement list = fun writer wasmFunc irInsc ->
      let g1 = match irInsc with
        | Return v -> 
            if irValueIsUnit v then []
            else writeValue writer wasmFunc v
        | Apply (f, args) -> writeApply writer wasmFunc f args
        | Special s -> writeSpecial writer wasmFunc s 
        | Case _ -> raise (NotImplemented __LOC__)
        | If (value, ifIns, elseIns) -> writeIfExpression writer wasmFunc value ifIns elseIns
      in g1
    and writeIfExpression writer wasmFunc value ifIns elseIns =
      let valueElements =
      match writer.printer.ctrlInscStyle with
      | FoldedStyle ->
        let oldStyle = writer.printer.argumentPrint in
        writer.printer.argumentPrint <- ArgStyle;
        let a = writeValue writer wasmFunc value in
        writer.printer.argumentPrint <- oldStyle;
        a
      | PlainStyle -> writeValue writer wasmFunc value
      in
      let ifElements = writeComputation writer wasmFunc ifIns in
      let elseElements = writeComputation writer wasmFunc elseIns in
      let (_, ifResult) = ifIns in
      let ifResultType = typeOfTailComputation writer ifResult in
      let (_, elseResult) = elseIns in
      let elseResultType = typeOfTailComputation writer elseResult in
      let _ = if ifResultType = elseResultType then ()
        else raise (NotImplemented "if else type mismatch")
      in
      let resultTypeElement = Paren [Literal "result"; IdSep; Literal (Grammar.toString ifResultType)] in
      match writer.printer.ctrlInscStyle with
      | PlainStyle -> (
        let all = valueElements@[LineIndent; Literal "if"; IdSep; resultTypeElement; IncIndent]@ifElements@[DecWithLineIndent; Literal "else"; IncIndent]@elseElements@[DecWithLineIndent; Literal "end"] in
        all
      )
      | FoldedStyle -> (
        let connect keywordName elements =
          if List.length elements <= 1 then Paren ([Literal keywordName; IdSep]@elements)
          else Paren ([Literal keywordName; IncIndent]@elements@[DecWithLineIndent])
        in
        let thenElement = connect "then" ifElements in
        let elseElement = connect "else" elseElements in
        let all = Paren ([Literal "if"; IdSep; resultTypeElement; IncIndent]@valueElements@[LineIndent; thenElement; LineIndent; elseElement; DecWithLineIndent]) in
        [LineIndent; all]
      )
    and typeOfTailComputation (writer: wasmWriter) (comp: tail_computation) =
      match comp with
      | Return v -> typeOfValue writer v
      | Apply (func, value) -> (
        match func with
        | TApp (funcValue, _) -> typeOfApply writer funcValue value
        | _ -> raise (NotImplemented __LOC__)
      )
      | Special _ -> linksIntType
      | Case _ -> linksIntType
      | If _ -> linksIntType
    and writeApply: wasmWriter -> funcDef -> Ir.value -> Ir.value list -> prettyPrintElement list = fun writer wasmFunc f args ->
      writeApplyPure writer wasmFunc f args
    and writeApplyPure: wasmWriter -> funcDef -> Ir.value -> Ir.value list -> prettyPrintElement list = fun writer wasmFunc value args ->
      match value with
      | TApp (appliedValue, _) -> (
        match appliedValue with
        | Variable v -> writeIrFuncCall writer wasmFunc v args
        | _ -> raise (NotImplemented __LOC__)
      )
      | _ -> raise (NotImplemented __LOC__)
    and writeSpecial: wasmWriter -> funcDef -> Ir.special -> prettyPrintElement list = fun _writer _wasmFunc _special ->
      raise (NotImplemented __LOC__)
    and writeValue: wasmWriter -> funcDef -> Ir.value -> prettyPrintElement list = fun writer wasmFunc irValue ->
      match irValue with
      | Constant c -> writeConstant writer c
      | Variable v -> writeReadVar writer (findBinder writer v |> (fun t -> match t with | Some t -> t | None -> raise (NotImplemented (string_of_int v))))
      | ApplyPure (f, args) -> writeApplyPure writer wasmFunc f args
      | Project  _ -> raise (NotImplemented __LOC__)
      | Extend _ -> raise (NotImplemented __LOC__)
      | Erase  _ -> raise (NotImplemented __LOC__)
      | Inject  _ -> raise (NotImplemented __LOC__)
      | TAbs  _ -> raise (NotImplemented __LOC__)
      | TApp   _ -> raise (NotImplemented __LOC__)
      | XmlNode _ -> raise (NotImplemented __LOC__)
      | Closure _ -> raise (NotImplemented __LOC__)
      | Coerce _ -> raise (NotImplemented __LOC__)
    and writeConstant: wasmWriter -> CommonTypes.Constant.t -> prettyPrintElement list = fun writer constant ->
      const writer.printer constant
    and writeReadVar: wasmWriter -> Ir.binder -> prettyPrintElement list = fun writer binder ->
      readVar writer.printer binder
    and writeLetBinding: wasmWriter -> funcDef -> Ir.binder -> Ir.tyvar list -> Ir.tail_computation -> prettyPrintElement list = fun writer wasmFunc binder _typeVarList tailComputation ->
      let localName = getBinderName binder in
      let localType = Printf.printf "%i%s\n%!" (var_of_binder binder) __LOC__; irType2Wasm (type_of_binder binder) |> toValType in
      let _ = addBinder writer binder in
      let _ = 
        if scope_of_binder binder |> Scope.is_local then addLocal wasmFunc (Some localName) localType 
        else addGlobal writer.wasmModule (Some localName) localType 
      in
      let comp = writeTailComputation writer wasmFunc tailComputation in
      writeVar writer.printer binder comp
    and writeBinding: wasmWriter -> funcDef -> Ir.binding -> prettyPrintElement list = fun writer wasmFunc binding ->
      let binding' = match binding with
        | Let (binder, (typeVarList, tailComputation)) -> writeLetBinding writer wasmFunc binder typeVarList tailComputation
        | Fun funDef -> 
            collectIrFun writer funDef; []
        | Rec funDefs ->
            List.iter (collectIrFun writer) funDefs; []
      in binding'
    and collectIrFun: wasmWriter -> Ir.fun_def -> unit = fun writer irFunc ->
      let wasmFunc = irFunc2Wasm writer.wasmModule irFunc in
      irFunc.fn_params |> List.iter (addBinder writer);
      writer.funcMap <- writer.funcMap@[wasmFunc, irFunc];
      addBinder writer irFunc.fn_binder

    let writeLocal: wasmWriter -> string option * valType -> prettyPrintElement list = fun _writer local ->
      [IdSep]@(
        let (possibleName, _) = local in
        match possibleName with
        | Some s -> [Literal (prependDollar s); IdSep]
        | None -> []
      )@[Literal (let (_, valType) = local in toStringValType valType)]
    let writeGlobal (_writer: wasmWriter) globalName valType isMutable =
      let nameLiterals = match globalName with
      | Some s -> [Literal (prependDollar s); IdSep]
      | None -> [] in
      let typeLiterals = Literal (toStringValType valType) in
      let typeLiterals = if isMutable then Paren [Literal "mut"; IdSep; typeLiterals] else typeLiterals in
      let initLiterals = Paren [Literal (toStringValType valType ^ ".const"); IdSep; Literal "0"] in
      let elements = [Literal "global"; IdSep]@nameLiterals@[typeLiterals; IdSep; initLiterals] in
      Paren elements
      
    let writeWasmFunTitleParams writer wasmFunc =
      let title = [Literal "func"; IdSep; Literal (prependDollar (nameOfFunc wasmFunc))] in
      let funcType' = wasmFunc.type_ in
      let params = (
        let p = funcType'.p in
        let p' = p 
          |> List.map (fun t -> [Literal "param"]@(writeLocal writer t))
          |> List.map (fun t -> [IdSep; Paren t])
          |> List.flatten in
        p'
      ) in
      title@params
    let writeWasmFunLocals writer wasmFunc =
      let locals = wasmFunc.locals 
        |> List.map (fun t -> Paren (Literal "local"::(writeLocal writer t)))
        |> List.fold_left (fun l r -> match l with
            | [] -> [r]
            | _ -> l@[IdSep; r]
          ) [] in
      let locals = match locals with
        | [] -> [IncIndent]
        | _ -> IncWithLineIndent::locals in
      locals
    let writeWasmFunWith: wasmWriter -> funcDef -> prettyPrintElement list -> prettyPrintElement = fun writer wasmFunc body ->
      let titleParams = writeWasmFunTitleParams writer wasmFunc in
      let resultType = List.map toStringValType wasmFunc.type_.r |> List.map (fun t -> [IdSep; Literal t]) |> List.flatten in
      let resultType2 = match resultType with
        | [] -> []
        | _ -> [IdSep; Paren ([Literal "result"]@resultType)] in
      let locals = writeWasmFunLocals writer wasmFunc in
      let funBody = titleParams@resultType2@locals@body@[DecWithLineIndent] in
      Paren funBody
    let writeMainFunc writer wasmFunc body =
      let (bindings, tail) = body in
      let bindings = bindings |> List.map (writeBinding writer wasmFunc) |> List.flatten in
      let tailComputation = tail |> writeTailComputation writer wasmFunc in
      let pop = if typeOfTailComputation writer tail = linksUnitType then [] else inscLiteralArg0 writer.printer "drop" in
      let body = bindings@tailComputation@pop in
      let titleParams = writeWasmFunTitleParams writer wasmFunc in
      (* let resultType = tail |> typeOfTailComputation writer |> toValType |> toStringValType in
      let resultType1 = [IdSep; Literal resultType] in *)
      let resultType1 = [] in
      let locals = writeWasmFunLocals writer wasmFunc in
      let funBody = titleParams@resultType1@locals@body@[DecWithLineIndent] in
      Paren funBody
    let writeFunctionComputation: wasmWriter -> funcDef -> computation -> prettyPrintElement = fun writer wasmFunc body ->
      let (bindings, tail) = body in
      let bindings = bindings |> List.map (writeBinding writer wasmFunc) |> List.flatten in
      let tailComputation = tail |> writeTailComputation writer wasmFunc in
      writeWasmFunWith writer wasmFunc (bindings@tailComputation)
    let writeWasmFun: wasmWriter -> funcDef -> Ir.fun_def -> prettyPrintElement = fun writer wasmFunc irFunc ->
      writeFunctionComputation writer wasmFunc irFunc.fn_body
    let writeGlobals writer =
      let globals = writer.wasmModule.gl in
      let globalElements = List.map (fun (t: globalDef) -> writeGlobal writer t.id t.type_.t t.type_.isMutable) globals in
      flattenDefs globalElements
    let writeProgram: wasmWriter -> Ir.program -> prettyPrintElement = fun writer program ->
      let wasmModule = writer.wasmModule in
      let mainFunc = nonNull wasmModule.st in
      let mainFunc1 = writeMainFunc writer mainFunc program in
      let functions = writer.funcMap |> List.map (fun t -> let (wasmFunc, irFunc) = t in writeWasmFun writer wasmFunc irFunc) in
      let functions = functions@[mainFunc1] |> flattenDefs in
      let globals = writeGlobals writer in
      let moduleDefs = flattenParts [globals; functions] in
      if writer.printer.abbreviateSingleModuleDef then Paren moduleDefs
      else (
        let g1 = [Literal "module"; IdSep] in
        let g2 = g1@[Literal (prependDollar wasmModule.id)] in
        let g3 = g2@[IncWithLineIndent]@moduleDefs@[DecWithLineIndent] in
        let g4 = Paren g3 in
        g4
      )

    let writeIr2Wasm writer program =
      let ppe = writeProgram writer program in
      toString writer.printer ppe |> Printf.fprintf writer.writer "%s";
      close_out writer.writer
  end
end



let run (result: Backend.result) outputWat =
  let open Wasm.PrettyPrinting in
  let program = result.program in
  let outputStream = open_out outputWat in
  let writer = Wasm.PrettyPrinting.newWasmWriter (Wasm.PrettyPrinting.defaultPrinter ()) outputStream (Wasm.Grammar.newModule "defaultModule") in
  let _ = writeIr2Wasm writer program in
  let outputStream2 = open_out ((SysExt.getFileNameWithoutExtension outputWat) ^ "-reverse-style.wat") in
  let writer2 = Wasm.PrettyPrinting.newWasmWriter (Wasm.PrettyPrinting.reverseStylePrinter ()) outputStream2 (Wasm.Grammar.newModule "defaultModule") in
  let _ = writeIr2Wasm writer2 program in
  ()
