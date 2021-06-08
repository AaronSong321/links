
(* open Irtojs *)

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
end
let (>>): ('a->'b) -> ('b->'c) -> ('a->'c) = fun a b ->
  fun m -> m |> a |> b

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
    let nameOfFunc: funcDef -> string = fun f ->
      match f.id with
      | Some s -> s 
      | None -> raise (NotImplemented __LOC__)
    
    let foldLeft: type_ -> type_ -> type_ = fun oldType resultType ->
      let toCombineType: type_ -> type_ option = fun ot ->
        match ot with 
        | ErrorType -> Some ErrorType
        | UnitType -> None
        | _ -> Some ot
      in

      let params = match oldType with 
        | FuncType oldFuncType -> (
            let p=oldFuncType.p in
            let r=oldFuncType.r in
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
      | Some params', UnitType -> FuncType {p = params'; r = []}
      | Some params', ValType t -> FuncType { p = params'; r = [t]}
      | _, _ -> ErrorType


    let addLocal: funcDef -> string option -> valType -> unit = fun f id v ->
      f.locals <- f.locals@[id,v]
    let newFuncDef id funcType = {
      id = id;
      type_ = funcType;
      locals = [];
      body = []
    }
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
    type startPoint = {
      func: funcDef
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
      mutable st: startPoint option;
    } 
    let declareFun: moduleDef -> string option -> paramAnnotator -> resultAnnotator -> funcDef = fun module_ id p r ->
      let funcDef_ = newFuncDef id ({ p = p; r = r }) in
      let _ = module_.fn = module_.fn@[funcDef_] in
      funcDef_
    let newModule id = 
      let linksFileFunc = newFuncDef (Some "_links_wasm_fileFunc") ({
          p = []; r = [] } ) in
      {
        id = id; 
        ty = [];
        fn = [linksFileFunc];
        tb = [];
        me = [];
        gl = [];
        ex = [];
        st = Some { func = linksFileFunc }
      }
    let mainFuncOfModule m = 
      match m.st with
      | Some s -> s 
      | None -> assert false
      
  end
  
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
      | PPE_Par of string option * string
      | PPE_RetType of string
      | IdSep
    type instructionPrintStyle =
      | PlainStyle
      | FoldedStyle
    type argumentPrintStyle =
      | StackStyle 
      | ArgStyle

    type printer = {
      mutable indent:int;
      indentWSNum:int;
      style:instructionPrintStyle;
      argumentPrint: argumentPrintStyle;
      abbreviateSingleModuleDef: bool
    }
    let defaultPrinter () = { 
      indent = 0; 
      indentWSNum = 2; 
      style = PlainStyle;
      argumentPrint = StackStyle;
      abbreviateSingleModuleDef = true
    }
  
    let irPrimitiveType2Wasm: CommonTypes.Primitive.t -> valType = fun p ->
      let open CommonTypes.Primitive in
      match p with
      | Bool   
      | Int    
      | Char    -> (NumberType (NInt, L32))
      | Float   -> (NumberType (NFloat, L64))
      | XmlItem 
      | DB      
      | String  -> raise (NotImplemented __LOC__)

    module PPInstruction = struct
      open CommonTypes.Constant
      let mangleLocalName: Var.var -> string = fun p -> "_irloc_" ^ (string_of_int p)
      let irLocalName2Wasm: Var.var -> string = fun p -> "$" ^ (mangleLocalName p)
      let flattenArgs: prettyPrintElement list list -> prettyPrintElement list = fun args ->
        List.fold_left (fun l r -> 
          match l with 
          | [] -> r
          | _ -> l@[LineIndent]@r
        ) [] args
      let flattenInscs: prettyPrintElement list list -> prettyPrintElement list = fun args ->
        List.fold_left (fun l r -> 
          match l with 
          | [] -> r
          | _ -> l@[LineIndent]@r
        ) [] args
      let flattenDefs: prettyPrintElement list -> prettyPrintElement list = fun args ->
        List.fold_left (fun l r ->
          match l with
          | [] -> [r]
          | _ -> l@[LineIndent; r]
        ) [] args
      let inscArgs: printer -> prettyPrintElement list -> prettyPrintElement list list -> prettyPrintElement list = fun printer insc args ->
        let inscArgs_inner: printer -> prettyPrintElement list -> prettyPrintElement list -> prettyPrintElement list = fun printer insc args ->
          match printer.argumentPrint with
          | StackStyle ->
              args@[LineIndent]@insc
          | ArgStyle -> 
              [Paren (insc@IncWithLineIndent::args@[DecWithLineIndent])]
        in
        inscArgs_inner printer insc (flattenArgs args)
      let inscLiteralArgs: printer -> string -> prettyPrintElement list list -> prettyPrintElement list = fun printer inscName args ->
        inscArgs printer [Literal inscName] args
      let inscLiteralArg0: printer -> string -> prettyPrintElement list = fun printer inscName ->
        [Literal inscName]
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
      let add: printer -> valType -> prettyPrintElement list -> prettyPrintElement list -> prettyPrintElement list = fun printer valueType arg1 arg2 ->
        let inscName = (toStringValType valueType) ^ ".add" in
        inscLiteralArgs printer inscName [arg1; arg2]
      let readVar: printer -> Var.var -> prettyPrintElement list = fun printer var ->
        inscArgs printer [Literal "local.get"; IdSep; Literal (irLocalName2Wasm var)] []
      let writeVar: printer -> Var.var -> prettyPrintElement list -> prettyPrintElement list = fun printer var arg1 ->
        inscArgs printer [Literal "local.set"; IdSep; Literal (irLocalName2Wasm var)] [arg1]
    end

    let giveIndentString pt = 
      String.make (pt.indent*pt.indentWSNum) ' '
    let incrementIndent pt = 
      pt.indent<-pt.indent+1
    let decrementIndent pt =
      pt.indent<-pt.indent-1
    let give:printer->string->string = fun pt some ->
      some
    let giveLine: printer->string option->string = fun pt some ->
      match some with
      | Some alp -> alp ^ (give pt (alp ^ "\n"))
      | None -> give pt "\n"
    let giveLineIndent pt =
      (giveIndentString pt) ^ (giveLine pt None)
    let giveIdSep _ =
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
      | IdSep -> giveIdSep pt
        
    type wasmWriter = {
      printer: printer;
      writer: out_channel;
      wasmModule: moduleDef;
      mutable funcMap: (funcDef * Ir.fun_def) list
    }

    let rec irType2Wasm: Types.datatype -> valType = fun irType ->
      let open CommonTypes in
      match irType with
      | Primitive p_ -> irPrimitiveType2Wasm p_ 
      (* | Function functionType -> 
          let inType,effect,outType = functionType in
          let inWasmType = irType2Wasm inType in
          let outWasmType = irType2Wasm outType in
          foldLeft inWasmType outWasmType *)
      | Function irFuncType ->
        let (inType, effect, outType) = irFuncType in
        Printf.printf "%s -> %s" (Types.string_of_datatype inType) (Types.string_of_datatype outType);
        raise (NotImplemented __LOC__)
      | Effect _ -> raise (NotImplemented __LOC__)
      | Var _ -> raise (NotImplemented __LOC__)
      | Recursive _ -> raise (NotImplemented __LOC__)
      | Not_typed -> raise (NotImplemented __LOC__)
      | Var _ -> raise (NotImplemented __LOC__)
      | Recursive _ -> raise (NotImplemented __LOC__)
      | Alias _ -> raise (NotImplemented __LOC__)
      | Application _ -> raise (NotImplemented __LOC__)
      | RecursiveApplication _ -> raise (NotImplemented __LOC__)
      | Meta _ -> raise (NotImplemented __LOC__)
      | Lolli _ -> raise (NotImplemented __LOC__)
      | Record _ -> raise (NotImplemented __LOC__)
      | Variant _ -> raise (NotImplemented __LOC__)
      | Table _ -> raise (NotImplemented __LOC__)
      | Lens _ -> raise (NotImplemented __LOC__)
      | ForAll _ -> raise (NotImplemented __LOC__)
      (* Effect *)
      | Effect _ -> raise (NotImplemented __LOC__)
      (* Row *)
      | Row _ -> raise (NotImplemented __LOC__)
      | Closed _ -> raise (NotImplemented __LOC__)
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
      let open Var in
      let functionBinder = (Printf.printf "%s\n%!" __LOC__; irFunc.fn_binder) in
      let params = 
        irFunc.fn_params 
        |> List.map (fun binder -> type_of_binder binder, name_of_binder binder)
        |> List.map (fun (irType, name) -> 
            let wasmType = irType2Wasm irType in Some name, wasmType
          )
      in
        (* if List.any (fun t -> match t with | ErrorType -> true | _ -> false) params then ErrorType *)
      let result = functionBinder |> type_of_binder |> irType2Wasm in
      let newFun = declareFun wasmModule (Some (name_of_binder functionBinder)) params [result] in
      newFun
      

    let rec writeComputation: wasmWriter -> funcDef -> Ir.computation -> prettyPrintElement list = fun writer wasmFunc comp ->
      let (bindings, tail) = comp in
      let b1 = bindings |> List.map (fun t -> writeBinding writer wasmFunc t) |> List.flatten in
      let t1 = tail |> writeTailComputation writer wasmFunc in
      b1@t1
    and writeTailComputation: wasmWriter -> funcDef -> Ir.tail_computation -> prettyPrintElement list = fun writer wasmFunc irInsc ->
      let open Ir in
      let g1 = match irInsc with
        | Return v -> writeValue writer wasmFunc v
        | Apply (f, args) -> writeApply writer wasmFunc f args
        | Special s -> writeSpecial writer wasmFunc s 
        | Case _ -> raise (NotImplemented __LOC__)
        (* | If (value, ifIns, elseIns) -> (
            let process = (List.map (fun t -> writeComputation writer wasmFunc t)) >> PPInstruction.flattenInscs in
            let ifWasm = process ifIns in
            let elseWasm = process elseIns in 
            let innerContent = (Literal "if")::IncWithLineIndent::ifWasm@elseWasm@[] in
            let retVal = [LineIndent; Paren innerContent] in
            retVal
          ) *)
        | If _ -> raise (NotImplemented __LOC__)
      in LineIndent::g1
    and writeApply: wasmWriter -> funcDef -> Ir.value -> Ir.value list -> prettyPrintElement list = fun writer wasmFunc f args ->
      let funToCall = match f with
        | Variable v -> [Literal "call"; IdSep; Literal (PPInstruction.irLocalName2Wasm v)] 
        | _ -> raise (NotImplemented __LOC__) in
      let args' = args |> List.map (fun t -> writeValue writer wasmFunc t) in
      let p = PPInstruction.inscArgs writer.printer funToCall args' in
      p
    and writeSpecial: wasmWriter -> funcDef -> Ir.special -> prettyPrintElement list = fun writer wasmFunc special ->
      raise (NotImplemented __LOC__)
    and writeValue: wasmWriter -> funcDef -> Ir.value -> prettyPrintElement list = fun writer wasmFunc irValue ->
      let open Ir in
      match irValue with
      | Constant c -> writeConstant writer c
      | Variable v -> writeReadVar writer v
      | ApplyPure (f, args) -> writeApply writer wasmFunc f args
      | Project  _ -> raise (NotImplemented __LOC__)
      | Extend _ -> raise (NotImplemented __LOC__)
      | Erase  _ -> raise (NotImplemented __LOC__)
      | Inject  _ -> raise (NotImplemented __LOC__)
      | TAbs  _ -> raise (NotImplemented __LOC__)
      | TApp   _ -> raise (NotImplemented __LOC__)
      | XmlNode _ -> raise (NotImplemented __LOC__)
      | Closure   _ -> raise (NotImplemented __LOC__)
      | Coerce _ -> raise (NotImplemented __LOC__)
    and writeConstant: wasmWriter -> CommonTypes.Constant.t -> prettyPrintElement list = fun writer constant ->
      PPInstruction.const writer.printer constant
    and writeReadVar: wasmWriter -> Var.var -> prettyPrintElement list = fun writer var ->
      PPInstruction.readVar writer.printer var
    and writeLetBinding: wasmWriter -> funcDef -> Ir.binder -> Ir.tyvar list -> Ir.tail_computation -> prettyPrintElement list = fun writer wasmFunc binder typeVarList tailComputation ->
      let open Var in
      let localName = PPInstruction.irLocalName2Wasm (var_of_binder binder) in
      let localType = irType2Wasm (type_of_binder binder) in
      let _ = wasmFunc.locals <- wasmFunc.locals @ [Some localName, localType] in
      let comp = writeTailComputation writer wasmFunc tailComputation in
      PPInstruction.writeVar writer.printer (var_of_binder binder) comp
    and writeBinding: wasmWriter -> funcDef -> Ir.binding -> prettyPrintElement list = fun writer wasmFunc binding ->
      let binding' = match binding with
        | Let (binder, (typeVarList, tailComputation)) -> writeLetBinding writer wasmFunc binder typeVarList tailComputation
        | Fun funDef -> 
            collectIrFun writer funDef; []
        | Rec funDefs ->
            List.iter (collectIrFun writer) funDefs; []
        (* write functions outside any function *)
      in LineIndent::binding'
    and collectIrFun: wasmWriter -> Ir.fun_def -> unit = fun writer irFunc ->
      let wasmFunc = irFunc2Wasm writer.wasmModule irFunc in
      writer.funcMap <- writer.funcMap@[wasmFunc, irFunc]

    let writeLocal: wasmWriter -> string option * valType -> prettyPrintElement list = fun writer local ->
      [IdSep]@(
        let (possibleName, _) = local in
        match possibleName with
        | Some s -> [Literal ("$"^s)]
        | None -> []
      )@[IdSep; Literal (let (_, valType) = local in toStringValType valType)]
      
    let writeWasmFunWith: wasmWriter -> funcDef -> prettyPrintElement list -> prettyPrintElement = fun writer wasmFunc body ->
      let title = [Literal "func"; IdSep; Literal (nameOfFunc wasmFunc)] in
      let funcType' = wasmFunc.type_ in
      let params = (
        let p = funcType'.p in
        let p' = p |> List.map (fun t -> [Literal "param"]@(writeLocal writer t))
          |> List.map (fun t -> [IdSep; Paren t])
          |> List.flatten in
        p'
      ) in
      let resultType = List.map toStringValType funcType'.r |> List.map (fun t -> [IdSep; Literal t]) |> List.flatten in
      let resultType2 = [IdSep; Paren ([Literal "result"; IdSep]@resultType)] in
      let locals = List.map (fun t -> [IdSep; Paren (Literal "local"::(writeLocal writer t))]) wasmFunc.locals |> List.flatten in
      let funBody = title@params@resultType2@[IncWithLineIndent]@locals@[LineIndent]@body@[DecWithLineIndent] in
      Paren funBody
    let writeWasmFun: wasmWriter -> funcDef -> Ir.fun_def -> prettyPrintElement = fun writer wasmFunc irFunc ->
      writeWasmFunWith writer wasmFunc (writeComputation writer wasmFunc irFunc.fn_body)

    let writeProgram: wasmWriter -> Ir.program -> prettyPrintElement = fun writer program ->
      let (bindings, tailComputation) = program in
      let wasmModule = writer.wasmModule in
      let mainFunc = match wasmModule.st with 
        | Some k -> k.func 
        | None -> exit 0
      in
      let bindings' = bindings |> List.map (writeBinding writer mainFunc) |> List.flatten in
      let tailComputation' = tailComputation |> writeTailComputation writer mainFunc in
      let mainFunc1 = writeWasmFunWith writer mainFunc (bindings'@tailComputation') in
      let functions = writer.funcMap |> List.map (fun t -> let (wasmFunc, irFunc) = t in writeWasmFun writer wasmFunc irFunc) in
      let allFunctions = functions@[mainFunc1] |> PPInstruction.flattenDefs in
      if writer.printer.abbreviateSingleModuleDef then Paren allFunctions
      else (
        let g1 = [Literal "module"; IdSep] in
        let g2 = g1@[Literal wasmModule.id] in
        let g3 = g2@[IncWithLineIndent]@allFunctions@[DecWithLineIndent] in
        let g4 = Paren g3 in
        g4
      )
    (* let writeGlobal: wasmWriter -> Types.datatype -> unit = fun writer datatype ->
    
      () *)
  end
end



let run : Backend.result -> unit = fun result ->
  let open Wasm.PrettyPrinting in
  let context = result.context in
  let datatype = result.datatype in
  let program = result.program in
  let wasmFileName = "a.wat" in
  let wasmFilePath = "./tests/wasm/" ^ wasmFileName in
  let outputStream = open_out wasmFilePath in
  let writer = Wasm.PrettyPrinting.{ printer = Wasm.PrettyPrinting.defaultPrinter (); writer = outputStream; wasmModule = Wasm.Grammar.newModule "defaultModule"; funcMap = [] } in
  let printer = writer.printer in
  let p = writeProgram writer program |> toString printer in
  Printf.fprintf outputStream "%s" p;
  close_out outputStream
