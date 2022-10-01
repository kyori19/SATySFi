
open LengthInterface
open GraphicBase
open SyntaxBase

exception ParseErrorDetail of Range.t * string
exception IllegalArgumentLength of Range.t * int * int


let string_of_uchar (uch : Uchar.t) : string =
  let buf = Buffer.create 10 in
  Buffer.add_utf_8_uchar buf uch;
  Buffer.contents buf


type 'a ranged =
  Range.t * 'a
[@@deriving show]

type command_name       = string  [@@deriving show]
type var_name           = string  [@@deriving show]
type id_name            = string  [@@deriving show]
type class_name         = string  [@@deriving show]
type type_name          = string  [@@deriving show]
type kind_name          = string  [@@deriving show]
type constructor_name   = string  [@@deriving show]
type macro_name         = string  [@@deriving show]
type module_name        = string  [@@deriving show]
type signature_name     = string  [@@deriving show]
type length_unit_name   = string  [@@deriving show]
type type_variable_name = string  [@@deriving show]
type row_variable_name  = string  [@@deriving show]
type label              = string  [@@deriving show]


type input_position = {
  input_file_name : string;
  input_line      : int;
  input_column    : int;
}
[@@deriving show { with_path = false }]

type header_element =
  | HeaderRequire of string
  | HeaderImport  of string


type quantifiability = Quantifiable | Unquantifiable
[@@deriving show]

type level = Level.t
[@@deriving show]


module StoreIDHashTable = Hashtbl.Make(StoreID)

module EvalVarIDMap = Map.Make(EvalVarID)

module OpaqueIDSet = Set.Make(TypeID)

type manual_type =
  Range.t * manual_type_main

and manual_type_main =
  | MTypeName          of (module_name ranged) list * type_name ranged * manual_type list
  | MTypeParam         of var_name
  | MFuncType          of (label ranged * manual_type) list * (row_variable_name ranged) option * manual_type * manual_type
  | MProductType       of manual_type TupleList.t
  | MRecordType        of (label ranged * manual_type) list * (row_variable_name ranged) option
  | MInlineCommandType of manual_command_argument_type list
  | MBlockCommandType  of manual_command_argument_type list
  | MMathCommandType   of manual_command_argument_type list
[@@deriving show]

and manual_command_argument_type =
  | MArgType of (label ranged * manual_type) list * manual_type

type manual_row_base_kind =
  (label ranged) list
[@@deriving show]

type manual_base_kind =
  | MKindName of kind_name ranged
[@@deriving show]

type manual_kind =
  | MKind of manual_base_kind list * manual_base_kind
[@@deriving show]

type base_type =
  | UnitType
  | BoolType
  | IntType
  | FloatType
  | LengthType
  | StringType
  | InlineTextType
  | BlockTextType
  | MathTextType
  | InlineBoxesType
  | BlockBoxesType
  | MathBoxesType
  | ContextType
  | PrePathType
  | PathType
  | GraphicsType
  | ImageType
  | DocumentType
  | RegExpType
  | TextInfoType
  | InputPosType
[@@deriving show]


module StringMap = struct
  include Map.Make(String)


  let pp_k ppf s =
    Format.fprintf ppf "\"%s\"" s


  let pp pp_v ppf oidmap =
    pp_map fold pp_k pp_v ppf oidmap
end

module ValueNameMap = StringMap

module TypeNameMap = StringMap

module ModuleNameMap = StringMap

module SignatureNameMap = StringMap

module MacroNameMap = StringMap

module ConstructorMap = StringMap


let base_type_map : base_type TypeNameMap.t =
  List.fold_left (fun map (tynm, bt) ->
    map |> TypeNameMap.add tynm bt
  ) TypeNameMap.empty
  [
    ("unit"          , UnitType);
    ("bool"          , BoolType);
    ("int"           , IntType);
    ("float"         , FloatType);
    ("length"        , LengthType);
    ("string"        , StringType);
    ("inline-text"   , InlineTextType);
    ("block-text"    , BlockTextType);
    ("math-text"     , MathTextType);
    ("inline-boxes"  , InlineBoxesType);
    ("block-boxes"   , BlockBoxesType);
    ("math-boxes"    , MathBoxesType);
    ("context"       , ContextType);
    ("pre-path"      , PrePathType);
    ("path"          , PathType);
    ("graphics"      , GraphicsType);
    ("image"         , ImageType);
    ("document"      , DocumentType);
    ("regexp"        , RegExpType);
    ("text-info"     , TextInfoType);
    ("input-position", InputPosType);
  ]


type row_base_kind =
  LabelSet.t
[@@deriving show { with_path = false }]

type base_kind =
  | TypeKind
[@@deriving show { with_path = false }]

type kind =
  | Kind of base_kind list
[@@deriving show { with_path = false }]

type ('a, 'b) typ =
  Range.t * ('a, 'b) type_main

and ('a, 'b) type_main =
  | BaseType          of base_type
  | FuncType          of ('a, 'b) row * ('a, 'b) typ * ('a, 'b) typ
  | ListType          of ('a, 'b) typ
  | RefType           of ('a, 'b) typ
  | ProductType       of (('a, 'b) typ) TupleList.t
  | TypeVariable      of 'a
  | DataType          of (('a, 'b) typ) list * TypeID.t
  | RecordType        of ('a, 'b) row
  | InlineCommandType of (('a, 'b) command_argument_type) list
  | BlockCommandType  of (('a, 'b) command_argument_type) list
  | MathCommandType   of (('a, 'b) command_argument_type) list
  | CodeType          of ('a, 'b) typ

and ('a, 'b) command_argument_type =
  | CommandArgType of (('a, 'b) typ) LabelMap.t * ('a, 'b) typ

and ('a, 'b) row =
  | RowCons  of label ranged * ('a, 'b) typ * ('a, 'b) row
  | RowVar   of 'b
  | RowEmpty

and mono_row_variable_updatable =
  | MonoRowFree of FreeRowID.t
  | MonoRowLink of mono_row

and mono_row_variable =
  | UpdatableRow   of mono_row_variable_updatable ref
  | MustBeBoundRow of MustBeBoundRowID.t

and poly_row_variable =
  | PolyRowFree  of mono_row_variable
  | PolyRowBound of BoundRowID.t

and mono_type_variable_updatable =
  | MonoFree of FreeID.t
  | MonoLink of mono_type

and mono_type_variable =
  | Updatable   of mono_type_variable_updatable ref
  | MustBeBound of MustBeBoundID.t

and poly_type_variable =
  | PolyFree  of mono_type_variable_updatable ref
  | PolyBound of BoundID.t

and mono_type =
  (mono_type_variable, mono_row_variable) typ

and poly_type_body =
  (poly_type_variable, poly_row_variable) typ

and poly_type =
  | Poly of poly_type_body

and mono_row =
  (mono_type_variable, mono_row_variable) row
[@@deriving show { with_path = false }]

type poly_row =
  (poly_type_variable, poly_row_variable) row

type ('a, 'b) normalized_row =
  | NormalizedRow of (('a, 'b) typ) LabelMap.t * 'b option

type normalized_mono_row =
  (mono_type_variable, mono_row_variable) normalized_row

type normalized_poly_row =
  (poly_type_variable, poly_row_variable) normalized_row

type mono_command_argument_type =
  (mono_type_variable, mono_row_variable) command_argument_type

type poly_command_argument_type =
  (poly_type_variable, poly_row_variable) command_argument_type

type ('a, 'b) macro_parameter_type =
  | LateMacroParameter  of ('a, 'b) typ
  | EarlyMacroParameter of ('a, 'b) typ
[@@deriving show { with_path = false }]

type ('a, 'b) macro_type =
  | InlineMacroType of (('a, 'b) macro_parameter_type) list
  | BlockMacroType  of (('a, 'b) macro_parameter_type) list
[@@deriving show { with_path = false }]

type mono_macro_parameter_type =
  (mono_type_variable, mono_row_variable) macro_parameter_type
[@@deriving show { with_path = false }]

type poly_macro_parameter_type =
  (poly_type_variable, poly_row_variable) macro_parameter_type
[@@deriving show { with_path = false }]

type mono_macro_type =
  (mono_type_variable, mono_row_variable) macro_type
[@@deriving show { with_path = false }]

type poly_macro_type =
  (poly_type_variable, poly_row_variable) macro_type
[@@deriving show { with_path = false }]

type constructor_branch_map = poly_type ConstructorMap.t


let pp_sep fmt () =
  Format.fprintf fmt ";@ "


type stage =
  | Persistent0
  | Stage0
  | Stage1
[@@deriving show { with_path = false }]

module TypeParameterMap = Map.Make(String)

type type_parameter_map = MustBeBoundID.t TypeParameterMap.t

module RowParameterMap = Map.Make(String)

type row_parameter_map = MustBeBoundRowID.t RowParameterMap.t

type pre = {
  level           : level;
  type_parameters : type_parameter_map;
  row_parameters  : row_parameter_map;
  quantifiability : quantifiability;  (* TODO: maybe omitted in the future *)
  stage           : stage;
}


let string_of_stage = function
  | Persistent0 -> "persistent stage"
  | Stage0      -> "stage 0"
  | Stage1      -> "stage 1"


type untyped_macro_parameter =
  | UTLateMacroParam  of var_name ranged
  | UTEarlyMacroParam of var_name ranged
[@@deriving show { with_path = false; } ]

type module_name_chain =
  module_name ranged * (module_name ranged) list
[@@deriving show { with_path = false; } ]

type untyped_binding =
  untyped_binding_main ranged

and untyped_binding_main =
  | UTBindValue       of stage * untyped_rec_or_nonrec
  | UTBindType        of untyped_type_binding list
  | UTBindModule      of module_name ranged * untyped_signature option * untyped_module
  | UTBindSignature   of signature_name ranged * untyped_signature
  | UTBindInclude     of untyped_module
  | UTBindInlineMacro of command_name ranged * untyped_macro_parameter list * untyped_abstract_tree
  | UTBindBlockMacro  of command_name ranged * untyped_macro_parameter list * untyped_abstract_tree

and untyped_module =
  untyped_module_main ranged

and untyped_module_main =
  | UTModVar     of module_name_chain
  | UTModBinds   of untyped_binding list
  | UTModFunctor of module_name ranged * untyped_signature * untyped_module
  | UTModApply   of module_name_chain * module_name_chain
  | UTModCoerce  of module_name ranged * untyped_signature

and untyped_signature =
  untyped_signature_main ranged

and untyped_signature_main =
  | UTSigVar     of signature_name
  | UTSigPath    of module_name_chain * signature_name ranged
  | UTSigDecls   of untyped_declaration list
  | UTSigFunctor of module_name ranged * untyped_signature * untyped_signature
  | UTSigWith    of untyped_signature * (module_name ranged) list * untyped_type_binding list

and untyped_declaration =
  untyped_declaration_main
    (* TODO; should be `untyped_declaration_main ranged`  *)

and untyped_declaration_main =
  | UTDeclValue      of stage * var_name ranged * manual_quantifier * manual_type
  | UTDeclTypeOpaque of type_name ranged * manual_kind
  | UTDeclModule     of module_name ranged * untyped_signature
  | UTDeclSignature  of signature_name ranged * untyped_signature
  | UTDeclInclude    of untyped_signature
  | UTDeclMacro      of macro_name ranged * manual_macro_type ranged

and manual_quantifier =
  (type_variable_name ranged) list * (row_variable_name ranged * manual_row_base_kind) list

and manual_macro_type =
  | MInlineMacroType of manual_macro_parameter_type list
  | MBlockMacroType  of manual_macro_parameter_type list

and manual_macro_parameter_type =
  | MLateMacroParameter  of manual_type
  | MEarlyMacroParameter of manual_type

and constructor_branch =
  | UTConstructorBranch of constructor_name ranged * manual_type option

and untyped_rec_or_nonrec =
  | UTNonRec  of untyped_let_binding
  | UTRec     of untyped_let_binding list
  | UTMutable of untyped_let_mutable_binding

and untyped_let_binding =
  var_name ranged * untyped_abstract_tree

and untyped_let_mutable_binding =
  var_name ranged * untyped_abstract_tree

and untyped_type_binding =
  type_name ranged * (type_variable_name ranged) list * untyped_synonym_or_variant

and untyped_synonym_or_variant =
  | UTBindSynonym of manual_type
  | UTBindVariant of constructor_branch list

and untyped_input_horz_element =
  Range.t * untyped_input_horz_element_main
    [@printer (fun fmt (_, utihmain) -> Format.fprintf fmt "%a" pp_untyped_input_horz_element_main utihmain)]

and untyped_input_horz_element_main =
  | UTInputHorzText             of string
      [@printer (fun fmt s -> Format.fprintf fmt "IT:%s" s)]
  | UTInputHorzApplyCommand     of untyped_abstract_tree * untyped_command_argument list
      [@printer (fun fmt (utast, lst) -> Format.fprintf fmt "IC:%a %a" pp_untyped_abstract_tree utast (Format.pp_print_list ~pp_sep pp_untyped_command_argument) lst)]
  | UTInputHorzContent          of untyped_abstract_tree
  | UTInputHorzEmbeddedMath     of untyped_abstract_tree
  | UTInputHorzEmbeddedCodeArea of string
  | UTInputHorzMacro            of (Range.t * (module_name ranged) list * macro_name ranged) * untyped_macro_argument list

and untyped_macro_argument =
  | UTLateMacroArg  of untyped_abstract_tree
  | UTEarlyMacroArg of untyped_abstract_tree

and untyped_input_vert_element = Range.t * untyped_input_vert_element_main
  [@printer (fun fmt (_, utivmain) -> Format.fprintf fmt "%a" pp_untyped_input_vert_element_main utivmain)]

and untyped_input_vert_element_main =
  | UTInputVertApplyCommand of untyped_abstract_tree * untyped_command_argument list
      [@printer (fun fmt (utast, lst) ->
        Format.fprintf fmt "BC:%a %a"
          pp_untyped_abstract_tree utast
          (Format.pp_print_list ~pp_sep pp_untyped_command_argument) lst
      )]
  | UTInputVertContent  of untyped_abstract_tree
  | UTInputVertMacro    of (Range.t * (module_name ranged) list * macro_name ranged) * untyped_macro_argument list

and untyped_input_math_element =
  Range.t * untyped_input_math_element_main

and untyped_input_math_element_main =
  | UTInputMathElement of {
      base : untyped_input_math_base;
      sup  : (bool * untyped_input_math_element list) option;
      sub  : (bool * untyped_input_math_element list) option;
    }

and untyped_input_math_base =
  | UTInputMathChar of Uchar.t
      [@printer (fun ppf uch ->
        Format.fprintf ppf "(UTInputMathChar \"%s\")" (string_of_uchar uch)
      )]
  | UTInputMathApplyCommand of untyped_abstract_tree * untyped_command_argument list
  | UTInputMathContent      of untyped_abstract_tree

and untyped_abstract_tree =
  Range.t * untyped_abstract_tree_main
    [@printer (fun fmt (_, utastmain) -> Format.fprintf fmt "%a" pp_untyped_abstract_tree_main utastmain)]

and untyped_abstract_tree_main =
(* Literals: *)
  | UTUnitConstant
  | UTBooleanConstant      of bool
  | UTIntegerConstant      of int
  | UTFloatConstant        of float
  | UTLengthDescription    of float * length_unit_name
  | UTStringConstant       of string
  | UTPositionedString     of input_position * string
(* Input texts: *)
  | UTInlineText          of untyped_input_horz_element list
  | UTBlockText           of untyped_input_vert_element list
  | UTMathText            of untyped_input_math_element list
(* Command abstractions: *)
  | UTLambdaHorzCommand of {
      parameters       : untyped_parameter_unit list;
      context_variable : var_name ranged;
      body             : untyped_abstract_tree;
    }
  | UTLambdaVertCommand of {
      parameters       : untyped_parameter_unit list;
      context_variable : var_name ranged;
      body             : untyped_abstract_tree;
    }
  | UTLambdaMathCommand of {
      parameters       : untyped_parameter_unit list;
      context_variable : var_name ranged;
      script_variables : (var_name ranged * var_name ranged) option;
      body             : untyped_abstract_tree;
    }
(* For lightweight command definitions: *)
  | UTLexHorz              of untyped_abstract_tree * untyped_abstract_tree
  | UTLexVert              of untyped_abstract_tree * untyped_abstract_tree
(* Lists: *)
  | UTListCons             of untyped_abstract_tree * untyped_abstract_tree
  | UTEndOfList
(* Tuples: *)
  | UTTuple               of untyped_abstract_tree TupleList.t
(* Records: *)
  | UTRecord               of (label ranged * untyped_abstract_tree) list
  | UTAccessField          of untyped_abstract_tree * label ranged
  | UTUpdateField          of untyped_abstract_tree * label ranged * untyped_abstract_tree
(* Fundamentals: *)
  | UTContentOf            of ((module_name ranged) list) * var_name ranged
  | UTApply                of (label ranged * untyped_abstract_tree) list * untyped_abstract_tree * untyped_abstract_tree
  | UTLetIn                of untyped_rec_or_nonrec * untyped_abstract_tree
  | UTIfThenElse           of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTFunction             of untyped_parameter_unit * untyped_abstract_tree
  | UTOpenIn               of module_name ranged * untyped_abstract_tree
  | UTPatternMatch         of untyped_abstract_tree * untyped_pattern_branch list
  | UTConstructor          of constructor_name * untyped_abstract_tree
  | UTOverwrite            of var_name ranged * untyped_abstract_tree
(* Lightweight itemizes: *)
  | UTItemize              of untyped_itemize
(* Multi-stage constructs: *)
  | UTNext                 of untyped_abstract_tree
  | UTPrev                 of untyped_abstract_tree

and untyped_itemize =
  | UTItem of untyped_abstract_tree * (untyped_itemize list)

and untyped_pattern_tree =
  Range.t * untyped_pattern_tree_main

and untyped_pattern_tree_main =
  | UTPIntegerConstant     of int
  | UTPBooleanConstant     of bool
  | UTPStringConstant      of string
  | UTPUnitConstant
  | UTPListCons            of untyped_pattern_tree * untyped_pattern_tree
  | UTPEndOfList
  | UTPTuple               of untyped_pattern_tree TupleList.t
  | UTPWildCard
  | UTPVariable            of var_name
  | UTPAsVariable          of var_name * untyped_pattern_tree
  | UTPConstructor         of constructor_name * untyped_pattern_tree

and untyped_pattern_branch =
  | UTPatternBranch of untyped_pattern_tree * untyped_abstract_tree

and untyped_unkinded_type_argument =
  Range.t * var_name

and untyped_type_argument =
  Range.t * var_name * manual_kind

and untyped_command_argument =
  | UTCommandArg of (label ranged * untyped_abstract_tree) list * untyped_abstract_tree

and untyped_parameter_unit =
  | UTParameterUnit of (label ranged * var_name ranged) list * untyped_pattern_tree * manual_type option
[@@deriving show { with_path = false; }]

type untyped_source_file =
  | UTLibraryFile  of (module_name ranged * untyped_signature option * untyped_binding list)
  | UTDocumentFile of untyped_abstract_tree
[@@deriving show { with_path = false; }]

type untyped_letrec_pattern_branch =
  | UTLetRecPatternBranch of untyped_pattern_tree list * untyped_abstract_tree

type 'a input_horz_element_scheme =
  | InputHorzText             of string
  | InputHorzContent          of 'a
  | InputHorzEmbeddedMath     of 'a
  | InputHorzEmbeddedCodeArea of string
  | InputHorzApplyCommand of {
      command   : 'a;
      arguments : ('a LabelMap.t * 'a) list;
    }
[@@deriving show { with_path = false; }]

type 'a input_vert_element_scheme =
  | InputVertContent  of 'a
  | InputVertApplyCommand of {
      command   : 'a;
      arguments : ('a LabelMap.t * 'a) list;
    }
[@@deriving show { with_path = false; }]

type 'a input_math_base_scheme =
  | InputMathChar         of Uchar.t
      [@printer (fun fmt _ -> Format.fprintf fmt "<math-text-chars>")]
  | InputMathContent      of 'a
  | InputMathApplyCommand of {
      command   : 'a;
      arguments : ('a LabelMap.t * 'a) list;
    }
[@@deriving show { with_path = false; }]

type 'a input_math_element_scheme =
  | InputMathElement of {
      base : 'a input_math_base_scheme;
      sub  : (('a input_math_element_scheme) list) option;
      sup  : (('a input_math_element_scheme) list) option;
    }
[@@deriving show { with_path = false; }]

type ('a, 'b) path_component_scheme =
  | PathLineTo        of 'a
  | PathCubicBezierTo of 'b * 'b * 'a
[@@deriving show { with_path = false; }]

type page_break_style =
  | SingleColumn
  | MultiColumn of length list
[@@deriving show { with_path = false; }]

type base_constant =
  | BCUnit
  | BCBool     of bool
  | BCInt      of int
  | BCFloat    of float
  | BCLength   of length
  | BCString   of string
  | BCRegExp   of Str.regexp
      [@printer (fun fmt _ -> Format.fprintf fmt "<regexp>")]
  | BCPath     of path list
      [@printer (fun fmt _ -> Format.fprintf fmt "<path>")]
  | BCPrePath  of PrePath.t
      [@printer (fun fmt _ -> Format.fprintf fmt "<pre-path>")]
  | BCImageKey of ImageInfo.key
      [@printer (fun fmt _ -> Format.fprintf fmt "<image-key>")]
  | BCHorz     of HorzBox.horz_box list
  | BCVert     of HorzBox.vert_box list
  | BCGraphics of (HorzBox.intermediate_horz_box list) GraphicD.t
      [@printer (fun fmt _ -> Format.fprintf fmt "<graphics>")]
  | BCDocument        of (length * length) * page_break_style * HorzBox.column_hook_func * HorzBox.column_hook_func * HorzBox.page_content_scheme_func * HorzBox.page_parts_scheme_func * HorzBox.vert_box list
  | BCInputPos        of input_position
[@@deriving show { with_path = false; }]

type 'a letrec_binding_scheme =
  | LetRecBinding of EvalVarID.t * 'a pattern_branch_scheme

and letrec_binding =
  abstract_tree letrec_binding_scheme

and rec_or_nonrec =
  | Rec     of letrec_binding list
  | NonRec  of EvalVarID.t * abstract_tree
  | Mutable of EvalVarID.t * abstract_tree

and binding =
  | Bind of stage * rec_or_nonrec

and environment =
  location EvalVarIDMap.t * (syntactic_value StoreIDHashTable.t) ref
    [@printer (fun fmt _ -> Format.fprintf fmt "<env>")]

and location =
  syntactic_value ref

and vmenv =
  environment * (syntactic_value array) list

and compiled_input_horz_element =
  | CompiledInputHorzText         of string
  | CompiledInputHorzEmbedded     of instruction list
  | CompiledInputHorzContent      of instruction list
  | CompiledInputHorzEmbeddedMath of instruction list
  | CompiledInputHorzEmbeddedCodeText of string

and compiled_intermediate_input_horz_element =
  | CompiledImInputHorzText         of string
  | CompiledImInputHorzEmbedded     of instruction list
  | CompiledImInputHorzContent of compiled_intermediate_input_horz_element list * vmenv
  | CompiledImInputHorzEmbeddedMath of instruction list
  | CompiledImInputHorzEmbeddedCodeText of string

and compiled_input_vert_element =
  | CompiledInputVertEmbedded of instruction list
  | CompiledInputVertContent  of instruction list

and compiled_intermediate_input_vert_element =
  | CompiledImInputVertEmbedded of instruction list
  | CompiledImInputVertContent  of compiled_intermediate_input_vert_element list * vmenv

and compiled_intermediate_input_math_element =
  unit (* TODO: define this *)

and ir_input_horz_element =
  | IRInputHorzText         of string
  | IRInputHorzEmbedded     of ir
  | IRInputHorzContent      of ir
  | IRInputHorzEmbeddedMath of ir
  | IRInputHorzEmbeddedCodeText of string

and ir_input_vert_element =
  | IRInputVertEmbedded of ir
  | IRInputVertContent  of ir

and ir_input_math_element =
  unit (* TODO: define this *)

and varloc =
  | GlobalVar of location * EvalVarID.t * int ref
  | LocalVar  of int * int * EvalVarID.t * int ref

and ir =
  | IRConstant              of syntactic_value
  | IRTerminal
  | IRInputHorz             of ir_input_horz_element list
  | IRInputVert             of ir_input_vert_element list
  | IRInputMath             of ir_input_math_element list
  | IRRecord                of label list * ir list
  | IRAccessField           of ir * label
  | IRUpdateField           of ir * label * ir
  | IRLetRecIn              of (varloc * ir) list * ir
  | IRLetNonRecIn           of ir * ir_pattern_tree * ir
  | IRContentOf             of varloc
  | IRSymbolOf              of varloc
  | IRIfThenElse            of ir * ir * ir
  | IRFunction              of int * varloc LabelMap.t * ir_pattern_tree list * ir
  | IRApply                 of int * ir * ir list
  | IRApplyPrimitive        of instruction * int * ir list
  | IRApplyOptional         of ir * ir
  | IRApplyOmission         of ir
  | IRTuple                 of int * ir list
  | IRPatternMatch          of Range.t * ir * ir_pattern_branch list
  | IRNonValueConstructor   of constructor_name * ir
  | IRLetMutableIn          of varloc * ir * ir
  | IROverwrite             of varloc * ir
  | IRDereference           of ir

  | IRCodeCombinator        of (code_value list -> code_value) * int * ir list
  | IRCodeRecord            of label list * ir list
  | IRCodeInputHorz         of (ir input_horz_element_scheme) list
  | IRCodeInputVert         of (ir input_vert_element_scheme) list
  | IRCodeInputMath         of (ir input_math_element_scheme) list
  | IRCodePatternMatch      of Range.t * ir * ir_pattern_branch list
  | IRCodeLetRecIn          of ir_letrec_binding list * ir
  | IRCodeLetNonRecIn       of ir_pattern_tree * ir * ir
  | IRCodeFunction          of varloc LabelMap.t * ir_pattern_tree * ir
  | IRCodeLetMutableIn      of varloc * ir * ir
  | IRCodeOverwrite         of varloc * ir
  | IRLift                  of ir

and 'a ir_letrec_binding_scheme =
  | IRLetRecBinding of varloc * 'a ir_pattern_branch_scheme

and ir_letrec_binding =
  ir ir_letrec_binding_scheme

and 'a ir_pattern_branch_scheme =
  | IRPatternBranch      of ir_pattern_tree * 'a
  | IRPatternBranchWhen  of ir_pattern_tree * 'a * 'a

and ir_pattern_branch =
  ir ir_pattern_branch_scheme

and ir_pattern_tree =
  | IRPUnitConstant
  | IRPBooleanConstant      of bool
  | IRPIntegerConstant      of int
  | IRPStringConstant       of string
  | IRPListCons             of ir_pattern_tree * ir_pattern_tree
  | IRPEndOfList
  | IRPTuple                of ir_pattern_tree TupleList.t
  | IRPWildCard
  | IRPVariable             of varloc
  | IRPAsVariable           of varloc * ir_pattern_tree
  | IRPConstructor          of constructor_name * ir_pattern_tree

and instruction =
  | OpAccessField of label
  | OpUpdateField of label
  | OpForward of int
  | OpApply of int
  | OpApplyT of int
  | OpApplyOptional
  | OpApplyOmission
  | OpBindGlobal of syntactic_value ref * EvalVarID.t * int
  | OpBindLocal of int * int * EvalVarID.t * int
  | OpBindClosuresRec of (varloc * instruction list) list
  | OpBranch of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpBranch(...)")]
  | OpBranchIf of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpBranchIf(...)")]
  | OpBranchIfNot of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpBranchIfNot(...)")]
  | OpLoadGlobal of syntactic_value ref * EvalVarID.t * int
      [@printer ((fun fmt (_r, evid, _refs) -> Format.fprintf fmt "OpLoadGlobal(%s)" (EvalVarID.show_direct evid)))]
  | OpLoadLocal of int * int * EvalVarID.t * int
  | OpDereference
      (* !! pdf, no-interp *)
  | OpDup
  | OpError of string
  | OpMakeConstructor of constructor_name
  | OpMakeRecord of label list
  | OpMakeTuple of int
  | OpPop
  | OpPush of syntactic_value
  | OpPushEnv
  | OpCheckStackTopBool of bool * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopBool(...)")]
  | OpCheckStackTopCtor of constructor_name * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopCtor(...)")]
  | OpCheckStackTopEndOfList of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopEndOfList(...)")]
  | OpCheckStackTopInt of int * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopInt(...)")]
  | OpCheckStackTopListCons of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopListCons(...)")]
  | OpCheckStackTopStr of string * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopStr(...)")]
  | OpCheckStackTopTupleCons of instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpCheckStackTopTupleCons(...)")]
  | OpClosure of varloc LabelMap.t * int * int * instruction list
  | OpClosureInputHorz of compiled_input_horz_element list
  | OpClosureInputVert of compiled_input_vert_element list
  | OpBindLocationGlobal of syntactic_value ref * EvalVarID.t
  | OpBindLocationLocal of int * int * EvalVarID.t
  | OpUpdateGlobal of syntactic_value ref * EvalVarID.t
      [@printer (fun fmt _ -> Format.fprintf fmt "OpUpdateGlobal(...)")]
  | OpUpdateLocal of int * int * EvalVarID.t
      [@printer (fun fmt _ -> Format.fprintf fmt "OpUpdateLocal(...)")]
  | OpSel of instruction list * instruction list
      [@printer (fun fmt _ -> Format.fprintf fmt "OpSel(...)")]

  | OpInsertArgs of syntactic_value list
  | OpApplyCodeCombinator of (code_value list -> code_value) * int
  | OpCodeMakeRecord of label list
  | OpCodeMakeTuple of int
  | OpCodeMakeInputHorz of ((instruction list) input_horz_element_scheme) list
  | OpCodeMakeInputVert of ((instruction list) input_vert_element_scheme) list
  | OpCodePatternMatch  of Range.t * ((instruction list) ir_pattern_branch_scheme) list
  | OpCodeLetRec        of ((instruction list) ir_letrec_binding_scheme) list * instruction list
  | OpCodeLetNonRec     of ir_pattern_tree * instruction list * instruction list
  | OpCodeFunction      of varloc LabelMap.t * ir_pattern_tree * instruction list
  | OpCodeLetMutable    of varloc * instruction list * instruction list
  | OpCodeOverwrite     of instruction list
  | OpConvertSymbolToCode
#include "__insttype.gen.ml"

and input_horz_value_element =
  | InputHorzValueText             of string
  | InputHorzValueCommandClosure   of horz_command_closure
  | InputHorzValueEmbeddedMath     of input_math_value_element list
  | InputHorzValueEmbeddedCodeArea of string

and input_vert_value_element =
  | InputVertValueCommandClosure of vert_command_closure

and input_math_value_element =
  | InputMathValueElement of {
      base : input_math_value_base;
      sub  : (input_math_value_element list) option;
      sup  : (input_math_value_element list) option;
    }

and input_math_value_base =
  | InputMathValueChar     of Uchar.t
      [@printer (fun fmt uch -> Format.fprintf fmt "ImInputMathChar \"%s\"" (string_of_uchar uch))]
  | InputMathValueEmbedded of math_command_closure
  | InputMathValueGroup    of input_math_value_element list

and horz_command_closure =
  | HorzCommandClosureSimple of {
      context_binder : EvalVarID.t;
      body           : abstract_tree;
      environment    : environment;
    }

and vert_command_closure =
  | VertCommandClosureSimple of {
      context_binder : EvalVarID.t;
      body           : abstract_tree;
      environment    : environment;
    }

and math_command_closure =
  | MathCommandClosureSimple of {
      context_binder : EvalVarID.t;
      body           : abstract_tree;
      environment    : environment;
    }
  | MathCommandClosureWithScripts of {
      context_binder : EvalVarID.t;
      sub_binders    : EvalVarID.t;
      sup_binders    : EvalVarID.t;
      body           : abstract_tree;
      environment    : environment;
    }

and syntactic_value =
  | Nil  (* -- just for brief use -- *)
  | BaseConstant of base_constant
  | Constructor  of constructor_name * syntactic_value
  | List         of syntactic_value list
  | Tuple        of syntactic_value list
  | RecordValue  of syntactic_value LabelMap.t
      [@printer (fun fmt _ -> Format.fprintf fmt "<record-value>")]
  | Location     of StoreID.t

  | Context      of input_context
  | TextModeContext of text_mode_input_context

  | CodeValue    of code_value
  | CodeSymbol   of CodeSymbol.t
  | MathBoxes    of math_box list

(* -- for the naive interpreter, i.e. 'evaluator.cppo.ml' -- *)
  | Closure          of EvalVarID.t LabelMap.t * pattern_branch * environment
  | PrimitiveClosure of pattern_branch * environment * int * (abstract_tree list -> abstract_tree)
  | InputHorzValue   of input_horz_value_element list
  | InputVertValue   of input_vert_value_element list
  | InputMathValue   of input_math_value_element list

  | HorzCommandClosure of horz_command_closure
  | VertCommandClosure of vert_command_closure
  | MathCommandClosure of math_command_closure

(* -- for the SECD machine, i.e. 'vm.cppo.ml' -- *)
  | CompiledClosure          of varloc LabelMap.t * int * syntactic_value list * int * instruction list * vmenv
  | CompiledPrimitiveClosure of int * syntactic_value list * int * instruction list * vmenv * (abstract_tree list -> abstract_tree)
  | CompiledInputHorzClosure of compiled_intermediate_input_horz_element list * vmenv
  | CompiledInputVertClosure of compiled_intermediate_input_vert_element list * vmenv
  | CompiledInputMathClosure of compiled_intermediate_input_math_element list * vmenv

and abstract_tree =
  | ASTBaseConstant       of base_constant
  | ASTEndOfList
(* -- input texts -- *)
  | InlineText            of input_horz_element list
  | BlockText             of input_vert_element list
  | MathText              of input_math_element list
  | LambdaHorz            of EvalVarID.t * abstract_tree
  | LambdaVert            of EvalVarID.t * abstract_tree
  | LambdaMath            of EvalVarID.t * (EvalVarID.t * EvalVarID.t) option * abstract_tree
(* -- record value -- *)
  | Record                of abstract_tree LabelMap.t
  | AccessField           of abstract_tree * label
  | UpdateField           of abstract_tree * label * abstract_tree
(* -- fundamental -- *)
  | LetRecIn              of letrec_binding list * abstract_tree
  | LetNonRecIn           of pattern_tree * abstract_tree * abstract_tree
  | ContentOf             of Range.t * EvalVarID.t
  | IfThenElse            of abstract_tree * abstract_tree * abstract_tree
  | Function              of EvalVarID.t LabelMap.t * pattern_branch
  | Apply                 of abstract_tree LabelMap.t * abstract_tree * abstract_tree
(* -- pattern match -- *)
  | PatternMatch          of Range.t * abstract_tree * pattern_branch list
  | NonValueConstructor   of constructor_name * abstract_tree
(* -- imperative -- *)
  | LetMutableIn          of EvalVarID.t * abstract_tree * abstract_tree
  | Dereference           of abstract_tree
  | Overwrite             of EvalVarID.t * abstract_tree
  | PrimitiveTuple        of abstract_tree TupleList.t
(* -- staging constructs -- *)
  | Next                  of abstract_tree
  | Prev                  of abstract_tree
  | Persistent            of Range.t * EvalVarID.t
  | Lift                  of abstract_tree
  | ASTCodeSymbol         of CodeSymbol.t
#include "__attype.gen.ml"

and input_horz_element =
  abstract_tree input_horz_element_scheme

and input_vert_element =
  abstract_tree input_vert_element_scheme

and input_math_element =
  abstract_tree input_math_element_scheme

and 'a path_component =
  ('a, abstract_tree) path_component_scheme

and 'a pattern_branch_scheme =
  | PatternBranch      of pattern_tree * 'a
  | PatternBranchWhen  of pattern_tree * 'a * 'a (* TODO (enhance): remove this constructor *)

and pattern_branch =
  abstract_tree pattern_branch_scheme

and pattern_tree =
  | PUnitConstant
  | PBooleanConstant      of bool
  | PIntegerConstant      of int
  | PStringConstant       of string
  | PListCons             of pattern_tree * pattern_tree
  | PEndOfList
  | PTuple                of pattern_tree TupleList.t
  | PWildCard
  | PVariable             of EvalVarID.t
  | PAsVariable           of EvalVarID.t * pattern_tree
  | PConstructor          of constructor_name * pattern_tree

and input_context =
  HorzBox.context_main * context_sub

and context_sub = {
  math_command      : math_command_func;
  code_text_command : code_text_command_func;
}
and text_mode_input_context =
  TextBackend.context_main * text_mode_context_sub

and text_mode_context_sub = {
  text_mode_math_command      : math_command_func;
  text_mode_code_text_command : code_text_command_func;
  text_mode_math_scripts_func : math_scripts_func;
}

and math_command_func =
  | MathCommand of syntactic_value
      (* Function values of the form `λ(m : math). λinline(ctx : context). e` *)

and code_text_command_func =
  | DefaultCodeTextCommand
  | CodeTextCommand of syntactic_value
      (* Function values of the form `λ(s : string). λinline(ctx : context). e` *)

and math_scripts_func =
  | MathScriptsFunc of syntactic_value
      (* Function values of the form `λ(base : string). λ(sub : option string). λ(sup : option string). e` *)

and math_box_atom =
  | MathChar of {
      context : input_context;
      is_big  : bool;
      chars   : Uchar.t list;
    } [@printer (fun ppf _ _ _ -> Format.fprintf ppf "<math-char>")]

  | MathCharWithKern of {
      context    : input_context;
      is_big     : bool;
      chars      : Uchar.t list;
      left_kern  : HorzBox.math_char_kern_func;
      right_kern : HorzBox.math_char_kern_func;
    } [@printer (fun ppf _ _ _ _ _ -> Format.fprintf ppf "<math-char'>")]

  | MathEmbeddedHorz of HorzBox.horz_box list

and math_box =
  | MathBoxAtom of {
      kind : HorzBox.math_kind;
      main : math_box_atom;
    }
  | MathBoxSubscript of {
      context : input_context;
      base    : math_box list;
      sub     : math_box list;
    }
  | MathBoxSuperscript of {
      context : input_context;
      base    : math_box list;
      sup     : math_box list;
    }
  | MathBoxGroup of {
      left  : HorzBox.math_kind;
      right : HorzBox.math_kind;
      inner : math_box list;
    }
  | MathBoxFraction of {
      context     : input_context;
      numerator   : math_box list;
      denominator : math_box list;
    }
  | MathBoxRadical of {
      context : input_context;
      radical : HorzBox.radical;
      degree  : (math_box list) option;
      inner   : math_box list;
    }
  | MathBoxParen of {
      context : input_context;
      left    : paren;
      right   : paren;
      inner   : math_box list;
    }
  | MathBoxParenWithMiddle of {
      context : input_context;
      left    : paren;
      right   : paren;
      middle  : paren;
      inner   : (math_box list) list;
    }
  | MathBoxUpperLimit of {
      context : input_context;
      base    : math_box list;
      upper   : math_box list;
    }
  | MathBoxLowerLimit of {
      context : input_context;
      base    : math_box list;
      lower   : math_box list;
    }

and paren =
  length -> length -> input_context -> HorzBox.horz_box list * HorzBox.math_kern_func
    (* The type for adjustable parentheses.
       An adjustable parenthesis takes as arguments
       (1) the height of the inner contents,
       (2) the depth of the inner contents, and
       (3) the context,
       and then returns its inline box representation and the function for kerning. *)

and code_value =
  | CdPersistent    of Range.t * EvalVarID.t
  | CdBaseConstant  of base_constant
  | CdEndOfList
  | CdInputHorz     of code_input_horz_element list
  | CdInputVert     of code_input_vert_element list
  | CdInputMath     of code_input_math_element list
  | CdLambdaHorz    of CodeSymbol.t * code_value
  | CdLambdaVert    of CodeSymbol.t * code_value
  | CdLambdaMath    of CodeSymbol.t * (CodeSymbol.t * CodeSymbol.t) option * code_value
  | CdContentOf     of Range.t * CodeSymbol.t
  | CdLetRecIn      of code_letrec_binding list * code_value
  | CdLetNonRecIn   of code_pattern_tree * code_value * code_value
  | CdFunction      of CodeSymbol.t LabelMap.t * code_pattern_branch
  | CdApply         of code_value LabelMap.t * code_value * code_value
  | CdIfThenElse    of code_value * code_value * code_value
  | CdRecord        of code_value LabelMap.t
  | CdAccessField   of code_value * label
  | CdUpdateField   of code_value * label * code_value
  | CdLetMutableIn  of CodeSymbol.t * code_value * code_value
  | CdOverwrite     of CodeSymbol.t * code_value
  | CdDereference   of code_value
  | CdPatternMatch  of Range.t * code_value * code_pattern_branch list
  | CdConstructor   of constructor_name * code_value
  | CdTuple         of code_value TupleList.t
#include "__codetype.gen.ml"

and code_input_horz_element =
  code_value input_horz_element_scheme

and code_input_vert_element =
  code_value input_vert_element_scheme

and code_input_math_element =
  code_value input_math_element_scheme

and 'a code_path_component =
  ('a, code_value) path_component_scheme

and code_letrec_binding =
  | CdLetRecBinding of CodeSymbol.t * code_pattern_branch

and code_pattern_branch =
  | CdPatternBranch     of code_pattern_tree * code_value
  | CdPatternBranchWhen of code_pattern_tree * code_value * code_value

and code_pattern_tree =
  | CdPUnitConstant
  | CdPBooleanConstant      of bool
  | CdPIntegerConstant      of int
  | CdPStringConstant       of string
  | CdPListCons             of code_pattern_tree * code_pattern_tree
  | CdPEndOfList
  | CdPTuple                of code_pattern_tree TupleList.t
  | CdPWildCard
  | CdPVariable             of CodeSymbol.t
  | CdPAsVariable           of CodeSymbol.t * code_pattern_tree
  | CdPConstructor          of constructor_name * code_pattern_tree
[@@deriving show { with_path = false; }]

type code_rec_or_nonrec =
  | CdRec     of code_letrec_binding list
  | CdNonRec  of CodeSymbol.t * code_value
  | CdMutable of CodeSymbol.t * code_value

type 'a cycle =
  | Loop  of 'a
  | Cycle of 'a TupleList.t
[@@deriving show { with_path = false; }]

type file_info =
  | DocumentFile of untyped_abstract_tree
  | LibraryFile  of (module_name ranged * untyped_signature option * untyped_binding list)


module BoundIDHashTable = Hashtbl.Make(BoundID)

module BoundRowIDHashTable = Hashtbl.Make(BoundRowID)

module FreeIDHashTable = Hashtbl.Make(FreeID)

module FreeRowIDHashTable = Hashtbl.Make(FreeRowID)

module BoundIDMap = Map.Make(BoundID)

module BoundRowIDMap = Map.Make(BoundRowID)

module FreeIDMap = Map.Make(FreeID)

module FreeRowIDMap = Map.Make(FreeRowID)

module OpaqueIDMap = struct
  include Map.Make(TypeID)


  let pp pp_v ppf oidmap =
    pp_map fold TypeID.pp pp_v ppf oidmap

end


let get_range (rng, _) = rng


let add_to_environment (env : environment) (evid : EvalVarID.t) (rfast : location) : environment =
  let (valenv, stenvref) = env in
    (valenv |> EvalVarIDMap.add evid rfast, stenvref)


let find_in_environment (env : environment) (evid : EvalVarID.t) : location option =
  let (valenv, _) = env in
    valenv |> EvalVarIDMap.find_opt evid


let register_location (env : environment) (value : syntactic_value) : StoreID.t =
  let (_, stenvref) = env in
  let stid = StoreID.fresh () in
  StoreIDHashTable.add (!stenvref) stid value;
  stid


let update_location (env : environment) (stid : StoreID.t) (value : syntactic_value) : unit =
  let (_, stenvref) = env in
  let stenv = !stenvref in
  if StoreIDHashTable.mem stenv stid then
    StoreIDHashTable.replace stenv stid value
  else
    assert false


let find_location_value (env : environment) (stid : StoreID.t) : syntactic_value option =
  let (_, stenvref) = env in
  StoreIDHashTable.find_opt (!stenvref) stid


let map_input_horz f ihlst =
  ihlst |> List.map (function
  | InputHorzText(s) ->
      InputHorzText(s)

  | InputHorzApplyCommand{
      command = ast_cmd;
      arguments;
    } ->
      InputHorzApplyCommand{
        command =
          f ast_cmd;

        arguments =
          arguments |> List.map (fun (ast_labmap, ast) ->
            (ast_labmap |> LabelMap.map f, f ast)
          );
      }

  | InputHorzContent(ast)        -> InputHorzContent(f ast)
  | InputHorzEmbeddedMath(ast)   -> InputHorzEmbeddedMath(f ast)
  | InputHorzEmbeddedCodeArea(s) -> InputHorzEmbeddedCodeArea(s)
  )


let map_input_vert f ivlst =
  ivlst |> List.map (function
  | InputVertContent(ast) ->
      InputVertContent(f ast)

  | InputVertApplyCommand{
      command = ast_cmd;
      arguments;
    } ->
      InputVertApplyCommand{
        command =
          f ast_cmd;

        arguments =
          arguments |> List.map (fun (ast_labmap, ast) ->
            (ast_labmap |> LabelMap.map f, f ast)
          );
      }
  )


let rec map_input_math f ms =
  ms |> List.map (fun m ->
    let InputMathElement{ base; sub; sup } = m in
    let base =
      match base with
      | InputMathChar(uch) ->
          InputMathChar(uch)

      | InputMathContent(ast) ->
          InputMathContent(f ast)

      | InputMathApplyCommand{
          command = ast_cmd;
          arguments;
        } ->
          InputMathApplyCommand{
            command =
              f ast_cmd;

            arguments =
              arguments |> List.map (fun (ast_labmap, ast) ->
                (ast_labmap |> LabelMap.map f, f ast)
              )
          }
    in
    let sub = sub |> Option.map (map_input_math f) in
    let sup = sup |> Option.map (map_input_math f) in
    InputMathElement{ base; sub; sup }
  )


let map_path_component f g = function
  | PathLineTo(ast) ->
      PathLineTo(g ast)

  | PathCubicBezierTo(ast1, ast2, ast3) ->
      let v1 = f ast1 in
      let v2 = f ast2 in
      let v3 = g ast3 in
      PathCubicBezierTo(v1, v2, v3)


let rec unlift_code (code : code_value) : abstract_tree =
  let rec aux code =
    match code with
    | CdPersistent(rng, evid)              -> ContentOf(rng, evid)
    | CdBaseConstant(bc)                   -> ASTBaseConstant(bc)
    | CdEndOfList                          -> ASTEndOfList
    | CdInputMath(ms)                      -> MathText(ms |> map_input_math aux)
    | CdInputHorz(cdihlst)                 -> InlineText(cdihlst |> map_input_horz aux)
    | CdInputVert(cdivlst)                 -> BlockText(cdivlst |> map_input_vert aux)

    | CdLambdaHorz(symb_ctx, code0) ->
        LambdaHorz(CodeSymbol.unlift symb_ctx, aux code0)

    | CdLambdaVert(symb_ctx, code0) ->
        LambdaVert(CodeSymbol.unlift symb_ctx, aux code0)

    | CdLambdaMath(symb_ctx, symb_pair_opt, code0) ->
        let evid_pair_opt =
          symb_pair_opt |> Option.map (fun (symb_sub, symb_sup) ->
            (CodeSymbol.unlift symb_sub, CodeSymbol.unlift symb_sup)
          )
        in
        LambdaMath(CodeSymbol.unlift symb_ctx, evid_pair_opt, aux code0)

    | CdContentOf(rng, symb)               -> ContentOf(rng, CodeSymbol.unlift symb)
    | CdLetRecIn(cdrecbinds, code1)        -> LetRecIn(List.map unlift_letrec_binding cdrecbinds, aux code1)
    | CdLetNonRecIn(cdpat, code1, code2)   -> LetNonRecIn(unlift_pattern cdpat, aux code1, aux code2)
    | CdFunction(symb_labmap, cdpatbr)     -> Function(symb_labmap |> LabelMap.map CodeSymbol.unlift, unlift_pattern_branch cdpatbr)
    | CdApply(code_labmap, code1, code2)   -> Apply(code_labmap |> LabelMap.map aux, aux code1, aux code2)
    | CdIfThenElse(code1, code2, code3)    -> IfThenElse(aux code1, aux code2, aux code3)
    | CdRecord(cdasc)                      -> Record(cdasc |> LabelMap.map aux)
    | CdAccessField(code1, fldnm)          -> AccessField(aux code1, fldnm)
    | CdUpdateField(code1, fldnm, code2)   -> UpdateField(aux code1, fldnm, aux code2)
    | CdLetMutableIn(symb, code1, code2)   -> LetMutableIn(CodeSymbol.unlift symb, aux code1, aux code2)
    | CdOverwrite(symb, code1)             -> Overwrite(CodeSymbol.unlift symb, aux code1)
    | CdDereference(code1)                 -> Dereference(aux code1)
    | CdPatternMatch(rng, code1, cdpatbrs) -> PatternMatch(rng, aux code1, List.map unlift_pattern_branch cdpatbrs)
    | CdConstructor(constrnm, code1)       -> NonValueConstructor(constrnm, aux code1)
    | CdTuple(codes)                       -> PrimitiveTuple(TupleList.map aux codes)
#include "__unliftcode.gen.ml"
  in
  aux code


and unlift_pattern = function
  | CdPUnitConstant             -> PUnitConstant
  | CdPBooleanConstant(b)       -> PBooleanConstant(b)
  | CdPIntegerConstant(n)       -> PIntegerConstant(n)
  | CdPStringConstant(s)        -> PStringConstant(s)
  | CdPListCons(cdpat1, cdpat2) -> PListCons(unlift_pattern cdpat1, unlift_pattern cdpat2)
  | CdPEndOfList                -> PEndOfList
  | CdPTuple(cdpats)            -> PTuple(TupleList.map unlift_pattern cdpats)
  | CdPWildCard                 -> PWildCard
  | CdPVariable(symb)           -> PVariable(CodeSymbol.unlift symb)
  | CdPAsVariable(symb, cdpat)  -> PAsVariable(CodeSymbol.unlift symb, unlift_pattern cdpat)
  | CdPConstructor(ctor, cdpat) -> PConstructor(ctor, unlift_pattern cdpat)


and unlift_letrec_binding (CdLetRecBinding(symb, cdpatbr)) =
  LetRecBinding(CodeSymbol.unlift symb, unlift_pattern_branch cdpatbr)


and unlift_pattern_branch = function
  | CdPatternBranch(cdpat, code)            -> PatternBranch(unlift_pattern cdpat, unlift_code code)
  | CdPatternBranchWhen(cdpat, code, codeB) -> PatternBranchWhen(unlift_pattern cdpat, unlift_code code, unlift_code codeB)


let unlift_rec_or_nonrec (cd_rec_or_nonrec : code_rec_or_nonrec) : rec_or_nonrec =
  match cd_rec_or_nonrec with
  | CdNonRec(symb, code)  -> NonRec(CodeSymbol.unlift symb, unlift_code code)
  | CdRec(cdrecbinds)     -> Rec(List.map unlift_letrec_binding cdrecbinds)
  | CdMutable(symb, code) -> Mutable(CodeSymbol.unlift symb, unlift_code code)
