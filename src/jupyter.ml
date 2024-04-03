
open ConfigError
open ErrorReporting
open MyUtil
open ParserInterface
open StaticEnv
open SyntaxBase
open Types


external _start_kernel : string -> unit = "start_kernel"
external return_text : int -> string -> unit = "return_text"
external return_pdf : int -> string -> bool -> unit = "return_pdf"
external return_error : string -> string -> unit = "return_error"


type eval_config = {
  display_config : Logging.config;
  output_mode : output_mode;
  typecheck_config : typecheck_config;
  pdf_config : HandlePdf.config;
  page_number_limit : int;
  is_bytecomp_mode : bool;
  fnast : abstract_tree;
}


type eval_env = {
  tyenv : type_environment;
  env : environment;
  rasterize : bool;
}


let value_to_string = function
  | BaseConstant(BCUnit) -> "()"
  | BaseConstant(BCBool b) -> string_of_bool b
  | BaseConstant(BCInt i) -> string_of_int i
  | BaseConstant(BCFloat f) -> string_of_float f
  | BaseConstant(BCString s) -> "`" ^ s ^ "`"
  | _ -> "<not implemented>"


let handle_defs eval_config eval_env utbinds =
  let open ResultMonad in
  (* Typecheck *)
  let* binds =
    let* (tyenv, _, binds) =
      ModuleTypechecker.main eval_config.typecheck_config !eval_env.tyenv None utbinds
        |> Result.map_error (fun e -> TypeError(e))
    in
    eval_env := { !eval_env with tyenv = tyenv };
    return binds
  in

  (* Evaluate *)
  return begin
    let (env, codebinds) = Evaluator.interpret_bindings_0 ~run_tests:false !eval_env.env binds in
    let binds =
      codebinds |> List.map (fun cd_rec_or_nonrec -> Bind(Stage0, unlift_rec_or_nonrec cd_rec_or_nonrec))
    in
    let (env, _) = Evaluator.interpret_bindings_0 ~run_tests:false env binds in
    eval_env := { !eval_env with env = env }
  end


let handle_expr eval_config eval_env execution_counter utast =
  let open ResultMonad in
  (* Typecheck *)
  let* (ty, ast) =
    Typechecker.main eval_config.typecheck_config Stage1 !eval_env.tyenv utast
      |> Result.map_error (fun e -> TypeError(e))
  in

  (* Evaluate *)
  let ast =
    Evaluator.interpret_1 !eval_env.env ast
      |> unlift_code
  in

  (* Check output type *)
  let (ast, is_doc) =
    if Typechecker.are_unifiable ty (Range.dummy "block", BaseType(BlockTextType)) then
      Apply(LabelMap.empty, eval_config.fnast, ast), true
    else
      ast, Typechecker.are_unifiable ty (Range.dummy "document", BaseType(DocumentType))
  in

  (* Evaluate *)
  if is_doc then
    let path = Filename.temp_file "tmp" ".pdf" in
    let* () =
      BuildDocument.main
          eval_config.output_mode
          eval_config.pdf_config
          ~page_number_limit:eval_config.page_number_limit
          ~is_bytecomp_mode:eval_config.is_bytecomp_mode
          !eval_env.env
          ast
          (make_abs_path path)
          None
    in
    return (return_pdf execution_counter path !eval_env.rasterize)
  else
    let* value =
      BuildDocument.evaluate_once 
          BuildDocument.reset_pdf
          ~is_bytecomp_mode:eval_config.is_bytecomp_mode
          !eval_env.env
          ast
    in
    return (return_text execution_counter (value_to_string value))


let handle_cmd eval_env execution_counter = function
  | "render-in-pdf" ->
    eval_env := { !eval_env with rasterize = false };
    return_text execution_counter "Output mode is switched to PDF."

  | "render-in-image" ->
    eval_env := { !eval_env with rasterize = true };
    return_text execution_counter "Output mode is switched to image."

  | cmd ->
    return_error "Command Handler" ("Unknown command: " ^ cmd)


let _execute eval_config eval_env execution_counter code =
  let res =
    let open ResultMonad in
    let* utcell = parse_cell_main code |> Result.map_error (fun e -> FailedToParse(e)) in
    match utcell with
    | CellDefs(utbinds) -> handle_defs eval_config eval_env utbinds
    | CellExpr(utast) -> handle_expr eval_config eval_env execution_counter utast
    | CellCmd(cmd) -> return (handle_cmd eval_env execution_counter cmd)
  in
  match res with
  | Ok _ -> ()
  | Error(e) -> return_error "SATySFi" (make_config_error_message eval_config.display_config e)
