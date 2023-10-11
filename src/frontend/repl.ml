
open StaticEnv
open Types

(* REPL Types *)

type repl_context = {
  mutable tyenv: type_environment;
  mutable env: environment;
}

type repl_error =
  | EmptyInput
  | ParseError
  | TypecheckError of TypeError.type_error
  | NotImplemented (* TODO *)

type repl_expr =
  | Imm of abstract_tree
  | Def of type_environment * EvalVarID.t * abstract_tree

(* REPL Internals *)

let typecheck tyenv expr =
  match expr with
  | UTAST(utast) ->
    begin
      match Typechecker.main Stage0 tyenv utast with
      | Ok (_, ast) -> Result.ok (Imm ast)
      | Error err -> Result.error (TypecheckError err)
    end
  | UTVal(UTNonRec((ident, utast))) ->
    let typecheck_result =
      let open ResultMonad in

      let pre =
        {
          stage           = Stage0;
          type_parameters = TypeParameterMap.empty;
          row_parameters  = RowParameterMap.empty;
          quantifiability = Quantifiable;
          level           = Level.bottom;
        }
      in

      let presub = { pre with level = Level.succ pre.level; } in
      let (_, varnm) = ident in
      let evid = EvalVarID.fresh ident in
      let* (e1, ty1) = Typechecker.typecheck presub tyenv utast in
      let tyenv =
        let pty =
          if Typechecker.is_nonexpansive_expression e1 then
          (* If `e1` is polymorphically typeable: *)
            TypeConv.generalize pre.level (TypeConv.erase_range_of_type ty1)
          else
          (* If `e1` should be typed monomorphically: *)
            TypeConv.lift_poly (TypeConv.erase_range_of_type ty1)
        in
        let ventry =
          {
            val_type  = pty;
            val_name  = Some(evid);
            val_stage = pre.stage;
          }
        in
        tyenv |> Typeenv.add_value varnm ventry
      in
      return (tyenv, evid, e1)
    in
    begin
      match typecheck_result with
      | Ok (tyenv, evid, e1) -> Result.ok (Def (tyenv, evid, e1))
      | Error err -> Result.error (TypecheckError err)
    end
  | UTVal(_) ->
    Result.error NotImplemented
  | Empty ->
    Result.error EmptyInput

(* REPL APIs *)

let initialize () =
  let (tyenv, env) = Result.get_ok (Primitives.make_text_mode_environments ()) in
  { tyenv; env }

let feed ctx lexbuf =
  let stack = Lexer.reset_to_program () in
  let parse =
    try Result.ok (Parser.repl_main (Lexer.cut_token stack) lexbuf) with
    | _ -> Result.error ParseError
  in
  let typecheck expr =
    let result = typecheck ctx.tyenv expr in
    begin
      match result with
      | Ok(Def(tyenv, _, _)) -> ctx.tyenv <- tyenv
      | _ -> ()
    end;
    result
  in
  let handle expr =
    let eval = Evaluator.interpret_0 ctx.env in
    match expr with
    | Imm(ast) -> eval ast
    | Def(_, evid, ast) ->
      let result = eval ast in
      let env = Types.add_to_environment ctx.env evid (ref result) in
      ctx.env <- env;
      result
  in

  parse
  |> Fun.flip Result.bind typecheck
  |> Result.map handle
