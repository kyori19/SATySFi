
open StaticEnv
open Types
open TypeError

type repl_context = {
  mutable tyenv : type_environment;
  mutable env : environment;
}

type repl_error =
  | EmptyInput
  | ParseError
  | TypecheckError of type_error
  | NotImplemented

type repl_expr =
  | Imm of abstract_tree
  | Def of type_environment * EvalVarID.t * abstract_tree

val initialize : unit -> repl_context

val feed :
  repl_context -> Lexing.lexbuf -> (syntactic_value, repl_error) result
