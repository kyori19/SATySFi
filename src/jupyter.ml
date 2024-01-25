
open ErrorReporting
open MyUtil
open SyntaxBase
open Types


external start_kernel : string -> unit = "start_kernel"
external return_text : int -> string -> unit = "return_text"
external return_pdf : int -> bytes -> bool -> unit = "return_pdf"
external return_error_message : string -> string -> string list -> unit = "return_error_message"


let execute tyenv env output_in_pdf fnast abspath_out abspath_dump execution_counter code =
  try
    (* Lex / Parse *)
    let utcell =
      let module I = Parser.MenhirInterpreter in

      let stack = Lexer.initialize Lexer.CellState in
      let buf = Lexing.from_string code in
      Lexing.set_filename buf "<input>";
      let supp = I.lexer_lexbuf_to_supplier (Lexer.cut_token stack) buf in
      let cp = Parser.Incremental.cell_main buf.lex_curr_p in
      let k_fail chkpt =
        match chkpt with
        | I.HandlingError(penv) ->
            let (lposS, lposE) = I.positions penv in
            let cnumS = lposS.Lexing.pos_cnum - lposS.Lexing.pos_bol in
            let cnumE = lposE.Lexing.pos_cnum - lposE.Lexing.pos_bol in
            let rng = Range.make lposS.Lexing.pos_fname lposS.Lexing.pos_lnum cnumS cnumE in
            raise (ConfigError(FailedToParse(CannotProgressParsing(rng))))

        | _ ->
            assert false
      in
      I.loop_handle Fun.id k_fail supp cp
    in

    match utcell with
    | CellDefs(utbinds) ->
      (* Typecheck *)
      let binds =
        match ModuleTypechecker.typecheck_binding_list !tyenv utbinds with
        | Ok (tyenv_new, _, binds) ->
            tyenv := tyenv_new;
            binds
        | Error e -> raise (ConfigError(TypeError e))
      in

      (* Evaluate *)
      let _ =
        let codebinds =
          let (env_new, codebinds) = Evaluator.interpret_bindings_0 ~run_tests:false !env binds in
          env := env_new;
          codebinds
        in
        let binds =
          codebinds |> List.map (fun cd_rec_or_nonrec ->
            Bind(Stage0, unlift_rec_or_nonrec cd_rec_or_nonrec)
          )
        in
        let (env_new, _) = Evaluator.interpret_bindings_0 ~run_tests:false !env binds in
        env := env_new
      in
      ()
    | CellExpr(utast) ->
      (* Typecheck *)
      let (ty, ast) =
        match Typechecker.main Stage1 !tyenv utast with
        | Ok ast  -> ast
        | Error e -> raise (ConfigError(TypeError e))
      in

      (* Evaluate input *)
      let ast =
        let code = Evaluator.interpret_1 !env ast in
        unlift_code code
      in

      (* Check output type *)
      let (ast, is_doc) =
        if Typechecker.are_unifiable ty (Range.dummy "workspace-input", BaseType(BlockTextType)) then
          Apply(LabelMap.empty, fnast, ast), true
        else
          ast, Typechecker.are_unifiable ty (Range.dummy "pdf-mode", BaseType(DocumentType))
      in

      (* Evaluate code *)
      let eval_main i =
        Logging.start_evaluation i;
        ImageInfo.initialize ();
        NamedDest.initialize ();
        let value =
          Evaluator.interpret_0 !env ast
        in
        Logging.end_evaluation ();
        value
      in

      if is_doc then
        (* Export PDF *)
        let output_pdf (pdfret : HandlePdf.t) : unit =
          HandlePdf.write_to_file pdfret;
          let i = open_in_bin (get_abs_path_string abspath_out) in
          let len = in_channel_length i in
          let buf = Bytes.create len in
          really_input i buf 0 len;
          close_in i;
          return_pdf execution_counter buf (not !output_in_pdf)
        in
        let rec aux (i : int) =
          let value_doc = eval_main i in
          match value_doc with
          | BaseConstant(BCDocument(paper_size, pbstyle, columnhookf, columnendhookf, pagecontf, pagepartsf, imvblst)) ->
              Logging.start_page_break ();
              State.start_page_break ();
              let pdf =
                match pbstyle with
                | SingleColumn ->
                    PageBreak.main abspath_out ~paper_size
                      columnhookf pagecontf pagepartsf imvblst
    
                | MultiColumn(origin_shifts) ->
                    PageBreak.main_multicolumn abspath_out ~paper_size
                      origin_shifts columnhookf columnendhookf pagecontf pagepartsf imvblst
              in
              begin
                match CrossRef.needs_another_trial abspath_dump with
                | CrossRef.NeedsAnotherTrial ->
                    Logging.needs_another_trial ();
                    aux (i + 1);
    
                | CrossRef.CountMax ->
                    Logging.achieve_count_max ();
                    output_pdf pdf;
                    Logging.end_output abspath_out;
    
                | CrossRef.CanTerminate unresolved_crossrefs ->
                    Logging.achieve_fixpoint unresolved_crossrefs;
                    output_pdf pdf;
                    Logging.end_output abspath_out;
              end
    
          | _ ->
              EvalUtil.report_bug_value "main; not a DocumentValue(...)" value_doc
        in
        aux 1
      else
        let value = eval_main 1 in
        let value_str =
          match value with
          | BaseConstant(BCUnit) -> "()"
          | BaseConstant(BCBool b) -> string_of_bool b
          | BaseConstant(BCInt i) -> string_of_int i
          | BaseConstant(BCFloat f) -> string_of_float f
          | BaseConstant(BCString s) -> "`" ^ s ^ "`"
          | _ -> "<not implemented>"
        in
        return_text execution_counter value_str
    | CellCmd(cmd_name) ->
      begin
        match cmd_name with
        | "render-in-pdf" ->
            output_in_pdf := true;
            return_text execution_counter "Output mode is switched to PDF."
        | "render-in-image" ->
            output_in_pdf := false;
            return_text execution_counter "Output mode is switched to image."
        | _ ->
            return_error_message "SATySFi" ("Unknown command: " ^ cmd_name) []
      end
  with e ->
    let msg = exn_to_error_message e in
    return_error_message "SATySFi" msg []


let main connection_file tyenv env fnast abspath_out abspath_dump =
  let tyenv = ref tyenv in
  let env = ref env in
  let output_in_pdf = ref false in
  Callback.register "satysfi_jupyter_kernel_execute" (execute tyenv env output_in_pdf fnast abspath_out abspath_dump);
  let file_name =
    match connection_file with
    | None -> "connection.json"
    | Some file_name -> file_name
  in
  start_kernel file_name
