
open MyUtil
open Types
open StaticEnv
open PackageSystemBase
open ConfigError
open ErrorReporting


let version =
  Printf.sprintf "SATySFi version %s alpha"
    (SemanticVersion.to_string Constant.current_language_version)


(* Initialization that should be performed before every cross-reference-solving loop *)
let reset () =
  let open ResultMonad in
  if OptionState.is_text_mode () then
    return ()
  else begin
    ImageInfo.initialize ();
    NamedDest.initialize ();
    return ()
  end


(* Initialization that should be performed before typechecking *)
let initialize () : Typeenv.t * environment =
  FreeID.initialize ();
  BoundID.initialize ();
  EvalVarID.initialize ();
  StoreID.initialize ();
  FontInfo.initialize ();
  let res =
    if OptionState.is_text_mode () then
      Primitives.make_text_mode_environments ()
    else
      Primitives.make_pdf_mode_environments ()
  in
  let (tyenv, env) =
    match res with
    | Ok(pair) -> pair
    | Error(e) -> raise (ConfigError(e))
  in
  begin
    if OptionState.is_bytecomp_mode () then
      Bytecomp.compile_environment env
    else
      ()
  end;
  (tyenv, env)


module StoreIDMap = Map.Make(StoreID)


type frozen_environment = location EvalVarIDMap.t * (syntactic_value StoreIDHashTable.t) ref * syntactic_value StoreIDMap.t


let freeze_environment (env : environment) : frozen_environment =
  let (valenv, stenvref) = env in
  let stmap =
    StoreIDMap.empty |> StoreIDHashTable.fold (fun stid value stmap ->
      stmap |> StoreIDMap.add stid value
    ) (!stenvref)
  in
  (valenv, stenvref, stmap)


let unfreeze_environment ((valenv, stenvref, stmap) : frozen_environment) : environment =
  let stenv = StoreIDHashTable.create 128 in
  stmap |> StoreIDMap.iter (fun stid value -> StoreIDHashTable.add stenv stid value);
  stenvref := stenv;
  (valenv, ref stenv)


let output_pdf (pdfret : HandlePdf.t) : unit =
  HandlePdf.write_to_file pdfret


let output_text (abspath_out : abs_path) (data : string) : unit =
  Core.Out_channel.write_all (get_abs_path_string abspath_out) ~data


let eval_library_file ~(run_tests : bool) (env : environment) (abspath : abs_path) (binds : binding list) : environment =
  Logging.begin_to_eval_file abspath;
  if OptionState.is_bytecomp_mode () then
    failwith "TODO: eval_libary_file, Bytecomp"
(*
    let (value, _) = Bytecomp.compile_and_exec_0 env ast in
    add_to_environment env evid (ref value)
*)
  else
    let (env, _) = Evaluator.interpret_bindings_0 ~run_tests env binds in
    env


let eval_main (i : int) (env_freezed : frozen_environment) (ast : abstract_tree) : syntactic_value =
  Logging.start_evaluation i;
  let res = reset () in
  begin
    match res with
    | Ok(())   -> ()
    | Error(e) -> raise (ConfigError(e))
  end;
  let env = unfreeze_environment env_freezed in
  let value =
    if OptionState.is_bytecomp_mode () then
      let (value, _) = Bytecomp.compile_and_exec_0 env ast in
      value
    else
      Evaluator.interpret_0 env ast
  in
  Logging.end_evaluation ();
  value


let eval_document_file (env : environment) (ast : abstract_tree) (abspath_out : abs_path) (abspath_dump : abs_path) =
  let env_freezed = freeze_environment env in
  if OptionState.is_text_mode () then
    let rec aux (i : int) =
      let value_str = eval_main i env_freezed ast in
      let s = EvalUtil.get_string value_str in
      match CrossRef.needs_another_trial abspath_dump with
      | CrossRef.NeedsAnotherTrial ->
          Logging.needs_another_trial ();
          aux (i + 1);

      | CrossRef.CountMax ->
          Logging.achieve_count_max ();
          output_text abspath_out s;
          Logging.end_output abspath_out;

      | CrossRef.CanTerminate unresolved_crossrefs ->
          Logging.achieve_fixpoint unresolved_crossrefs;
          output_text abspath_out s;
          Logging.end_output abspath_out;
    in
    aux 1
  else
    let rec aux (i : int) =
      let value_doc = eval_main i env_freezed ast in
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


(* Performs preprecessing. the evaluation is run by the naive interpreter
   regardless of whether `--bytecomp` was specified. *)
let preprocess_bindings ~(run_tests : bool) (env : environment) (libs : (abs_path * binding list) list) : environment * (abs_path * code_rec_or_nonrec list) list =
  let (env, codebindacc) =
    libs |> List.fold_left (fun (env, codebindacc) (abspath, binds) ->
      Logging.begin_to_preprocess_file abspath;
      let (env, cd_rec_or_nonrecs) = Evaluator.interpret_bindings_0 ~run_tests env binds in
      (env, Alist.extend codebindacc (abspath, cd_rec_or_nonrecs))
    ) (env, Alist.empty)
  in
  let codebinds = Alist.to_list codebindacc in
  (env, codebinds)


(* Performs evaluation and returns the resulting environment. *)
let evaluate_bindings ~(run_tests : bool) (env : environment) (codebinds : (abs_path * code_rec_or_nonrec list) list) : environment =
  codebinds |> List.fold_left (fun env (abspath, cd_rec_or_nonrecs) ->
    let binds =
      cd_rec_or_nonrecs |> List.map (fun cd_rec_or_nonrec ->
        Bind(Stage0, unlift_rec_or_nonrec cd_rec_or_nonrec)
      )
    in
    eval_library_file ~run_tests env abspath binds
  ) env


let error_log_environment (suspended : unit -> unit) : unit =
  try
    suspended ()
  with e ->
    let m = exn_to_error_message e in
    print_string m;
    exit 1


let setup_root_dirs ~(no_default_config : bool) ~(extra_config_paths : (string list) option) (curdir : string) : abs_path =
  let runtime_dirs =
    if Sys.os_type = "Win32" then
      match Sys.getenv_opt "SATYSFI_RUNTIME" with
      | None    -> []
      | Some(s) -> [ s ]
    else
      [ "/usr/local/share/satysfi"; "/usr/share/satysfi" ]
  in
  let home_dirs =
    if Sys.os_type = "Win32" then
      match Sys.getenv_opt "userprofile" with
      | None    -> []
      | Some(s) -> [ Filename.concat s ".satysfi" ]
    else
      match Sys.getenv_opt "HOME" with
      | None    -> []
      | Some(s) -> [ Filename.concat s ".satysfi" ]
  in
  let default_dirs =
    if no_default_config then
      []
    else
      List.concat [ home_dirs; runtime_dirs ]
  in
  let extra_dirs =
    match extra_config_paths with
    | None             -> [ Filename.concat curdir ".satysfi" ]
    | Some(extra_dirs) -> extra_dirs
  in
  let dirs = List.concat [ extra_dirs; default_dirs ] in
  begin
    match dirs with
    | []     -> raise NoLibraryRootDesignation
    | _ :: _ -> Config.initialize dirs
  end;
  let libpath = Constant.library_root_config_file in
  match Config.resolve_lib_file libpath with
  | Error(candidates) ->
      raise (ConfigError(LibraryRootConfigNotFoundIn(libpath, candidates)))

  | Ok(abspath) ->
      make_abs_path (Filename.dirname (get_abs_path_string abspath))


let make_absolute_if_relative ~(origin : string) (s : string) : abs_path =
  let abspath_str = if Filename.is_relative s then Filename.concat origin s else s in
  make_abs_path abspath_str


let get_candidate_file_extensions (output_mode : OptionState.output_mode) =
  match output_mode with
  | PdfMode           -> [ ".satyh"; ".satyg" ]
  | TextMode(formats) -> List.append (formats |> List.map (fun s -> ".satyh-" ^ s)) [ ".satyg" ]


type build_input =
  | PackageBuildInput of {
      lock : abs_path;
    }
  | DocumentBuildInput of {
      kind : input_kind;
      lock : abs_path;
      out  : abs_path;
      dump : abs_path;
    }


let get_input_kind_from_extension (abspathstr_in : string) =
  match Filename.extension abspathstr_in with
  | ".saty"  -> Ok(InputSatysfi)
  | ".satyw" -> Ok(InputSatysfiWorkspace)
  | ".md"    -> Ok(InputMarkdown)
  | ext      -> Error(ext)


let check_depended_packages ~(use_test_only_lock : bool) ~(library_root : abs_path) ~(extensions : string list) (tyenv_prim : Typeenv.t) (lock_config : LockConfig.t) =
  (* Resolve dependency among locked packages: *)
  let sorted_packages =
    match ClosedLockDependencyResolver.main ~use_test_only_lock ~library_root ~extensions lock_config with
    | Ok(sorted_packages) -> sorted_packages
    | Error(e)            -> raise (ConfigError(e))
  in

  (* Typecheck every locked package: *)
  let (genv, configenv, libacc) =
    sorted_packages |> List.fold_left (fun (genv, configenv, libacc) (_lock_name, (config, package)) ->
      let main_module_name =
        match package with
        | UTLibraryPackage{ main_module_name; _ } -> main_module_name
        | UTFontPackage{ main_module_name; _ }    -> main_module_name
      in
      let (ssig, libs) =
        match PackageChecker.main tyenv_prim genv package with
        | Ok(pair) -> pair
        | Error(e) -> raise (ConfigError(e))
      in
      let genv = genv |> GlobalTypeenv.add main_module_name ssig in
      let configenv = configenv |> GlobalTypeenv.add main_module_name config in
      let libacc = Alist.append libacc libs in
      (genv, configenv, libacc)
    ) (GlobalTypeenv.empty, GlobalTypeenv.empty, Alist.empty)
  in
  (genv, configenv, Alist.to_list libacc)


let make_package_lock_config_path (abspathstr_in : string) =
  make_abs_path (Printf.sprintf "%s/package.satysfi-lock" abspathstr_in)


let make_document_lock_config_path (basename_without_extension : string) =
  make_abs_path (Printf.sprintf "%s.satysfi-lock" basename_without_extension)


let make_output_mode text_mode_formats_str_opt =
  match text_mode_formats_str_opt with
  | None    -> OptionState.PdfMode
  | Some(s) -> OptionState.TextMode(String.split_on_char ',' s)


let load_lock_config (abspath_lock_config : abs_path) : LockConfig.t =
  match LockConfig.load abspath_lock_config with
  | Ok(lock_config) -> lock_config
  | Error(e)        -> raise (ConfigError(e))


let load_package ~(use_test_files : bool) ~(extensions : string list) (abspath_in : abs_path) =
  match PackageReader.main ~use_test_files ~extensions abspath_in with
  | Ok(pair) -> pair
  | Error(e) -> raise (ConfigError(e))


let build
    ~(fpath_in : string)
    ~(fpath_out_opt : string option)
    ~(fpath_jupyter_connection_opt : string option)
    ~(config_paths_str_opt : string option)
    ~(text_mode_formats_str_opt : string option)
    ~(page_number_limit : int)
    ~(show_full_path : bool)
    ~(debug_show_bbox : bool)
    ~(debug_show_space : bool)
    ~(debug_show_block_bbox : bool)
    ~(debug_show_block_space : bool)
    ~(debug_show_overfull : bool)
    ~(type_check_only : bool)
    ~(bytecomp : bool)
    ~(no_default_config : bool)
=
  error_log_environment (fun () ->
    let curdir = Sys.getcwd () in

    let input_file = make_absolute_if_relative ~origin:curdir fpath_in in
    let output_file = fpath_out_opt |> Option.map (make_absolute_if_relative ~origin:curdir) in
    let extra_config_paths = config_paths_str_opt |> Option.map (String.split_on_char ':') in
    let output_mode = make_output_mode text_mode_formats_str_opt in
    OptionState.set OptionState.{
      command_state =
        BuildState{
          input_file;
          output_file;
          output_mode;
          page_number_limit;
          debug_show_bbox;
          debug_show_space;
          debug_show_block_bbox;
          debug_show_block_space;
          debug_show_overfull;
          type_check_only;
          bytecomp;
        };
      extra_config_paths;
      show_full_path;
      no_default_config;
    };

    let library_root = setup_root_dirs ~no_default_config ~extra_config_paths curdir in
    let abspath_in = input_file in
    let build_input =
      let abspathstr_in = get_abs_path_string abspath_in in
      if Sys.is_directory abspathstr_in then
      (* If the input is a package directory: *)
        let abspath_lock_config = make_package_lock_config_path abspathstr_in in
        PackageBuildInput{
          lock = abspath_lock_config;
        }
      else
      (* If the input is a document file: *)
        let input_kind_res = get_input_kind_from_extension abspathstr_in in
        match input_kind_res with
        | Error(ext) ->
            raise (UnexpectedExtension(ext))

        | Ok(input_kind) ->
            let basename_without_extension = Filename.remove_extension abspathstr_in in
            let abspath_lock_config = make_document_lock_config_path basename_without_extension in
            let abspath_out =
              match (output_mode, output_file) with
              | (_, Some(abspath_out)) -> abspath_out
              | (TextMode(_), None)    -> raise ShouldSpecifyOutputFile
              | (PdfMode, None)        -> make_abs_path (Printf.sprintf "%s.pdf" basename_without_extension)
            in
            let abspath_dump = make_abs_path (Printf.sprintf "%s.satysfi-aux" basename_without_extension) in
            DocumentBuildInput{
              kind = input_kind;
              lock = abspath_lock_config;
              out  = abspath_out;
              dump = abspath_dump;
            }
    in

    let extensions = get_candidate_file_extensions output_mode in
    let (tyenv_prim, env) = initialize () in

    match build_input with
    | PackageBuildInput{
        lock = abspath_lock_config;
      } ->
        Logging.lock_config_file abspath_lock_config;
        let lock_config = load_lock_config abspath_lock_config in

        let (_config, package) = load_package ~use_test_files:false ~extensions abspath_in in

        let (genv, _configenv, _libs_dep) =
          check_depended_packages ~use_test_only_lock:false ~library_root ~extensions tyenv_prim lock_config
        in

        begin
          match PackageChecker.main tyenv_prim genv package with
          | Ok((_ssig, _libs)) -> ()
          | Error(e)           -> raise (ConfigError(e))
        end

    | DocumentBuildInput{
        kind = input_kind;
        lock = abspath_lock_config;
        out  = abspath_out;
        dump = abspath_dump;
      } ->
        Logging.lock_config_file abspath_lock_config;
        let lock_config = load_lock_config abspath_lock_config in

        Logging.target_file abspath_out;

        let dump_file_exists = CrossRef.initialize abspath_dump in
        Logging.dump_file ~already_exists:dump_file_exists abspath_dump;

        let (genv, configenv, libs) =
          check_depended_packages ~use_test_only_lock:false ~library_root ~extensions tyenv_prim lock_config
        in

        (* Resolve dependency of the document and the local source files: *)
        let (sorted_locals, (_, header, utast)) =
          match OpenFileDependencyResolver.main ~extensions input_kind configenv abspath_in with
          | Ok(pair) -> pair
          | Error(e) -> raise (ConfigError(e))
        in

        (* Typechecking and elaboration of libraries: *)
        let (libs_local, tyenv) =
          match PackageChecker.main_document tyenv_prim genv sorted_locals header with
          | Ok(pair) -> pair
          | Error(e) -> raise (ConfigError(e))
        in
        let libs = List.append libs libs_local in

        (* Typechecking and elaboration of the document: *)
        let workspace_mode = (input_kind = InputSatysfiWorkspace) in
        let ast =
          match PackageChecker.typecheck_document_file workspace_mode tyenv abspath_in utast with
          | Ok(ast)  -> ast
          | Error(e) -> raise (ConfigError(e))
        in

        if type_check_only then
          ()
        else
          (* Performs preprocessing: *)
          let (env, codebinds) = preprocess_bindings ~run_tests:false env libs in
          let code_doc = Evaluator.interpret_1 env ast in

          (* Performs evaluation: *)
          let env = evaluate_bindings ~run_tests:false env codebinds in
          let ast = unlift_code code_doc in
          if workspace_mode then
            Jupyter.main fpath_jupyter_connection_opt tyenv env ast abspath_out abspath_dump
          else
            eval_document_file env ast abspath_out abspath_dump
  )


type test_input =
  | PackageTestInput of {
      lock : abs_path;
    }
  | DocumentTestInput of {
      kind : input_kind;
      lock : abs_path;
    }


let test
    ~(fpath_in : string)
    ~(config_paths_str_opt : string option)
    ~(text_mode_formats_str_opt : string option)
    ~(show_full_path : bool)
    ~(no_default_config : bool)
=
  error_log_environment (fun () ->
    let curdir = Sys.getcwd () in

    let input_file_to_test = make_absolute_if_relative ~origin:curdir fpath_in in
    let extra_config_paths = config_paths_str_opt |> Option.map (String.split_on_char ':') in
    let output_mode_to_test = make_output_mode text_mode_formats_str_opt in
    OptionState.set OptionState.{
      command_state =
        TestState{
          input_file_to_test;
          output_mode_to_test;
        };
      extra_config_paths;
      show_full_path;
      no_default_config;
    };

    let library_root = setup_root_dirs ~no_default_config ~extra_config_paths curdir in
    let abspath_in = input_file_to_test in
    let test_input =
      let abspathstr_in = get_abs_path_string abspath_in in
      if Sys.is_directory abspathstr_in then
      (* If the input is a package directory: *)
        let abspath_lock_config = make_package_lock_config_path abspathstr_in in
        PackageTestInput{
          lock = abspath_lock_config;
        }
      else
      (* If the input is a document file: *)
        let input_kind_res = get_input_kind_from_extension abspathstr_in in
        match input_kind_res with
        | Error(ext) ->
            raise (UnexpectedExtension(ext))

        | Ok(input_kind) ->
            let basename_without_extension = Filename.remove_extension abspathstr_in in
            let abspath_lock_config = make_document_lock_config_path basename_without_extension in
            DocumentTestInput{
              kind = input_kind;
              lock = abspath_lock_config;
            }
    in

    let extensions = get_candidate_file_extensions output_mode_to_test in
    let (tyenv_prim, env) = initialize () in

    begin
      match test_input with
      | PackageTestInput{
          lock = abspath_lock_config;
        } ->
          Logging.lock_config_file abspath_lock_config;
          let lock_config = load_lock_config abspath_lock_config in

          let (_config, package) = load_package ~use_test_files:true ~extensions abspath_in in

          let (genv, _configenv, _libs_dep) =
            check_depended_packages ~use_test_only_lock:true ~library_root ~extensions tyenv_prim lock_config
          in

          let libs =
            match PackageChecker.main tyenv_prim genv package with
            | Ok((_ssig, libs)) -> libs
            | Error(e)          -> raise (ConfigError(e))
          in
          let (env, codebinds) = preprocess_bindings ~run_tests:true env libs in
          let _env = evaluate_bindings ~run_tests:true env codebinds in
          ()

      | DocumentTestInput{
          kind = input_kind;
          lock = abspath_lock_config;
        } ->
          Logging.lock_config_file abspath_lock_config;
          let lock_config = load_lock_config abspath_lock_config in

          let (genv, configenv, libs) =
            check_depended_packages ~use_test_only_lock:true ~library_root ~extensions tyenv_prim lock_config
          in

          (* Resolve dependency of the document and the local source files: *)
          let (sorted_locals, (_, header, utast)) =
            match OpenFileDependencyResolver.main ~extensions input_kind configenv abspath_in with
            | Ok(pair) -> pair
            | Error(e) -> raise (ConfigError(e))
          in

          (* Typechecking and elaboration of libraries: *)
          let (libs_local, tyenv) =
            match PackageChecker.main_document tyenv_prim genv sorted_locals header with
            | Ok(pair) -> pair
            | Error(e) -> raise (ConfigError(e))
          in
          let libs = List.append libs libs_local in

          (* Typechecking and elaboration of the document: *)
          let workspace_mode = (input_kind = InputSatysfiWorkspace) in
          let _ = PackageChecker.typecheck_document_file workspace_mode tyenv abspath_in utast in
          let (env, codebinds) = preprocess_bindings ~run_tests:true env libs in
          let _env = evaluate_bindings ~run_tests:true env codebinds in
          ()
    end;
    let test_results = State.get_all_test_results () in
    let failure_found =
      test_results |> List.fold_left (fun failure_found test_result ->
        match test_result with
        | State.Pass{ test_name }          -> Logging.report_passed_test ~test_name; failure_found
        | State.Fail{ test_name; message } -> Logging.report_failed_test ~test_name ~message; true
      ) false
    in
    if failure_found then begin
      Logging.some_test_failed ();
      exit 1
    end else begin
      Logging.all_tests_passed ();
      ()
    end
  )


type solve_input =
  | PackageSolveInput of {
      root : abs_path; (* The absolute path of a directory used as the package root *)
      lock : abs_path; (* A path for writing a resulting lock file *)
    }
  | DocumentSolveInput of {
      kind : input_kind;
      path : abs_path; (* The absolute path to the document file *)
      lock : abs_path; (* A path for writing a resulting lock file *)
    }


let make_lock_name (lock : Lock.t) : lock_name =
  let Lock.{ registry_hash_value; package_name; locked_version } = lock in
  Printf.sprintf "registered.%s.%s.%s"
    registry_hash_value
    package_name
    (SemanticVersion.to_string locked_version)


let convert_solutions_to_lock_config (solutions : package_solution list) : LockConfig.t * implementation_spec list =
  let (locked_package_acc, impl_spec_acc) =
    solutions |> List.fold_left (fun (locked_package_acc, impl_spec_acc) solution ->
      let lock = solution.lock in
      let Lock.{ registry_hash_value; package_name; locked_version = version } = lock in
      let locked_package =
        LockConfig.{
          lock_name         = make_lock_name lock;
          lock_contents     = RegisteredLock{ registry_hash_value; package_name; version };
          lock_dependencies = solution.locked_dependencies |> List.map make_lock_name;
          test_only_lock    = solution.used_in_test_only;
        }
      in
      let impl_spec =
        ImplSpec{
          lock   = solution.lock;
          source = solution.locked_source;
        }
      in
      (Alist.extend locked_package_acc locked_package, Alist.extend impl_spec_acc impl_spec)
    ) (Alist.empty, Alist.empty)
  in
  let lock_config = LockConfig.{ locked_packages = Alist.to_list locked_package_acc } in
  (lock_config, Alist.to_list impl_spec_acc)


let extract_attributes_from_document_file (input_kind : input_kind) (abspath_in : abs_path) : (DocumentAttribute.t, config_error) result =
  let open ResultMonad in
  Logging.begin_to_parse_file abspath_in;
  match input_kind with
  | InputSatysfi
  | InputSatysfiWorkspace ->
      let* utsrc =
        ParserInterface.process_file abspath_in
          |> Result.map_error (fun rng -> FailedToParse(rng))
      in
      let* (attrs, _header, _utast) =
        match utsrc with
        | UTLibraryFile(_)      -> err @@ DocumentLacksWholeReturnValue(abspath_in)
        | UTDocumentFile(utdoc) -> return utdoc
      in
      DocumentAttribute.make attrs
        |> Result.map_error (fun e -> DocumentAttributeError(e))

  | InputMarkdown ->
      let* (docattr, _main_module_name, _md) =
        match read_file abspath_in with
        | Ok(data)   -> MarkdownParser.decode data |> Result.map_error (fun e -> MarkdownError(e))
        | Error(msg) -> err (CannotReadFileOwingToSystem(msg))
      in
      return docattr


let make_registry_hash_value (registry_remote : registry_remote) : (registry_hash_value, config_error) result =
  let open ResultMonad in
  match registry_remote with
  | GitRegistry{ url; branch } ->
      let* canonicalized_url =
        CanonicalRegistryUrl.make url
          |> Result.map_error (fun e -> CanonicalRegistryUrlError(e))
      in
      let hash_value =
        Digest.to_hex (Digest.string (Printf.sprintf "git#%s#%s" canonicalized_url branch))
      in
      Logging.report_canonicalized_url ~url ~canonicalized_url ~hash_value;
      return hash_value


let update_library_root_config_if_needed (registries : registry_remote RegistryHashValueMap.t) (registry_hash_value : registry_hash_value) (registry_remote : registry_remote) (abspath_library_root : abs_path) : unit =
  match
    registries |> RegistryHashValueMap.find_opt registry_hash_value
  with
  | None ->
      let library_root_config =
        LibraryRootConfig.{
          registries = registries |> RegistryHashValueMap.add registry_hash_value registry_remote;
        }
      in
      LibraryRootConfig.write abspath_library_root library_root_config

  | Some(_registry_remote) ->
      ()


let solve
    ~(fpath_in : string)
    ~(show_full_path : bool)
    ~(config_paths_str_opt : string option)
    ~(no_default_config : bool)
=
  error_log_environment (fun () ->
    let curdir = Sys.getcwd () in

    let extra_config_paths = config_paths_str_opt |> Option.map (String.split_on_char ':') in

    OptionState.set OptionState.{
      command_state = SolveState;
      extra_config_paths;
      show_full_path;
      no_default_config;
    };

    let library_root = setup_root_dirs ~no_default_config ~extra_config_paths curdir in
    let abspath_in = make_absolute_if_relative ~origin:curdir fpath_in in
    let solve_input =
      let abspathstr_in = get_abs_path_string abspath_in in
      if Sys.is_directory abspathstr_in then
      (* If the input is a package directory: *)
        let abspath_lock_config = make_package_lock_config_path abspathstr_in in
        PackageSolveInput{
          root = abspath_in;
          lock = abspath_lock_config;
        }
      else
        let abspathstr_in = get_abs_path_string abspath_in in
        let basename_without_extension = Filename.remove_extension abspathstr_in in
        let abspath_lock_config = make_document_lock_config_path basename_without_extension in
        let input_kind_res = get_input_kind_from_extension abspathstr_in in
        match input_kind_res with
        | Error(ext) ->
            raise (UnexpectedExtension(ext))

        | Ok(input_kind) ->
            DocumentSolveInput{
              kind = input_kind;
              path = abspath_in;
              lock = abspath_lock_config;
            }
    in

    let res =
      let open ResultMonad in
      let abspath_library_root_config =
        make_abs_path
          (Filename.concat
            (get_abs_path_string library_root)
            (get_lib_path_string Constant.library_root_config_file))
      in
      let* library_root_config = LibraryRootConfig.load abspath_library_root_config in
      let* (dependencies_with_flags, abspath_lock_config, registry_specs) =
        match solve_input with
        | PackageSolveInput{
            root = absdir_package;
            lock = abspath_lock_config;
          } ->
            let* PackageConfig.{ package_contents; registry_specs; _ } = PackageConfig.load absdir_package in
            begin
              match package_contents with
              | PackageConfig.Library{ dependencies; test_dependencies; _ } ->
                  let dependencies_with_flags =
                    List.append
                      (dependencies |> List.map (fun dep -> (SourceDependency, dep)))
                      (test_dependencies |> List.map (fun dep -> (TestOnlyDependency, dep)))
                  in
                  return (dependencies_with_flags, abspath_lock_config, registry_specs)

              | PackageConfig.Font(_) ->
                  return ([], abspath_lock_config, registry_specs)
            end

        | DocumentSolveInput{
            kind = input_kind;
            path = abspath_in;
            lock = abspath_lock_config;
          } ->
            let* DocumentAttribute.{ registry_specs; dependencies } =
              extract_attributes_from_document_file input_kind abspath_in
            in
            let dependencies_with_flags = dependencies |> List.map (fun dep -> (SourceDependency, dep)) in
            return (dependencies_with_flags, abspath_lock_config, registry_specs)
      in

      Logging.show_package_dependency_before_solving dependencies_with_flags;

      let* registries =
        RegistryLocalNameMap.fold (fun registry_local_name registry_remote res ->
          let* registries = res in
          let* registry_hash_value = make_registry_hash_value registry_remote in

          (* Manupulates the library root config: *)
          update_library_root_config_if_needed
            library_root_config.LibraryRootConfig.registries
            registry_hash_value
            registry_remote
            abspath_library_root_config;

          (* Fetches registry configs: *)
          let absdir_registry_repo =
            let libpath_registry_root = Constant.registry_root_directory registry_hash_value in
            make_abs_path
              (Filename.concat
                (get_abs_path_string library_root)
                (get_lib_path_string libpath_registry_root))
          in
          let git_command = "git" in (* TODO: make this changeable *)
          let* () =
            PackageRegistryFetcher.main ~git_command absdir_registry_repo registry_remote
              |> Result.map_error (fun e -> PackageRegistryFetcherError(e))
          in

          let* PackageRegistryConfig.{ packages = packages_in_registry } =
            let abspath_registry_config =
              make_abs_path
                (Filename.concat
                  (get_abs_path_string absdir_registry_repo)
                  Constant.package_registry_config_file_name)
            in
            PackageRegistryConfig.load abspath_registry_config
          in
          let registry_spec = { packages_in_registry; registry_hash_value } in
          return (registries |> RegistryLocalNameMap.add registry_local_name registry_spec)

        ) registry_specs (return RegistryLocalNameMap.empty)
      in

      let package_context = { registries } in
      let solutions_opt = PackageConstraintSolver.solve package_context dependencies_with_flags in
      begin
        match solutions_opt with
        | None ->
            err CannotSolvePackageConstraints

        | Some(solutions) ->

            Logging.show_package_dependency_solutions solutions;

            let (lock_config, impl_specs) = convert_solutions_to_lock_config solutions in

            let wget_command = "wget" in (* TODO: make this changeable *)
            let tar_command = "tar" in (* TODO: make this changeable *)
            let unzip_command = "unzip" in (* TODO: make this changeable *)
            let* () =
              impl_specs |> foldM (fun () impl_spec ->
                LockFetcher.main
                  ~wget_command ~tar_command ~unzip_command ~library_root impl_spec
              ) ()
            in
            LockConfig.write abspath_lock_config lock_config;
            return ()
      end
    in
    begin
      match res with
      | Ok(())   -> ()
      | Error(e) -> raise (ConfigError(e))
    end
  )
