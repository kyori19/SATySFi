
open MyUtil
open EnvelopeSystemBase
open PackageSystemBase
open ConfigError


let version =
  Printf.sprintf "Saphe version %s alpha"
    (SemanticVersion.to_string Constant.current_ecosystem_version)


type line =
  | NormalLine  of string
  | DisplayLine of string


let report_error (lines : line list) : unit =
  print_string "! ";
  lines |> List.fold_left (fun (is_first : bool) (line : line) ->
    begin
      match line with
      | NormalLine(s) ->
          if is_first then
            print_endline s
          else
            print_endline ("    " ^ s)

      | DisplayLine(s) ->
          if is_first then
            print_endline ("\n      " ^ s)
          else
            print_endline ("      " ^ s)
    end;
    false
  ) true |> ignore


let show_yaml_context (yctx : YamlDecoder.context) =
  Printf.sprintf "(context: %s)" (YamlDecoder.show_yaml_context yctx)


let make_yaml_error_lines : yaml_error -> line list = function
  | ParseError(s) ->
      [ NormalLine(Printf.sprintf "parse error: %s" s) ]

  | FieldNotFound(yctx, field) ->
      [ NormalLine(Printf.sprintf "field '%s' not found %s" field (show_yaml_context yctx)) ]

  | NotAFloat(yctx) ->
      [ NormalLine(Printf.sprintf "not a float value %s" (show_yaml_context yctx)) ]

  | NotAString(yctx) ->
      [ NormalLine(Printf.sprintf "not a string value %s" (show_yaml_context yctx)) ]

  | NotABool(yctx) ->
      [ NormalLine(Printf.sprintf "not a Boolean value %s" (show_yaml_context yctx)) ]

  | NotAnArray(yctx) ->
      [ NormalLine(Printf.sprintf "not an array %s" (show_yaml_context yctx)) ]

  | NotAnObject(yctx) ->
      [ NormalLine(Printf.sprintf "not an object %s" (show_yaml_context yctx)) ]

  | BreaksVersionRequirement(yctx, requirement) ->
      [ NormalLine(Printf.sprintf "breaks the requrement '%s' %s" (SemanticVersion.requirement_to_string requirement)(show_yaml_context yctx)) ]

  | NotASemanticVersion(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a semantic version: '%s' %s" s (show_yaml_context yctx)) ]

  | NotAVersionRequirement(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a version requirement: '%s' %s" s (show_yaml_context yctx)) ]

  | InvalidPackageName(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a package name: '%s' %s" s (show_yaml_context yctx)) ]

  | DuplicateRegistryHashValue{ context = yctx; registry_hash_value } ->
      [ NormalLine(Printf.sprintf "More than one definition for registry hash value '%s' %s" registry_hash_value (show_yaml_context yctx)) ]

  | CannotBeUsedAsAName(yctx, s) ->
      [ NormalLine(Printf.sprintf "'%s' cannot be used as a name %s" s (show_yaml_context yctx)) ]

  | UnsupportedConfigFormat(format) ->
      [ NormalLine(Printf.sprintf "unsupported config format '%s'" format) ]

  | NotACommand{ context = yctx; prefix = _; string = s } ->
      [ NormalLine(Printf.sprintf "not a command: '%s' %s" s (show_yaml_context yctx)) ]

  | BranchNotFound{ context = yctx; expected_tags; got_tags } ->
      [
        NormalLine(Printf.sprintf "expected tags not found; should contain exactly one of:");
        DisplayLine(expected_tags |> String.concat ", ");
        NormalLine("but only contains:");
        DisplayLine(got_tags |> String.concat ", ");
        NormalLine(Printf.sprintf "%s" (show_yaml_context yctx));
      ]

  | MoreThanOneBranchFound{ context = yctx; expected_tags; got_tags } ->
      [
        NormalLine(Printf.sprintf "more than one expected tag found:");
        DisplayLine(got_tags |> String.concat ", ");
        NormalLine("should be exactly one of:");
        DisplayLine(expected_tags |> String.concat ", ");
        NormalLine(Printf.sprintf "%s" (show_yaml_context yctx));
      ]



let report_config_error = function
  | CannotDetermineStoreRoot{ envvar } ->
      report_error [
        NormalLine("cannot determine where the store root is;");
        NormalLine(Printf.sprintf "set environment variable '%s'." envvar);
      ]

  | PackageDirectoryNotFound(candidate_paths) ->
      let lines =
        candidate_paths |> List.map (fun path ->
          DisplayLine(Printf.sprintf "- %s" path)
        )
      in
      report_error
        (NormalLine("cannot find package directory. candidates:") :: lines)

  | PackageConfigNotFound(abspath_package_config) ->
      report_error [
        NormalLine("cannot find a package config:");
        DisplayLine(get_abs_path_string abspath_package_config);
      ]

  | PackageConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: package config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | NotAPackageButADocument(abspath_package_config) ->
      report_error [
        NormalLine(Printf.sprintf "in %s:" (get_abs_path_string abspath_package_config));
        NormalLine("this file is expected to be a config for a package,");
        NormalLine("but is for a document.");
      ]

  | LockConfigNotFound(abspath) ->
      report_error [
        NormalLine("cannot find a lock config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | LockConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: lock config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | RegistryConfigNotFound(abspath_registry_config) ->
      report_error [
        NormalLine("cannot find a registry config:");
        DisplayLine(get_abs_path_string abspath_registry_config);
      ]

  | RegistryConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: registry config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | StoreRootConfigNotFound(abspath) ->
      report_error [
        NormalLine("cannot find a store root config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | StoreRootConfigError(abspath, e) ->
      report_error (List.concat [
        [ NormalLine(Printf.sprintf "in %s: store root config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | LockNameConflict(lock_name) ->
      report_error [
        NormalLine(Printf.sprintf "lock name conflict: '%s'" lock_name);
      ]

  | LockedPackageNotFound(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error
        (NormalLine(Printf.sprintf "package '%s' not found. candidates:" (get_lib_path_string libpath)) :: lines)

  | DependencyOnUnknownLock{ depending; depended } ->
      report_error [
        NormalLine(Printf.sprintf "unknown depended lock '%s' of '%s'." depended depending);
      ]
(*
  | CyclicLockDependency(cycle) ->
      let pairs =
        match cycle with
        | Loop(pair)   -> [ pair ]
        | Cycle(pairs) -> pairs |> TupleList.to_list
      in
      let lines =
        pairs |> List.map (fun (modnm, _lock) ->
          DisplayLine(Printf.sprintf "- '%s'" modnm)
        )
      in
      report_error
        (NormalLine("the following packages are cyclic:") :: lines)
*)
  | CannotFindLibraryFile(libpath, candidate_paths) ->
      let lines =
        candidate_paths |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      report_error
        (NormalLine(Printf.sprintf "cannot find '%s'. candidates:" (get_lib_path_string libpath)) :: lines)

  | CannotSolvePackageConstraints ->
      report_error [
        NormalLine("cannot solve package constraints.");
      ]
(*
  | DocumentAttributeError(e) ->
      report_document_attribute_error e

  | MarkdownClassNotFound(modnm) ->
      report_error [
        NormalLine(Printf.sprintf "package '%s' not found; required for converting Markdown documents." modnm);
      ]

  | NoMarkdownConversion(modnm) ->
      report_error [
        NormalLine(Printf.sprintf "package '%s' contains no Markdown conversion rule." modnm);
      ]

  | MoreThanOneMarkdownConversion(modnm) ->
      report_error [
        NormalLine(Printf.sprintf "package '%s' contains more than one Markdown conversion rule." modnm);
      ]

  | MarkdownError(e) ->
      begin
        match e with
        | InvalidHeaderComment ->
            report_error [
              NormalLine("invalid or missing header comment of a Markdown document.");
            ]

        | InvalidExtraExpression ->
            report_error [
              NormalLine("cannot parse an extra expression in a Markdown document.");
            ]

        | FailedToMakeDocumentAttribute(de) ->
            report_document_attribute_error de
      end
*)
  | FailedToFetchTarball{ lock_name; exit_status; command } ->
      report_error [
        NormalLine(Printf.sprintf "failed to fetch '%s' (exit status: %d). command:" lock_name exit_status);
        DisplayLine(command);
      ]

  | FailedToExtractTarball{ lock_name; exit_status; command } ->
      report_error [
        NormalLine(Printf.sprintf "failed to extract the tarball of '%s' (exit status: %d). command:" lock_name exit_status);
        DisplayLine(command);
      ]

  | FailedToFetchExternalZip{ url; exit_status; command } ->
      report_error [
        NormalLine(Printf.sprintf "failed to fetch file from '%s' (exit status: %d). command:" url exit_status);
        DisplayLine(command);
      ]

  | ExternalZipChecksumMismatch{ url; path; expected; got } ->
      report_error [
        NormalLine("checksum mismatch of an external zip file.");
        DisplayLine(Printf.sprintf "- fetched from: '%s'" url);
        DisplayLine(Printf.sprintf "- path: '%s'" (get_abs_path_string path));
        DisplayLine(Printf.sprintf "- expected: '%s'" expected);
        DisplayLine(Printf.sprintf "- got: '%s'" got);
      ]

  | TarGzipChecksumMismatch{ lock_name; url; path; expected; got } ->
      report_error [
        NormalLine("checksum mismatch of a tarball.");
        DisplayLine(Printf.sprintf "- lock name: '%s'" lock_name);
        DisplayLine(Printf.sprintf "- fetched from: '%s'" url);
        DisplayLine(Printf.sprintf "- path: '%s'" (get_abs_path_string path));
        DisplayLine(Printf.sprintf "- expected: '%s'" expected);
        DisplayLine(Printf.sprintf "- got: '%s'" got);
      ]

  | FailedToExtractExternalZip{ exit_status; command } ->
      report_error [
        NormalLine(Printf.sprintf "failed to extract a zip file (exit status: %d). command:" exit_status);
        DisplayLine(command);
      ]

  | FailedToCopyFile{ exit_status; command } ->
      report_error [
        NormalLine(Printf.sprintf "failed to copy a file (exit status: %d). command:" exit_status);
        DisplayLine(command);
      ]

  | PackageRegistryFetcherError(e) ->
      begin
        match e with
        | FailedToUpdateGitRegistry{ exit_status; command } ->
            report_error [
              NormalLine(Printf.sprintf "failed to update registry (exit status: %d). command:" exit_status);
              DisplayLine(command);
            ]
      end

  | CanonicalRegistryUrlError(e) ->
      begin
        match e with
        | ContainsQueryParameter{ url } ->
            report_error [
              NormalLine("registry URLs must not contain query parameters:");
              DisplayLine(url);
            ]

        | NoUriScheme{ url } ->
            report_error [
              NormalLine("the registry URL does not contain a scheme:");
              DisplayLine(url);
            ]

        | UnexpectedUrlScheme{ url; scheme } ->
            report_error [
              NormalLine(Printf.sprintf "unexpected scheme '%s' in a registry URL:" scheme);
              DisplayLine(url);
            ]
      end

  | CannotWriteEnvelopeConfig{ message; path } ->
      report_error [
        NormalLine(Printf.sprintf "cannot write an envelope config to '%s' (message: '%s')" (get_abs_path_string path) message);
      ]

  | CannotWriteLockConfig{ message; path } ->
      report_error [
        NormalLine(Printf.sprintf "cannot write a lock config to '%s' (message: '%s')" (get_abs_path_string path) message);
      ]

  | CannotWriteDepsConfig{ message; path } ->
      report_error [
        NormalLine(Printf.sprintf "cannot write a deps config to '%s' (message: '%s')" (get_abs_path_string path) message);
      ]

  | CannotWriteStoreRootConfig{ message; path } ->
      report_error [
        NormalLine(Printf.sprintf "cannot write a store root config to '%s' (message: '%s')" (get_abs_path_string path) message);
      ]

  | MultiplePackageDefinition{ package_name } ->
      report_error [
        NormalLine(Printf.sprintf "More than one definition for package '%s'." package_name)
      ]

  | DuplicateRegistryLocalName{ registry_local_name } ->
      report_error [
        NormalLine(Printf.sprintf "more than one definition for registry local name '%s'" registry_local_name)
      ]

  | UndefinedRegistryLocalName{ registry_local_name } ->
      report_error [
        NormalLine(Printf.sprintf "undefined registry local name '%s'" registry_local_name)
      ]


type solve_input =
  | PackageSolveInput of {
      root     : abs_path; (* The absolute path of a directory used as the package root *)
      lock     : abs_path; (* A path for writing a resulting lock config file *)
      envelope : abs_path; (* A path for writing a resulting envelope config file *)
    }
  | DocumentSolveInput of {
      doc    : abs_path; (* The absolute path to the document file *)
      config : abs_path; (* The absolute path to the config file *)
      lock   : abs_path; (* A path for writing a resulting lock file *)
    }


let update_store_root_config_if_needed (registries : registry_remote RegistryHashValueMap.t) (registry_hash_value : registry_hash_value) (registry_remote : registry_remote) (abspath_store_root : abs_path) : (unit, config_error) result =
  let open ResultMonad in
  match
    registries |> RegistryHashValueMap.find_opt registry_hash_value
  with
  | None ->
      let store_root_config =
        StoreRootConfig.{
          registries = registries |> RegistryHashValueMap.add registry_hash_value registry_remote;
        }
      in
      StoreRootConfig.write abspath_store_root store_root_config

  | Some(_registry_remote) ->
      return ()


let make_lock_name (lock : Lock.t) : lock_name =
  let Lock.{ package_id; locked_version } = lock in
  let PackageId.{ registry_hash_value; package_name } = package_id in
  Printf.sprintf "registered.%s.%s.%s"
    registry_hash_value
    package_name
    (SemanticVersion.to_string locked_version)


let make_lock_dependency (dep : locked_dependency) : LockConfig.lock_dependency =
  {
    depended_lock_name = make_lock_name dep.depended_lock;
    used_as            = dep.dependency_used_as;
  }


let convert_solutions_to_lock_config (solutions : package_solution list) : LockConfig.t * implementation_spec list =
  let (locked_package_acc, impl_spec_acc) =
    solutions |> List.fold_left (fun (locked_package_acc, impl_spec_acc) solution ->
      let { lock; locked_source; _ } = solution in
      let locked_package =
        LockConfig.{
          lock_name         = make_lock_name lock;
          lock_contents     = RegisteredLock(lock);
          lock_dependencies = solution.locked_dependencies |> List.map make_lock_dependency;
          test_only_lock    = solution.used_in_test_only;
        }
      in
      let impl_spec = ImplSpec{ lock; source = locked_source } in
      (Alist.extend locked_package_acc locked_package, Alist.extend impl_spec_acc impl_spec)
    ) (Alist.empty, Alist.empty)
  in
  let explicit_dependencies =
    solutions |> List.filter_map (fun solution ->
      solution.explicitly_depended |> Option.map (fun used_as ->
        LockConfig.{
          depended_lock_name = make_lock_name solution.lock;
          used_as;
        }
      )
    )
  in
  let lock_config =
    LockConfig.{
      locked_packages = Alist.to_list locked_package_acc;
      explicit_dependencies;
    }
  in
  (lock_config, Alist.to_list impl_spec_acc)


(*
let extract_attributes_from_document_file (display_config : Logging.config) (input_kind : input_kind) (abspath_in : abs_path) : (DocumentAttribute.t, config_error) result =
  let open ResultMonad in
  Logging.begin_to_parse_file display_config abspath_in;
  match input_kind with
  | InputSatysfi ->
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
*)


let make_envelope_config (abspath_package_config : abs_path) (package_contents : PackageConfig.package_contents) : (EnvelopeConfig.t, config_error) result =
  let open ResultMonad in
  match package_contents with
  | PackageConfig.Library{
      main_module_name;
      source_directories;
      test_directories;
      _
    } ->
      return {
        envelope_contents =
          Library{
            main_module_name;
            source_directories;
            test_directories;
            conversion_specs = []; (* TODO *)
          };
      }

  | PackageConfig.Font{
      main_module_name;
      font_file_descriptions;
    } ->
      return { envelope_contents = Font{ main_module_name; font_file_descriptions } }

  | PackageConfig.Document(_) ->
      err @@ NotAPackageButADocument(abspath_package_config)


let make_dependencies_with_flags (package_contents : PackageConfig.package_contents) =
  match package_contents with
  | PackageConfig.Library{ dependencies; test_dependencies; _ } ->
      List.append
        (dependencies |> List.map (fun dep -> (SourceDependency, dep)))
        (test_dependencies |> List.map (fun dep -> (TestOnlyDependency, dep)))

  | PackageConfig.Font(_) ->
      []

  | PackageConfig.Document{ dependencies } ->
      dependencies |> List.map (fun dep -> (SourceDependency, dep))


let get_minimum_language_version (language_requirement : SemanticVersion.requirement) : SemanticVersion.t =
  match language_requirement with
  | SemanticVersion.CompatibleWith(semver) -> semver


let get_store_root () : (abs_path, config_error) result =
  let open ResultMonad in
  let envvar_home =
    if String.equal Sys.os_type "Win32" then
      "userprofile"
    else
      "HOME"
  in
  match Sys.getenv_opt envvar_home with
  | None       -> err @@ CannotDetermineStoreRoot { envvar = envvar_home }
  | Some(home) -> return @@ make_abs_path (Filename.concat home ".saphe")


let solve ~(fpath_in : string) =
  let res =
    let open ResultMonad in

    (* Constructs the input: *)
    let solve_input =
      let absdir_current = Sys.getcwd () in
      let abspath_in = make_absolute_if_relative ~origin:absdir_current fpath_in in
      if is_directory abspath_in then
      (* If the input is a directory that forms a package: *)
        let abspath_lock_config = Constant.library_lock_config_path ~dir:abspath_in in
        let abspath_envelope_config = Constant.envelope_config_path ~dir:abspath_in in
        PackageSolveInput{
          root     = abspath_in;
          lock     = abspath_lock_config;
          envelope = abspath_envelope_config;
        }
      else
        let abspath_package_config = Constant.document_package_config_path ~doc:abspath_in in
        let abspath_lock_config = Constant.document_lock_config_path ~doc:abspath_in in
        DocumentSolveInput{
          doc    = abspath_in;
          config = abspath_package_config;
          lock   = abspath_lock_config;
        }
    in

    let* (language_version, dependencies_with_flags, abspath_lock_config, registry_remotes) =
      match solve_input with
      | PackageSolveInput{
          root     = absdir_package;
          lock     = abspath_lock_config;
          envelope = abspath_envelope_config;
        } ->
          let abspath_package_config = Constant.library_package_config_path ~dir:absdir_package in
          let*
            PackageConfig.{
              language_requirement;
              package_contents;
              registry_remotes;
              _
            } = PackageConfig.load abspath_package_config
          in

          (* Selects the minimum version according to the user's designation: *)
          (* TODO: consider taking dependencies into account when selecting a language version *)
          let language_version = get_minimum_language_version language_requirement in

          (* Writes the envelope config: *)
          let* envelope_config = make_envelope_config abspath_package_config package_contents in
          let* () =
            EnvelopeConfig.write abspath_envelope_config envelope_config
              |> Result.map_error (fun message ->
                CannotWriteEnvelopeConfig{ message; path = abspath_envelope_config }
              )
          in
          Logging.end_envelope_config_output abspath_envelope_config;

          let dependencies_with_flags = make_dependencies_with_flags package_contents in
          return (language_version, dependencies_with_flags, abspath_lock_config, registry_remotes)

      | DocumentSolveInput{
          doc    = _abspath_doc;
          config = abspath_package_config;
          lock   = abspath_lock_config;
        } ->
          let*
            PackageConfig.{
              language_requirement;
              package_contents;
              registry_remotes;
              _
            } = PackageConfig.load abspath_package_config
          in

          (* Selects the minimum version according to the user's designation: *)
          (* TODO: consider taking dependencies into account when selecting a language version *)
          let language_version = get_minimum_language_version language_requirement in

          let dependencies_with_flags = make_dependencies_with_flags package_contents in
          return (language_version, dependencies_with_flags, abspath_lock_config, registry_remotes)
(*
          let* DocumentAttribute.{ registry_specs; dependencies } =
            extract_attributes_from_document_file display_config input_kind abspath_in
          in
          let dependencies_with_flags = dependencies |> List.map (fun dep -> (SourceDependency, dep)) in
          return (dependencies_with_flags, abspath_lock_config, registry_specs)
*)
    in

    Logging.show_package_dependency_before_solving dependencies_with_flags;

    (* Arranges the store root config: *)
    let* absdir_store_root = get_store_root () in
    ShellCommand.mkdir_p absdir_store_root;
    let abspath_store_root_config = Constant.store_root_config_path ~store_root:absdir_store_root in
    let* (store_root_config, created) = StoreRootConfig.load_or_initialize abspath_store_root_config in
    begin
      if created then
        Logging.store_root_config_created abspath_store_root_config
      else
        ()
    end;

    (* Constructs a map that associates a package with its implementations: *)
    let* package_id_to_impl_list =
      registry_remotes |> List.fold_left (fun res registry_remote ->
        let* package_id_to_impl_list = res in
        let* registry_hash_value = ConfigUtil.make_registry_hash_value registry_remote in

        (* Manupulates the store root config: *)
        let* () =
          update_store_root_config_if_needed
            store_root_config.StoreRootConfig.registries
            registry_hash_value
            registry_remote
            abspath_store_root_config
        in

        (* Fetches the registry config: *)
        let absdir_registry_repo =
          Constant.registry_root_directory_path ~store_root:absdir_store_root registry_hash_value
        in
        let git_command = "git" in (* TODO: make this changeable *)
        let* () =
          PackageRegistryFetcher.main ~git_command absdir_registry_repo registry_remote
            |> Result.map_error (fun e -> PackageRegistryFetcherError(e))
        in

        (* Loads the registry config and grows `package_id_to_impl_list`: *)
        let* PackageRegistryConfig.{ packages = packages } =
          let abspath_registry_config =
            append_to_abs_directory absdir_registry_repo Constant.package_registry_config_file_name
          in
          PackageRegistryConfig.load abspath_registry_config
        in
        packages |> List.fold_left (fun res (package_name, impls) ->
          let* package_id_to_impl_list = res in
          let package_id = PackageId.{ registry_hash_value; package_name } in
          if package_id_to_impl_list |> PackageIdMap.mem package_id then
            err @@ MultiplePackageDefinition{ package_name }
          else
            return (package_id_to_impl_list |> PackageIdMap.add package_id impls)
        ) (return package_id_to_impl_list)

      ) (return PackageIdMap.empty)
    in

    let package_context = { language_version; package_id_to_impl_list } in
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
                ~wget_command ~tar_command ~unzip_command ~store_root:absdir_store_root impl_spec
            ) ()
          in
          let* () = LockConfig.write abspath_lock_config lock_config in
          Logging.end_lock_config_output abspath_lock_config;
          return ()
    end
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> report_config_error e; exit 1


type build_input =
  | PackageBuildInput of {
      root     : abs_path;
      lock     : abs_path;
      deps     : abs_path;
      envelope : abs_path;
      options  : SatysfiCommand.build_option;
    }
  | DocumentBuildInput of {
      doc     : abs_path;
      lock    : abs_path;
      deps    : abs_path;
      out     : abs_path;
      dump    : abs_path;
      options : SatysfiCommand.build_option;
    }


let make_envelope_dependency (lock_dep : LockConfig.lock_dependency) : envelope_dependency =
  {
    dependency_name    = lock_dep.depended_lock_name;
    dependency_used_as = lock_dep.used_as;
  }


let make_envelope_spec ~(store_root : abs_path) (locked_package : LockConfig.locked_package) : envelope_spec =
  let
    LockConfig.{
      lock_name;
      lock_dependencies;
      lock_contents = RegisteredLock(lock);
      test_only_lock;
    } = locked_package
  in
  let envelope_dependencies = lock_dependencies |> List.map make_envelope_dependency in
  {
    envelope_name = lock_name;
    envelope_path = get_abs_path_string (Constant.lock_envelope_config ~store_root lock);
    envelope_dependencies;
    test_only_envelope = test_only_lock;
  }


let make_deps_config ~(store_root : abs_path) (lock_config : LockConfig.t) : DepsConfig.t =
  let envelopes =
    lock_config.LockConfig.locked_packages |> List.map (make_envelope_spec ~store_root)
  in
  let explicit_dependencies =
    lock_config.LockConfig.explicit_dependencies
      |> List.map make_envelope_dependency
  in
  { envelopes; explicit_dependencies }


let build
    ~(fpath_in : string)
    ~(fpath_out_opt : string option)
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
=
  let res =
    let open ResultMonad in

    (* Constructs the input: *)
    let build_input =
      let options =
        SatysfiCommand.{
          text_mode   = text_mode_formats_str_opt;
          page_number_limit;
          show_full_path;
          debug_show_bbox;
          debug_show_space;
          debug_show_block_bbox;
          debug_show_block_space;
          debug_show_overfull;
          type_check_only;
          bytecomp;
        }
      in
      let absdir_current = Sys.getcwd () in
      let abspath_in = make_absolute_if_relative ~origin:absdir_current fpath_in in
      if is_directory abspath_in then
        let abspath_lock_config = Constant.library_lock_config_path ~dir:abspath_in in
        let abspath_deps_config = Constant.library_deps_config_path ~dir:abspath_in in
        let abspath_envelope_config = Constant.envelope_config_path ~dir:abspath_in in
        PackageBuildInput{
          root     = abspath_in;
          lock     = abspath_lock_config;
          deps     = abspath_deps_config;
          envelope = abspath_envelope_config;
          options;
        }
      else
        let abspath_lock_config = Constant.document_lock_config_path ~doc:abspath_in in
        let abspath_deps_config = Constant.document_deps_config_path ~doc:abspath_in in
        let abspath_out =
          match fpath_out_opt with
          | None ->
              Constant.default_output_path ~doc:abspath_in

          | Some(fpath_out) ->
              make_absolute_if_relative ~origin:absdir_current fpath_out
        in
        let abspath_dump = Constant.dump_path ~doc:abspath_in in
        DocumentBuildInput{
          doc     = abspath_in;
          lock    = abspath_lock_config;
          deps    = abspath_deps_config;
          out     = abspath_out;
          dump    = abspath_dump;
          options;
        }
    in

    let* absdir_store_root = get_store_root () in

    match build_input with
    | PackageBuildInput{
        root     = _absdir_package;
        lock     = abspath_lock_config;
        deps     = abspath_deps_config;
        envelope = abspath_envelope_config;
        options;
      } ->
        (* Updates the deps config: *)
        let* lock_config = LockConfig.load abspath_lock_config in
        let deps_config = make_deps_config ~store_root:absdir_store_root lock_config in
        let* () = DepsConfig.write abspath_deps_config deps_config in
        Logging.end_deps_config_output abspath_deps_config;

        (* Builds the package by invoking `satysfi`: *)
        let SatysfiCommand.{ exit_status = _; command = _ } = (* TODO: use `exit_status` *)
          SatysfiCommand.(build_package
            ~envelope:abspath_envelope_config
            ~deps:abspath_deps_config
            ~base_dir:absdir_store_root
            ~options)
        in
        return ()

    | DocumentBuildInput{
        doc  = abspath_doc;
        lock = abspath_lock_config;
        deps = abspath_deps_config;
        out  = abspath_out;
        dump = abspath_dump;
        options;
      } ->
        (* Updates the deps config: *)
        let* lock_config = LockConfig.load abspath_lock_config in
        let deps_config = make_deps_config ~store_root:absdir_store_root lock_config in
        let* () = DepsConfig.write abspath_deps_config deps_config in
        Logging.end_deps_config_output abspath_deps_config;

        (* Builds the document by invoking `satysfi`: *)
        let SatysfiCommand.{ exit_status = _; command = _ } = (* TODO: use `exit_status` *)
          SatysfiCommand.(build_document
            ~doc:abspath_doc
            ~out:abspath_out
            ~dump:abspath_dump
            ~deps:abspath_deps_config
            ~base_dir:absdir_store_root
            ~options)
        in
        return ()
  in
  match res with
  | Ok(())   -> ()
  | Error(e) -> report_config_error e; exit 1
