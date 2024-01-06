
open PackageSystemBase


module SolverInput = struct

  module Role = struct

    type t =
      | LocalRole of {
          requires : package_dependency list;
          context  : package_context;
        }
      | Role of {
          package_id    : PackageId.t;
          compatibility : string;
          context       : package_context;
        }


    let pp ppf (role : t) =
      match role with
      | Role{ package_id; _ } ->
          let PackageId.{ package_name; _ } = package_id in
          Format.fprintf ppf "%s" package_name

      | LocalRole(_) ->
          Format.fprintf ppf "local"


    let compare (role1 : t) (role2 : t) =
      match (role1, role2) with
      | (LocalRole(_), LocalRole(_)) -> 0
      | (LocalRole(_), _)            -> 1
      | (_, LocalRole(_))            -> -1

      | (
          Role{ package_id = pkgid1; compatibility = c1; _ },
          Role{ package_id = pkgid2; compatibility = c2; _ }
        ) ->
          begin
            match PackageId.compare pkgid1 pkgid2 with
            | 0       -> String.compare c1 c2
            | nonzero -> nonzero
          end

  end


  (* Unused *)
  type command = unit

  (* Unused *)
  type command_name = string

  type restriction = SemanticVersion.requirement

  type dependency =
    | Dependency of {
        role                : Role.t;
        used_as             : string;
        version_requirement : SemanticVersion.requirement;
      }

  type dep_info = {
    dep_role              : Role.t;
    dep_importance        : [ `Essential | `Recommended | `Restricts ];
    dep_required_commands : command_name list;
  }

  type requirements = {
    role    : Role.t;
    command : command_name option;
  }

  type impl =
    | DummyImpl
    | LocalImpl of {
        dependencies : dependency list;
      }
    | Impl of {
        package_name        : package_name;
        version             : SemanticVersion.t;
        registry_hash_value : registry_hash_value;
        source              : implementation_source;
        dependencies        : dependency list;
      }

  type role_information = {
    replacement : Role.t option;
    impls       : impl list;
  }

  (* Unused *)
  type machine_group = string

  type conflict_class = string

  type rejection = unit (* TODO: define this *)


  let pp_impl (ppf : Format.formatter) (impl : impl) =
    match impl with
    | DummyImpl ->
        Format.fprintf ppf "dummy"

    | LocalImpl(_) ->
        Format.fprintf ppf "local"

    | Impl{ package_name; version; _ } ->
        Format.fprintf ppf "%s %s" package_name (SemanticVersion.to_string version)


  let pp_impl_long (ppf : Format.formatter) (impl : impl) =
    pp_impl ppf impl (* TODO: show dependencies *)


  (* Unused *)
  let pp_command (_ppf : Format.formatter) (_cmd : command) =
    ()


  let pp_version (ppf : Format.formatter) (impl : impl) =
    match impl with
    | DummyImpl          -> Format.fprintf ppf "dummy"
    | LocalImpl(_)       -> Format.fprintf ppf "local"
    | Impl{ version; _ } -> Format.fprintf ppf "%s" (SemanticVersion.to_string version)


  (* Unused *)
  let get_command (_impl : impl) (_cmdnm : command_name) =
    None


  let dep_info (dep : dependency) : dep_info =
    let Dependency{ role; _ } = dep in
    { dep_role = role; dep_importance = `Essential; dep_required_commands = [] }


  let requires (_role : Role.t) (impl : impl) : dependency list * command_name list =
    match impl with
    | DummyImpl                 -> ([], [])
    | LocalImpl{ dependencies } -> (dependencies, [])
    | Impl{ dependencies; _ }   -> (dependencies, [])


  (* Unused *)
  let command_requires (_role : Role.t) (_cmd : command) =
    ([], [])


  let make_internal_dependency_from_registry (registry_hash_value : string) (context : package_context) (requires : package_dependency_in_registry list) : dependency list =
    requires |> List.map (function
    | PackageDependencyInRegistry{ package_name; used_as; version_requirement } ->
        let compatibility =
          match version_requirement with
          | SemanticVersion.CompatibleWith(semver) ->
              SemanticVersion.get_compatibility_unit semver
        in
        let package_id = PackageId.{ package_name; registry_hash_value } in
        let role =
          Role.Role{
            package_id;
            compatibility;
            context;
          }
        in
        Dependency{ role; used_as; version_requirement }
    )


  let make_internal_dependency (context : package_context) (requires : package_dependency list) : dependency list =
    requires |> List.map (fun dep ->
      let PackageDependency{ used_as; spec } = dep in
      match spec with
      | RegisteredDependency{ package_id; version_requirement } ->
          let compatibility =
            match version_requirement with
            | SemanticVersion.CompatibleWith(semver) ->
                SemanticVersion.get_compatibility_unit semver
          in
          let role = Role.Role{ package_id; compatibility; context } in
          Dependency{ role; used_as; version_requirement }
    )


  let implementations (role : Role.t) : role_information =
    match role with
    | Role{ package_id; compatibility; context } ->
        let PackageId.{ package_name; registry_hash_value } = package_id in
        let impl_records =
          context.package_id_to_impl_list
            |> PackageIdMap.find_opt package_id
            |> Option.value ~default:[]
        in
        let impls =
          impl_records |> List.filter_map (fun impl_record ->
            let ImplRecord{ version; source; language_requirement; dependencies } = impl_record in
            if context.language_version |> SemanticVersion.fulfill language_requirement then
              if String.equal (SemanticVersion.get_compatibility_unit version) compatibility then
                let dependencies =
                  make_internal_dependency_from_registry registry_hash_value context dependencies
                in
                Some(Impl{ package_name; version; registry_hash_value; source; dependencies })
              else
                None
            else
              None
          )
        in
        { replacement = None; impls }

    | LocalRole{ requires; context } ->
        let dependencies = make_internal_dependency context requires in
        let impls = [ LocalImpl{ dependencies } ] in
        { replacement = None; impls }


  let restrictions (dep : dependency) : restriction list =
    let Dependency{ version_requirement; _ } = dep in
    [ version_requirement ]


  let meets_restriction (impl : impl) (restr : restriction) : bool =
    match impl with
    | DummyImpl ->
        false

    | LocalImpl(_) ->
        true

    | Impl{ version = semver_provided; _} ->
        begin
          match restr with
          | CompatibleWith(semver_required) ->
              SemanticVersion.is_compatible ~old:semver_required ~new_:semver_provided
        end


  (* Unused *)
  let machine_group (_impl : impl) : machine_group option =
    None


  let conflict_class (impl : impl) : conflict_class list =
    match impl with
    | DummyImpl | LocalImpl(_) ->
        [ "*" ]

    | Impl{ package_name; version; _ } ->
        let compat = SemanticVersion.get_compatibility_unit version in
        [ Printf.sprintf "%s/%s" package_name compat ]


  let rejects (_role : Role.t) : (impl * rejection) list * string list =
    ([], []) (* TODO: define `rejection` and implement this *)


  let compare_version (impl1 : impl) (impl2 : impl) : int =
    match (impl1, impl2) with
    | (DummyImpl, DummyImpl) -> 0
    | (DummyImpl, _)         -> 1
    | (_, DummyImpl)         -> -1

    | (LocalImpl(_), LocalImpl(_)) -> 0
    | (LocalImpl(_), _)            -> 1
    | (_, LocalImpl(_))            -> -1

    | (Impl{ version = semver1; _ }, Impl{ version = semver2; _ }) ->
        SemanticVersion.compare semver1 semver2


  let user_restrictions (_role : Role.t) : restriction option =
    None


  let format_machine (_impl : impl) : string =
    ""


  let string_of_restriction (restr : restriction) : string =
    match restr with
    | CompatibleWith(semver) -> SemanticVersion.to_string semver


  let describe_problem (_impl : impl) (_rej : rejection) : string =
    "" (* TODO: define `rejection` and implement this *)


  let dummy_impl : impl =
    DummyImpl

end


module InternalSolver = Zeroinstall_solver.Make(SolverInput)

module LockDependencyGraph = DependencyGraph.Make(Lock)

module VertexSet = LockDependencyGraph.VertexSet

module VertexMap = LockDependencyGraph.VertexMap


let solve (context : package_context) (dependencies_with_flags : (dependency_flag * package_dependency) list) : (package_solution list) option =
  let (explicit_source_dependencies, explicit_test_dependencies, dependency_acc) =
    dependencies_with_flags |> List.fold_left (fun acc (flag, dep) ->
      let (explicit_source_dependencies, explicit_test_dependencies, dependency_acc) = acc in
      match dep with
      | PackageDependency{ spec; used_as } ->
          let RegisteredDependency{ package_id; _ } = spec in
          let (explicit_source_dependencies, explicit_test_dependencies) =
            match flag with
            | SourceDependency ->
                (explicit_source_dependencies |> PackageIdMap.add package_id used_as, explicit_test_dependencies)

            | TestOnlyDependency ->
                (explicit_source_dependencies, explicit_test_dependencies |> PackageIdMap.add package_id used_as)
          in
          (explicit_source_dependencies, explicit_test_dependencies, Alist.extend dependency_acc dep)
    ) (PackageIdMap.empty, PackageIdMap.empty, Alist.empty)
  in
  let requires = Alist.to_list dependency_acc in
  let output_opt =
    InternalSolver.do_solve ~closest_match:false {
      role    = LocalRole{ requires; context };
      command = None;
    }
  in
  output_opt |> Option.map (fun output ->
    let open InternalSolver in

    (* Adds vertices to the graph: *)
    let rolemap = output |> Output.to_map in
    let (quad_acc, graph, explicit_vertex_to_used_as, explicit_test_vertex_to_used_as, lock_to_vertex_map) =
      Output.RoleMap.fold (fun _role impl acc ->
        let impl = Output.unwrap impl in
        match impl with
        | DummyImpl | LocalImpl(_) ->
            acc

        | Impl{ package_name; version = locked_version; registry_hash_value; source; dependencies } ->
            let package_id = PackageId.{ registry_hash_value; package_name } in
            let lock = Lock.{ package_id; locked_version } in
            let (quad_acc, graph, explicit_vertex_to_used_as, explicit_test_vertex_to_used_as, lock_to_vertex_map) = acc in
            let (graph, vertex) =
              match graph |> LockDependencyGraph.add_vertex lock () with
              | Error(_) -> assert false
              | Ok(pair) -> pair
            in
            let quad_acc = Alist.extend quad_acc (lock, source, dependencies, vertex) in
            let explicit_vertex_to_used_as =
              match explicit_source_dependencies |> PackageIdMap.find_opt package_id with
              | Some(used_as) -> explicit_vertex_to_used_as |> VertexMap.add vertex used_as
              | None          -> explicit_vertex_to_used_as
            in
            let explicit_test_vertex_to_used_as =
              match explicit_test_dependencies |> PackageIdMap.find_opt package_id with
              | Some(used_as) -> explicit_test_vertex_to_used_as |> VertexMap.add vertex used_as
              | None          -> explicit_test_vertex_to_used_as
            in
            let lock_to_vertex_map = lock_to_vertex_map |> LockMap.add lock vertex in
            (quad_acc, graph, explicit_vertex_to_used_as, explicit_test_vertex_to_used_as, lock_to_vertex_map)

      ) rolemap (Alist.empty, LockDependencyGraph.empty, VertexMap.empty, VertexMap.empty, LockMap.empty)
    in

    (* Add edges to the graph: *)
    let (solmap, graph) =
      quad_acc |> Alist.to_list |> List.fold_left (fun acc quint ->
        let open SolverInput in
        let (solmap, graph) = acc in
        let (lock, source, dependencies, vertex) = quint in
        let (locked_dependency_acc, graph) =
          dependencies |> List.fold_left (fun (locked_dependency_acc, graph) dep ->
            let Dependency{ role = role_dep; used_as; _ } = dep in
            match role_dep with
            | Role{ package_id = package_id_dep; _ } ->
                let lock_dep =
                  match rolemap |> Output.RoleMap.find_opt role_dep |> Option.map Output.unwrap with
                  | None | Some(DummyImpl) | Some(LocalImpl(_)) ->
                      assert false

                  | Some(Impl{ version = version_dep; _ }) ->
                      Lock.{
                        package_id     = package_id_dep;
                        locked_version = version_dep;
                      }
                in
                let locked_dependency =
                  {
                    depended_lock      = lock_dep;
                    dependency_used_as = used_as;
                  }
                in
                let locked_dependency_acc = Alist.extend locked_dependency_acc locked_dependency in
                let vertex_dep =
                  match lock_to_vertex_map |> LockMap.find_opt lock_dep with
                  | None    -> assert false
                  | Some(v) -> v
                in
                let graph = graph |> LockDependencyGraph.add_edge ~from:vertex ~to_:vertex_dep in
                (locked_dependency_acc, graph)

            | LocalRole(_) ->
                (locked_dependency_acc, graph)

          ) (Alist.empty, graph)
        in
        let locked_dependencies = Alist.to_list locked_dependency_acc in
        let solmap = solmap |> LockMap.add lock (source, locked_dependencies) in
        (solmap, graph)

      ) (LockMap.empty, graph)
    in

    (* Computes the set of source dependencies: *)
    let resulting_source_dependencies =
      let explicit_vertices =
        LockDependencyGraph.map_domain explicit_vertex_to_used_as
      in
      LockDependencyGraph.reachability_closure graph explicit_vertices
    in

    let solution_acc =
      LockMap.fold (fun lock (locked_source, locked_dependencies) solution_acc ->
        let vertex =
          match lock_to_vertex_map |> LockMap.find_opt lock with
          | None    -> assert false
          | Some(v) -> v
        in
        let used_in_test_only = not (resulting_source_dependencies |> VertexSet.mem vertex) in
        let explicitly_depended =
          explicit_vertex_to_used_as |> VertexMap.find_opt vertex
        in
        let explicitly_test_depended =
          explicit_test_vertex_to_used_as |> VertexMap.find_opt vertex
        in
        Alist.extend solution_acc {
          lock;
          locked_source;
          locked_dependencies;
          used_in_test_only;
          explicitly_depended;
          explicitly_test_depended;
        }
      ) solmap Alist.empty
    in
    Alist.to_list solution_acc
  )
