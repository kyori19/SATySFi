
open MyUtil
open ConfigError
open FontError
open Format
open StaticEnv
open Types
open TypeError


exception NoLibraryRootDesignation
exception ShouldSpecifyOutputFile
exception UnexpectedExtension of string
exception ConfigError of config_error


type line =
  | NormalLine  of string
  | DisplayLine of string


type error_category =
  | Lexer
  | Parser
  | Typechecker
  | Evaluator
  | Interface
  | System


let convert_abs_path_to_show (abspath : abs_path) : string =
  let abspathstr = get_abs_path_string abspath in
  if OptionState.does_show_full_path () then
    abspathstr
  else
    Filename.basename abspathstr


let show_error_category = function
  | Lexer       -> "Syntax Error at Lexer"
  | Parser      -> "Syntax Error at Parser"
  | Typechecker -> "Type Error"
  | Evaluator   -> "Error during Evaluation"
  | Interface   -> "Error"
  | System      -> "Error"


let make_error_message (cat : error_category) (lines : line list) =
  let buf = Buffer.create 512 in
  let formatter = formatter_of_buffer buf in
  let print_with_newline s =
    pp_print_string formatter s;
    pp_print_newline formatter ()
  in
  pp_print_string formatter (Printf.sprintf "! [%s] " (show_error_category cat));
  lines |> List.fold_left (fun (is_first : bool) (line : line) ->
    begin
      match line with
      | NormalLine(s) ->
          if is_first then
            print_with_newline s
          else
            print_with_newline ("    " ^ s)

      | DisplayLine(s) ->
          if is_first then
            print_with_newline ("\n      " ^ s)
          else
            print_with_newline ("      " ^ s)
    end;
    false
  ) true |> ignore;
  pp_print_flush formatter ();
  Buffer.contents buf


let make_candidates_message (candidates : string list) =
  let quote s = Printf.sprintf "'%s'" s in
  let aux (rev_rest : string list) (last : string) =
    match rev_rest with
    | []     -> quote last
    | _ :: _ -> Printf.sprintf "%s or %s" (String.concat ", " (List.map quote (List.rev rev_rest))) (quote last)
  in
  match List.rev candidates with
  | []               -> None
  | last :: rev_rest -> Some(Printf.sprintf "Did you mean %s?" (aux rev_rest last))


let make_unification_error_message (dispmap : DisplayMap.t) (ue : unification_error) =
  match ue with
  | TypeContradiction(ty1_sub, ty2_sub) ->
      let dispmap =
        dispmap
          |> Display.collect_ids_mono ty1_sub
          |> Display.collect_ids_mono ty2_sub
      in
      let str_ty1_sub = Display.show_mono_type_by_map dispmap ty1_sub in
      let str_ty2_sub = Display.show_mono_type_by_map dispmap ty2_sub in
      [
        NormalLine("Type");
        DisplayLine(str_ty1_sub);
        NormalLine("is not compatible with");
        DisplayLine(Printf.sprintf "%s." str_ty2_sub);
      ]

  | TypeVariableInclusion(fid, ty) ->
      let dispmap = dispmap |> Display.collect_ids_mono ty in
      let (dispmap, str_fid) = dispmap |> DisplayMap.add_free_id fid in
      let str_ty = Display.show_mono_type_by_map dispmap ty in
      [
        NormalLine(Printf.sprintf "Type variable %s occurs in" str_fid);
        DisplayLine(Printf.sprintf "%s." str_ty);
      ]

  | RowContradiction(row1, row2) ->
      let dispmap =
        dispmap
          |> Display.collect_ids_mono_row row1
          |> Display.collect_ids_mono_row row2
      in
      let str_row1 = Display.show_mono_row_by_map dispmap row1 |> Option.value ~default:"" in
      let str_row2 = Display.show_mono_row_by_map dispmap row1 |> Option.value ~default:"" in
      [
        NormalLine("Row");
        DisplayLine(str_row1);
        NormalLine("is not compatible with");
        DisplayLine(Printf.sprintf "%s." str_row2);
      ]

  | RowVariableInclusion(frid, row) ->
      let labset = FreeRowID.get_label_set frid in
      let (dispmap, str_frid) = dispmap |> DisplayMap.add_free_row_id frid labset in
      let dispmap = dispmap |> Display.collect_ids_mono_row row in
      let str_row = Display.show_mono_row_by_map dispmap row |> Option.value ~default:"" in
      [
        NormalLine(Printf.sprintf "Row variable %s occurs in" str_frid);
        DisplayLine(Printf.sprintf "%s." str_row);
      ]

  | CommandArityMismatch(len1, len2) ->
      [
        NormalLine(Printf.sprintf "The command type has %d type argument(s), but is expected to have %d." len1 len2);
      ]

  | CommandOptionalLabelMismatch(label) ->
      [
        NormalLine(Printf.sprintf "Label '%s' in a command type makes the contradiction." label);
      ]

  | BreaksRowDisjointness(label) ->
      [
        NormalLine(Printf.sprintf "The row must not contain label '%s'." label);
      ]

  | BreaksLabelMembershipByFreeRowVariable(_frid, label, _labset) ->
      [
        NormalLine(Printf.sprintf "The row does not contain label '%s'." label);
      ] (* TODO (error): detailed report *)

  | BreaksLabelMembershipByBoundRowVariable(_mbbrid, label) ->
      [
        NormalLine(Printf.sprintf "The row does not contain label '%s'." label);
      ] (* TODO (error): detailed report *)

  | BreaksLabelMembershipByEmptyRow(label) ->
      [
        NormalLine(Printf.sprintf "The row does not contain label '%s'." label);
      ]

  | InsufficientRowVariableConstraint(_mbbrid, _labset_expected, _labset_actual) ->
      [] (* TODO (error): detailed report *)


let make_parse_error_message = function
  | CannotProgressParsing(rng) ->
      make_error_message Parser [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
      ]

  | IllegalItemDepth{ range = rng; before; current } ->
      make_error_message Parser [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "illegal item depth %d after %d" before current);
      ]

  | EmptyInputFile(rng) ->
      make_error_message Parser [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("empty input.");
      ]


let make_type_error_message = function
  | UndefinedVariable(rng, varnm, candidates) ->
      let candidates_message_lines =
        match make_candidates_message candidates with
        | None    -> []
        | Some(s) -> [ NormalLine(s) ]
      in
      make_error_message Typechecker (List.concat [
        [
          NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
          NormalLine(Printf.sprintf "undefined variable '%s'." varnm);
        ];
        candidates_message_lines;
      ])

  | UndefinedConstructor(rng, constrnm, candidates) ->
      let candidates_message_lines =
        match make_candidates_message candidates with
        | None    -> []
        | Some(s) -> [ NormalLine(s) ]
      in
      make_error_message Typechecker (List.concat [
        [
          NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
          NormalLine(Printf.sprintf "undefined constructor '%s'." constrnm);
        ];
        candidates_message_lines;
      ])

  | UndefinedTypeName(rng, tynm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined type '%s'." tynm);
      ]

  | UndefinedTypeVariable(rng, tyvarnm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined type variable '%s'." tyvarnm);
      ]

  | UndefinedRowVariable(rng, rowvarnm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined row variable '%s'." rowvarnm);
      ]

  | UndefinedKindName(rng, kdnm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined kind '%s'." kdnm);
      ]

  | UndefinedModuleName(rng, modnm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined module '%s'." modnm);
      ]

  | UndefinedSignatureName(rng, signm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined signature '%s'." signm);
      ]
  | UndefinedMacro(rng, csnm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined macro '%s'." csnm);
      ]

  | InvalidNumberOfMacroArguments(rng, macparamtys) ->
      make_error_message Typechecker (List.append [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("invalid number of macro arguments; types expected on arguments are:");
      ] (macparamtys |> List.map (function
        | LateMacroParameter(ty)  -> DisplayLine(Printf.sprintf "* %s" (Display.show_mono_type ty))
        | EarlyMacroParameter(ty) -> DisplayLine(Printf.sprintf "* ~%s" (Display.show_mono_type ty))
      )))

  | LateMacroArgumentExpected(rng, ty) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("an early macro argument is given, but a late argument of type");
        DisplayLine(Display.show_mono_type ty);
        NormalLine("is expected.");
      ]

  | EarlyMacroArgumentExpected(rng, ty) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("a late macro argument is given, but an early argument of type");
        DisplayLine(Display.show_mono_type ty);
        NormalLine("is expected.");
      ]

  | UnknownUnitOfLength(rng, unitnm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "undefined unit of length '%s'." unitnm);
      ]

  | InlineCommandInMath(rng) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("an inline command is used as a math command.");
      ]

  | MathCommandInInline(rng) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("a math command is used as an inline command.");
      ]

  | BreaksValueRestriction(rng) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("this expression breaks the value restriction;");
        NormalLine("it should be a syntactic function.");
      ]

  | MultiplePatternVariable(rng1, rng2, varnm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s" (Range.to_string rng1));
        NormalLine(Printf.sprintf "and at %s:" (Range.to_string rng2));
        NormalLine(Printf.sprintf "pattern variable '%s' is bound more than once." varnm);
      ]

  | LabelUsedMoreThanOnce(rng, label) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "'%s' is used more than once." label);
      ]

  | InvalidExpressionAsToStaging(rng, stage) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("invalid expression as to stage;");
        NormalLine(Printf.sprintf "should be used at %s." (string_of_stage stage));
      ]

  | InvalidOccurrenceAsToStaging(rng, varnm, stage) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "invalid occurrence of variable '%s' as to stage;" varnm);
        NormalLine(Printf.sprintf "should be used at %s." (string_of_stage stage));
      ]

  | ApplicationOfNonFunction(rng, ty) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("this expression has type");
        DisplayLine(Display.show_mono_type ty);
        NormalLine("and thus it cannot be applied to arguments.");
      ]


  | MultiCharacterMathScriptWithoutBrace(rng) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("more than one character is used as a math sub/superscript without braces;");
        NormalLine("use braces for making association explicit.");
      ]

  | IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "'%s' is expected to have %d type argument(s)," tynm lenexp);
        NormalLine(Printf.sprintf "but it has %d type argument(s) here." lenerr);
      ]

  | TypeUnificationError(((rng1, _) as ty1), ((rng2, _) as ty2), ue) ->
      let dispmap =
        DisplayMap.empty
          |> Display.collect_ids_mono ty1
          |> Display.collect_ids_mono ty2
      in
      let strty1 = Display.show_mono_type_by_map dispmap ty1 in
      let strty2 = Display.show_mono_type_by_map dispmap ty2 in
      let strrng1 = Range.to_string rng1 in
      let strrng2 = Range.to_string rng2 in
      let (posmsg, strtyA, strtyB, additional) =
        match (Range.is_dummy rng1, Range.is_dummy rng2) with
        | (true, true) ->
            (Printf.sprintf "(cannot report position; '%s', '%s')" (Range.message rng1) (Range.message rng2),
                strty1, strty2, [])

        | (true, false) ->
            (Printf.sprintf "at %s:" strrng2, strty2, strty1, [])

        | (false, true) ->
            (Printf.sprintf "at %s:" strrng1, strty1, strty2, [])

        | (false, false) ->
            (Printf.sprintf "at %s:" strrng1, strty1, strty2,
                [
                  NormalLine("This constraint is required by the expression");
                  NormalLine(Printf.sprintf "at %s." strrng2);
                ])
      in
      let detail = make_unification_error_message dispmap ue in
      make_error_message Typechecker (List.concat [
        [
          NormalLine(posmsg);
          NormalLine("this expression has type");
          DisplayLine(Printf.sprintf "%s," strtyA);
          NormalLine("but is expected of type");
          DisplayLine(Printf.sprintf "%s." strtyB);
        ];
        detail;
        additional;
      ])

  | RowUnificationError(rng, row1, row2, ue) ->
      let dispmap =
        DisplayMap.empty
          |> Display.collect_ids_mono_row row1
          |> Display.collect_ids_mono_row row2
      in
      let str_row1 = Display.show_mono_row_by_map dispmap row1 |> Option.value ~default:"" in
      let str_row2 = Display.show_mono_row_by_map dispmap row2 |> Option.value ~default:"" in
      let detail = make_unification_error_message dispmap ue in
      make_error_message Typechecker (List.concat [
        [
          NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
          NormalLine("the option row is");
          DisplayLine(str_row1);
          NormalLine("and");
          DisplayLine(Printf.sprintf "%s," str_row2);
          NormalLine("at the same time, but these are incompatible.");
        ];
        detail;
      ])

  | TypeParameterBoundMoreThanOnce(rng, tyvarnm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "type variable %s is bound more than once." tyvarnm);
      ]

  | ConflictInSignature(rng, member) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "'%s' is declared more than once in a signature." member);
      ]

  | NotAStructureSignature(rng, _fsig) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("not a structure signature (TODO (enhance): detailed report)");
      ]

  | NotAFunctorSignature(rng, _ssig) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("not a functor signature (TODO (enhance): detailed report)");
      ]

  | MissingRequiredValueName(rng, x, pty) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required value '%s' of type" x);
        DisplayLine(Display.show_poly_type pty);
      ]

  | MissingRequiredMacroName(rng, csnm, pmacty) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required macro '%s' of type" csnm);
        DisplayLine(Display.show_poly_macro_type pmacty);
      ]

  | MissingRequiredConstructorName(rng, ctornm, _centry) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required constructor '%s' (TODO (enhance): detailed report)" ctornm);
      ]

  | MissingRequiredTypeName(rng, tynm, arity) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required type '%s' of arity %d" tynm arity);
      ]

  | MissingRequiredModuleName(rng, modnm, _modsig) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required module '%s' (TODO (enhance): detailed report)" modnm);
      ]

  | MissingRequiredSignatureName(rng, signm, _absmodsig) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required signature '%s' (TODO (enhance): detailed report)" signm);
      ]

  | NotASubtypeAboutValue(rng, x, pty1, pty2) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about value '%s'; type" x);
        DisplayLine(Display.show_poly_type pty1);
        NormalLine("is not a subtype of");
        DisplayLine(Display.show_poly_type pty2);
      ]

  | NotASubtypeAboutValueStage(rng, x, stage1, stage2) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about the stage of value '%s';" x);
        DisplayLine(string_of_stage stage1);
        NormalLine("is not consistent with");
        DisplayLine(string_of_stage stage2);
      ]

  | NotASubtypeAboutMacro(rng, csnm, pmacty1, pmacty2) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about macro '%s'; type" csnm);
        DisplayLine(Display.show_poly_macro_type pmacty1);
        NormalLine("is not a subtype of");
        DisplayLine(Display.show_poly_macro_type pmacty2);
      ]

  | NotASubtypeAboutConstructor(rng, ctornm, _tyscheme1, _tyscheme2) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about constructor '%s' (TODO (enhance): detailed report)" ctornm);
      ]

  | NotASubtypeAboutType(rng, tynm, tentry1, tentry2) ->
      Format.printf "1: %a,@ 2: %a@," pp_type_entry tentry1 pp_type_entry tentry2; (* TODO: remove this *)
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a subtype about type '%s' (TODO (enhance): detailed report)" tynm);
      ]

  | NotASubtypeSignature(rng, _modsig1, _modsig2) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("not a subtype signature (TODO (enhance): detailed report)");
      ]

  | UnexpectedOptionalLabel(rng, label, ty_cmd) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "unexpected application of label '%s';" label);
        NormalLine(Printf.sprintf "the command used here has type");
        DisplayLine(Display.show_mono_type ty_cmd);
      ]

  | InvalidArityOfCommandApplication(rng, arity_expected, arity_actual) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "this command expects %d argument(s)," arity_expected);
        NormalLine(Printf.sprintf "but is applied to %d argument(s) here." arity_actual);
      ]

  | CannotRestrictTransparentType(rng, tynm) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot restrict transparent type '%s'." tynm);
      ]

  | KindContradiction(rng, tynm, kd_expected, kd_actual) ->
      let Kind(bkds_expected) = kd_expected in
      let Kind(bkds_actual) = kd_actual in
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "type '%s' expects %d type argument(s)," tynm (List.length bkds_expected));
        NormalLine(Printf.sprintf "but is applied to %d type argument(s)." (List.length bkds_actual));
      ]

  | CyclicSynonymTypeDefinition(cycle) ->
      let pairs =
        match cycle with
        | Loop(pair)   -> [ pair ]
        | Cycle(pairs) -> pairs |> TupleList.to_list
      in
      let lines =
        pairs |> List.map (fun (tynm, data) ->
          let rng = data.SynonymDependencyGraph.position in
          DisplayLine(Printf.sprintf "- '%s' (%s)" tynm (Range.to_string rng))
        )
      in
      make_error_message Typechecker
        (NormalLine("the following synonym types are cyclic:") :: lines)

  | MultipleSynonymTypeDefinition(tynm, rng1, rng2) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s" (Range.to_string rng1));
        NormalLine(Printf.sprintf "and %s:" (Range.to_string rng2));
        NormalLine(Printf.sprintf "synonym type '%s' is defined more than once." tynm);
      ]

  | ValueAttributeError(ValueAttribute.Unexpected(rng)) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("unexpected value attributes.");
      ]

  | TestMustBeStage1NonRec(rng) ->
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("tests must be stage-1 non-recursive bindings.");
      ]


let show_yaml_context (context : YamlDecoder.context) =
  match context with
  | [] ->
      ""

  | _ :: _ ->
      let s_context =
        let open YamlDecoder in
        context |> List.map (function
        | Field(field) -> Printf.sprintf ".%s" field
        | Index(index) -> Printf.sprintf ".[%d]" index
        ) |> String.concat ""
      in
      Printf.sprintf " (context: %s)" s_context


let make_yaml_error_lines : yaml_error -> line list = function
  | ParseError(s) ->
      [ NormalLine(Printf.sprintf "parse error: %s" s) ]

  | FieldNotFound(yctx, field) ->
      [ NormalLine(Printf.sprintf "field '%s' not found%s" field (show_yaml_context yctx)) ]

  | NotAFloat(yctx) ->
      [ NormalLine(Printf.sprintf "not a float value%s" (show_yaml_context yctx)) ]

  | NotAString(yctx) ->
      [ NormalLine(Printf.sprintf "not a string value%s" (show_yaml_context yctx)) ]

  | NotABool(yctx) ->
      [ NormalLine(Printf.sprintf "not a Boolean value%s" (show_yaml_context yctx)) ]

  | NotAnArray(yctx) ->
      [ NormalLine(Printf.sprintf "not an array%s" (show_yaml_context yctx)) ]

  | NotAnObject(yctx) ->
      [ NormalLine(Printf.sprintf "not an object%s" (show_yaml_context yctx)) ]

  | UnexpectedTag(yctx, tag) ->
      [ NormalLine(Printf.sprintf "unexpected type tag '%s'%s" tag (show_yaml_context yctx)) ]

  | UnexpectedLanguage(s_language_version) ->
      [ NormalLine(Printf.sprintf "unexpected language version '%s'" s_language_version) ]

  | NotASemanticVersion(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a semantic version: '%s'%s" s (show_yaml_context yctx)) ]

  | NotAVersionRequirement(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a version requirement: '%s'%s" s (show_yaml_context yctx)) ]

  | InvalidPackageName(yctx, s) ->
      [ NormalLine(Printf.sprintf "not a package name: '%s'%s" s (show_yaml_context yctx)) ]

  | MultiplePackageDefinition{ context = yctx; package_name } ->
      [ NormalLine(Printf.sprintf "More than one definition for package '%s'%s" package_name (show_yaml_context yctx)) ]

  | DuplicateRegistryLocalName{ context = yctx; registry_local_name } ->
      [ NormalLine(Printf.sprintf "More than one definition for registry local name '%s'%s" registry_local_name (show_yaml_context yctx)) ]

  | DuplicateRegistryHashValue{ context = yctx; registry_hash_value } ->
      [ NormalLine(Printf.sprintf "More than one definition for registry hash value '%s'%s" registry_hash_value (show_yaml_context yctx)) ]

  | CannotBeUsedAsAName(yctx, s) ->
      [ NormalLine(Printf.sprintf "'%s' cannot be used as a name%s" s (show_yaml_context yctx)) ]

  | UnsupportedConfigFormat(format) ->
      [ NormalLine(Printf.sprintf "unsupported config format '%s'" format) ]

  | NotACommand{ context = yctx; prefix = _; string = s } ->
      [ NormalLine(Printf.sprintf "not a command: '%s'%s" s (show_yaml_context yctx)) ]


let make_document_attribute_error_message : DocumentAttribute.error -> string = function
  | NoConfigArgument(rng) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("no config argument is given.");
      ]

  | DuplicateConfigAttribute(rng1, rng2) ->
      make_error_message Interface [
        NormalLine("More than one attribute defines the config:");
        DisplayLine(Printf.sprintf "- %s" (Range.to_string rng1));
        DisplayLine(Printf.sprintf "- %s" (Range.to_string rng2));
      ]

  | NotAVersionRequirement(rng, s) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a version requirement: '%s'" s);
      ]

  | NotAPackageDependency(rng) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a package dependency description.");
      ]

  | NotARegistry(rng) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a registry description.");
      ]

  | NotARegistryRemote(rng) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a registry remote description.");
      ]

  | LabelNotFound{ record_range; label } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string record_range));
        NormalLine(Printf.sprintf "this record does not have label '%s'." label);
      ]

  | DuplicateLabel{ record_range; label } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string record_range));
        NormalLine(Printf.sprintf "this record has more than one value for label '%s'." label);
      ]

  | NotAStringLiteral(rng) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a string literal.");
      ]

  | NotAListLiteral(rng) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "not a list literal.");
      ]

  | DuplicateRegistryLocalName{ list_range; registry_local_name } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string list_range));
        NormalLine(Printf.sprintf "this list has more than one registy named '%s'." registry_local_name);
      ]


let module_name_chain_to_string (((_, modnm0), modidents) : module_name_chain) =
  let modidents = modidents |> List.map (fun (_, modnm) -> modnm) in
  let modidents = modnm0 :: modidents in
  modidents |> String.concat "."


let make_config_error_message : config_error -> string = function
  | NotADocumentFile(abspath_in, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "file '%s' is not a document file; it is of type" fname);
        DisplayLine(Display.show_mono_type ty);
      ]

  | NotAStringFile(abspath_in, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "file '%s' is not a file for generating text; it is of type" fname);
        DisplayLine(Display.show_mono_type ty);
      ]

  | NotAWorkspaceFile(abspath_in, ty) ->
      let fname = convert_abs_path_to_show abspath_in in
      make_error_message Typechecker [
        NormalLine(Printf.sprintf "file '%s' is not a workspace file; it is of type" fname);
        DisplayLine(Display.show_mono_type ty);
      ]

  | FileModuleNotFound(rng, modnm) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot find a source file that defines module '%s'." modnm);
      ]

  | FileModuleNameConflict(modnm, abspath1, abspath2) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "more than one file defines module '%s':" modnm);
        DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath1));
        DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath2));
      ]

  | NoMainModule(modnm) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "no main module '%s'." modnm);
      ]

  | UnknownPackageDependency(rng, modnm) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "dependency on unknown package '%s'" modnm);
      ]

  | TypeError(tyerr) ->
      make_type_error_message tyerr

  | CyclicFileDependency(cycle) ->
      let pairs =
        match cycle with
        | Loop(pair)   -> [ pair ]
        | Cycle(pairs) -> pairs |> TupleList.to_list
      in
      make_error_message Interface (
        (NormalLine("cyclic dependency detected:")) ::
          (pairs |> List.map (fun (abspath, _) -> DisplayLine(get_abs_path_string abspath)))
      )

  | CannotReadFileOwingToSystem(msg) ->
      make_error_message Interface [
        NormalLine("cannot read file:");
        DisplayLine(msg);
      ]

  | LibraryContainsWholeReturnValue(abspath) ->
      let fname = get_abs_path_string abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "file '%s' is not a library; it has a return value." fname);
      ]

  | DocumentLacksWholeReturnValue(abspath) ->
      let fname = get_abs_path_string abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "file '%s' is not a document; it lacks a return value." fname);
      ]

  | CannotUseHeaderUse((rng, mod_chain)) ->
      let modnm = module_name_chain_to_string mod_chain in
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot specify 'use %s' here; use 'use %s of ...' instead." modnm modnm);
      ]

  | CannotUseHeaderUseOf((rng, mod_chain)) ->
      let modnm = module_name_chain_to_string mod_chain in
      make_error_message Interface [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "cannot specify 'use %s of ...' here; use 'use %s' instead." modnm modnm);
      ]

  | FailedToParse(e) ->
      make_parse_error_message e

  | MainModuleNameMismatch{ expected; got } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "main module name mismatch; expected '%s' but got '%s'." expected got);
      ]

  | PackageDirectoryNotFound(candidate_paths) ->
      let lines =
        candidate_paths |> List.map (fun path ->
          DisplayLine(Printf.sprintf "- %s" path)
        )
      in
      make_error_message Interface
        (NormalLine("cannot find package directory. candidates:") :: lines)

  | PackageConfigNotFound(abspath) ->
      make_error_message Interface [
        NormalLine("cannot find a package config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | PackageConfigError(abspath, e) ->
      make_error_message Interface (List.concat [
        [ NormalLine(Printf.sprintf "in %s: package config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | LockConfigNotFound(abspath) ->
      make_error_message Interface [
        NormalLine("cannot find a lock config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | LockConfigError(abspath, e) ->
      make_error_message Interface (List.concat [
        [ NormalLine(Printf.sprintf "in %s: lock config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | RegistryConfigNotFound(abspath) ->
      make_error_message Interface [
        NormalLine("cannot find a registry config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | RegistryConfigNotFoundIn(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      make_error_message Interface (List.concat [
        [ NormalLine(Printf.sprintf "cannot find a registry config '%s'. candidates:" (get_lib_path_string libpath)) ];
        lines;
      ])

  | RegistryConfigError(abspath, e) ->
      make_error_message Interface (List.concat [
        [ NormalLine(Printf.sprintf "in %s: registry config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | LibraryRootConfigNotFound(abspath) ->
      make_error_message Interface [
        NormalLine("cannot find a library root config at:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | LibraryRootConfigNotFoundIn(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      make_error_message Interface (List.concat [
        [ NormalLine(Printf.sprintf "cannot find a library root config '%s'. candidates:" (get_lib_path_string libpath)) ];
        lines;
      ])

  | LibraryRootConfigError(abspath, e) ->
      make_error_message Interface (List.concat [
        [ NormalLine(Printf.sprintf "in %s: library root config error;" (get_abs_path_string abspath)) ];
        make_yaml_error_lines e;
      ])

  | LockNameConflict(lock_name) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "lock name conflict: '%s'" lock_name);
      ]

  | LockedPackageNotFound(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      make_error_message Interface
        (NormalLine(Printf.sprintf "package '%s' not found. candidates:" (get_lib_path_string libpath)) :: lines)

  | DependencyOnUnknownLock{ depending; depended } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "unknown depended lock '%s' of '%s'." depended depending);
      ]

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
      make_error_message Interface
        (NormalLine("the following packages are cyclic:") :: lines)

  | NotALibraryFile(abspath) ->
      make_error_message Interface [
        NormalLine("the following file is expected to be a library file, but is not:");
        DisplayLine(get_abs_path_string abspath);
      ]

  | CannotFindLibraryFile(libpath, candidate_paths) ->
      let lines =
        candidate_paths |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      make_error_message Interface
        (NormalLine(Printf.sprintf "cannot find '%s'. candidates:" (get_lib_path_string libpath)) :: lines)

  | LocalFileNotFound{ relative; candidates } ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      make_error_message Interface
        (NormalLine(Printf.sprintf "cannot find local file '%s'. candidates:" relative) :: lines)

  | CannotSolvePackageConstraints ->
      make_error_message Interface [
        NormalLine("cannot solve package constraints.");
      ]

  | DocumentAttributeError(e) ->
      make_document_attribute_error_message e

  | MarkdownClassNotFound(modnm) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "package '%s' not found; required for converting Markdown documents." modnm);
      ]

  | NoMarkdownConversion(modnm) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "package '%s' contains no Markdown conversion rule." modnm);
      ]

  | MoreThanOneMarkdownConversion(modnm) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "package '%s' contains more than one Markdown conversion rule." modnm);
      ]

  | MarkdownError(e) ->
      begin
        match e with
        | InvalidHeaderComment ->
            make_error_message Interface [
              NormalLine("invalid or missing header comment of a Markdown document.");
            ]

        | InvalidExtraExpression ->
            make_error_message Interface [
              NormalLine("cannot parse an extra expression in a Markdown document.");
            ]

        | FailedToMakeDocumentAttribute(de) ->
            make_document_attribute_error_message de
      end

  | FailedToFetchTarball{ lock_name; exit_status; command } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "failed to fetch '%s' (exit status: %d). command:" lock_name exit_status);
        DisplayLine(command);
      ]

  | FailedToExtractTarball{ lock_name; exit_status; command } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "failed to extract the tarball of '%s' (exit status: %d). command:" lock_name exit_status);
        DisplayLine(command);
      ]

  | FailedToFetchExternalZip{ url; exit_status; command } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "failed to fetch file from '%s' (exit status: %d). command:" url exit_status);
        DisplayLine(command);
      ]

  | ExternalZipChecksumMismatch{ url; path; expected; got } ->
      make_error_message Interface [
        NormalLine("checksum mismatch of an external zip file.");
        DisplayLine(Printf.sprintf "- fetched from: '%s'" url);
        DisplayLine(Printf.sprintf "- path: '%s'" (get_abs_path_string path));
        DisplayLine(Printf.sprintf "- expected: '%s'" expected);
        DisplayLine(Printf.sprintf "- got: '%s'" got);
      ]

  | TarGzipChecksumMismatch{ lock_name; url; path; expected; got } ->
      make_error_message Interface [
        NormalLine("checksum mismatch of a tarball.");
        DisplayLine(Printf.sprintf "- lock name: '%s'" lock_name);
        DisplayLine(Printf.sprintf "- fetched from: '%s'" url);
        DisplayLine(Printf.sprintf "- path: '%s'" (get_abs_path_string path));
        DisplayLine(Printf.sprintf "- expected: '%s'" expected);
        DisplayLine(Printf.sprintf "- got: '%s'" got);
      ]

  | FailedToExtractExternalZip{ exit_status; command } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "failed to extract a zip file (exit status: %d). command:" exit_status);
        DisplayLine(command);
      ]

  | FailedToCopyFile{ exit_status; command } ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "failed to copy a file (exit status: %d). command:" exit_status);
        DisplayLine(command);
      ]

  | PackageRegistryFetcherError(e) ->
      begin
        match e with
        | FailedToUpdateGitRegistry{ exit_status; command } ->
            make_error_message Interface [
              NormalLine(Printf.sprintf "failed to update registry (exit status: %d). command:" exit_status);
              DisplayLine(command);
            ]
      end

  | CanonicalRegistryUrlError(e) ->
      begin
        match e with
        | ContainsQueryParameter{ url } ->
            make_error_message Interface [
              NormalLine("registry URLs must not contain query parameters:");
              DisplayLine(url);
            ]

        | NoUriScheme{ url } ->
            make_error_message Interface [
              NormalLine("the registry URL does not contain a scheme:");
              DisplayLine(url);
            ]

        | UnexpectedUrlScheme{ url; scheme } ->
            make_error_message Interface [
              NormalLine(Printf.sprintf "unexpected scheme '%s' in a registry URL:" scheme);
              DisplayLine(url);
            ]
      end


let make_font_error_message : font_error -> string = function
  | FailedToReadFont(abspath, msg) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "cannot load font file '%s';" fname);
        DisplayLine(msg);
      ]

  | FailedToDecodeFont(abspath, e) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "cannot decode font file '%s';" fname);
        NormalLine(Format.asprintf "%a" Otfed.Decode.Error.pp e);
      ]

  | FailedToMakeSubset(abspath, e) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "cannot make a subset of font file '%s';" fname);
        NormalLine(Format.asprintf "%a" Otfed.Subset.Error.pp e);
      ]

  | NotASingleFont(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "the font file '%s' is not a single font file." fname);
      ]

  | NotAFontCollectionElement(abspath, index) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "the font file '%s' (used with index %d) is not a collection." fname index);
      ]

  | CannotFindLibraryFileAsToFont(libpath, candidates) ->
      let lines =
        candidates |> List.map (fun abspath ->
          DisplayLine(Printf.sprintf "- %s" (get_abs_path_string abspath))
        )
      in
      make_error_message Interface
        (NormalLine(Printf.sprintf "cannot find '%s'. candidates:" (get_lib_path_string libpath)) :: lines)

  | NoMathTable(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "font file '%s' does not have a 'MATH' table." fname);
      ]

  | PostscriptNameNotFound(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "font file '%s' does not have a PostScript name." fname);
      ]

  | CannotFindUnicodeCmap(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "font file '%s' does not have a 'cmap' subtable for Unicode code points." fname);
      ]

  | CollectionIndexOutOfBounds{ path; index; num_elements } ->
      let fname = convert_abs_path_to_show path in
      make_error_message Interface [
        NormalLine(Printf.sprintf "%d: index out of bounds;" index);
        NormalLine(Printf.sprintf "font file '%s' has %d elements." fname num_elements);
      ]


let exn_to_error_message e =
  match e with
  | RemainsToBeImplemented(msg) ->
      make_error_message Interface [
        NormalLine("remains to be supported:");
        DisplayLine(msg);
      ]

  | NoLibraryRootDesignation ->
      make_error_message Interface [
        NormalLine("cannot determine where the SATySFi library root is;");
        NormalLine("set appropriate environment variables");
        NormalLine("or specify configuration search paths with -C option.");
      ]

  | ShouldSpecifyOutputFile ->
      make_error_message Interface [
        NormalLine("should specify output file for text mode.");
      ]

  | UnexpectedExtension(ext) ->
      make_error_message Interface [
        NormalLine(Printf.sprintf "unexpected file extension '%s'." ext);
      ]

  | LoadHyph.InvalidPatternElement(rng) ->
      make_error_message System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine("invalid string for hyphenation pattern.");
      ]

  | HorzBox.FontIsNotSet{ raw; normalized } ->
      make_error_message Interface [
        NormalLine("font is not set;");
        DisplayLine(Printf.sprintf "- raw script: %s" (CharBasis.show_script raw));
        DisplayLine(Printf.sprintf "- normalized script: %s" (CharBasis.show_script normalized));
      ]

  | HorzBox.MathFontIsNotSet ->
      make_error_message Interface [
        NormalLine("math font is not set.");
      ]

  | ConfigError(e) ->
      make_config_error_message e

  | FontInfo.FontInfoError(e) ->
      make_font_error_message e

  | ImageHashTable.CannotLoadPdf(msg, abspath, pageno) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "cannot load PDF file '%s' page #%d;" fname pageno);
        DisplayLine(msg);
      ]

  | ImageHashTable.CannotLoadImage(msg, abspath) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine(msg);
      ]

  | ImageHashTable.ImageOfWrongFileType(abspath) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine("This file format is not supported.");
      ]

  | ImageHashTable.UnsupportedColorModel(_, abspath) ->
      let fname = convert_abs_path_to_show abspath in
      make_error_message Interface [
        NormalLine(Printf.sprintf "cannot load image file '%s';" fname);
        DisplayLine("This color model is not supported.");
      ]

  | Lexer.LexError(rng, s) ->
      make_error_message Lexer [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(s);
      ]

  | MyYojsonUtil.SyntaxError(fname, msg) ->
      make_error_message System [
        NormalLine(Printf.sprintf "in '%s':" fname);
        NormalLine(msg);
      ]

  | MyYojsonUtil.MultipleDesignation(rng, key) ->
      make_error_message System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "multiple designation for key '%s'." key);
      ]

  | Yojson.SafePos.Util.Type_error(msg, (pos, _)) ->
      let rng = MyYojsonUtil.make_range pos in
      make_error_message System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(msg);
      ]

  | MyYojsonUtil.MissingRequiredKey(rng, key) ->
      make_error_message System [
        NormalLine(Printf.sprintf "at %s:" (Range.to_string rng));
        NormalLine(Printf.sprintf "missing required key '%s'." key);
      ]

  | Evaluator.EvalError(s)
  | Vm.ExecError(s)
      -> make_error_message Evaluator [ NormalLine(s); ]

  | State.NotDuringPageBreak ->
      make_error_message Evaluator [
        NormalLine("a primitive as to PDF annotation was called before page breaking starts.");
      ]

  | PageBreak.PageNumberLimitExceeded(m) ->
      make_error_message Evaluator [
        NormalLine(Printf.sprintf "page number limit (= %d) exceeded." m);
        NormalLine(Printf.sprintf "If you really want to output more than %d pages, use '--page-number-limit'." m);
      ]

  | Sys_error(s) ->
      make_error_message System [ NormalLine(s); ]

  | e -> raise e
