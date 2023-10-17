
open MyUtil
open Types


type 'a t =
  (syntactic_value -> 'a) * (abs_path -> 'a -> unit)


let text =
  let output abspath_out s =
    Core.Out_channel.write_all (get_abs_path_string abspath_out) ~data:s
  in
  (EvalUtil.get_string, output)


let pdf =
  let post_eval value =
    match value with
    | BaseConstant(BCDocument(paper_size, pbstyle, columnhookf, columnendhookf, pagecontf, pagepartsf, imvblst)) ->
        Logging.start_page_break ();
        State.start_page_break ();
        begin
          match pbstyle with
          | SingleColumn ->
              PageBreak.main ~paper_size
                columnhookf pagecontf pagepartsf imvblst

          | MultiColumn(origin_shifts) ->
              PageBreak.main_multicolumn ~paper_size
                origin_shifts columnhookf columnendhookf pagecontf pagepartsf imvblst
        end
    | _ ->
        EvalUtil.report_bug_value "main; not a DocumentValue(...)" value
  in
  (post_eval, HandlePdf.write_to_file)
