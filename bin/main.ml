open Frontend
open Cmdliner

type options = { mode : [ `Stdin | `File of string ] }

let main { mode } =
  match Driver.process_file mode with
  | Ok () -> `Ok ()
  | Error () -> `Error (false, "encountered one or more errors")

let proginfo =
  let doc =
    "A pedagogic abstract bidirectional elaborator for dependent type theory."
  in
  let err_exit = Term.exit_info ~doc:"on ill-formed types or terms." 1 in
  Term.info "dreamtt" ~version:"0.0" ~doc
    ~exits:(err_exit :: Term.default_exits)

let opt_input_file =
  let doc = "The file to typecheck. When $(docv) is -, read stdin." in
  let parse_dash =
    Term.(
      app @@ const @@ Option.map
      @@ fun str -> if str = "-" then `Stdin else `File str)
  in
  Arg.(
    parse_dash & value
    & pos ~rev:true 0 (some string) None
    & info [] ~doc ~docv:"FILE")

let consolidate_options input_file : options Term.ret =
  match input_file with
  | Some input_file -> `Ok { mode = input_file }
  | None -> `Error (true, "scripting mode expects an input file")

let () =
  let options : options Term.t =
    Term.(ret (const consolidate_options $ opt_input_file))
  in
  let t = Term.ret @@ Term.(const main $ options) in
  Term.exit @@ Term.eval ~catch:true ~err:Format.std_formatter (t, proginfo)
