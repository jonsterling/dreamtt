let process_file input =
  match input with
  | `Stdin ->
      print_endline "stdin";
      Ok ()
  | `File f ->
      print_endline f;
      Ok ()
