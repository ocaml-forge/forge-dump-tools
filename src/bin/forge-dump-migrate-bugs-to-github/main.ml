let () =
  let zipfn = ref "" in
  let args =
    [
      "--zip",
      Arg. Set_string zipfn,
      "fn The zip file of the forge dump.";
    ]
  in
  let lst = ref [] in
  let usage_msg = "\
forge-dump-migrate-bugs-to-github v%s

Example:
$> forge-dump-migrate-bugs-to-github -zip ounit.zip

Options:\n"
  in
  let () =
    Arg.parse
      (Arg.align args)
      (fun str -> lst := str :: !lst)
      usage_msg
  in
  Forge_dump_tools.Zip.load !zipfn;
  exit 1

