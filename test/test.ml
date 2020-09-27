open OUnit2
open Forge_dump_tools

let test_can_open_zip_file_and_read_it test_ctxt =
  let fn = in_testdata_dir test_ctxt ["ocamlmod.zip"] in
  let zip = Forge_dump_zip.load fn in
  assert_equal
    ~printer:(fun s -> s)
    ~msg:"Name of the forge dump"
    "ocamlmod"
    zip.Forge_dump_zip.name;
  assert_equal
    ~printer:string_of_bool
    ~msg:"Forge dump contains frs.json"
    true
    (Forge_dump_zip.file_exists zip "frs.json")

let () =
  run_test_tt_main
    ("forge-dump-tools" >::: [
      "CanOpenZipFileAndReadIt" >:: test_can_open_zip_file_and_read_it
    ])
