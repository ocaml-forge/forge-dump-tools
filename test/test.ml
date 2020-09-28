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

let test_can_read_file_from_zip_file test_ctxt =
  let fn = in_testdata_dir test_ctxt ["ocamlmod.zip"] in
  let zip = Forge_dump_zip.load fn in
  assert_equal
    ~printer:string_of_int
    ~msg:"Can read file group.json"
    91
    (String.length (Forge_dump_zip.file_content zip "group.json"))

let test_can_decode_group_json test_ctxt =
  let fn = in_testdata_dir test_ctxt ["ocamlmod.zip"] in
  let zip = Forge_dump_zip.load fn in
  let grp = Forge_dump_zip.group zip in
  assert_equal
    ~printer:(fun s -> s)
    "ocamlmod"
    grp.Forge_dump_t.group_name;
  assert_equal
    ~printer:(fun s -> s)
    "ocamlmod"
    grp.Forge_dump_t.unix_group_name;
  assert_equal
    ~printer:string_of_int
    244
    grp.Forge_dump_t.group_id

let test_can_decode_artifact_json test_ctxt =
  let fn = in_testdata_dir test_ctxt ["ocamlmod.zip"] in
  let zip = Forge_dump_zip.load fn in
  let artfct = Forge_dump_zip.artifact zip in
  assert_equal
    ~printer:string_of_int
    3
    (List.length artfct)

let test_can_decode_frs_json test_ctxt =
  let fn = in_testdata_dir test_ctxt ["ocamlmod.zip"] in
  let zip = Forge_dump_zip.load fn in
  let frs = Forge_dump_zip.frs zip in
  assert_equal
    ~printer:string_of_int
    15
    (List.length frs)

let test_can_extract_frs_file test_ctxt =
  let fn = in_testdata_dir test_ctxt ["ocamlmod.zip"] in
  let tmpfn, chn = OUnit2.bracket_tmpfile test_ctxt in
  let zip = Forge_dump_zip.load fn in
  Forge_dump_zip.frs_file zip Forge_dump_t.{
    package_name = "ocamlmod";
    release_name = "0.0.1";
    filename = "ocamlmod-0.0.1.tar.gz";
    release_id = 437;
    file_id = 623;
    package_id = 289;
  } chn;
  close_out chn;
  assert_equal
    ~printer:(fun s -> s)
    "2eb2eb97642ab56c5d68bd04697c904f"
    (Digest.to_hex (Digest.file tmpfn))


let () =
  run_test_tt_main
    ("forge-dump-tools" >::: [
      "CanOpenZipFileAndReadIt" >:: test_can_open_zip_file_and_read_it;
      "CanReadFileFromZipFile" >:: test_can_read_file_from_zip_file;
      "CanDecodeGroupJSON" >:: test_can_decode_group_json;
      "CanDecodeArtifactJSON" >:: test_can_decode_artifact_json;
      "CanDecodeFRSJSON" >:: test_can_decode_frs_json;
      "CanExtractFRSFile" >:: test_can_extract_frs_file;
    ])
