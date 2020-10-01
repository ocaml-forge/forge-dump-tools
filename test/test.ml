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

let test_migrate_forge_artifact_to_github_issue_body _test_ctxt =
  let entry = Forge_dump_t.{
    artifact_id = 1745;
    priority = 3;
    status = "Open";
    submitted_by = "user102";
    assigned_to = "user103";
    open_date = 1601408669;
    close_date = 0;
    summary = "&quot;-chooser&quot; failfirst is very slow";
    details = "
I have an &gt;OUnit test program which generates OUnit tests from the contents of directories. These directories contain source files for testing a compiler. One of the directories contains a plethora of source files which were generated from a synthesis tool. Overall, my test suite has about 4,500 tests.

I often want to identify the first bug immediately so I can begin working on it. For this reason, I usually run the tester with \"-chooser failfirst\" so I can get information about the first failed test without waiting for the other tests to run. Unfortunately, even this is very slow. Instead of immediately stopping the test run and printing the results of the failed test, each of the other thousands of tests must be skipped. This process takes about twenty times as long as the first test did to fail.

test = new LinkedList&lt;Test&gt;();
";
    messages = [
      {
        submitted_by = "user102";
        add_date = 1601408955;
        body = "Thank you!";
      };
      {
        submitted_by = "user103";
        add_date = 1601408875;
        body = "So you have a LOT of very small test.

How long does the 4500 tests take to run in skipped mode?";
      };
    ];
    files = [];
  }
  in
  let tracker = Forge_dump_t.{
    name = "Bugs";
    group_artifact_id = 0;
    entries = [];
    dropped_entries = [];
  }
  in
  let new_issue =
    Github_migration.Forge_artifact_to_github_issue.body tracker entry
  in
  let open Github_t in
  assert_equal
    ~msg:"title"
    ~printer:(fun s -> s)
    "\"-chooser\" failfirst is very slow"
    new_issue.new_issue_title;
  assert_equal
    ~msg:"labels"
    ~printer:(String.concat ", ")
    ["bug"]
    new_issue.new_issue_labels;
  match new_issue.new_issue_body with
  | None -> assert_failure "new_issue_body should be set"
  | Some s -> begin
      assert_bool
        "body contains the initial issue number"
        (BatString.exists s "#1745");
      assert_bool
        "body contains the >OUnit"
        (BatString.exists s ">OUnit")
  end


let () =
  run_test_tt_main
    ("forge-dump-tools" >::: [
      "CanOpenZipFileAndReadIt" >:: test_can_open_zip_file_and_read_it;
      "CanReadFileFromZipFile" >:: test_can_read_file_from_zip_file;
      "CanDecodeGroupJSON" >:: test_can_decode_group_json;
      "CanDecodeArtifactJSON" >:: test_can_decode_artifact_json;
      "CanDecodeFRSJSON" >:: test_can_decode_frs_json;
      "CanExtractFRSFile" >:: test_can_extract_frs_file;
      "MigrateForgeArtifactToGithubIssueBody" >::
        test_migrate_forge_artifact_to_github_issue_body;
    ])
