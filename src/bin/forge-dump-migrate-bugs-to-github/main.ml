let () =
  let zipfn = ref "" in
  let todofn = ref "" in
  let dry_run = ref false in
  let user = ref "" in
  let repo = ref "" in
  let args =
    [
      "--zip",
      Arg.Set_string zipfn,
      "fn The zip file of the forge dump.";

      "--todo",
      Arg.Set_string todofn,
      "fn The list of things to do on the issues have been created (e.g. uploading files).";

      "--user",
      Arg.Set_string user,
      "user Github user.";

      "--repo",
      Arg.Set_string repo,
      "repo Github repository.";

      "--dry_run",
      Arg.Set dry_run,
      " Don't act, just show what need to be done.";
    ]
  in
  let lst = ref [] in
  let usage_msg = "\
forge-dump-migrate-bugs-to-github v%s

Create issues and list the actions that need to be done. Follow instructions
on https://github.com/mirage/ocaml-github#git-jar to add credentials, use
forge.o.o.


Example:
$> opam install lwt_ssl github
$> export SSL_CERT_DIR=/etc/ssl/certs/
$> git jar make gildor478 forge.o.o -s public_repo
$> forge-dump-migrate-bugs-to-github \
   --zip ounit.zip \
   --todo ounit-todo.txt \
   --user gildor478 \
   --repo forge-dump-migration-test

Options:\n"
  in
  let log ?(todo=false) str =
    Printf.eprintf "%s\n" str;
    if todo then begin
      let fd =
        open_out_gen
          [Open_wronly; Open_append; Open_creat; Open_text]
          0o644
          !todofn
      in
      Printf.fprintf fd "%s\n" str;
      close_out fd
    end
  in
  let () =
    Arg.parse
      (Arg.align args)
      (fun str -> lst := str :: !lst)
      usage_msg;
    if !user = "" then failwith "--user is not set";
    if !repo = "" then failwith "--repo is not set"
  in
  let zip = Forge_dump_tools.Forge_dump_zip.load !zipfn in
  let open Forge_dump_tools.Forge_dump_t in
  let token =
    let opt =
      Lwt_main.run
        (Github_cookie_jar.(
          init () |> Lwt_main.run |> get ~name:"forge.o.o"))
    in
    match opt with
    | Some auth -> Github.Token.of_auth auth
    | None -> failwith "unable to retrieve credentials from git-jar."
  in

  List.iter
    (fun tracker ->
      List.iter
        (fun entry ->
          let issue =
            Github_migration.Forge_artifact_to_github_issue.body tracker entry
          in
          let issue_url, issue_number =
            log (Printf.sprintf "create an issue for #%d" entry.artifact_id);
            if !dry_run then begin
              Printf.sprintf
                "https://github.com/%s/%s/issues/12345"
                !user !repo,
              12345
            end else begin
              let i =
                Github.Response.value
                  (Lwt_main.run
                    (Github.Monad.run
                      (Github.Issue.create
                        ~user:!user ~repo:!repo ~token ~issue ())))
              in
              i.issue_html_url, i.issue_number
            end
          in
          if entry.status = "Closed" then begin
            log
              (Printf.sprintf
                "close the issue %d, since old #%d was closed"
                issue_number entry.artifact_id);
            if not !dry_run then begin
              let _ =
                Lwt_main.run
                  (Github.Monad.run
                    (Github.Issue.update
                      ~user:!user ~repo:!repo ~num:issue_number
                      ~token ~issue:{
                        update_issue_title = None;
                        update_issue_body = None;
                        update_issue_state = Some `Closed;
                        update_issue_assignee = None;
                        update_issue_milestone = None;
                        update_issue_labels = None;
                      } ()))
              in ()
            end
          end;
          List.iter
            (fun (file: file) ->
              log ~todo:true
                (Printf.sprintf
                  "Visit %s and upload file %s" issue_url file.filename))
            entry.files)
        tracker.entries)
    (Forge_dump_tools.Forge_dump_zip.artifact zip)
