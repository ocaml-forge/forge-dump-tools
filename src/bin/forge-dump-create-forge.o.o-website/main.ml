let github_re = Re.compile (Re.Posix.re "(https://github.com/[^ ]*)")
let redirect_re =  Re.compile (Re.Posix.re "Redirect +[0-9]+ +/ +(http[^ ]*)")

let write_file fn content =
  let fd = open_out fn in
  output_string fd content;
  close_out fd


let detect_migration dump =
  let open Forge_dump_tools.Forge_dump_t in
  let open Forge_dump_tools.Forge_dump_zip in
  let description =
    let grp = group dump in
    match Re.exec_opt github_re grp.short_description with
    | Some grp -> Some (Re.Group.get grp 1)
    | None -> None
  in
  let htaccess =
    let fn = "home/htdocs/.htaccess" in
    if file_exists dump fn then begin
      match Re.exec_opt redirect_re (file_content dump fn) with
      | Some grp -> Some (Re.Group.get grp 1)
      | None -> None
    end else begin
      None
    end
  in
  match description, htaccess with
  | Some s, None
  | None, Some s -> s
  | Some s1, Some s2 when s1 = s2 ->
      s1
  | Some s1, Some s2 ->
      s1 ^ " or " ^ s2
  | None, None ->
      ""

let create_project dir fn =
  let open Forge_dump_tools.Forge_dump_t in
  let open Jingoo in
  let tr = Netencoding.Html.decode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 () in
  let user u =
    Jg_types.Tstr
      (Printf.sprintf "[%s](/users/%s)" u u)
  in
  let date_from_timestamp i =
    Jg_types.Tstr
      (CalendarLib.Printer.Calendar.to_string
        (CalendarLib.Calendar.from_unixfloat (float_of_int i)))
  in
  let dump = Forge_dump_tools.Forge_dump_zip.load fn in
  let grp = Forge_dump_tools.Forge_dump_zip.group dump in
  let artifacts = Forge_dump_tools.Forge_dump_zip.artifact dump in
  let count_open_artifacts s =
    List.fold_left
      (fun c t ->
        if t.name = s then
          List.fold_left
            (fun c e -> if e.status = "Open" then c + 1 else c)
            c t.entries
        else
          c)
      0 artifacts
  in
  let frs = Forge_dump_tools.Forge_dump_zip.frs dump in
  let count_filename glob =
    let re = Re.seq [ Re.bos; Re.Glob.glob glob; Re.eos ] |> Re.compile in
    Forge_dump_tools.Forge_dump_zip.SetFilename.fold
      (fun fn c -> if Re.execp re fn then begin
        c + 1
      end else begin
        c
      end)
      dump.Forge_dump_tools.Forge_dump_zip.filenames
      0
    in
  let body =
    Jg_template.from_string
    ~env:{Jg_types.std_env with autoescape = false}
    ~models:[
      ("unix_group_name", Jg_types.Tstr grp.unix_group_name);
      ("register_date", date_from_timestamp grp.register_date);
      ("description", Jg_types.Tstr (tr grp.short_description));
      ("members", Jg_types.Tlist (List.map user grp.members));
      ("admins", Jg_types.Tlist (List.map user grp.admins));
      ("open_bugs", Jg_types.Tint (count_open_artifacts "Bugs"));
      ("open_feature_requests",
        Jg_types.Tint (count_open_artifacts "Feature Requests"));
      ("released_files", Jg_types.Tint (List.length frs));
      ("mailing_list", Jg_types.Tint (count_filename "*/mailman/*.mbox"));
      ("vcs", Jg_types.Tint (
        (count_filename "*/scm/bzrroot/*/.bzr/")
        + (count_filename "*/scm/gitroot/*.git/")
        + (count_filename "*/scm/svnroot/")
        + (count_filename "*/scm/cvsroot/")
        + (count_filename "*/scm/darcsroot/*/_darcs/")
        + (count_filename "*/scm/hgroot/*/.hg/")));
      ("new_website", Jg_types.Tstr (detect_migration dump));
    ]
    "---
title: \"{{ unix_group_name }}\"
{%- if new_website %}
new_website: {{ new_website }}
{%- endif %}
no_index: true
---

{{ description }}

{% if admins %}
* Admins:
{%- for user in admins %}
  * {{ user }}
{%- endfor -%}
{% endif %}
{%- if members %}
* Members:
{%- for user in members %}
  * {{ user }}
{%- endfor -%}
{% endif %}
* [Releases](https://download.ocamlcore.org/{{ unix_group_name }})
* Registered: {{ register_date }}
* Archived data:
  * {{ open_bugs }} open bugs
  * {{ open_feature_requests }} open feature requests
  * {{ mailing_list }} mailing list
  * {{ vcs }} VCS
  * {{ released_files }} released files
"
  in
  let fn = dir^"/"^grp.unix_group_name^".md" in
  if not (Sys.file_exists dir) || not (Sys.is_directory dir) then
    Unix.mkdir dir 0o755;
  write_file fn body

let create_users dir lst =
  let open Jingoo in
  let module SetString = Set.Make(String) in
  let module MapString = Map.Make(String) in
  let append m u e =
    let st =
      try
        MapString.find u m
      with Not_found ->
        SetString.empty
    in
    MapString.add u (SetString.add e st) m
  in
  let projects_of_admin, projects_of_member =
    List.fold_left
      (fun (a, m) fn ->
        let dump = Forge_dump_tools.Forge_dump_zip.load fn in
        let grp = Forge_dump_tools.Forge_dump_zip.group dump in
        List.fold_left
          (fun a u -> append a u grp.unix_group_name)
          a grp.admins,
        List.fold_left
          (fun m u -> append m u grp.unix_group_name)
          m grp.members)
      (MapString.empty, MapString.empty)
      lst
  in
  let projects_of_member =
    (* Remove membership when user is already an admin. *)
    MapString.mapi
      (fun u st ->
        try
          SetString.diff st (MapString.find u projects_of_admin)
      with Not_found ->
        st)
      projects_of_member
  in
  let users =
    let st = MapString.fold
      (fun u _ st -> SetString.add u st)
      projects_of_admin
      SetString.empty
    in
    MapString.fold
      (fun u _ st -> SetString.add u st)
      projects_of_member
      st
  in

  let create_file u =
    let find_or_empty m k =
      try
        Jg_types.Tlist
          (List.map
            (fun s -> Jg_types.Tstr s)
            (SetString.elements (MapString.find k m)))
      with Not_found ->
        Jg_types.Tlist []
    in
    let body =
      Jg_template.from_string
      ~env:{Jg_types.std_env with autoescape = false}
      ~models:[
        ("user", Jg_types.Tstr u);
        ("admins", find_or_empty projects_of_admin u);
        ("members", find_or_empty projects_of_member u);
      ]
      "---
title: \"{{ user }}\"
no_index: true
---

* Projects:
{%- for project in admins %}
  * [{{ project }}](/projects/{{ project }}/) (admin)
{%- endfor -%}
{%- for project in members %}
  * [{{ project }}](/projects/{{ project }}/)
{%- endfor -%}
"
    in
    let fn = dir^"/"^u^".md" in
    write_file fn body
  in

  if not (Sys.file_exists dir) || not (Sys.is_directory dir) then
    Unix.mkdir dir 0o755;
  SetString.iter create_file users

let () =
  let lst = ref [] in
  let output_dir = ref "" in
  let args = [
    "--output_dir",
    Arg.Set_string output_dir,
    "db Directory where output files will be created.";
  ]
  in
  Arg.parse (Arg.align args) (fun s -> lst := s :: !lst) "";
  List.iter
    (create_project (Filename.concat !output_dir "projects"))
    (List.rev !lst);
  create_users (Filename.concat !output_dir "users") !lst

