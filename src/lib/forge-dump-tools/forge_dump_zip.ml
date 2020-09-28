module SetFilename = Set.Make(String)

type zip = {
  zip_filename: string;
  name: string;
  filenames: SetFilename.t;
}


let load fn =
  let zip = Zip.open_in fn in
  let lst = Zip.entries zip in
  let filenames =
    List.fold_left
      (fun s e -> SetFilename.add e.Zip.filename s)
      SetFilename.empty
      lst
  in
  let name =
    List.fold_left
      (fun name e ->
        match String.split_on_char '/' e.Zip.filename with
        | topdir :: _ ->
            if name == "" then
              topdir
            else if name <> topdir then
              failwith
                (Printf.sprintf
                  "zip file %s should only contains one root \
                  directory, here we found %s and %s" fn name topdir)
            else
              name
        | [] ->
            assert false)
      ""
      lst
  in
  Zip.close_in zip;
  {
    zip_filename = fn;
    name         = name;
    filenames    = filenames;
  }

(**
 * Check existence of a file in the forge dump.
 * To check the presence of a directory, end the filename by '/'.
 *)
let file_exists zip fn =
  SetFilename.mem (zip.name^"/"^fn) zip.filenames

(**
 * Read and return the content of the given file.
 *)
let file_content zip fn =
  let z = Zip.open_in zip.zip_filename in
  try begin
    let entry = Zip.find_entry z (zip.name^"/"^fn) in
    let s = Zip.read_entry z entry in
    Zip.close_in z;
    s
  end with Not_found -> begin
    Zip.close_in z;
    failwith
      (Printf.sprintf "unable to find file %s in zip %s" fn zip.zip_filename)
  end

(**
 * Read group.json and return it.
 *)
let group zip =
  Forge_dump_j.group_of_string (file_content zip "group.json")
