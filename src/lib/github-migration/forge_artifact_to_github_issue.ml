let body tracker entry =
  let open Forge_dump_tools.Forge_dump_t in
  let open Jingoo in
  let tr = Netencoding.Html.decode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 () in
  let date_from_timestamp i =
    Jg_types.Tstr
      (CalendarLib.Printer.Calendar.to_string
        (CalendarLib.Calendar.from_unixfloat (float_of_int i)))
  in
  let user u =
    Jg_types.Tstr
      (Printf.sprintf "[%s](https://forge.ocamlcore.org/users/%s)" u u)
  in
  let message (msg: message) =
    Jg_types.Tobj [
      ("user", user msg.submitted_by);
      ("date", date_from_timestamp msg.add_date);
      ("body", Jg_types.Tstr (tr msg.body));
    ]
  in
  let artifact_type, labels =
    match tracker.name with
    | "Bugs" -> "bug", ["bug"]
    | "Patches" -> "patch", []
    | "Feature Requests" -> "feature request", ["enhancement"]
    | _ -> assert false
  in
  let body =
    Jg_template.from_string
    ~env:{Jg_types.std_env with autoescape = false}
    ~models:[
      ("artifact_type", Jg_types.Tstr artifact_type);
      ("artifact_id", Jg_types.Tint entry.artifact_id);
      ("details", Jg_types.Tstr (tr entry.details));
      ("submitted_by", user entry.submitted_by);
      ("assigned_to", user entry.assigned_to);
      ("open_date", date_from_timestamp entry.open_date);
      ("is_closed", Jg_types.Tbool (entry.close_date != 0));
      ("close_date", date_from_timestamp entry.close_date);
      ("messages", Jg_types.Tlist (
        List.map message (
          List.sort
            (fun (msg1: message) (msg2: message) ->
              msg1.add_date - msg2.add_date)
            entry.messages)));
    ]
    "__This {{ artifact_type }} has been migrated from artifact #{{ artifact_id }} on forge.ocamlcore.org. It was assigned to {{ assigned_to }}.
{%- if is_closed %} It was closed on {{ close_date }}.{% endif %}__

## {{ submitted_by }} posted on {{ open_date }}:

{{ details }}
{%- for msg in messages %}
## {{ msg.user }} replied on {{ msg.date }}:

{{ msg.body }}

{% endfor %}
"
  in
  Github_t.{
    new_issue_title = tr entry.summary;
    new_issue_body = Some body;
    new_issue_assignee = None;
    new_issue_milestone = None;
    new_issue_labels = labels;
  }
