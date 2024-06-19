open Core

let () =
  let hashtable = Hashtbl.create (module String) in
  let ch = In_channel.create "input" in
  In_channel.iter_lines ch ~f:(fun l ->
    let station, measurement =
      match String.split l ~on:';' with
      | [ station; measurement ] -> station, float_of_string measurement
      | _ -> raise (Invalid_argument "")
    in
    let insert_data =
      match Hashtbl.find hashtable station with
      | Some (min, mean, max) ->
        Float.min min measurement, (mean +. measurement) /. 2., Float.max max measurement
      | None -> measurement, measurement, measurement
    in
    Hashtbl.set hashtable ~key:station ~data:insert_data);
  let ch = Out_channel.create "output" in
  Out_channel.fprintf ch "{";
  Hashtbl.to_alist hashtable
  |> List.sort ~compare:(fun (key, _) (key', _) -> String.compare key key')
  |> List.map ~f:(fun (key, (min, mean, max)) -> sprintf "%s=%f/%f/%f" key min mean max)
  |> String.concat ~sep:","
  |> Out_channel.fprintf ch "%s";
  Out_channel.fprintf ch "}\n";
  Out_channel.close ch
;;
