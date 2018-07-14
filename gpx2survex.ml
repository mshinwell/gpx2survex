open Core

type xml =
  | Element of Xmlm.tag * xml list
  | Data of string

(*
let rec dump_xml = function
  | Element (((_, name), _attrs), xmls) ->
    Printf.eprintf "Element %s:\n" name;
    dump_xmls xmls;
    Printf.eprintf "Element %s ends.\n" name
  | Data d -> Printf.eprintf "Data %s\n" d

and dump_xmls xmls =
  List.iter xmls ~f:dump_xml
*)

let input_xml file =
  let chan = In_channel.create file in
  let el tag children = Element (tag, children)  in
  let data str = Data str in
  try
    let input = Xmlm.make_input (`Channel chan) in
    let _dtd, xml = Xmlm.input_doc_tree ~el ~data input in
    In_channel.close chan;
    xml
  with Xmlm.Error ((line, column), error) ->
    Printf.eprintf "XML parsing error (line %d, column %d): %s\n"
      line column
      (Xmlm.error_message error);
    failwith "Cannot parse GPX file"

let fold_nodes xmls name ~init ~f =
  List.fold xmls ~init ~f:(fun acc xml ->
    match xml with
    | Element (((_, name'), attrs), children) ->
      if name = name' then f acc attrs children
      else acc
    | _ -> acc)

let find_unique_node xmls name =
  let matching =
    List.filter_map xmls ~f:(fun xml ->
      match xml with
      | Element (((_, name'), _), children) ->
        if name = name' then Some children
        else None
      | _ -> None)
  in
  match matching with
  | [children] -> Some children
  | _ -> None

let find_unique_datum_toplevel xmls name =
  let matching =
    List.filter_map xmls ~f:(fun xml ->
      match xml with
      | Element (((_, name'), _), [Data datum])
          when name = name' -> Some datum
      | _ -> None)
  in
  match matching with
  | [datum] -> Some datum
  | _ -> None

let find_attr attrs name =
  let matching =
    List.filter_map attrs ~f:(fun ((_, name'), value) ->
      if name = name' then Some value else None)
  in
  match matching with
  | [value] -> Some value
  | _ -> None

type track_point = {
  latitude : string;
  longitude : string;
  altitude : string;
  time : Time.t;
}

type track_segment = track_point list

type track = {
  name : string option;
  segments : track_segment list;
}

type waypoint = {
  point : track_point;
  name : string option;
}

type gpx = {
  time : Time.t option;
  tracks : track list;
  waypoints : waypoint list;
}

let parse ~gpx_file =
  let malformed () = failwith "Cannot parse GPX file; malformed?" in
  let xml = input_xml gpx_file in
  match xml with
  | Element (((_, "gpx"), _), contents) ->
    let metadata = find_unique_node contents "metadata" in
    let time =
      match metadata with
      | None -> None
      | Some metadata -> find_unique_datum_toplevel metadata "time"
    in
    let time = Option.map time ~f:Time.of_string in
    let tracks =
      fold_nodes contents "trk" ~init:[] ~f:(fun tracks _trk_attrs trk ->
        let name = find_unique_datum_toplevel trk "name" in
        let track =
          { name;
            segments = [];
          }
        in
        let track =
          fold_nodes trk "trkseg" ~init:track
            ~f:(fun track _trkseg_attrs trkseg_contents ->
              let segment =
                fold_nodes trkseg_contents "trkpt" ~init:[]
                  ~f:(fun track_points trkpt_attrs trkpt_contents ->
                    let latitude = find_attr trkpt_attrs "lat" in
                    let longitude = find_attr trkpt_attrs "lon" in
                    let altitude =
                      find_unique_datum_toplevel trkpt_contents "ele"
                    in
                    let time =
                      find_unique_datum_toplevel trkpt_contents "time"
                    in
                    match latitude, longitude, altitude, time with
                    | Some latitude, Some longitude, Some altitude, Some time ->
                      let time = Time.of_string time in
                      let track_point =
                        { latitude;
                          longitude;
                          altitude;
                          time;
                        }
                      in
                      track_points @ [track_point]
                    | _, _, _, _ -> malformed ())
              in
              { track with
                segments = track.segments @ [segment];
              })
        in
        tracks @ [track])
    in
    let waypoints =
      fold_nodes contents "wpt" ~init:[]
        ~f:(fun waypoints wpt_attrs wpt_contents ->
          let latitude = find_attr wpt_attrs "lat" in
          let longitude = find_attr wpt_attrs "lon" in
          let altitude = find_unique_datum_toplevel wpt_contents "ele" in
          let time = find_unique_datum_toplevel wpt_contents "time" in
          let name = find_unique_datum_toplevel wpt_contents "name" in
          match latitude, longitude, altitude, time with
          | Some latitude, Some longitude, Some altitude, Some time ->
            let time = Time.of_string time in
            let point =
              { latitude;
                longitude;
                altitude;
                time;
              }
            in
            let waypoint =
              { point;
                name;
              }
            in
            waypoints @ [waypoint]
          | _, _, _, _ -> malformed ())
    in
    { time;
      tracks;
      waypoints;
    }
  | Element _ | Data _ -> malformed ()

let write_svx ~gpx ~output ~gpx_file ~gpx_coordinate_system:_
      ~svx_time_zone =
  Out_channel.fprintf output
    "; ** This file is autogenerated; do not edit **\n";
  Out_channel.fprintf output
    "; Generated by gps2survex from %s at %s\n"
    gpx_file
    (Time.to_string_abs (Time.now ()) ~zone:(Lazy.force Time.Zone.local));
  begin match gpx.tracks with
  | track::tracks ->
    begin match track.name with
    | None -> ()
    | Some name -> Out_channel.fprintf output "*title \"%s\"\n" name
    end
  | [] -> ()
  end;
  begin match gpx.time with
  | None -> ()
  | Some time ->
    Out_channel.fprintf output "; Track log timestamp: %s\n"
      (Time.to_string_abs time ~zone:svx_time_zone);
    let date = Time.to_date time ~zone:svx_time_zone in
    Out_channel.fprintf output "*date %s  ; time zone %s\n"
      (String.tr (Date.to_string date)
        ~target:'-'
        ~replacement:'.')
      (Time.Zone.to_string svx_time_zone)
  end;
  let svx_name = Filename.chop_extension (Filename.basename gpx_file) in
  Out_channel.fprintf output "\n*begin %s\n\n" svx_name;
  List.iteri gpx.waypoints ~f:(fun index waypoint ->
    (* CR mshinwell: assign earlier then remove duplicate code below *)
    let name =
      match waypoint.name with
      | None -> Printf.sprintf "wpt%d" index
      | Some name -> String.tr name ~target:' ' ~replacement:'_'
    in
    Out_channel.fprintf output "*export %s\n" name);
(*
  begin match gpx_coordinate_system with
  | None -> ()
  | Some system -> Out_channel.fprintf output "*cs %s\n\n" system
  end;
*)
  Out_channel.fprintf output "*cs out UTM33N\n\n";
  Out_channel.fprintf output "*cs long-lat\n\n";
  Out_channel.fprintf output "*data normal from to tape compass clino\n";
  Out_channel.fprintf output "*flags surface\n\n";
  List.iteri gpx.tracks ~f:(fun track_index track ->
    Out_channel.fprintf output "*begin track%d\n" track_index;
    List.iteri track.segments ~f:(fun segment_index segment ->
      Out_channel.fprintf output "*begin seg%d\n" segment_index;
      List.iteri segment ~f:(fun station point ->
        if station > 0 then begin
          Out_channel.fprintf output "%d %d 1.0 000 00\n"
            (station - 1)
            station
        end;
        Out_channel.fprintf output "*fix %d reference %s %s %s  ; %s\n"
          station
          point.longitude point.latitude
          point.altitude
          (Time.to_string_abs point.time ~zone:svx_time_zone));
      Out_channel.fprintf output "*end seg%d\n\n" segment_index);
    Out_channel.fprintf output "*end track%d\n\n" track_index);
  List.iteri gpx.waypoints ~f:(fun index waypoint ->
    let name =
      match waypoint.name with
      | None -> Printf.sprintf "wpt%d" index
      | Some name -> String.tr name ~target:' ' ~replacement:'_'
    in
    Out_channel.fprintf output "*fix %s reference %s %s %s  ; %s\n"
      name
      waypoint.point.longitude waypoint.point.latitude
      waypoint.point.altitude
      (Time.to_string_abs waypoint.point.time ~zone:svx_time_zone));
  Out_channel.fprintf output "\n*end %s\n\n" svx_name

let run ~gpx_coordinate_system ~svx_time_zone ~gpx_file ~output_dir =
  let gpx = parse ~gpx_file in
  let svx_file = (Filename.chop_extension gpx_file) ^ ".svx" in
  let svx_file =
    match output_dir with
    | None -> svx_file
    | Some output_dir -> output_dir ^/ (Filename.basename svx_file)
  in
  let svx_time_zone =
    match svx_time_zone with
    | None -> Lazy.force Time.Zone.local
    | Some svx_time_zone -> Time.Zone.of_string svx_time_zone
  in
  let output = Out_channel.create svx_file in
  write_svx ~gpx ~output ~gpx_file ~gpx_coordinate_system ~svx_time_zone;
  Out_channel.close output

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Convert GPX files to Survex format"
    [%map_open
     let gpx_coordinate_system = (* CR mshinwell: remove this *)
        flag "coordinate-system" (optional string)
          ~doc:"NAME Set the coordinate system which the GPX file is in"
      and svx_time_zone =
        flag "svx-time-zone" (optional string)
           ~doc:"ZONE Set the time zone for times appearing in the Survex file"
      and output_dir =
        flag "output-dir" (optional string)
           ~doc:"DIR Write .svx file into the given directory"
      and gpx_file =
        anon ("gpx-file" %: string)
      in
      fun () ->
        run ~gpx_coordinate_system ~svx_time_zone ~gpx_file ~output_dir
    ]
  |> Command.run
