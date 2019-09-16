module B = Basic

(** path to a file *)
type path = string

type cin

type cout

(** Gather out_channel and in_channel in a GADT for input and output files. In the case of an output file, we store also the formatter associated with. *)
type _ channel =
  | In : in_channel ->  cin channel
  | Out : out_channel * Format.formatter -> cout channel

type 'a t =
  {
    path : path;
    (** Path to the current file *)

    md : B.mident;
    (** Md of this file *)

    channel : 'a channel
    (** OCaml channel to this file *)
  }


(** Output directory where output files are created *)
let output_directory : string option ref = ref None

type step = [ `Input | `Output | `Config ]

(** return the suffix according to the step [s] *)
let suffix_of_step : step -> string = function
  | `Input -> ""
  | `Output -> ""
  | `Config -> ""

(** [add_sufix file suffix] returns the string [file'] where suffix is_added at then end of [file] *)
let add_suffix : path -> string -> string = fun file suffix ->
  let ext = Filename.extension file in
  let name = Filename.chop_extension file in
  name ^ suffix ^ ext

(** [add_dir dir file] prefix the filename [file] with the directory [dir] *)
let add_dir : string -> string -> string = fun dir file ->
  dir ^ Filename.dir_sep ^ (Filename.basename file)

let mk_dir : string option ref -> string -> unit = fun rf dir ->
  begin
    try
      ignore(Unix.stat dir)
    with _ -> Unix.mkdir dir 0o755
  end;
  rf := Some dir

(** [get_out_path p s] returns the path that corresponds to the step [s] for path [p] *)
let get_out_path : path -> step -> path = fun path step ->
  let file_suffix = add_suffix path (suffix_of_step step) in
  match step with
  | `Input -> file_suffix
  | `Config -> file_suffix
  | `Output ->
    match !output_directory with
    | None -> failwith "Output_directory must be set. See --help for more information"
    | Some dir -> add_dir dir file_suffix

(** [md_of_file f] returns the [mident] of the file [f] *)
let md_of_path : path -> Basic.mident = fun path ->
  Basic.mk_mident (Filename.basename path)

(** [from_string f s] returns the filename that corresponds to the step [s] for file [f] *)
let out_from_string : path -> step -> cout t = fun path step ->
    let path = get_out_path path step in
    let md = md_of_path path in
    let oc = open_out path in
    let fmt = Format.formatter_of_out_channel oc in
    {path;md;channel=Out(oc,fmt)}

(** [from_string f s] returns the filename that corresponds to the step [s] for file [f] *)
let in_from_string : path -> step -> cin t = fun path step ->
    let path = get_out_path path step in
    let md = md_of_path path in
    let ic = open_in path in
    {path;md;channel=In ic}

(** [in_channel_of_file in_file] returns the channel associated to an [in_file] *)
let in_channel_of_file : cin t -> in_channel = fun file ->
  match file.channel with
  | In ic -> ic

(** [fmt_of_file out_file] returns the formatter associated to an [out_file] *)
let fmt_of_file : cout t -> Format.formatter = fun file ->
  match file.channel with
  | Out(_,fmt) -> fmt

(** [close file] closes [file] *)
let close : type a. a t -> unit = fun file ->
  match file.channel with
  | Out (oc,_) -> close_out oc
  | In ic -> close_in ic
