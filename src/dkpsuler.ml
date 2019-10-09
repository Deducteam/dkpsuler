module B  = Kernel.Basic
module Errors = Api.Errors
module F  = Files
module TC = Api.Processor.TypeChecker
module P  = Parsers.Parser
module S = Kernel.Signature
(* TODO:
   -name of rules are not supported
   -instantiation is supported only for variables
*)

type info =
  {
    mpat:Kernel.Rule.pattern;
    name:B.name;
    rule:Kernel.Rule.untyped_rule;
  }

let config : (B.name, info list) Hashtbl.t = Hashtbl.create 11

let meta = ref None

let normalize entry =
  let meta = match !meta with None -> assert false | Some meta -> meta in
  let env = Api.Env.init (P.input_from_string (B.mk_mident "") "") in
  S.fail_on_symbol_not_found := false;
  let e = Dkmeta.mk_entry env meta entry in
  S.fail_on_symbol_not_found := true;
  e

let mk_config : Kernel.Rule.untyped_rule list -> unit = fun rs ->
  let add cst info =
    if Hashtbl.mem config cst then
      let infos = Hashtbl.find config cst in
      Hashtbl.replace config cst (info::infos)
    else
      Hashtbl.add config cst [info]
  in
  let mk_rule : Kernel.Rule.untyped_rule -> unit = fun r ->
    let cst =
      match r.pat with
      | Kernel.Rule.Pattern(_,cst,_) -> cst
      | _ -> assert false
    in
    let name =
      match r.rhs with
      | Kernel.Term.Const(_,cst) -> cst
      | _ -> failwith "RHS should be a constant"
    in
    add cst {mpat=r.pat;name;rule=r}
  in
  List.iter mk_rule rs

let handle_config in_path =
  let in_file = F.get_out_path in_path `Input in
  let input = P.input_from_file in_file in
  let entries = P.parse input in
  let mk_entry = function
    | Parsers.Entry.Rules(_,rs) -> mk_config rs
    | _ -> assert false
  in
  List.iter mk_entry entries;
  let rules = Hashtbl.fold (fun _ info rules ->
      (List.map (fun v -> v.rule) info)@rules) config [] in
  S.fail_on_symbol_not_found := false;
  meta := Some (Dkmeta.meta_of_rules rules Dkmeta.default_config);
  S.fail_on_symbol_not_found := true

let mk_decl env lc st info =
  let ty = Api.Env.infer env (Kernel.Rule.pattern_to_term info.mpat) in
  let e = Parsers.Entry.Decl(lc,B.id info.name, st, ty) in
  TC.handle_entry env e;
  e

let mk_def env lc opaque ty_opt te info =
  let ty' =
    match ty_opt with
    | None -> None
    | Some _ -> Some (Api.Env.infer env (Kernel.Rule.pattern_to_term info.mpat))
  in
  let app =
    match info.mpat with
    | Kernel.Rule.Pattern(_,_,ps) -> List.map Kernel.Rule.pattern_to_term ps
    | _ -> assert false
  in
  let te' = Kernel.Term.mk_App2 te app in
  let e' = Parsers.Entry.Def(lc,B.id info.name, opaque, ty', te') in
  TC.handle_entry env e';
  e'

let rec extract acc l n =
  if n = 0 then
    List.rev acc,l
  else match l with
    | [] -> assert false
    | Kernel.Rule.Var(_,id,_,[])::ps -> extract (id::acc) ps (n-1)
    | _ -> failwith "Not supported"

let rec mk_rhs subst =
  let module Term = Kernel.Term in
  function
  | Term.DB(_,id,_) when List.mem_assoc id subst -> List.assoc id subst
  | Term.App(f,a,args) -> Term.mk_App2 (mk_rhs subst f) (List.map (mk_rhs subst) (a::args))
  | Term.Lam(lc,id,None,te) -> Term.mk_Lam lc id None (mk_rhs subst te)
  | Term.Lam(lc,id,Some ty,te) -> Term.mk_Lam lc id (Some (mk_rhs subst ty)) (mk_rhs subst te)
  | Term.Pi(lc,id,tya,tyb) -> Term.mk_Pi lc id (mk_rhs subst tya) (mk_rhs subst tyb)
  | _ as t -> t

let mk_rules env lc rs info =
  let open Kernel.Rule in
  let mk_rule r =
    let n = match info.mpat with | Pattern(_,_,l) -> List.length l | _ -> assert false in
    let vars,pat =
      match r.pat with
      | Pattern(_,_,l) ->
        extract [] l n
      | _ -> assert false
    in
    let subst = match info.mpat with
      | Pattern(_,_,l) -> List.combine vars (List.map pattern_to_term l)
      | _ -> assert false in
    let rhs = mk_rhs subst r.rhs in
    let ctx = List.filter (fun (_,id) -> not @@ List.mem id vars) r.ctx in
    {name=r.name;pat = Pattern(B.dloc,info.name,pat);ctx;rhs}
  in
  let e = Parsers.Entry.Rules(lc, List.map mk_rule rs) in
  TC.handle_entry env e;
  e

module Pulser =
struct
  type t = Parsers.Entry.entry list

  let entries = ref []

  let handle_entry = fun env entry ->
    let open Parsers.Entry in
    let entry = normalize entry in
    TC.handle_entry env entry;
    let md = Api.Env.get_name env in
    match entry with
    | Decl(lc,id,st,_) ->
      let cst = B.mk_name md id in
      if Hashtbl.mem config cst then
        entries := (List.map (mk_decl env lc st) (Hashtbl.find config cst))::!entries
      else
        entries := [entry]::!entries
    | Def(lc,id,opaque,ty_opt,te) ->
      let cst = B.mk_name md id in
      if Hashtbl.mem config cst then
        entries := (List.map (mk_def env lc opaque ty_opt te) (Hashtbl.find config cst))::!entries
      else
        entries := [entry]::!entries
    | Rules(lc,rs) ->
      begin
        let cst =
          match rs with
          | r::_ ->
            begin
              match r.pat with | Pattern(_,n,_) -> n | _ -> assert false
            end
          | _ -> assert false
        in
        if Hashtbl.mem config cst then
          entries := (List.map (mk_rules env lc rs) (Hashtbl.find config cst))::!entries
        else
          entries := [entry]::!entries
      end
    | _ ->
      entries := [entry]::!entries

  let get_data () =
    List.rev (List.concat !entries)
end

let cmd_options =
  [ ( "-o"
    , Arg.String (fun s -> F.mk_dir (F.output_directory) s; Api.Files.add_path s)
    , " (MANDATORY) Set the output directory" )
  ; ( "-l"
    , Arg.Unit (fun () -> ())
    , " Debug flag" )
  ; ( "--config"
    , Arg.String (fun s -> handle_config s)
    , " (MANDATORY) Set the configuration file" )
  ; ( "-I"
    , Arg.String Api.Files.add_path
    , " DIR Add the directory DIR to the load path" )
  ]

let run_on_file in_path =
  let in_file = F.get_out_path in_path `Input in
  let entries = Api.Processor.handle_files [in_file] (module Pulser) in
  let out_file = F.out_from_string in_path `Output in
  let out_fmt = F.fmt_of_file out_file in
  List.iter (Api.Pp.Default.print_entry out_fmt) entries;
  F.close out_file

let _ =
  let options = Arg.align cmd_options in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]... \n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  List.iter run_on_file files
