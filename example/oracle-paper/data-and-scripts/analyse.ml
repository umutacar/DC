open Scanf
open Printf
open Shared


(***************************************************************)
(** Parsing the description of runs *)

type run = {
  machine : string;
  prog : string;
  size : int;
  param : int;
  proc : int;
  cutoff : int;
  optim : int;
  time : float;
  csts : (string * (float * float) list) list;
  meas : (string * (float * float) list) list; }

let parse_cst_line line =
   let words = string_split ' ' line in
   match words with
   | [] -> assert false
   | name::values -> 
      let pairs = List.filter (fun p -> p <> (0.,0.))
        (List.map (fun (x,y) -> 
        try (float_of_string x, float_of_string y) 
        with _ -> (0.,0.)) (list_pairing values)) in
      let cst,mea = List.partition (fun (x,y) -> x = -1.0) pairs in
      let cst' = list_mapi (fun i (x,y) -> (float_of_int i,y)) cst in 
      ((name, cst'), (name, mea))

let parse_runs file = 
   let rest = ref (file_get_lines file) in
   let runs = ref [] in
   while !rest <> [] do
     match !rest with
     | [] -> assert false
     | line::lines -> if line = "" then rest := lines else begin
        sscanf line " %s %s %d %d %d %d %d %d %f" 
          (fun machine prog proc size param cutoff optim nb_csts time ->
             let csts_lines, lines' = list_take_drop nb_csts lines in 
             let csts,meas = List.split (List.map parse_cst_line csts_lines) in
             let run = 
               { machine = machine;
                 prog = prog;
                 size = size;
                 param = param;
                 proc = proc;
                 cutoff = cutoff;
                 optim = optim;
                 time = time;
                 csts = csts;
                 meas = meas } in
             rest := lines';
             runs := run :: !runs)
        end
   done;
   !runs

(***************************************************************)
(** Collating data about the runs *)

(* data is a multidimensional array with coordinates :
       prog, size, proc, param, cutoff, optim 
   and each cell contains a "measures" object;
   (measures.worktime only contains information about the last run)
   *)

type measures = {
   mutable values : float list;
   mutable mean : float;
   mutable stddev : float; 
   mutable worktime : (string, (float * float) list) Imap.t;
   mutable constants : (string, (float * float) list) Imap.t }

let empty_measures () = {
   values = [];
   mean = 0.;
   stddev = 0.;
   worktime = Imap.create();
   constants = Imap.create() }

type data = (string, (string, (int, (int, (int, (int, (int, measures) Imap.t) Imap.t) Imap.t) Imap.t) Imap.t) Imap.t) Imap.t
  
let build_data runs = 
   let void = Imap.create in
   let lookup = Imap.find_or_create in
   let foreach = Imap.foreach in
   let map_machine : data = void() in
   let add_run run =
      let map_prog = lookup void run.machine map_machine in
      let map_size = lookup void run.prog map_prog in
      let map_proc = lookup void run.size map_size in
      let map_param = lookup void run.proc map_proc in
      let map_cutoff = lookup void run.param map_param in
      let map_optim = lookup void run.cutoff map_cutoff in
      let measures = lookup empty_measures run.optim map_optim in
      measures.values <- run.time :: measures.values;
      measures.worktime <- Imap.from_list (List.filter (fun (name,vals) -> vals <> []) run.meas);
      measures.constants <- Imap.from_list (List.filter (fun (name,vals) -> vals <> []) run.csts);
      in
   List.iter add_run runs;
   foreach map_machine (fun machine map_prog ->
      foreach map_prog (fun prog map_size ->
         foreach map_size (fun size map_proc ->
            foreach map_proc (fun proc map_param ->
               foreach map_param (fun param map_cutoff ->
                  foreach map_cutoff (fun cutoff map_optim ->
                     foreach map_optim (fun optim measures ->
                        let (mean,stddev) = list_mean_and_stddev measures.values in
                        measures.mean <- mean;
                        measures.stddev <- stddev)))))));
   map_machine


(***************************************************************)


let arg_mode = ref "speedup"
let arg_prog = ref []
let arg_size = ref 0
let arg_proc = ref 0
let arg_cutoff = ref 0
let arg_virtual = ref false
let arg_input = ref ""
let arg_notitle = ref false

let split_args s = 
  Str.split (Str.regexp_string ",") s
let split_int_args s = 
  List.map int_of_string (Str.split (Str.regexp_string ",") s)
let int_args_to_ref r s =
  r := split_int_args s
let string_args_to_ref r s =
  r := split_args s

let _ = 
   Arg.parse  
     [ ("-prog", Arg.String (string_args_to_ref arg_prog), "fix program (comma-separated)");
       ("-size", Arg.Set_int arg_size, "fix one size");
       ("-cutoff", Arg.Set_int arg_cutoff, "fix one cutoff");
       ("-proc", Arg.Set_int arg_proc, "fix one nb-processor");
       ("-virtual", Arg.Set arg_virtual, "only simulates");
       ("-notitle", Arg.Set arg_notitle, "no titles in plots");       
       ("-input", Arg.String (fun s -> arg_input := s), "specify input file"); ]
       (fun f -> arg_mode := f)
       ("usage: analyse.out [options] mode");
   if !arg_input = "" then arg_input := sprintf "results_%s.txt" !arg_mode;
   let _runs = parse_runs !arg_input in
   (* do whatever *)
   ()

(***************************************************************)
(** GNUplot options *)

type curve_style = StyleLines | StylePoints | StyleLinespoints
type range = float * float

type plot_options = {
   mutable logX : bool;
   mutable logY : bool;
   mutable curves : curve_style;
   mutable labelX : string;
   mutable labelY : string;
   mutable rangeX : range option;
   mutable rangeY : range option; 
   mutable zeroX : bool;
   mutable zeroY : bool;
   mutable equations : string list; }

let default_plot_options = {
   logX = false;
   logY = false;
   curves = StyleLinespoints;
   labelX = "";
   labelY = "";
   rangeX = None;
   rangeY = None;
   zeroX = true;
   zeroY = true;
   equations = []; }

let unsome_act opt f =
  match opt with
  | None -> ()
  | Some x -> f x

let plot_options ?logX ?logY ?curves ?rangeX ?rangeY ?zeroX ?zeroY ?equations ?plotDiag labelX labelY =
  let o = { default_plot_options with labelX = labelX; labelY = labelY } in
  unsome_act logX (fun x -> o.logX <- x);
  unsome_act logY (fun x -> o.logY <- x);
  unsome_act curves (fun x -> o.curves <- x);
  unsome_act rangeX (fun x -> o.rangeX <- x);
  unsome_act rangeY (fun x -> o.rangeY <- x);
  unsome_act zeroX (fun x -> o.zeroX <- x);
  unsome_act zeroY (fun x -> o.zeroY <- x);
  unsome_act equations (fun x -> o.equations <- x);
  unsome_act plotDiag (fun x -> o.equations <- "x with lines title 'linear'"::o.equations );
  o


(***************************************************************)
(** Generate curve descriptions *)

(* TODO: print the stddev as well *)

(* plot options, and a list of pairs made of
    label of each graph and list of coordinates *)

type plot = plot_options * (string, (string, (string * float) list ref) Imap.t) Imap.t

let create_curve () = 
  ref []

let no_optim = 0

let foreach = Imap.foreach 
let find = Imap.find 
let mem = Imap.mem 

let create_plot_base options handle =
   let graphs = Imap.create() in
   let add_point graph_label curve_label x y =
      let curves = Imap.find_or_create Imap.create graph_label graphs in
      let points = Imap.find_or_create create_curve curve_label curves in
      points := (x,y) :: !points;  (* todo: remove duplicates at the end *)
      in
   handle add_point;
   Imap.ksort string_cmp graphs;
   (options,graphs)

let create_bar_graph_base block_labels options handle =
   let graphs = Imap.create() in
   let add_cluster graph_label cluster_label x =
      let clusters = Imap.find_or_create Imap.create graph_label graphs in
      let blocks = Imap.find_or_create create_curve cluster_label clusters in
      blocks := x :: !blocks;  (* todo: remove duplicates at the end *)
      in
   handle add_cluster;
   Imap.ksort string_cmp graphs;
   (options,graphs,block_labels)

let create_plot map_machine options handle =
   create_plot_base options (fun add_point ->
      foreach map_machine (fun machine map_prog ->
      foreach map_prog (fun prog map_size ->
      foreach map_size (fun size map_proc ->
      foreach map_proc (fun proc map_param ->
      foreach map_param (fun param map_cutoff ->
      foreach map_cutoff (fun cutoff map_optim ->
      foreach map_optim (fun optim measures ->
         handle add_point machine prog size proc param cutoff optim measures))))))))

let use_prog prog =
  !arg_prog == [] || List.mem prog !arg_prog

let normal prog = 
  not (string_ends_with "_sq" prog || string_ends_with "_ws" prog || string_ends_with "_count" prog)

let nonseq prog = 
  not (string_ends_with "_sq" prog || string_ends_with "_count" prog)

let count prog = 
  string_ends_with "_count" prog

let pretty_machine (machine:string) =
  if machine = "srv-53-07" then "Intel" 
  else if machine = "hexi" then "AMD"
  else machine

(* todo: factorize with above *)
let create_plot_filter f (map_machine:data) options handle =
   create_plot_base options (fun add_point ->
      foreach map_machine (fun machine map_prog ->
      foreach map_prog (fun prog map_size -> if use_prog prog && f prog then
      foreach map_size (fun size map_proc -> 
      foreach map_proc (fun proc map_param ->
      foreach map_param (fun param map_cutoff ->
      foreach map_cutoff (fun cutoff map_optim ->
      foreach map_optim (fun optim measures ->
         handle add_point machine prog size proc param cutoff optim measures))))))))

(* todo: factorize with above *)
let create_bar_graph_filter f (map_machine:data) block_labels options handle =
   create_bar_graph_base block_labels options (fun add_cluster ->
      foreach map_machine (fun machine map_prog ->
      foreach map_prog (fun prog map_size -> if use_prog prog && f prog then
      foreach map_size (fun size map_proc ->
      foreach map_proc (fun proc map_param ->
      foreach map_param (fun param map_cutoff ->
      foreach map_cutoff (fun cutoff map_optim ->
      foreach map_optim (fun optim measures ->
         handle add_cluster machine prog size proc param cutoff optim measures))))))))

let tryfound f =
  try f() with Not_found -> () 

let tau_vs_prog_by_param data =
   let a = ref 0 in let next () = incr a; !a in
   create_plot_filter count data (plot_options ~rangeY:(Some(-500.,1000.)) "progparam" "taunanosec")
   (fun add_point machine prog_count size proc param cutoff optim measures ->
      let prog = String.sub prog_count 0 (String.length prog_count - String.length "_count") in
      if proc = 1 then begin 
      let graph_label = Printf.sprintf "machine=%s" machine in
      let curve_label = sprintf "%s--size=%d--param=%d" prog size param in
      tryfound (fun () ->
         let prog_sq = prog ^ "_sq" in 
         let prog_ws = prog ^ "_ws" in
         let sq_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let ws_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_ws (find machine data)))))) in
         let count_measures = measures in
         let nb_forks = count_measures.mean in
         (* let nb_forks = match prog with
            | "bintree-sum" -> (float_of_int size /. 2.) -. 1.
            | "array-sum" -> (float_of_int size /. 2.) -. 1.
            | "worst" -> float_of_int size /. float_of_int param
            | "dmm" -> float_of_int (pow2 (3*size)) 
            | _ -> raise Not_found
            in *)
         if nb_forks < 1.0 then (printf "Warning: zero count for %s--size=%d--param=%d\n" prog size param; raise Not_found);
         let x = string_of_int (next()) in
         let y = (ws_measures.mean -. sq_measures.mean) /. (nb_forks /. 1000000000.) in
         printf "%s--size=%d--param=%d sq=%f ws=%f forks=%f tau=%f\n" prog size param sq_measures.mean ws_measures.mean nb_forks y;
         add_point graph_label curve_label x y);
      end)

let time_vs_cutoff_by_param data =
   create_plot_filter normal data (plot_options ~logX:true "cutoff" "timesecond")
   (fun add_point machine prog size proc param cutoff optim measures ->
      let graph_label = Printf.sprintf "%s--size=%d--optim=%d--proc=%d--machine=%s" prog size optim proc machine in
      let curve_label = sprintf "-param%d" param in
      let x = string_of_int cutoff in
      let y = measures.mean in
      add_point graph_label curve_label x y;
      tryfound (fun () ->
         let prog_sq = prog ^ "_sq" in 
         let sq_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let curve_label = sprintf "SQ" in
         let y = sq_measures.mean in
         add_point graph_label curve_label x y);
      tryfound (fun () ->
         let prog_ws = prog ^ "_ws" in
         let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
         let curve_label = sprintf "WS" in
         let y = ws_measures.mean in
         add_point graph_label curve_label x y);
      )

let time_vs_param_by_cutoff data = 
   create_plot_filter normal data (plot_options ~logX:true "param" "timesecond")
   (fun add_point machine prog size proc param cutoff optim measures ->
      let graph_label = Printf.sprintf "%s--size=%d--optim=%d--proc=%d--machine=%s" prog size optim proc machine in
      let curve_label = sprintf "-cutoff%d" cutoff in
      let x = string_of_int param in
      let y = measures.mean in
      add_point graph_label curve_label x y;
      tryfound (fun () ->
         let prog_sq = prog ^ "_sq" in 
         let sq_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let curve_label = sprintf "SQ" in
         let y = sq_measures.mean in
         add_point graph_label curve_label x y);
      tryfound (fun () ->
         let prog_ws = prog ^ "_ws" in
         let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
         let curve_label = sprintf "WS" in
         let y = ws_measures.mean in
         add_point graph_label curve_label x y);
      )

let time_vs_cutoff_by_proc data =
   create_plot_filter normal data (plot_options ~logX:true "cutoff" "timesecond")
   (fun add_point machine prog size proc param cutoff optim measures ->
      let graph_label = Printf.sprintf "%s--size=%d--param=%d--optim=%d--machine=%s" prog size param optim machine in
      let curve_label = sprintf "-p%d" proc in
      let x = string_of_int cutoff in
      let y = measures.mean in
      add_point graph_label curve_label x y;
      tryfound (fun () ->
         let prog_sq = prog ^ "_sq" in 
         let sq_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let curve_label = sprintf "SQ" in
         let y = sq_measures.mean in
         add_point graph_label curve_label x y);
      tryfound (fun () ->
         let prog_ws = prog ^ "_ws" in
         let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
         let curve_label = sprintf "WS-p%d" proc in
         let y = ws_measures.mean in
         add_point graph_label curve_label x y);
      )
 
let time_vs_size_by_proc data =
   create_plot_filter normal data (plot_options ~logX:true ~logY:true "size" "timesecond")
   (fun add_point machine prog size proc param cutoff optim measures ->
      let graph_label = Printf.sprintf "%s--param=%d--cutoff=%d--optim=%d--machine=%s" prog param cutoff optim machine in
      let curve_label = sprintf "-p%d" proc in
      let x = string_of_int size in
      let y = measures.mean in
      add_point graph_label curve_label x y;
      tryfound (fun () ->
         let prog_sq = prog ^ "_sq" in
         let sq_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let curve_label = sprintf "SQ" in
         let y = sq_measures.mean in
         add_point graph_label curve_label x y);
      tryfound (fun () ->
         let prog_ws = prog ^ "_ws" in
         let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
         let curve_label = sprintf "WS-p%d" proc in
         let y = ws_measures.mean in
         add_point graph_label curve_label x y);
      )

let efficiency_vs_cutoff_by_proc_size data =
   create_plot_filter nonseq data (plot_options ~logX:true "cutoff" "efficiency")
   (fun add_point machine prog size proc param cutoff optim measures ->
      tryfound (fun () ->
         let prog_sq = prog ^ "_sq" in
         let ref_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let efficiency = ref_measures.mean /. measures.mean /. (float_of_int proc) in
         let graph_label = Printf.sprintf "%s--param=%d--optim=%d--machine=%s" prog param optim machine in
         let curve_label = Printf.sprintf "p%d--size=%d" proc size in
         let x = string_of_int cutoff in
         let y = efficiency in
         add_point graph_label curve_label x y);
      )

let improvews_vs_cutoff_by_proc_size data =
   create_plot_filter normal data (plot_options ~logX:true "cutoff" "improvews")
   (fun add_point machine prog size proc param cutoff optim measures ->
      tryfound (fun () ->
         let prog_ws = prog ^ "_ws" in
         let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
         let improve = ws_measures.mean /. measures.mean in
         let graph_label = Printf.sprintf "%s--param=%d--optim=%d--machine=%s" prog param optim machine in
         let curve_label = Printf.sprintf "p%d--size=%d" proc size in
         let x = string_of_int cutoff in
         let y = improve in
         add_point graph_label curve_label x y);
      )

let improvews_vs_proc_by_cutoff_machine_optim data =
   create_plot_filter nonseq data (plot_options "proc" "improvews")
   (fun add_point machine prog size proc param cutoff optim measures ->
      tryfound (fun () ->
         let prog_ws = prog ^ "_ws" in
         let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
         let improve = ws_measures.mean /. measures.mean in
         let graph_label = Printf.sprintf "%s--size=%d--param=%d" prog size param in
         let curve_label = Printf.sprintf "cutoff=%d--machine=%s--optim=%d" cutoff machine optim in
         let x = string_of_int proc in
         let y = improve in
         add_point graph_label curve_label x y);
      )


let improvews_vs_wsslowdownsq_by_proc data =
   create_plot_filter nonseq data (plot_options ~curves:StylePoints ~plotDiag:true ~equations:["1 with lines"] "wsslowdownsq" "improvews")
   (fun add_point machine prog size proc param cutoff optim measures ->
      tryfound (fun () ->
         let prog_ws = prog ^ "_ws" in
         let prog_sq = prog ^ "_sq" in
         let sq_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let ws1_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_ws (find machine data)))))) in
         let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
         let improvews = ws_measures.mean /. measures.mean in
         let wsslowdownsq = ws1_measures.mean /. sq_measures.mean in
         let graph_label = Printf.sprintf "cutoff=%d--machine=%s--optim=%d--p%d" cutoff machine optim proc in
         let curve_label = Printf.sprintf "Measures" in
         let x = string_of_float wsslowdownsq in
         let y = improvews in
         add_point graph_label curve_label x y);
      )

let efficiency_vs_proc_by_cutoff_machine_optim data =
   create_plot_filter nonseq data (plot_options "proc" "efficiency")
   (fun add_point machine prog size proc param cutoff optim measures ->
      tryfound (fun () ->
         let prog_sq = prog ^ "_sq" in
         let ref_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let efficiency = ref_measures.mean /. measures.mean /. (float_of_int proc) in
         let graph_label = Printf.sprintf "%s--size=%d--param=%d" prog size param in
         let curve_label = Printf.sprintf "cutoff=%d--machine=%s--optim=%d" cutoff machine optim in
         let x = string_of_int proc in
         let y = efficiency in
         add_point graph_label curve_label x y);
      )

let speedup_vs_proc data =
   create_plot_filter normal data (plot_options ~plotDiag:true "processors" "speedup")
   (fun add_point machine prog size proc param cutoff optim measures ->
      let graph_label = Printf.sprintf "%s--size=%d--param=%d--optim=%d--machine=%s" prog size param optim machine in
      let x = string_of_int proc in
      let prog_ws = prog ^ "_ws" in
      let prog_sq = prog ^ "_sq" in
      tryfound (fun () ->
         let ref_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let ws1_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_ws (find machine data)))))) in
         let _ =    
            let curve_label = Printf.sprintf "oracle" in
            let speedup = ref_measures.mean /. measures.mean in
            let y = speedup in
            add_point graph_label curve_label x y
            in
         tryfound (fun () ->
            let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
            let curve_label = Printf.sprintf "ws" in
            let speedup = ref_measures.mean /. ws_measures.mean in
            let y = speedup in
            add_point graph_label curve_label x y);
         tryfound (fun () ->
            let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
            let curve_label = Printf.sprintf "ws-ideal" in
            let speedup = ws1_measures.mean /. ws_measures.mean in
            let y = speedup in
            (* add_point graph_label curve_label x y; *) () ); 
          )
      )

(* generate a graph with speedup curves for all benchmarks *)
let speedup_vs_proc_all data =
   create_plot_filter normal data (plot_options ~plotDiag:true "processors" "speedup")
    (fun add_point machine prog size proc param cutoff optim measures -> 
      let graph_label = Printf.sprintf "param=%d--optim=%d--machine=%s" param optim machine in
      let x = string_of_int proc in
      let prog_sq = prog ^ "_sq" in
      tryfound (fun () ->
  	 let ref_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let _ =    
            let curve_label = Printf.sprintf "%s" prog in
            let speedup = ref_measures.mean /. measures.mean in
            let y = speedup in
            add_point graph_label curve_label x y in
	 ())
    )

let speedup_vs_proc_by_prog_size_param data =
   create_plot_filter normal data (plot_options ~plotDiag:true "processors" "speedup")
   (fun add_point machine prog size proc param cutoff optim measures ->
      let graph_label = Printf.sprintf "cutoff=%d--optim=%d--machine=%s" cutoff optim machine in
      let x = string_of_int proc in
      let prog_ws = prog ^ "_ws" in
      let prog_sq = prog ^ "_sq" in
      tryfound (fun () ->
         let ref_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
         let _ =    
            let curve_label = Printf.sprintf "%s--size=%d--param=%d" prog size param in
            let speedup = ref_measures.mean /. measures.mean in
            let y = speedup in
            add_point graph_label curve_label x y
            in
         tryfound (fun () ->
            let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
            let curve_label = Printf.sprintf "WS--%s--size=%d--param=%d" prog size param in
            let speedup = ref_measures.mean /. ws_measures.mean in
            let y = speedup in
            add_point graph_label curve_label x y)
          );
      )

let optimizing_vs_cutoff_by_optim data =
   create_plot data (plot_options ~logX:true "cutoff" "optimspeedup")
   (fun add_point machine prog size proc param cutoff optim measures ->
      let d = find param (find cutoff (find proc (find size (find prog (find machine data))))) in
      if mem no_optim d then begin 
         let ref_measures = find no_optim d in
         let speedup = measures.mean /. ref_measures.mean in
         let graph_label = Printf.sprintf "%s--size=%d--param=%d--proc=%d--machine=%s" prog size param proc machine in
         let curve_label = Printf.sprintf "optim_%d" optim in
         let x = string_of_int cutoff in
         let y = speedup in
         add_point graph_label curve_label x y
      end)

let time_vs_work_by_cstname data =
   create_plot data (plot_options ~curves:StylePoints ~rangeY:(Some(0.,40.)) "work" "timemicrosec")
   (fun add_point machine prog size proc param cutoff optim measures ->
      foreach measures.worktime (fun cst_name worktime ->
         (* printf "params -prog %s -size %d -proc %d -cutoff %d -optim %d -cst_name %s -nbvalues\n" prog size proc cutoff optim cst_name; *)
         let n = List.length worktime in
         let max_n = 50 in
         let worktime' = (* pick max_n values at regular intervals in the worktime list *)
            if n < max_n 
              then worktime
              else list_filter_every (n / max_n) worktime in
         list_foreach worktime' (fun (work,time) ->
            let graph_label = Printf.sprintf "%s--proc=%d--size=%d--param=%d--cutoff=%d--machine=%s" prog proc size param cutoff machine in
            let curve_label = Printf.sprintf "cst=%s" cst_name in
            let x = string_of_float work in
            let y = time in
            add_point graph_label curve_label x y)))

let constant_vs_iter_by_cstname data =
   create_plot data (plot_options "measures" "constant")
   (fun add_point machine prog size proc param cutoff optim measures ->
      foreach measures.constants (fun cst_name cst_values ->
         let n = List.length cst_values in
         let max_n = 50 in
         let cst_values = (* pick max_n first values out of the cst_values list *)
            if n < max_n 
              then cst_values 
              else take max_n cst_values in
         list_foreach cst_values (fun (step,cst) ->
            let graph_label = Printf.sprintf "%s--proc=%d--size=%d--param=%d--cutoff=%d--machine=%s" prog proc size param cutoff machine in
            let curve_label = Printf.sprintf "cst=%s" cst_name in
            let x = string_of_float step in
            let y = cst in
            add_point graph_label curve_label x y)))

let compare_one_proc_times_all data =
   create_bar_graph_filter normal data ["sequential"; "oracle"; "work-stealing"] (plot_options "" "execution time (normalized)")
    (fun add_cluster machine prog size proc param cutoff optim measures -> 
      if proc = 1 then
      let graph_label = sprintf "machine=%s--proc=%d" machine proc in
      let prog_sq = prog ^ "_sq" in
      let prog_ws = prog ^ "_ws" in
      tryfound (fun () ->
  	 let sq_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
  	 let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
         let _ =    
           let cluster_label = prog in
           let oracle_time = measures.mean /. sq_measures.mean in
	   let ws_time = ws_measures.mean /. sq_measures.mean in
	   let sq_time = 1.0 in
           add_cluster graph_label cluster_label [sq_time; oracle_time; ws_time] in
	 ())
    )

let compare_one_proc_times_all_summary data =
   create_bar_graph_filter normal data ["sequential"; "oracle"; "work-stealing"] (plot_options "" "execution time (normalized)")
    (fun add_cluster machine prog size proc param cutoff optim measures -> 
      if proc = 1 then
      let graph_label = sprintf "machine=%s--proc=%d" machine proc in
      let prog_sq = prog ^ "_sq" in
      let prog_ws = prog ^ "_ws" in
      tryfound (fun () ->
  	 let sq_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
  	 let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
         let _ =    
           let cluster_label = prog in
           let oracle_time = measures.mean  in
	   let ws_time = ws_measures.mean  in
	   let sq_time = sq_measures.mean in
           add_cluster graph_label cluster_label [sq_time; oracle_time; ws_time] in
	 ())
    )

let compare_sixteen_proc_times_all data =
   create_bar_graph_filter normal data ["work-stealing"; "oracle"(*; "work-stealing-ideal" *)] (plot_options "" "speedup")
    (fun add_cluster machine prog size proc param cutoff optim measures -> 
      if proc = 16 then
      let graph_label = sprintf "machine=%s--proc=%d" machine proc in
      let prog_sq = prog ^ "_sq" in
      let prog_ws = prog ^ "_ws" in
      tryfound (fun () ->
  	 let sq_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_sq (find machine data)))))) in
  	 let ws1_measures = find no_optim (find 0 (find param (find 1 (find size (find prog_ws (find machine data)))))) in
  	 let ws_measures = find no_optim (find 0 (find param (find proc (find size (find prog_ws (find machine data)))))) in
         let _ =    
           let cluster_label = prog in
	   let oracle_speedup = sq_measures.mean /. measures.mean in
	   let ws_speedup = sq_measures.mean /. ws_measures.mean in
	   let ideal_speedup = ws1_measures.mean /. ws_measures.mean in
           add_cluster graph_label cluster_label [ws_speedup; oracle_speedup (*; ideal_speedup*)] in
	 ())
    )

(***************************************************************)
(** Generate gnuplot data *)

(* let _ = printf "outputting file %s\n" fname in *)

let gnuplot_curve filename points =
   let spoints = List.map (fun (x,y) -> sprintf "%s\t%f\n" x y) points in
   let content = String.concat "" spoints in
   file_put_contents filename content

let gnuplot_header options label title =
  let lines =
      [ (* "set terminal png nocrop enhanced";*)
        (*"set terminal pdf color enhanced size 5cm,5cm";*)
         "set terminal postscript eps enhanced color solid rounded \"Helvetica\" 17";
         sprintf "set output '%s.eps'" label;
         "set size 0.55,0.55";
         "set pointsize 2";
         if not !arg_notitle then sprintf "set title '%s'" title else "";
         (* command below sets the legend to top left of plot *)
         (* sprintf "set key left top"; *)  ]
      @ (if options.logX then ["set log x"] else [])
      @ (if options.logY then ["set log y"] else [])
      @ (match options.rangeX with
         | Some (a,b) -> [sprintf "set xrange [%f:%f]" a b]
         | None -> if not options.logX && options.zeroX then ["set xrange [0:]"] else [])
      @ (match options.rangeY with
         | Some (a,b) -> [sprintf "set yrange [%f:%f]" a b]
         | None -> if not options.logY && options.zeroY then ["set yrange [0:]"] else [])
      @ [ "set key top left" ]
      @ [ sprintf "set xlabel '%s'" options.labelX;
          sprintf "set ylabel '%s'" options.labelY; ]
      in
   String.concat "\n" lines

let all_plots = ref []

let gnuplot_plot (options, graphs) =
  (* let graphs = Imap.filter (fun _ curves -> not (Imap.is_empty curves)) graphs in *)
  Imap.foreach graphs (fun graph_label curves ->
    let label = sprintf "%s--vs--%s--for--%s" options.labelY options.labelX graph_label in
(* fixme *)
    let [prog;size;param;optim;machine;] = Str.split (Str.regexp_string "--") graph_label in
    let [_;machine] = Str.split (Str.regexp_string "=") machine in
    let title = sprintf "%s (%s)" prog (pretty_machine machine) in
    let plot_files = 
       List.map (fun (curve_label,points) ->
             let style =  
               match options.curves with
               | StyleLines -> " with lines " 
               | StyleLinespoints -> " with linespoints linewidth 3 " 
               | StylePoints -> ""
               in
             let cfilename = sprintf "%s--%s.dat" label curve_label in
             let content = sprintf "'%s' %s title '%s'" cfilename style curve_label in
             gnuplot_curve cfilename !points;
             content)
          (Imap.to_list curves) in
    let content = String.concat "" 
      [ gnuplot_header options label title;
        "\n\nplot ";
        String.concat ", " plot_files;
        (if options.equations <> [] then ", " else "");
        String.concat ", " options.equations;
        "\n"; ] in
    let filename = sprintf "%s.gnuplot" label in
    file_put_contents filename content;
    add_to_list_ref all_plots label)

let bargraph_plot (options, graphs, block_labels) =
  Imap.foreach graphs (fun graph_label clusters ->
    let label = sprintf "%s--for--%s" (String.concat "-vs-" block_labels) graph_label in
    let data = ref [] in
    let _ = 
       List.iter (fun (cluster_label,blocks) ->
	 List.iter (fun (cols:float list ) -> 
	   let cols_str = String.concat " " (List.map string_of_float cols) in
	   data := sprintf "%s %s" cluster_label cols_str :: !data)
	   !blocks)
        (Imap.to_list clusters) in
    let filename = sprintf "%s.perf" label in
    let instrs = 
      [ "=cluster;"^(String.concat ";" block_labels);
	"colors=black,yellow,red";
	"=table";
	"yformat=%4.2f";
	"=norotate";
	"fontsz=14";
	sprintf "ylabel=%s" options.labelY;
	"";
      ]	in
    file_put_contents filename (String.concat "\n" (List.append instrs !data));
    unix_command (sprintf "./bargraph.pl -pdf %s > %s.pdf" filename label))

let bargraph_summarize (options, graphs, block_labels) =
  Imap.foreach graphs (fun graph_label clusters ->
    let label = sprintf "%s--for--%s" (String.concat "-vs-" block_labels) graph_label in
    let data  = ref ([]:float list list) in
    let _ = 
       List.iter (fun (cluster_label,blocks) ->
	 List.iter (fun (cols:float list) -> 
	   data := cols :: !data)
	   !blocks)
        (Imap.to_list clusters) in
    let filename = sprintf "%s.summary" label in
    let min_diff = list_min (List.map (fun xs -> abs_float (List.hd xs -. list_last xs) /. list_max xs) !data) in 
    let max_diff = list_max (List.map (fun xs -> abs_float (List.hd xs -. list_last xs) /. list_max xs) !data) in 
    let times = List.map (fun xs -> String.concat " " (List.map string_of_float xs)) !data in
    file_put_contents filename (String.concat "\n" ([ "min_diff(%) "^string_of_float min_diff^" "; 
						     "max_diff(%) "^string_of_float max_diff^" "] @ times)
				);
      ())


(***************************************************************)
(** Select *)

let compile_plots plots =
   list_foreach plots (fun f ->
      unix_command (sprintf "gnuplot %s.gnuplot" f);
      unix_command (sprintf "epstopdf %s.eps" f))

let generate_pdf plots =
   let lines = List.map (fun f -> "\\myfig{" ^ f ^ ".pdf}\n") plots in
   file_put_contents "list.tex" (String.concat "" lines);
   unix_command "pdflatex -interaction=batchmode plots.tex > null"

let _ =  
   let runs = parse_runs !arg_input in
   let data = build_data runs in
   let action func =
      let plot = func data in
      gnuplot_plot plot
      in
   begin match !arg_mode with
   | "all" ->
      (*
      action time_vs_cutoff_by_proc;
      action efficiency_vs_cutoff_by_proc_size;
      action speedup_vs_proc;
      action efficiency_vs_proc_by_prog_size_param;
      *) ()
   | "tau" ->
      action tau_vs_prog_by_param; 
   | "cutoff" -> 
      action time_vs_param_by_cutoff; 
      action time_vs_cutoff_by_param;   
      (*action time_vs_cutoff_by_proc;*)
      action efficiency_vs_cutoff_by_proc_size;
      action improvews_vs_cutoff_by_proc_size; 
   | "speedup" ->
      bargraph_plot (compare_one_proc_times_all data);
      bargraph_plot (compare_sixteen_proc_times_all data);
      bargraph_summarize (compare_one_proc_times_all_summary data);
      bargraph_summarize (compare_sixteen_proc_times_all data);
(*      action improvews_vs_proc_by_cutoff_machine_optim;
      action efficiency_vs_proc_by_cutoff_machine_optim; 
      action improvews_vs_wsslowdownsq_by_proc;
*)
      action speedup_vs_proc;
(*      action speedup_vs_proc_all;*)
     ()
      (*action time_vs_size_by_proc;*)
   | "optim" ->
      action optimizing_vs_cutoff_by_optim;
   | "cst" ->
      action time_vs_work_by_cstname;
      action constant_vs_iter_by_cstname;
   | "size" ->
      (* action time_vs_size_by_proc *)
      printf "should be implemented\n"
   | _ -> 
      failwith "unknown mode"
   end;  
   let plots = (*List.rev*) !all_plots in
   compile_plots plots;
   generate_pdf plots


(* todo: generate plots in subdirectory *)
