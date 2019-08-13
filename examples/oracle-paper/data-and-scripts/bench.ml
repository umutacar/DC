open Scanf
open Printf
open Shared


let median prog_sizes =
   List.map (fun (n,ss,cs,ps) -> (n, [list_median ss], cs,ps)) prog_sizes

let biggest prog_sizes =
   List.map (fun (n,ss,cs,ps) -> (n, [list_max ss], cs,ps)) prog_sizes

let remove_csts prog_sizes =
  List.map (fun (n,ss,cs,ps) -> (n,ss,[],ps)) prog_sizes


(***************************************************************)
(** Description of runs parameters *)

type params = {
   mutable machine : string list; (* empty list means ["local"] *)
   mutable cutoff : int list;
   mutable proc : int list;
   mutable prog : (string * int list * string list * int list) list;
   mutable tries : int; 
   mutable optim : int list }

let prog_sizes =
   [ "array-sum", [ 12000000 ; 30000000 ], ["sum"], []; (* "sum2"*)
     "worst", [ 2000000 ], [ "worst"; "task" ], [ 1; 10 ; 50; 100; 1000 ]; 
     "tree", [ 5000000; 25000000 ], [ "sum" ], []; 
     "tree-nlgn", [ 5000000; 25000000 ], [ "sum" ], []; 
     "tree-nsqu", [ 5000000; 25000000 ], [ "sum" ], []; 
     "barnes-hut", [ 10000; 100000 ], [], [];
     "quickhull", [ 1000000; 3000000 ], [ "filter"; "quickhull'" ], []; 
     "quicksort", [ 1000000; 2000000 ], [ "select_less"; "select_equal"; "select_greater"; "quicksort_rec"; "balance" ], []; 
     "quicksort-n", [ 1000000; 20000000 ], [ "select_less"; "select_equal"; "select_greater"; "quicksort_rec"; "balance" ], []; 
     "bintree-sum", [ 3000000; 10000000 ], [ ], [ ];
     "dmm", [ 8; 9; ], ["dn"; "dn_1"; "up" ], [];
     "dmm-n2", [ 8; 9 ], ["dn"; "dn_1"; "up" ], [];
     "dmm-n4", [ 8; 9 ], ["dn"; "dn_1"; "up" ], [];
     "smvm", [ 3000000; 50000000 ], ["par_array_sum"; "tos"; "tos1"; "segmented_sums_rec"; "aux"; "aux1"], [];  
   ]

let paper_benchmarks = [ "array-sum"; "barnes-hut"; "quicksort"; "quickhull"; "dmm"; "smvm" ]


     (* "tau", [ 10000000 ], [ "task" ], [ 3; 10; 50; 100; 1000; 10000; 100000; 1000000 ];
     "tautree", [ 10000000 ], [ "diamond1";"diamond8" ], [ 3; 10; 50; 100; 1000; 10000; 100000; 1000000 ];
     "tau-mem", [ 3000000 ], [ ], [ 1; 10; 100; 1000; 10000; 100000; 1000000 ]; *)


let remove prog_name progs =
  List.filter (fun (n,_,_,_) -> n <> prog_name) prog_sizes

let all_params =
  { machine = [ "srv-53-07"; (*"hexi.cs.uchicago.edu"*) ];
    cutoff = [ 8; 10; 16; 20 ]; (* ; 1000; 5000; 10000  *)
    proc = [1; 2; 4; 8; 16; 24; 32];
    prog = remove_csts (remove "worst" prog_sizes);
    tries = 5;
    optim = [0]; }


let fast_params = { all_params with
    cutoff = [ 12 ];
    proc = [1; 4; 16; 32];
    tries = 1; }

let speedup_params = { all_params with
    cutoff = [ 12; 50 ];
    tries = 1; } (* should be more *)

let fast_speedup_params = { speedup_params with 
    cutoff = [ 12 ];
    tries = 1; }

let cutoff_params = { all_params with
    cutoff = [ 5; 12; 20; 50 ];
    proc = [ 1; 2; 8; 16; 32 ];
    tries = 1; }

let optim_params = { fast_params with
    proc = cutoff_params.proc;
    tries = 5; }

let tau_params = { fast_params with
    cutoff = [0];
    proc = [1];
    prog = remove_csts prog_sizes;
      (* List.filter (fun (n,_,_,_) -> List.mem n ["bintree-sum"; "worst"; "dmm"; "array-sum"])*)
    tries = 1; }

let cst_params = { fast_params with
    proc = cutoff_params.proc;
    prog = remove "worst" prog_sizes;
    tries = 1; }

let size_params = 
  { machine = [ "srv-53-07"; (*"hexi.cs.uchicago.edu"*) ];
    cutoff = [ 16 ]; (* ; 1000; 5000; 10000  *)
    proc = [16];
    prog = ["quickhull", [ 10000; 100000; 1000000; 2000000; 4000000; 8000000 ], [], []]; 
    tries = 3;
    optim = [0]; }

let choices_params = 
  [ "all", all_params;
    "optim", optim_params;
    "cutoff", cutoff_params;
    "fast", fast_params; 
    "cst", cst_params;
    "tau", tau_params;
    "speedup", speedup_params; 
    "fastspeedup", fast_speedup_params;
    "size", size_params ]

let arg_mode = ref "speedup"
let arg_prog = ref []
let arg_size = ref []
let arg_proc = ref []
let arg_cutoff = ref []
let arg_param = ref []
let arg_optim = ref []
let arg_tries = ref (-1)
let arg_timeout = ref 250
let arg_virtual = ref false
let arg_big = ref false
let arg_output = ref ""
let arg_append = ref false
let arg_machine = ref []
let arg_localhostname = ref ""
let arg_dir = ref (Sys.getcwd ())
let arg_paper_benchmarks = ref false


(***************************************************************)
(** Running the benchmarks *)

let exec_path = "../examples/"

let failed = ref [] 

let get_hostname machine = 
  let machine = if machine = "local" then Unix.gethostname () else machine in
  List.hd (Str.split (Str.regexp_string ".") machine)

(*/scripts*)
let remote_exec machine cmd result_file = 
  if machine = "local" then cmd else if !arg_localhostname = "" then
  sprintf "ssh contact.mpi-sws.org \"ssh %s \\\"cd %s; %s\\\"\"" machine !arg_dir cmd
  else
    begin
      printf "ssh contact.mpi-sws.org \"ssh %s \\\"cd %s; %s; scp %s %s\\\"\"\n" machine !arg_dir cmd result_file (!arg_localhostname^":"^Sys.getcwd ());
      sprintf "ssh contact.mpi-sws.org \"ssh %s \\\"cd %s; %s; scp %s %s\\\"\"" machine !arg_dir cmd result_file (!arg_localhostname^":"^Sys.getcwd ())
    end

let run machine channel prog size param csts cutoff proc optim  =
  let tmp_file = "output_tmp.txt" in
  let machine_name = get_hostname machine (*if machine <> "local" then machine else
      (ignore (Unix.system ("hostname > " ^ tmp_file));
       List.hd (file_get_lines tmp_file))*) in
  let result_file = sprintf "%s/output_%s.txt" !arg_dir machine_name in
  (* let scsts = String.concat "@" ("csts"::csts) in*)
  let nb_csts = List.length csts in
(*
  let cmd = remote_exec machine (sprintf "./gather-data.sh -t %d -e %s%s.out -p %d -s %d -m %d -c %d -o %d -k %d -f %s"
			      !arg_timeout exec_path prog proc size param cutoff optim (if csts = [] then 0 else 1) result_file) result_file in
*)
  let exe = sprintf "%s%s.out" exec_path prog in
  let exe_args = sprintf "-p %d -size %d -oracle-kappa %d -parallel-log-file %s" proc size cutoff result_file in
  let cmd_str = sprintf "./timeout %d %s %s" !arg_timeout exe exe_args in
  let cmd = remote_exec machine cmd_str result_file in
  printf "%s" cmd; print_newline();
  if not !arg_virtual then begin
     let c = Unix.system cmd in
     if c <> Unix.WEXITED 0 then begin
        printf "Warning: failure on executing on %s\n %s\n" machine cmd; 
        failed := cmd::!failed;
     end else begin
        try 
        ignore (Unix.system "sleep 1");
        let lines = file_get_lines result_file in
        match lines with 
        | [] -> (*printf "Warning: no output file produced on %s\n %s" machine cmd*) raise End_of_file
        | stime::csts_details ->
           let time = sscanf stime " %f" (fun x -> x) in
           let line = sprintf "%s %s %d %d %d %d %d %d %f\n" machine_name prog proc size param cutoff optim nb_csts time in
           fprintf channel "%s" line;
           flush channel;
           printf "%s" line;
           if csts <> []  
              then list_foreach csts_details (fun line -> 
                 let name = sscanf line " %s" (fun x -> x) in
                 if List.mem name csts then fprintf channel "%s\n" line)
        with End_of_file -> 
           printf "Warning: failure: no output on\n %s\n\n" cmd; 
           failed := cmd::!failed;
      end
   end

let no_optim = 0
let no_cutoff = 0
let no_csts = []

(* list of scheduling costs for each machine *)
(* format of list element: *)
(*   (machine, c_1, c_tr, c_ss, s, s_s) *)
(* constant c_1 measures the constant-factor overhead of work stealing
relative to the sequential elision (i.e., the parallel program with
parallel tuples replaced with tuples) *)
(* constant c_tr measures the constant-factor overhead of work
stealing relative to the sequential program (i.e., the program we
would have written for a sequential processor) *)
(* constant s measures the cost of work stealing relative to
sequential elision for a single parallel tuple *)
(* constant s_ss measures the cost of work stealing relative to
sequential program for a single parallel tuple *)
let scheduling_costs = ref []
    
let time_of_result_line line =
    sscanf line " %s %s %d %d %d %d %d %d %f" 
        (fun machine prog proc size param cutoff optim nb_csts time -> time)

(* for given machine, returns scheduling cost per parallel tuple evaluation *)
let get_scheduling_cost machine =
  let machine = get_hostname machine in
  let rec get cs =
    match cs with
      [] ->
	None
    | (machine', c_1, c_tr, c_ss, s, s_s, c_oracle, s_oracle) :: cs ->
	if machine = machine' then
	  Some s_oracle
	else
	  get cs
  in
  get !scheduling_costs

let alpha = 160.0

let get_kappa machine = 
  match get_scheduling_cost machine with
    None -> None
  | Some s-> Some (alpha *. s)

(* caches scheduling cost information in file
scheduling_cost_machine.txt where machine is the hostname of a given
machine *)
let measure_scheduling_cost_work_stealing params =
  let size = 30000000 in
  let scheduling_cost_file machine = 
    "scheduling_cost_"^get_hostname machine^".txt" in
  list_foreach params.machine (fun machine ->
    let hostname = get_hostname machine in
    if Sys.file_exists (scheduling_cost_file machine) then
      match file_get_lines (scheduling_cost_file hostname) with
	[csts] ->
	  let (hostname, c_1, c_tr, c_ss, s, s_s, c_oracle, s_oracle) = 
	    sscanf csts "%s %f %f %f %f %f %f %f" (fun machine c_1 c_tr c_ss s s_s c_oracle s_oracle -> 
	      (hostname, c_1, c_tr, c_ss, s, s_s, c_oracle, s_oracle))
	  in
	  add_to_list_ref scheduling_costs (hostname, c_1, c_tr, c_ss, s, s_s, c_oracle, s_oracle)
      | _ -> failwith "measure_scheduling_cost_work_stealing"
    else
      let tmp_file = "scheduling_cost_work_stealing_" ^ machine in
      let channel = open_out tmp_file in
      run machine channel "bintree-sum_ws" size 0 [] 0 1 0;
      run machine channel "bintree-sum_sq" size 0 [] 0 1 0;
      run machine channel "seq-sum_sq" size 0 [] 0 1 0;
      run machine channel "bintree-sum" size 0 [] 0 1 0;
      close_out channel;
      match file_get_lines tmp_file with
	[bt_ws; bt_sq; ss; bt_oracle] ->
	  let t_bt_ws = time_of_result_line bt_ws in
	  let t_bt_sq = time_of_result_line bt_sq in
	  let t_ss = time_of_result_line ss in
	  let t_oracle = time_of_result_line bt_oracle in
	  let c_1 = t_bt_ws /. t_bt_sq in
	  let c_tr = t_bt_sq /. t_ss in
	  let c_ss = t_bt_ws /. t_ss in
	  let s = (t_bt_ws -. t_bt_sq) /. (float_of_int (size-1)) *. 1000000.0 in
(*c_1 /. (float_of_int (size-1))    *. 1000000.0 in *)
	  let s_s = c_ss /. (float_of_int (size-1)) *. 1000000.0 in
	  let c_oracle = t_oracle /. t_bt_sq in
	  let s_oracle = (t_oracle -. t_bt_sq) /. (float_of_int (size-1)) *. 1000000.0in
(*	  let s_oracle = c_oracle /. (float_of_int (size-1))    *. 1000000.0 in*)
	  add_to_list_ref scheduling_costs (hostname, c_1, c_tr, c_ss, s, s_s, c_oracle, s_oracle);
	  file_put_contents (scheduling_cost_file hostname) 
	    (sprintf "%s %f %f %f %f %f %f %f" hostname c_1 c_tr c_ss s s_s c_oracle s_oracle)
      | _ -> failwith "measure_scheduling_cost_work_stealing");
  printf "scheduling costs by machine\n";
  List.iter (fun (machine, c_1, c_tr, c_ss, s, s_s, c_oracle, s_oracle) -> 
      printf "machine=%s c_1=%f c_tr=%f c_ss=%f s=%fus s_s=%fus c_oracle=%f s_oracle=%fus\n" machine c_1 c_tr c_ss s s_s c_oracle s_oracle)
    !scheduling_costs;
  ()

let benchmark params =
  let saved_content =
    if !arg_append then (String.concat "\n" (file_get_lines !arg_output)) ^ "\n" else "" in
  let channel = open_out !arg_output in
  output_string channel saved_content;
  let runs = ref [] in
  let add_run f = add_to_list_ref runs f in
  list_foreach params.machine (fun machine ->
     let _ =
       if !arg_cutoff = [] then
	 match get_kappa machine with
	   None -> ()
	 | Some kappa ->
	     let cutoff = int_of_float kappa in
	     params.cutoff <- [cutoff];
	     printf "Using cutoff of %d for machine %s\n" cutoff (get_hostname machine) in
     list_foreach params.prog (fun (prog, sizes, csts, param_values) -> 
        let param_values = if param_values = [] then [0] else param_values in
        list_foreach param_values (fun param ->
           list_foreach sizes (fun size ->
              list_foreach params.proc (fun proc ->
                 repeat params.tries (fun trial ->
                    if !arg_mode <> "tau" && not (string_ends_with "_sq" prog && proc > 1) then                 
                       list_foreach params.cutoff (fun cutoff ->
                          list_foreach params.optim (fun optim -> 
                             add_run (fun () -> run machine channel prog size param csts cutoff proc optim)));
                    if !arg_mode = "speedup" || !arg_mode = "cutoff" || !arg_mode = "tau" then begin
                          if proc = 1 
                             then add_run (fun () -> run machine channel (prog ^ "_sq") size param no_csts no_cutoff proc no_optim); 
                          add_run (fun () -> run machine channel (prog ^ "_ws") size param no_csts no_cutoff proc no_optim);
                       end;
                    if !arg_mode = "tau" && proc = 1 then
                       add_run (fun () -> run machine channel (prog ^ "_count") size param no_csts no_cutoff 1 no_optim);
                   ))))));
  let nb_runs = List.length !runs in
  list_iteri (fun id_run make_run -> printf "%d/%d: " (1+id_run) nb_runs; make_run()) (List.rev !runs);
  close_out channel;
  if !failed = [] then printf "Benchmark successful\n" else begin
     printf "Benchmark encountered errors on running the following commands:\n";
     list_foreach (List.rev !failed) (fun s -> printf "%s\n" s)
  end

(***************************************************************)
(** Command-line options *)

(* todo: make options take list of values *)

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
     [ ("-machine", Arg.String (string_args_to_ref arg_machine), "machines on which to run the experiment (comma-separated)");
       ("-prog", Arg.String (string_args_to_ref arg_prog), "fix program (comma-separated)");
       ("-paper-benchmarks", Arg.Set arg_paper_benchmarks, "just do benchmarks that we will use in the paper");
       ("-size", Arg.String (int_args_to_ref arg_size), "fix size (comma-separated)");
       ("-cutoff", Arg.String (int_args_to_ref arg_cutoff), "fix cutoff (comma-separated)");
       ("-param", Arg.String (int_args_to_ref arg_param), "fix param (comma-separated)");
       ("-proc", Arg.String (int_args_to_ref arg_proc), "fix nb-processor (comma-separated)");
       ("-optim", Arg.String (int_args_to_ref arg_optim), "fix optimization profiles (comma-separated)");
       ("-tries", Arg.Set_int arg_tries, "fix number of tries");
       ("-timeout", Arg.Set_int arg_timeout, "fix timeout");
       ("-big", Arg.Set arg_big, "use bigger sizes");       
       ("-virtual", Arg.Set arg_virtual, "only simulates");
       ("-append", Arg.Set arg_append, "keep data from output file");
       ("-directory", Arg.String (fun s -> arg_dir := s), "path to the complexity-based-scheduling /script directory");
       ("-local-host-name", Arg.String (fun s -> arg_localhostname := s), "local host name");
       ("-output", Arg.String (fun s -> arg_output := s), "specify output file"); ]
       (fun f -> arg_mode := f)
       ("usage: bench.out [options] mode");
   if !arg_mode = "" then arg_mode := "speedup";
   if !arg_output = "" then arg_output := sprintf "results_%s.txt" !arg_mode;
   if not (List.mem_assoc !arg_mode choices_params) then failwith "unknown mode";
   let params = List.assoc !arg_mode choices_params in
   params.optim <- (if !arg_optim = [] then [0] else !arg_optim);
   if !arg_proc <> [] then params.proc <- !arg_proc;
   if !arg_tries <> -1 then params.tries <- !arg_tries;
   if !arg_cutoff <> [] then params.cutoff <- !arg_cutoff;
   if !arg_param <> [] then params.prog <- List.map (fun (n,ss,cs,ps) -> (n,ss,cs, !arg_param)) params.prog;
   if !arg_prog <> [] then 
      params.prog <- List.map (fun prog ->
           try List.find (fun (n,_,_,_) -> n = prog) params.prog 
           with Not_found -> failwith (sprintf "data for %s not found in bench.ml; try recompiling it" prog))
        !arg_prog;
   if !arg_paper_benchmarks then
     params.prog <- List.filter (fun (n,_,_,_) ->
       List.mem n paper_benchmarks)
	 params.prog;
   if !arg_size <> [] then
      params.prog <- List.map (fun (n,ss,cs,ps) -> (n,!arg_size,cs,ps)) params.prog
   else if !arg_big then    
      params.prog <- List.map (fun (n,ss,cs,ps) -> (n,[list_max ss],cs,ps)) params.prog
   else
      params.prog <- List.map (fun (n,ss,cs,ps) -> (n,[list_min ss],cs,ps)) params.prog;
   if !arg_machine <> ["original"] then 
      params.machine <- (if !arg_machine = [] then [ "local" ] else !arg_machine);
   measure_scheduling_cost_work_stealing params;
   benchmark params;
   ignore (Unix.system ("./analyse.out " ^ !arg_mode))

