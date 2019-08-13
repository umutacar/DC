
let rec pow10 n =
   if n = 0 then 1 else 10 * pow10 (n-1)

let rec pow10_list n m =
   if n > m then [] else pow10 n :: pow10_list (n+1) m

let rec pow10_list_upto n =
   pow10_list 0 n

let rec pow2 n =
   if n = 0 then 1 else 2 * pow2 (n-1)

let rec pow2_list n m =
   if n > m then [] else pow2 n :: pow2_list (n+1) m

let rec pow2_list_upto n =
   pow2_list 0 n

let list_median l =
   List.nth l (List.length l / 2)

let list_max l =
   match l with 
   | [] -> assert false
   | x::ls -> List.fold_left max x ls

let list_min l =
   match l with 
   | [] -> assert false
   | x::ls -> List.fold_left min x ls

let list_foreach l f =
   List.iter f l

let list_last l =
  List.nth l (List.length l - 1)

let repeat n f =
   for i = 0 to pred n do f i done


let string_ends_with s str =
   let n = String.length s in
   let m = String.length str in
   if m < n 
      then false 
      else (s = String.sub str (m - n) n)


let add_to_list_ref r x =
  r := x :: !r

let unix_command s =
   ignore (Unix.system s)

let file_get_lines file = 
   if not (Sys.file_exists file)
      then failwith ("file does not exists " ^ file);
   let lines = ref [] in
   let f = 
      try open_in file with End_of_file -> failwith ("file does not exists " ^ file)
      in
   begin try while true do
      lines := input_line f :: !lines 
   done with End_of_file -> () end;
   close_in f;
   List.rev !lines

let file_put_contents filename str =
  let channel = open_out filename in
  output_string channel str;
  close_out channel    

let string_split char_sep str =
   let n = String.length str in
   let words = ref [] in 
   let i = ref 0 in
   begin try while true do 
      let j = String.index_from str !i char_sep in
      let k = j - !i in
      if k > 0
        then (let word = String.sub str !i k in
              words := word :: !words);
      i := j+1;
   done with Not_found -> 
      if n - !i > 0 then
        (let rest = String.sub str !i (n - !i) in
         words := rest :: !words)
   end;
   List.rev !words

let rec take n xs = 
   match n, xs with 
        0, xs -> [] 
      | n, [] -> failwith "take" 
      | n, x::xs -> x :: take (n-1) xs

let rec drop n xs = 
   match n, xs with 
        0, xs -> xs
      | n, [] -> failwith "drop" 
      | n, x::xs -> drop (n-1) xs

let rec list_take_drop n l =
  if n = 0 then ([], l) else
  match l with
  | [] -> failwith "invalid argument for list_take_drop"
  | x::l' -> let (h,t) = list_take_drop (n-1) l' in
             (x::h, t)

let rec list_pairing l =
  match l with
  | [] -> []
  | [x] -> failwith "invalid argument for list_pairing"
  | x::y::l' -> (x,y) :: (list_pairing l')

let list_mean l =
  let n = float_of_int (List.length l) in
  let s = List.fold_left (+.) 0. l in
  s /. n

let float_sq x =
  x *. x

let list_mean_and_stddev l =  
  let mean = list_mean l in
  let variance = list_mean (List.map (fun x -> float_sq (x -. mean)) l) in
  mean, sqrt variance

let string_cmp x y =
  if x < y then -1 else if x = y then 0 else -1

let list_foreach l f =
   List.iter f l

let list_iteri f l =
  let rec aux i = function
    | [] -> ()
    | h::t -> (f i h); (aux (i+1) t)
    in
  aux 0 l

let list_foreachi l f =
   list_iteri f l

let list_mapi f l =
  let rec aux i = function
    | [] -> []
    | h::t -> (f i h)::(aux (i+1) t)
    in
  aux 0 l

let list_filter_every n l =
  if n <= 0 then failwith "invalid arg for list_filter";
  let rec aux acc k = function
      | [] -> acc
      | x::l' ->
          if k = 0 then aux (x::acc) (n-1) l'
                   else aux acc (k-1) l'
     in
  List.rev (aux [] (n-1) l)

let unsome_or default = function
  | None -> default
  | Some x -> x

(***************************************************************)
(** imperative finite maps *)

module Imap :
sig
   type ('a,'b) t
   val from_list : ('a * 'b) list -> ('a, 'b) t
   val to_list : ('a, 'b) t -> ('a * 'b) list
   val create : unit -> ('a,'b) t
   val is_empty : ('a,'b) t -> bool
   val filter : ('a -> 'b -> bool) -> ('a,'b) t -> ('a,'b) t
   val mem : 'a -> ('a,'b) t -> bool
   val find : 'a -> ('a,'b) t -> 'b
   val find_or_create : (unit -> 'b) -> 'a -> ('a,'b) t -> 'b
   val iter : ('a -> 'b -> unit) -> ('a,'b) t -> unit
   val foreach : ('a,'b) t -> ('a -> 'b -> unit) -> unit
   val map : ('a * 'b -> 'c * 'd) -> ('a,'b) t -> ('c,'d) t
   val fold : ('c -> 'a -> 'b -> 'c) -> 'c -> ('a,'b) t -> 'c
   val ksort : ('a -> 'a -> int) -> ('a,'b) t -> unit
end
= 
struct

   type ('a,'b) t = (('a * 'b) list) ref 

   let from_list m =
      ref m

   let to_list m =
      !m

   let create () = 
      ref []

   let is_empty m =
     !m = []

   let filter f m =
     from_list (List.filter (fun (k,v) -> f k v) !m)

   let mem k m =
      List.mem_assoc k !m

   let find k m = 
      List.assoc k !m

   let find_or_create c k m =
      try find k m 
      with Not_found -> 
        let v = c() in
        m := (k,v)::!m; 
        v

   let iter f m =
       List.iter (fun (k,v) -> f k v) !m

   let foreach m f = 
      iter f m

   let map f m =
      from_list (List.map f !m)

   let fold f i m =
      List.fold_left (fun acc (k,v) -> f acc k v) i !m
   
   let ksort cmp m =
      m := List.sort (fun (k1,_) (k2,_) -> cmp k1 k2) !m
end


