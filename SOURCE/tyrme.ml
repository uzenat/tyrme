open Ast;;
open Lexer;;
open Parser;;


(* les instructions *)
type instr = 
| Halt (* 0 *)
| Push (* 1 *)
| Print (* 2 *)
| Acc of int (* 4 *)
| Const of int (* 5 *)
| Pop of int (* 7 *)
| Str of string (* 14 *)
| Binop of int (* 13 *)
| BranchIf of int (* 8 *)
| Branch of int (* 9 *)
| MakeBlock of int * int (* 11 *)
| GetBlock of int (* 10 *)
| GetBlock2 of int (* 31 *)
| Closure of int * int (* 12 *)
| Apply (* 3 *)
| Return of int (* 6 *)
| AppTerm of int * int (* 30 *)

let string_of_instr = function
  | Halt -> "Halt"
  | Push -> "Push"
  | Print -> "Print"
  | Const i -> "Const " ^ (string_of_int i)
  | Acc n -> "Acc " ^ (string_of_int n)
  | Pop n -> "Pop " ^ (string_of_int n)
  | Str s -> "Str " ^ s
  | Binop b -> "Binop " ^ (string_of_int b)
  | Branch i -> "Branch " ^ (string_of_int i)
  | BranchIf i -> "BranchIf " ^ (string_of_int i)
  | MakeBlock (o, n) -> "MakeBlock (" ^ (string_of_int o) ^ ", " ^ (string_of_int n) ^ ")"
  | GetBlock n -> "GetBlock " ^ (string_of_int n)
  | GetBlock2 n -> "GetBlock2 " ^ (string_of_int n)
  | Closure (o, n) -> "Closure (" ^ (string_of_int o) ^ ", " ^ (string_of_int n) ^ ")"
  | Apply -> "Apply"
  | Return n -> "Return " ^ (string_of_int n)
  | AppTerm (m, n) -> "AppTerm " ^ (string_of_int m) ^ ", " ^ (string_of_int n)

(* affiche la liste d'instruction *)
let print_listinstr l = 
  let a =  Array.of_list l in
  for i=0 to (Array.length a) -1 do
      print_endline ((string_of_int i) ^ " - " ^ string_of_instr a.(i))
  done;;

(* Fonctions de lecture et d'ecriture d'entier 8bits et 32bits *)
let out_i8  (buf : out_channel) (i : int) : unit = output_char buf (char_of_int i)
let out_i32 (buf : out_channel) (i : int) : unit = output_binary_int buf i 

let in_i8   (buf : in_channel) : int = int_of_char (input_char buf)
let in_i32  (buf : in_channel) : int = input_binary_int buf

(** PARSER & LEXER *)

let parse (s : string) : expr = 
    Parser.main Lexer.token (Lexing.from_string s);;

(** ASSEMBLAGE **)

(* Fonction d'assemblage d'instruction *)
let assemble_instr (buf : out_channel) : instr -> unit = function
  | Halt -> out_i8 buf 0
  | Push -> out_i8 buf 1
  | Print -> out_i8 buf 2
  | Acc n -> out_i8 buf 4; out_i32 buf n
  | Const n -> out_i8 buf 5; out_i32 buf n
  | Pop n -> out_i8 buf 7; out_i32 buf n
  | Str s -> 
    out_i8 buf 14;
    let n = String.length s in
    out_i32 buf n;
    for i = 0 to n-1 do
      out_i8 buf (Char.code s.[i]);
    done
  | Binop b -> out_i8 buf 13; out_i8 buf b
  | BranchIf n -> out_i8 buf 8; out_i32 buf n
  | Branch n -> out_i8 buf 9; out_i32 buf n
  | MakeBlock (t, n) -> out_i8 buf 11; out_i8 buf t; out_i32 buf n
  | GetBlock n -> out_i8 buf 10; out_i32 buf n
  | GetBlock2 n -> out_i8 buf 31; out_i32 buf n
  | Closure (n, o) -> out_i8 buf 12; out_i32 buf n; out_i32 buf o
  | Apply -> out_i8 buf 3
  | Return n -> out_i8 buf 6; out_i32 buf n
  | AppTerm (m, n) -> out_i8 buf 30; out_i32 buf m; out_i32 buf n


(* Fonction d'assemblage d'une liste d'instructions *)
let rec assemble (buf : out_channel) : instr list -> unit = function
  | [] -> ()
  | h::tl -> assemble_instr buf h; assemble buf tl 

(* Ecrite pour vous: une fonction d'assemblage qui ecrit dans un fichier *)
let assemble_filename (name : string) (is : instr list) : unit = 
  let buf = open_out_bin name in
  begin
    assemble buf is;
    close_out buf
  end;;

(** DESASSEMBLAGE **)

(* fonction de desassemblage: stub *)
let rec disassemble (buf : in_channel) : instr list =
  (* Get the next char, and make sure to capture the end of the file *)
  let inc = (try Some (in_i8 buf) with | End_of_file -> None) in
  (* Test if there were a char *)
  match inc with
  | None   -> []  (* Nope: end of the file *)
  | Some c ->  (* Yep ! Carry on *)
    begin
      match c with
      | 0 -> Halt :: (disassemble buf)
      | 1 -> Push :: (disassemble buf)
      | 2 -> Print :: (disassemble buf)
      | 4 -> 
	let n = in_i32 buf in
	(Acc n)::(disassemble buf)
      | 5 ->
	let n = in_i32 buf in
	(Const n)::(disassemble buf)
      | 7 -> 
	let n = in_i32 buf in
	(Pop n)::(disassemble buf)
      | 14 ->
	let n = in_i32 buf in
	let s = ref "" in
	for i = 0 to n-1 do 
	  s := !s ^ (String.make 1 (Char.chr (in_i8 buf)));
	done;
	(Str !s)::(disassemble buf)	  
      | 13 -> 
	let n = in_i8 buf in
	(Binop n)::(disassemble buf)
      | 8 ->
	let n = in_i32 buf in
	(BranchIf n)::(disassemble buf)
      | 9 ->
	let n = in_i32 buf in
	(Branch n)::(disassemble buf)
      | 11 -> 
	let t = in_i8 buf in
	let n = in_i32 buf in
	(MakeBlock (t,n))::(disassemble buf)
      | 10 ->
	let n = in_i32 buf in
	(GetBlock n)::(disassemble buf)
      | 12 ->
	let n = in_i32 buf in
	let o = in_i32 buf in
	(Closure (n, o))::(disassemble buf)
      | 3 -> (Apply)::(disassemble buf)
      | 6 ->
	let n = in_i32 buf in
	(Return n)::(disassemble buf)
      | 30 -> 
	let m = in_i32 buf in
	let n = in_i32 buf in
	(AppTerm (m, n)) :: (disassemble buf)
      | 31 ->
	let n = in_i32 buf in
	(GetBlock2 n)::(disassemble buf)
      | _ -> failwith "error"

    end;;
  
(* Ecrite pour vous: une fonction de desassemblage qui lit d'un fichier *)
let disassemble_filename (name : string) : instr list = 
  let buf = open_in_bin name in
  let insts = disassemble buf in
  let _ = close_in buf in
  insts;;

(** MACHINE VIRTUELLE **)

type tag = int

type mot =
| MotInt of int
| PointString of string
| PointBloc of (tag * (mot list))

type mv_state ={
  mutable acc: mot;
  code: instr array;
  mutable pc: int;
  stack: mot array;
  mutable sp: int;
};;

let rec string_of_mot = function
  | MotInt i -> string_of_int i
  | PointString s -> s
  | PointBloc (tag, q) -> 
    let rec aux =function
      | [] -> ""
      | t::q' -> (string_of_mot t) ^ " " ^ (aux q')
    in
    let t = aux q in
    "(" ^ string_of_int tag ^ ", " ^ t ^ ")";;

let print_state s =
  print_string ("Stack:\n");
  (if (s.sp < 0) 
   then print_string "<empty>\n"
   else for i = 0 to s.sp do
          print_string ("#" ^ (string_of_int (s.sp - i)) ^ " -> " 
                        ^ (string_of_mot (s.stack.(i))) ^ "\n")
        done);
  print_string ("Acc = " ^ (string_of_mot s.acc) ^ "\n");
  print_string ("PC = " ^ (string_of_int s.pc) ^ "\n\n")


(* La fonction d'execution de la machine *)
let machine (s : mv_state) : mv_state = 
  let idx = ref 0 in
  begin
    while s.pc < (Array.length s.code) do
      idx := !idx + 1;
      begin
	match s.code.(s.pc) with
	| Halt -> 
	  failwith "Terminaison de la machine"
	| Push ->
	  s.sp <- s.sp +1;
	  s.stack.(s.sp) <- s.acc
	| Print ->
	  begin
	    match s.acc with
	    | PointString s1 -> print_endline s1
	    | MotInt i -> print_int i; print_endline ""
	    | _ -> failwith "erreur"
	  end
	| Acc n ->
	  s.acc <- s.stack.(s.sp - n)
	| Const n -> 
	  s.acc <- MotInt n
	| Pop n ->
	  s.sp <- s.sp - n
	| Str s1 ->
	  s.acc <- PointString s1
	| Binop b ->
	  begin
	    match b, s.acc, s.stack.(s.sp) with
	    | 15, MotInt i, MotInt j -> 
	      s.acc <- MotInt(j + i)
	    | 16, MotInt i, MotInt j ->
	      s.acc <- MotInt(j - i)
	    | 17, MotInt i, MotInt j ->
	      s.acc <- MotInt(j * i)
	    | 18, MotInt i, MotInt j ->
	      s.acc <- MotInt(j / i)
	    | 19, MotInt i, MotInt j ->
	      s.acc <- MotInt(if j = i then 1 else 0)
	    | 20, PointString s1, PointString s2 ->
	      s.acc <- PointString (s2 ^ s1)
	    | 21, MotInt i, MotInt j ->
	      let n = 
		match i,j with
		| 0,0 -> 0
		| 0,1 -> 0
		| 1,0 -> 0
		| 1,1 -> 1
		| _ -> failwith "erreur compilation"
	      in
	      s.acc <- MotInt(n) 
	    | _ -> failwith "erreur compilation"
	  end;
	  s.sp <- s.sp - 1;
	| BranchIf n ->
	  s.pc <- s.pc + (if (s.acc = MotInt(0)) then 0 else n - 1);
	| Branch n ->
	  s.pc <- s.pc + n - 1
	| Return n ->
	  s.sp <- s.sp - n;
	  let pc0 = s.stack.(s.sp) in
	      let pc = 
		begin
		  match pc0 with 
		  | MotInt i -> i
		  | _ -> failwith "erreur apply"
		end
	      in
	  s.pc <- pc - 1;
	  s.sp <- s.sp - 1
	| Apply ->
	  begin
	    match s.acc with
	    | PointBloc (88, t) ->
	      let n = s.stack.(s.sp) in (* stock la tete de la pile -> argument de la fonction *)
	      s.stack.(s.sp) <- MotInt (s.pc + 1); (* a la place, met le pc + 1 *)
	      let rec aux x = match x with (* fonction qui remplie dans la pile l'environnement *)
		| [] -> ()
		| t::q -> aux q; s.sp <- s.sp + 1; s.stack.(s.sp) <- t 
	      in
	      aux t;
	      let pc0 = s.stack.(s.sp) in (* on aura a la tete, la position de la fonction *)
	      let pc2 = (* recupere le int *)
		begin
		  match pc0 with 
		  | MotInt i -> i
		  | _ -> failwith "erreur apply"
		end
	      in
	      s.pc <- pc2 - 1; (* on met a jour le pc *)
	      s.stack.(s.sp) <- n (* on ne veut pas garder cette position dans la pile, a la place
				     on met l'argument de la fonction *)
	    | _ -> failwith "erreur apply"
	  end
	| Closure (n, o) ->
	  let rec aux i  =
	      if i = n then []
	      else [ s.stack.(s.sp - i) ] @ (aux (i+1))
	  in
	  let t = [ MotInt (s.pc + o) ] @ (aux 0) in
	  s.acc <- PointBloc (88, t)
	| MakeBlock (t, n) ->
	  let rec aux i =
	    if i = n then []
	    else [ s.stack.(s.sp - i) ] @ (aux (i+1))
	  in
	  let t = aux 0 in
	  s.acc <- PointBloc (0, t);
	  s.sp <- s.sp - n
	| GetBlock n ->
	  let x = 
	    match s.acc with
	    | PointBloc (0, t) ->
	      let rec aux x i = match x with
		| [] -> failwith "erreur getblock"
		| h::tl -> 
		  if i = n then h else aux tl (i+1)
	      in
	      aux t 0
	    | _ -> failwith "erreur getblock"
	  in
	  s.acc <- x
	| GetBlock2 n ->
	  let n' =  match s.stack.(s.sp - n) with
	    | MotInt i -> i
	    | _ -> failwith "erreur getblock2"
	  in
	  let x = 
	    match s.acc with
	    | PointBloc (0, t) ->
	      let rec aux x i = match x with
		| [] -> failwith "erreur getblock"
		| h::tl -> 
		  if i = n' then h else aux tl (i+1)
	      in
	      aux t 0
	    | _ -> failwith "erreur getblock"
	  in
	  s.acc <- x
	| AppTerm (m, n) ->
	  begin

	  (* on effectue le Apply *)
	    begin
	      match s.acc with
	      | PointBloc (88, t) ->
		let n = s.stack.(s.sp) in (* stock la tete de la pile -> argument de la fonction *)
		s.stack.(s.sp) <- MotInt (s.pc + 1); (* a la place, met le pc + 1 *)
		let rec aux x = match x with (* fonction qui remplie dans la pile l'environnement *)
		  | [] -> ()
		  | t::q -> aux q; s.sp <- s.sp + 1; s.stack.(s.sp) <- t 
		in
		aux t;
		let pc0 = s.stack.(s.sp) in (* on aura a la tete, la position de la fonction *)
		let pc2 = (* recupere le int *)
		  begin
		    match pc0 with 
		    | MotInt i -> i
		    | _ -> failwith "erreur apply"
		  end
		in
		s.pc <- pc2 - 1; (* on met a jour le pc *)
		s.stack.(s.sp) <- n (* on ne veut pas garder cette position dans la pile, a la place
				       on met l'argument de la fonction *)
	      | _ -> failwith "erreur apply"
	    end;
	  
	  (* dans la pile on aura à present : 
	     l'argument -> l'environnement -> le pc + 1 -> la signature -> l'argument precedent -> ... 
	     on recupere l'argument, on pop l'environnement, on pop le pc + 1, on pop la signature,
	     on met l'argument à la place de l'argument precedent *)
	    
	  (* On effectue le Return *)

	    begin
	      let arg = s.stack.(s.sp) in
	      s.sp <- s.sp - 1; (* pop de l'argument *)
	      s.sp <- s.sp - (n - m); (* pop de l'environnement *)
	      s.sp <- s.sp - 1; (* pop du pc + 1 *)
	      s.sp <- s.sp - 1; (* pop de la signature *)
	      s.stack.(s.sp) <- arg; (* place l'argument à la place de l'argument precedent *)
	    end

	  end  
      end;
      s.pc <- s.pc + 1;
    done;
  end; 
  s;;


(* La fonction d'execution de la machine version debuggage *)
let machine_debugg (s : mv_state) : mv_state = 
  let idx = ref 0 in
  begin
    while s.pc < (Array.length s.code) do
      idx := !idx + 1;
       print_string ("=== Step " ^ (string_of_int !idx) ^ " ===\n");
      print_string ("With PC = " ^ (string_of_int s.pc) ^ " (" ^ (string_of_instr (s.code.(s.pc))) ^ ")\n");
      begin
	match s.code.(s.pc) with
	| Halt -> 
	  failwith "Terminaison de la machine"
	| Push ->
	  s.sp <- s.sp +1;
	  s.stack.(s.sp) <- s.acc
	| Print ->
	  begin
	    match s.acc with
	    | PointString s1 -> print_endline s1
	    | MotInt i -> print_int i; print_endline ""
	    | _ -> failwith "erreur"
	  end
	| Acc n ->
	  s.acc <- s.stack.(s.sp - n)
	| Const n -> 
	  s.acc <- MotInt n
	| Pop n ->
	  s.sp <- s.sp - n
	| Str s1 ->
	  s.acc <- PointString s1
	| Binop b ->
	  begin
	    match b, s.acc, s.stack.(s.sp) with
	    | 15, MotInt i, MotInt j -> 
	      s.acc <- MotInt(j + i)
	    | 16, MotInt i, MotInt j ->
	      s.acc <- MotInt(j - i)
	    | 17, MotInt i, MotInt j ->
	      s.acc <- MotInt(j * i)
	    | 18, MotInt i, MotInt j ->
	      s.acc <- MotInt(j / i)
	    | 19, MotInt i, MotInt j ->
	      s.acc <- MotInt(if j = i then 1 else 0)
	    | 20, PointString s1, PointString s2 ->
	      s.acc <- PointString (s1 ^ s2)
	    | 21, MotInt i, MotInt j ->
	      let n = 
		match i,j with
		| 0,0 -> 0
		| 0,1 -> 0
		| 1,0 -> 0
		| 1,1 -> 1
		| _ -> failwith "erreur compilation"
	      in
	      s.acc <- MotInt(n) 
	    | _ -> failwith "erreur compilation"
	  end;
	  s.sp <- s.sp - 1;
	| BranchIf n ->
	  s.pc <- s.pc + (if (s.acc = MotInt(0)) then 0 else n - 1);
	| Branch n ->
	  s.pc <- s.pc + n - 1
	| Return n ->
	  s.sp <- s.sp - n;
	  let pc0 = s.stack.(s.sp) in
	      let pc = 
		begin
		  match pc0 with 
		  | MotInt i -> i
		  | _ -> failwith "erreur apply"
		end
	      in
	  s.pc <- pc - 1;
	  s.sp <- s.sp - 1
	| Apply ->
	  begin
	    match s.acc with
	    | PointBloc (88, t) ->
	      let n = s.stack.(s.sp) in
	      s.stack.(s.sp) <- MotInt (s.pc + 1);
	      let rec aux x = match x with
		| [] -> ()
		| t::q -> aux q; s.sp <- s.sp + 1; s.stack.(s.sp) <- t 
	      in
	      aux t;
	      let pc0 = s.stack.(s.sp) in
	      let pc = 
		begin
		  match pc0 with 
		  | MotInt i -> i
		  | _ -> failwith "erreur apply"
		end
	      in
	      s.pc <- pc - 1;
	      s.stack.(s.sp) <- n
	    | _ -> failwith "erreur apply"
	  end
	| Closure (n, o) ->
	  let rec aux i  =
	      if i = n then []
	      else [ s.stack.(s.sp - i) ] @ (aux (i+1))
	  in
	  let t = [ MotInt (s.pc + o) ] @ (aux 0) in
	  s.acc <- PointBloc (88, t)
	| MakeBlock (t, n) ->
	  let rec aux i =
	    if i = n then []
	    else [ s.stack.(s.sp - i) ] @ (aux (i+1))
	  in
	  let t = aux 0 in
	  s.acc <- PointBloc (0, t);
	  s.sp <- s.sp - n
	| GetBlock n ->
	  let x = 
	    match s.acc with
	    | PointBloc (0, t) ->
	      let rec aux x i = match x with
		| [] -> failwith "erreur getblock"
		| h::tl -> 
		  if i = n then h else aux tl (i+1)
	      in
	      aux t 0
	    | _ -> failwith "erreur getblock"
	  in
	  s.acc <- x
	| GetBlock2 n ->
	  let n' =  match s.stack.(s.sp - n) with
	    | MotInt i -> i
	    | _ -> failwith "erreur getblock2"
	  in
	  let x = 
	    match s.acc with
	    | PointBloc (0, t) ->
	      let rec aux x i = match x with
		| [] -> failwith "erreur getblock"
		| h::tl -> 
		  if i = n' then h else aux tl (i+1)
	      in
	      aux t 0
	    | _ -> failwith "erreur getblock"
	  in
	  s.acc <- x
	| AppTerm (m, n) ->
	  begin
	    
	    (* on effectue le Apply *)
	    begin
	      match s.acc with
	      | PointBloc (88, t) ->
		let n = s.stack.(s.sp) in (* stock la tete de la pile -> argument de la fonction *)
		s.stack.(s.sp) <- MotInt (s.pc + 1); (* a la place, met le pc + 1 *)
		let rec aux x = match x with (* fonction qui remplie dans la pile l'environnement *)
		  | [] -> ()
		  | t::q -> aux q; s.sp <- s.sp + 1; s.stack.(s.sp) <- t 
		in
		aux t;
		let pc0 = s.stack.(s.sp) in (* on aura a la tete, la position de la fonction *)
		let pc2 = (* recupere le int *)
		  begin
		    match pc0 with 
		    | MotInt i -> i
		    | _ -> failwith "erreur apply"
		  end
		in
		s.pc <- pc2 - 1; (* on met a jour le pc *)
		s.stack.(s.sp) <- n (* on ne veut pas garder cette position dans la pile, a la place
				       on met l'argument de la fonction *)
	      | _ -> failwith "erreur apply"
	    end;
	  
	  (* dans la pile on aura à present : 
	     l'argument -> l'environnement -> le pc + 1 -> la signature -> l'argument precedent -> ... 
	     on recupere l'argument, on pop l'environnement, on pop le pc + 1, on pop la signature,
	     on met l'argument à la place de l'argument precedent *)
	    
	  (* On effectue le Return *)

	    begin
	      let arg = s.stack.(s.sp) in
	      s.sp <- s.sp - 1; (* pop de l'argument *)
	      s.sp <- s.sp - (n - m); (* pop de l'environnement *)
	      s.sp <- s.sp - 1; (* pop du pc + 1 *)
	      s.sp <- s.sp - 1; (* pop de la signature *)
	      s.stack.(s.sp) <- arg; (* place l'argument à la place de l'argument precedent *)
	    end

	  end  
      end;
      s.pc <- s.pc + 1;
      print_string ("New state is\n");
      print_state s;
    done;
    print_string ("\n" ^ (string_of_int !idx) ^ " steps in total\n\n")
  end; 
  s;;


let init c = {
  code = Array.of_list c;
  stack = Array.make 1000 (MotInt(42));
  pc = 0;
  sp = -1;
  acc = MotInt(52);
}

(* retourne l'accumulateur de l'etat donne en argument *)
let get_acc (s : mv_state) : mot = s.acc

(* La fonction d'evaluation: retourne l'accumulateur a la fin de l'evaluation *)
let eval (c : instr list) : mot =
  let s = machine (init c) in get_acc s

let eval_debugg (c : instr list) : mot =
  let s = machine_debugg (init c) in get_acc s

(** COMPILATION **)

(* environnement *)
type env = (var * int) list;;

(* environnement vide *)
let empty_env = [];;

(* environnement successeur *)
let envsucc e =
  let succ (x,y) = (x, y+1) in
  List.map succ e;;

(* recupere l'opcode d'une operation *)
let op = function
  | Add -> 15
  | Sub  -> 16
  | Mult -> 17
  | Div -> 18
  | Eq -> 19
  | Cat -> 20
  | And -> 21
  | App -> 22
  | Leq -> 23

(* La fonction de compilation *)
let rec compil : env * expr -> instr list = function
  | env, Var s -> [ Acc (List.assoc s env) ]
  | env, Const v ->
    begin
      match v with
      | Int i -> [ Const i ]
      | Bool i ->
	begin
	  match i with
	  | true -> [ Const 1 ]
	  | false -> [ Const 0 ]
	end
      | String s -> [ Str s ]
      | Unit -> []
      | _ -> failwith "erreur compilation"
    end
  | env, Binop (o, e1, e2) ->
    begin
      match o with
      | App ->
	compil (env, e2) @
	  [ Push ] @
	  compil (envsucc env, e1) @
	  [ Apply ]
      | _ ->
	compil (env, e1) @
	  [ Push ] @
	  compil (envsucc env, e2) @
	  [ Binop (op o) ]
    end
  | env, If (e1, e2, e3) ->
    let i2 = compil (env, e2) in
    let i3 = compil (env, e3) in
    compil (env, e1) @
      [ BranchIf ( 2 + (List.length i3) ) ] @
      i3 @
      [ Branch ( 1 + (List.length i2) ) ] @
      i2
  | env, Let (s, e1, e2) ->
    let new_env = (s, 0) :: (envsucc env) in
    compil (env, e1) @
      [ Push ] @
      compil (new_env, e2) @
      [ Pop 1 ]
  | env, Letf (s1, s2, e1, e2) ->
    let rec optimise code = 
      match code with
      | [] -> []
      | h1 :: tl ->
	begin 
	  match h1 with 
	  | Branch n ->  [ Return (2 + List.length env) ] @ (optimise tl)
	  | _ -> h1 :: (optimise tl)
	end
    in 
    let rec optimise2 code =
      match code with
      | [] -> []
      | h :: [] -> [ h ]
      | h1 :: h2 :: tl ->
	match h1, h2 with
	| Apply, Return n -> [ AppTerm (1, (n-1)) ] @ (optimise2 tl)
	| _, _ -> h1 :: ( optimise2 (h2 :: tl) )
    in
    let rec optimise3 code =
	match code with
	| [] -> []
	| (BranchIf n) :: tl ->
	  begin
	    let rec compte code' =
	      match code' with
	      | [] -> 0
	      | AppTerm (m, n) :: tl -> 1 + (compte tl)
	      | _ :: tl -> (compte tl)
	    in
	    let n' = compte tl in
	    (BranchIf (n - n')) :: (optimise3 tl)
	  end
	| h :: tl -> h :: (optimise3 tl)
    in
    let new_env0 = (s2, 0) :: (envsucc env) in
    let new_env1 = (s1, 0) :: (envsucc new_env0) in
    let new_env2 = (s1, 0) :: (envsucc env) in
    let i2= compil (new_env1, e1) in
    let i3= compil (new_env2, e2) in
    let op1 = optimise i2 in
    let op2 = optimise2 op1 in
    let op3 = optimise3 op2 in
    [ Closure ( List.length env, (((List.length i3)+4))) ] @
      [ Push ] @
      i3 @
      [ Pop 1 ] @
      [ Branch ((List.length i2) + 3) ] @
      [ Push ] @
      op3 @
      [ Return (2 + List.length env) ]
  | env, Print (e1, e2) ->
    compil (env, e1) @
      [ Print ] @
      compil (env, e2)
  | env, Pair (e1, e2) ->
    compil (env, e2) @
      [ Push ] @
      compil (env, e1) @
      [ Push ; MakeBlock (0, 2) ]
  | env, Fst e ->
    compil (env, e) @
      [ GetBlock 0 ]
  | env, Snd e ->
    compil (env, e) @
      [ GetBlock 1 ]
  | env, Liste l ->
    let rec aux x = match x with
      | [] -> []
      | h :: tl ->  aux tl @ compil (env, h) @ [ Push ]
    in (aux l) @ [ MakeBlock (0, List.length l) ]
  | env, Proj (e1, e2) ->
    let i = compil (env, e2) in
    match e1 with
    | Const( Int x ) -> i @ [ GetBlock x ]
    | Var s -> i @ [ GetBlock2 (List.assoc s env) ]
    | _ -> failwith "erreur proj"



(** FONCTION POUR LE PROGRAMME *)

(* lire le contenue d'un fichier et renvoyer une chaine *)
let filein_to_string file = 
    let input i = 
      try 
	let c= input_char i in
	Some(c)
      with
	End_of_file -> None
    in
    let rec filein_to_string' file str =
      let s = input file in
      match s with
      | Some(c) -> 
	filein_to_string' file (str ^ (String.make 1 c))
      | None -> str
    in
    let str = filein_to_string' file "" in
    let n = String.length str in
    if str.[n-1] = '\n' then 
      String.sub str 0 (n-1)
    else str;;

(* transforme une chaine en expression *)
let str_to_expr str = parse str;;

(* compile un fichier en bytecode vers un fichier .bty *)
let compile_bytecode file =
  let ffile = open_in file in
  let str = filein_to_string ffile in
  let expr = str_to_expr str in
  let comp = compil (empty_env, expr) in
  let n = String.length file in
  let bty = (String.sub file 0 (n-2)) ^ "bty" in 
  assemble_filename bty comp;;

(* execute du bytecode *)
let execute_bytecode file =
  let instr = disassemble_filename file in
  let r = eval instr in
  print_endline (string_of_mot r);;

let execute_bytecode_debugg file =
  let instr = disassemble_filename file in
  let r = eval_debugg instr in
  print_endline ("Le resultat est : " ^ (string_of_mot r));;

(* interprete le code source *)
let interp_source file =
  let ffile = open_in file in
  let str = filein_to_string ffile in
  let expr = str_to_expr str in
  let instr = compil (empty_env, expr) in
  let r = eval instr in
  print_endline (string_of_mot r);;

let interp_source_debugg file =
  let ffile = open_in file in
  let str = filein_to_string ffile in
  let expr = str_to_expr str in
  let instr = compil (empty_env, expr) in
  let r = eval_debugg instr in
  print_endline (string_of_mot r);;

(* affiche les instruction du source *)
let affiche_instr_source file =
  let ffile = open_in file in
  let str = filein_to_string ffile in
  let expr = str_to_expr str in
  let instr = compil (empty_env, expr) in
  print_listinstr instr;;

(* affiche les instruction du bytecode *)
let affiche_instr_bytecode file =
  let instr = disassemble_filename file in
  print_listinstr instr;;

(* PROGRAMME MAIN *)



let () =

  let isTY arg = 
    let n = String.length arg in
    if n <= 3 then false 
    else
      begin
	if arg.[n-1] <> 'y' then false
	else if arg.[n-2] <> 't' then false
	else if arg.[n-3] <> '.' then false
	else true
      end
  in

  let isBTY arg = 
    let n = String.length arg in
    if n <= 3 then false 
    else
      begin
	if arg.[n-1] <> 'y' then false
	else if arg.[n-2] <> 't' then false
	else if arg.[n-3] <> 'b' then false
	else if arg.[n-4] <> '.' then false
	else true
      end
  in

  let arg= Sys.argv in
  let err= "usage: " ^ arg.(0) ^ " [-c/-d/-e/-i] fichier.[ty/bty] [option]" in
  
  if Array.length arg = 3 then
    begin
      if arg.(1) = "-c" then
	if isTY(arg.(2)) then
	  begin
	    compile_bytecode arg.(2)
	  end
	else print_endline "erreur: le fichier n'est pas d'extansion .ty"
      else if arg.(1) = "-dinstr" then
	if isTY(arg.(2)) then
	  affiche_instr_source arg.(2)
	else if isBTY(arg.(2)) then
	  affiche_instr_bytecode arg.(2)
	else print_endline "erreur: le fichier n'est pas d'extansion .ty ou .bty"
      else if arg.(1) = "-e" then
	if isBTY(arg.(2)) then
	  execute_bytecode arg.(2)
	else print_endline "erreur: le fichier n'est pas d'extansion .bty"
      else if arg.(1) = "-i" then
	if isTY(arg.(2)) then
	  interp_source arg.(2)
	else print_endline "erreur: le fichier n'est pas d'extansion .ty"
      else print_endline err
    end
  else  if Array.length arg = 4  && arg.(2) = "-d" then
    begin
      if arg.(1) = "-c" then
	if isTY(arg.(3)) then
	  begin
	    compile_bytecode arg.(3)
	  end
	else print_endline "erreur: le fichier n'est pas d'extansion .ty"
      else if arg.(1) = "-dinstr" then
	print_endline "erreur: impossible d'utiliser -d et -dinstr"
      else if arg.(1) = "-e" then
	if isBTY(arg.(3)) then
	  execute_bytecode_debugg arg.(3)
	else print_endline "erreur: le fichier n'est pas d'extansion .bty"
      else if arg.(1) = "-i" then
	if isTY(arg.(3)) then
	  interp_source_debugg arg.(3)
	else print_endline "erreur: le fichier n'est pas d'extansion .ty"
      else print_endline err
    end
  else print_endline err;;
