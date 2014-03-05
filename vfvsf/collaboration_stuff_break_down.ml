open ParseTree;;
open List;;
open Lexer;;
open Parser;;

(* This is where all the variables are stored it is a map from strings (the var name)         *)
(* To their int values. There are two maps. The global map is available throughout the entire *)
(* run of the program and the localVars are cleared every time we consume the next elements   *)
(* on the stream                                                                              *)
module VarMap = Map.Make(String);;
let localVars =  ref VarMap.empty;;
let globalVars =  ref VarMap.empty;;

(* Stream information *)
let outStream = ref [];;
let inStream = ref [];;
let streamC = ref 0;;
let streamLen = ref 0;;

(* The entry point is the process block and is stored the first time it is reached *)
(* so that we can come back to it as more ints get passed from the input stream    *)
let entryPointSet = ref false;;
let entryPoint = ref (Leaf(0, 0));;

(* Stores the current line number for this part of the tree *)
(* Used for printing helpful error message to the user      *)
let lineN = ref 0;;

let rec parseTree tree = 
    
    (**)
    let handleBodyEnd x1 = 
        (parseTree x1)
    in
    let handleBodyExt x1 x2 = 
        (parseTree x1);
        (parseTree x2)
    in
    let handleInject x1 =
        let v = (parseTree x1) in
        outStream := v :: !outStream;
        0
    in
    let handleIf x1 x2 = 
        if (parseTree x1)!=0 then (parseTree x2) else 0
    in
    let handleIfElse x1 x2 x3 = 
        if (parseTree x1)!=0 then (parseTree x2) else (parseTree x3)
    in

    let handleWhile x1 x2 = 
        while ((parseTree x1)!=0) do
            (parseTree x2);
        done;
        0
    in

    let handleAss label value = 
        let resError l= 
            (Printf.fprintf stderr "Variable '%s' is reserved and can't be assigned\nLine number:\t%d\nUnable to complete execution\n" l !lineN); 
            (exit 9)
        in
        if (label="COUNT" or label="LENGTH" or label="stream") then 
             (resError label)
        else
            try 
                (* If it's a global variable save it in the global map *)
                (VarMap.find label !globalVars);
                globalVars := (VarMap.add label (parseTree value) !globalVars);
                0  
            with Not_found ->
                (* If not a global var then save it as a local var *)
                localVars := (VarMap.add label (parseTree value) !localVars);
                0
    in

    let handleVar label = 
        try
            (VarMap.find label !localVars)
        with Not_found -> try 
            (VarMap.find label !globalVars)              
        with Not_found -> (Printf.fprintf stderr "Variable '%s' can't be found\nLine number:\t%d\nUnable to complete execution\n" label !lineN); (exit 4)
    in

    let incrementVar label n = 
        let newV = (handleVar label) + n in
            (handleAss label (Leaf(newV, -1))); newV
    in

    let multVar label n = 
        let newV = (handleVar label) * n in
            (handleAss label (Leaf(newV, -1))); newV
    in

    let divVar label n = 
        let newV = (handleVar label) / n in
            (handleAss label (Leaf(newV, -1))); newV
    in

    let handleAssG label value = 
        globalVars := VarMap.add label value !globalVars;
        0
    in

    let handleStreamV streamN = 
        try
            (VarMap.find ("$" ^ (string_of_int streamN)) !localVars)
        with Not_found -> (Printf.fprintf stderr "Stream %d is empty\nLine number:\t%d\nUnable to complete execution\n" streamN !lineN); flush stderr; (exit 1)
    in

    let handleExit code = 
        if (code == 0) then 
            raise Lexer.Eof
        else 
            (Printf.fprintf stderr "Code exited early\nExit code:\t%d\nLine number:\t%d\nUnable to complete execution\n" code !lineN); flush stderr; 
            exit(code)
    in

    (* Should never be run as the parser should have caught this mistake *)
    (* The case has to be handled though to stop OCaml complaining       *)
    let error x1 = 
        (Printf.fprintf stderr "Error: Invalid syntax \"%s\"\nLine number:\t%d\nUnable to complete execution\n" x1 !lineN); flush stderr; (exit 2)
    in

    let updateEntryPoint node = 
        (entryPoint := node); 
        (entryPointSet := true); 
        0
    in

    (* Matches every possible type of node and sends it to the appropriate function     *)
    (* The "lineN := line" bit makes the current line number of the part of the tree we *)
    (* are on visible to the functions above so they can print helpful error messages   *)
    match tree with 
        | Name( x1, l )                           -> lineN := l; (handleVar x1)
        | Leaf( x1, line )                        -> lineN := line; x1
        | Node1("bodyEnd", x1, line)              -> lineN := line; if (!entryPointSet == false) then (updateEntryPoint tree) else 0; (handleBodyEnd x1) 
        | Node1("inj", x1, l)                     -> lineN := l; (handleInject x1)
        | Node1("injName", x1, line)              -> lineN := line; (handleInject x1)
        | Node1("streamV", x1,line2) -> lineN := line2; (handleStreamV (parseTree x1))
        | Node1("value", x1, line)                -> lineN := line; (parseTree x1)
        | Node1("-", x1, line)                    -> lineN := line; -(parseTree x1)
        | Node1("NOT", x1, line)                  -> lineN := line; if ((parseTree x1) == 0) then 1 else 0
        | Node1("++", Name( x1, line1 ), line2)   -> lineN := line2; (incrementVar x1 1)
        | Node1("--", Name( x1, line1 ), line2)   -> lineN := line2; (incrementVar x1 (-1))
        | Node1("exit", x1, line)                 -> lineN := line; (handleExit (parseTree x1))
        | Node1( x1, x2, line)                    -> lineN := line;  (error x1)
        | Node2( "bodyExt", x1, x2, line)         -> lineN := line; if (!entryPointSet == false) then (updateEntryPoint tree) else 0; (handleBodyExt x1 x2)
        | Node2( "assG", Name( x1, line1 ), x2, line3)    
                                                  -> lineN := line3;  (handleAssG x1 (parseTree x2))
        | Node2( "assG", x1, x2, line3)           -> (Printf.fprintf stderr "Invalid global variable declaration\nLine number:\t%d\nUnable to complete execution\n" line3); flush stderr; (exit 17)
        | Node2( "GvarExt", x1, x2, line)         -> lineN := line; (parseTree x1); (parseTree x2)
        | Node2( "if", x1, x2, line)              -> lineN := line; (handleIf x1 x2)
        | Node2("while", x1, x2, line)            -> lineN := line; (handleWhile x1 x2)
        | Node2( "ass", Name( x1, line1 ), x2, line)    
                                                  -> lineN := line; (handleAss x1 x2)
        | Node2("mainExtGV", x1, x2, line)        -> lineN := line; (parseTree x1); (parseTree x2)
        | Node2("+", x1, x2, line)                -> lineN := line; (parseTree x1) + (parseTree x2)
        | Node2("-", x1, x2, line)                -> lineN := line; (parseTree x1) - (parseTree x2)
        | Node2("*", x1, x2, line)                -> lineN := line; (parseTree x1) * (parseTree x2)
        | Node2("/", x1, x2, line)                -> lineN := line; (parseTree x1) / (parseTree x2)
        | Node2("<", x1, x2, line)                -> lineN := line; if ((parseTree x1) < (parseTree x2)) then 1 else 0
        | Node2(">", x1, x2, line)                -> lineN := line; if ((parseTree x1) > (parseTree x2)) then 1 else 0
        | Node2("<=", x1, x2, line)               -> lineN := line;  if ((parseTree x1) <= (parseTree x2)) then 1 else 0
        | Node2(">=", x1, x2, line)               -> lineN := line;  if ((parseTree x1) >= (parseTree x2)) then 1 else 0
        | Node2("==", x1, x2, line)               -> lineN := line; if ((parseTree x1) == (parseTree x2)) then 1 else 0
        | Node2("!=", x1, x2, line)               -> lineN := line; if ((parseTree x1) != (parseTree x2)) then 1 else 0
        | Node2("and", x1, x2, line)              -> lineN := line; if ( ((parseTree x1) != 0) && ((parseTree x2) != 0) ) then 1 else 0
        | Node2("or", x1, x2, line)               -> lineN := line; if ( ((parseTree x1) != 0) ||  ((parseTree x2) != 0) ) then 1 else 0
        | Node2("+=", Name(x1, line1), x2, line2) -> lineN := line2; (incrementVar x1 (parseTree x2))
        | Node2("-=", Name(x1, line1), x2, line2) -> lineN := line2; (incrementVar x1 (-(parseTree x2)))
        | Node2("*=", Name(x1, line1), x2, line2) -> lineN := line2; (multVar x1 (parseTree x2))
        | Node2("/=", Name(x1, line1), x2, line2) -> lineN := line2; (divVar x1 (parseTree x2))
        | Node2( x1, x2, x3, line)                -> lineN := line; (error x1)   
        | Node3( "if", x1, x2, x3, line)          -> lineN := line;  (handleIfElse x1 x2 x3)
        | Node3( x1, x2, x3, x4, line)            -> lineN := line;  (error x1)
;;

let syntaxError lexbuf = 
        (Printf.fprintf stderr "Syntax error\nLine number:\t%d\nNear token:\t'%s'\nUnable to complete execution\n" !lineno (Lexing.lexeme lexbuf)); 
        flush stderr
;;


let rec print_list = function 
    [] -> ()
  | h::[] -> (Printf.printf "%d" h)
  | h::t -> (Printf.printf "%d " h); print_list t;;

(* Reads the input streams from stdin and formats them into a list of list of ints *)
let getInStreams = 
    let currStream = ref 0 in (* Current stream is empty*)
    (* Note n is the number of streams and k is the length of the streams *)
    let rec collect_lists n k =
         match n with
          | 0 -> [] (* No stream then return empty list *) 
          (* Else: Count number of streams. Read each line of stream recursively => read_line => "1 2 3 4"; then split " 
          Then Str.split give ["1"; "2"; "3"] ; . Then make a ref to this list of lists of int    *)
          | x -> currStream := x; ref (List.map int_of_string (Str.split (Str.regexp " ") (read_line ()))) :: collect_lists (x - 1) k in
    let helper length count =
        streamC := count;
        streamLen := length;
        if ( !streamC == 0 ) then ((Printf.fprintf stderr "Stream error: No input stream\n\nUnable to complete execution\n"); flush stderr; (exit 30)) else 0;
        if ( !streamLen == 0 ) then ((Printf.fprintf stderr "Stream error: Empty input streams\n\nUnable to complete execution\n"); flush stderr; (exit 30)) else 0;
        List.rev (collect_lists count length) in
    try
      helper (read_int ()) (read_int ()) 
    with 
      End_of_file -> ((Printf.fprintf stderr "Stream error: Missing input stream\nStream number:\t%d\nUnable to complete execution\n" 
                                ((!currStream))); 
                            flush stderr; (exit 30))
;;

(* Consumes the next set of elements off the streams and binds them to their values *)
let rec setLocalStreamBindings n inS = 
    let ass label value = localVars := (VarMap.add label value !localVars); in
    match !inS with
        | [] -> ref ()
        | x1 :: x2 ->   (ass ("$" ^ (string_of_int n)) (hd !x1)); 
                        (x1 := (tl !x1)); 
                        let t = ref x2 in
                            (setLocalStreamBindings (n+1) t)
;;

(* This is the bit which runs the code!                                                   *)
(* This function collects the input stream, parses the users program then runs it i times *)
(* where i is the depth of the streams                                                    *)
let run =
   inStream := (getInStreams);
   (*(print_list (List.rev !(hd !inStream))); print_string "\n"; print_string "\n"; print_int !streamLen; print_string "<--\n";*)
   globalVars := (VarMap.add "COUNT" !streamC !globalVars);
   globalVars := (VarMap.add "LENGTH" !streamLen !globalVars);
   try
       let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
       try
           while true do
               let result = (Parser.main Lexer.token lexbuf) in
                entryPoint := result;
                for i = 0 to !streamLen-1 do
                    localVars := VarMap.empty;
                    (setLocalStreamBindings 0 inStream);
                    flush stdout;
                    parseTree !entryPoint;
                done;
           done
       with  Parsing.Parse_error -> (syntaxError lexbuf); exit 1
   with 
    | Lexer.Eof ->
       print_int (List.length !outStream); print_string "\n";
       (print_list (List.rev !outStream)); print_string "\n"; print_string "\n";
       exit 0
    | Failure f -> 
            if (f = "hd") then ((Printf.fprintf stderr "Stream error: Input stream too short\n\nUnable to complete execution\n" ); (exit 31)) 
            else
            ((Printf.fprintf stderr "Syntax error\nError string:\t\"%s\"\nLine number:\t%d\nUnable to complete execution\n" f !lineno);
            (exit 14))
