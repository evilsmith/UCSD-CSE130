(* CSE 130: Programming Assignment 2 
 *     Name: Jingwen Xu
 *       ID: A10890992
 * Due Date: 4/23/2014
*)

(************************ Problem 1: Tail Recursion ***************************)

(* assoc : int * string * (string * int) list -> int
 * or more generally, assoc : 'a * 'b * ('b * 'a) list -> 'a
 * assoc (d,k,[(k1,v1);(k2,v2);(k3,v3);...]) searches the list for the first i such
 *   that ki = k.  If such a ki is found, then vi is returned.  Otherwise, if no such
 *   ki exists in the list, d is returned.
 * e.g. (assoc (-1,"william",[("ranjit",85);("william",23);("moose",44)]))
 *        returns 23
 *      (assoc (-1,"bob",[("ranjit",85);("william",23);("moose",44)]))
 *        returns -1
 *
 *  ** your function should be tail recursive **
*)

let rec assoc (d,k,l) =
  match l with
    | [] -> d
    | (s, i)::li -> if k = s then i else assoc(d, k, li)

(* uncomment after implementing assoc*)

let _ = assoc (-1,"william",[("ranjit",85);("william",23);("moose",44)]);;    

let _ = assoc (-1,"bob",[("ranjit",85);("william",23);("moose",44)]);;



(* removeDuplicates : int list -> int list 
 * or more generally, removeDuplicates : 'a list -> 'a list
 * (removeDuplicates l) is the list of elements of l with duplicates (second,
 * third ... occurrences) removed, and where the remaining elements 
 * appear in the same order as in l.
 * e.g. (removeDuplicates [1;6;2;4;12;2;13;6;9]) is [1;6;2;4;12;13;9]
 *
 *  ** your function "helper" should be tail recursive **
 * for this problem only, you may use the library function List.mem and List.rev
*)

let removeDuplicates l = 
  let rec helper (seen,rest) = 
    match rest with 
      | [] -> seen
      | h::t -> 
          let seen' = if (List.mem h seen) = false 
            then seen@[h] else seen in
          let rest' = t in 
            helper (seen',rest') 
  in
    (helper ([],l))

(* uncomment after implementing removeDuplicates *)

let _ = removeDuplicates [1;6;2;4;12;2;13;6;9];;


(* wwhile : (int -> int * bool) * int -> int
 * or more generally, ('a -> 'a * bool) * 'a -> 'a
 * wwhile (f,b) should call the function f on input b, to get a pair (b',c').
 *   wwhile should continue calling f on b' to update the pair as long as c' is true
 *   once f returns a c' that is false, wwhile should return b'
 * e.g. let f x = let xx = x*x*x in (xx,xx<100);;
 *   wwhile (f,2) should return 512
 *
 *  ** your function should be tail recursive **
*)
let rec wwhile (f,b) = 
  let helper = (f b) in 
    match helper with
      | (x, y) -> if y = false then x 
          else wwhile (f, x);;

(* uncomment after implementing wwhile*)

let f x = let xx = x*x*x in (xx, xx < 100) in
  wwhile (f, 2);;


(* fixpoint : (int -> int) * int -> int
 * or more generally, fixpoint : ('a -> 'a) * 'a -> 'a
 * fixpoint (f,b) repeatedly replaces b with f(b) until b=f(b) and then returns b
 * e.g. let g x = truncate (1e6 *. cos (1e-6 *. float x));;
 *   fixpoint (g,0) should return 739085    (this is because cos 0.739085 is approximately 0.739085)
*)

(* fill in the code wherever it says : failwith "to be written" *)
let fixpoint (f,b) = wwhile (( fun x -> let b = (f x) in (b, b != x)),b)

let g x = truncate (1e6 *. cos (1e-6 *. float x)) in fixpoint (g, 0);; 

let collatz n = match n with 1 -> 1 | _ when n mod 2 = 0 -> n/2 | _ -> 3*n + 1;;

let _ = fixpoint (collatz, 1) ;;
let _ = fixpoint (collatz, 3) ;;
let _ = fixpoint (collatz, 48) ;;
let _ = fixpoint (collatz, 107) ;;
let _ = fixpoint (collatz, 9001) ;;

(****************************************************************************************)
(*** Problem 2: Random Art **************************************************************)
(****************************************************************************************)

(* based on code by Chris Stone *) 

type expr = 
      VarX
    | VarY
    | Sine     of expr
    | Cosine   of expr
    | Average  of expr * expr
    | Times    of expr * expr
    | Thresh   of expr * expr * expr * expr	
    | Extra    of expr * expr * expr
    | Stuff    of expr

(* exprToString : expr -> string
   Complete this function to convert an expr to a string 
*)
let rec exprToString e = 
  let ex = exprToString in 
    match e with
      | VarX               -> "x"
      | VarY               -> "y"
      | Sine(t)            -> ( "sin(pi*"^(ex t)^")" )
      | Cosine(t)          -> ( "cos(pi*"^(ex t)^")" )
      | Average(s, t)      -> ( "(("^(ex s)^"+"^(ex t)^")/2)" )
      | Times(s, t)        -> ( (ex s)^"*"^(ex t) )
      | Thresh(s, t, u, v) -> ( "("^(ex s)^"<"^(ex t)^"?"^(ex u)^":"^(ex v)^")" )
      | Extra(s, t, u)     -> ( "sin(pi*"^(ex s)^") * cos ("^(ex t)^") * sin(" ^(ex u)^")")
      | Stuff(t)           -> ("cos(pi*"^"(sin(pi*"^(ex t)^")))")

(* uncomment after implementing exprToString *)

let sampleExpr = Thresh(VarX,VarY,VarX,(Times(Sine(VarX),Cosine(Average(VarX,VarY)))));;

let _ = exprToString sampleExpr




(* build functions:
   Use these helper functions to generate elements of the expr
   datatype rather than using the constructors directly.  This
   provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildExtra(e1, e2, e3)         = Extra(e1, e2, e3)
let buildStuff(e)                  = Stuff(e)



let pi = 4.0 *. atan 1.0

(* eval : expr -> float * float -> float 
   Evaluator for expressions in x and y *)

let rec eval (e, x, y) =
  match e with
    | Sine(t)            -> sin (pi *. eval (t, x, y))
    | Cosine(t)          -> cos (pi *. eval (t, x, y))
    | Average(s, t)      -> (((eval (s, x, y)) +. (eval (t, x, y))) /. 2.0)
    | Times(s, t)        -> ((eval (s, x, y)) *. (eval (t, x, y)))
    | Thresh(s, t, u, v) -> if ((eval (s, x, y)) < (eval (t, x, y))) 
        then (eval (u, x, y)) else (eval (v, x, y))
    | VarX               -> x
    | VarY               -> y
    | Extra(s, t, u)     -> sin(pi *. eval(s, x, y)) *. cos(pi*. eval(t, x, y)) *. sin(eval(u, x, y))
    | Stuff(t)           -> cos(pi *. sin(pi *. eval(t, x, y)))

;;

(* uncomment after implementing eval*)
let _ = eval (Sine(Average(VarX,VarY)),0.5,-0.5);;
let _ = eval (Sine(Average(VarX,VarY)),0.3,0.3);;
let _ = eval (sampleExpr,0.5,0.2);;



let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
    assert (-1.0 <= rv && rv <= 1.0);
    rv

let sampleExpr =
  buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
                                                              buildX()),buildTimes(buildCosine (buildCosine (buildAverage
                                                                                                               (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
                                                                                   buildCosine (buildTimes (buildSine (buildCosine
                                                                                                                         (buildY())),buildAverage (buildSine (buildX()), buildTimes
                                                                                                                                                                           (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(******************* Functions you need to write **********)

(* build: (int*int->int) * int -> Expr 
   Build an expression tree.  The second argument is the depth, 
   the first is a random function.  A call to rand(2,5) will give
   you a random number in the range [2,5].

   Your code should call buildX, buildSine, etc. to construct
   the expression.
*)

let rec build (rand, depth) = 
  if (depth > 0) then
    let r = (rand(0, 30)) in
    let d = (depth - 1) in 

      match r with 
        | 0 -> buildSine((build(rand, d)))
        | 1 -> buildSine((build(rand, d)))
        | 2 -> buildSine((build(rand, d)))

        | 3 -> buildTimes((buildX()), (build(rand,d)))
        | 4 -> buildTimes((buildX()), (build(rand,d)))
        | 5 -> buildTimes((buildX()), (build(rand,d)))
        | 6 -> buildTimes((buildX()), (build(rand,d)))
        | 7 -> buildTimes((buildX()), (build(rand,d)))

        | 8 -> buildAverage((build(rand,d)), (build(rand, d)))
        | 9 -> buildAverage((build(rand,d)), (build(rand, d)))
        | 10 -> buildAverage((build(rand,d)), (build(rand, d)))

        | 11 -> buildCosine((build(rand, d)))
        | 12 -> buildCosine((build(rand, d)))
        | 13 -> buildCosine((build(rand, d)))

        | 14 -> buildThresh((build(rand, d)), (buildX()), (buildY()), (buildX()))

        | 15 -> buildExtra((build(rand, d)), (buildX()), (buildY()))
        | 16 -> buildExtra((build(rand, d)), (buildX()), (buildY()))
        | 17 -> buildExtra((build(rand, d)), (buildX()), (buildY()))
        | 18 -> buildExtra((build(rand, d)), (buildX()), (buildY()))

        | 19 -> buildStuff((build(rand, d)))
        | 20 -> buildStuff((build(rand, d)))
        | 21 -> buildStuff((build(rand, d)))
        | 22 -> buildStuff((build(rand, d)))
        | 23 -> buildStuff((build(rand, d)))

        | 24  -> buildSine((build(rand, d)))
        | 25  -> buildSine((build(rand, d)))
        | 26  -> buildSine((build(rand, d)))

        | 27 -> buildTimes((buildX()), (build(rand,d)))
        | 28 -> buildTimes((buildX()), (build(rand,d)))
        | 29 -> buildTimes((buildX()), (build(rand,d)))
        | _ -> buildThresh((build(rand, d)), (buildX()), (buildY()), (buildX()))
  else 
    let r = (rand(0, 1)) in
      match r with 
        | 0 -> buildX()
        | _ -> buildY()

(* g1,g2,g3,c1,c2,c3 : unit -> int * int * int
 * these functions should return the parameters needed to create your 
 * top three color / grayscale pictures.
 * they should return (depth,seed1,seed2)
*)

let g1 () = (11, 0, 1)  
let g2 () = (9, 9, 33)
let g3 () = (5, 13, 14) 

let c1 () = (8, 17, 18)
let c2 () = (12, 0, 1)
let c3 () = (10, 13, 14)


(******************** Random Number Generators ************)

(* makeRand int * int -> (int * int -> int)
   Returns a function that, given a low and a high, returns
   a random int between the limits.  seed1 and seed2 are the
   random number seeds.  Pass the result of this function
   to build 

   Example:
   let rand = makeRand(10,39) in 
   let x =  rand(1,4) in 
   (* x is 1,2,3, or 4 *)
*)

let makeRand (seed1, seed2) = 
  let seed = (Array.of_list [seed1;seed2]) in
  let s = Random.State.make seed in
    (fun (x,y) -> (x + (Random.State.int s (y-x))))


let rec rseq g r n =
  if n <= 0 then [] else (g r)::(rseq g r (n-1))

(********************* Bitmap creation code ***************)

(* 
You should not have to modify the remaining functions.
Add testing code to the bottom of the file.
*)

(* Converts an integer i from the range [-N,N] into a float in [-1,1] *)
let toReal (i,n) = (float_of_int i) /. (float_of_int n)

(* Converts real in [-1,1] to an integer in the range [0,255]  *)
let toIntensity z = int_of_float (127.5 +. (127.5 *. z))


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
*)

let rec ffor (low,high,f) = 
  if low > high then () else 
    let _ = f low in 
      ffor (low+1,high,f)

(* emitGrayscale :  ((real * real) -> real) * int -> unit
   emitGrayscale(f, N) emits the values of the expression
   f (converted to intensity) to the file art.pgm for an 
   2N+1 by 2N+1 grid of points taken from [-1,1] x [-1,1].

   See "man pgm" on turing for a full description of the file format,
   but it's essentially a one-line header followed by
   one byte (representing gray value 0..255) per pixel.
*)

let emitGrayscale (f,n,name) =
  (* Open the output file and write the header *)
  let fname  = ("art_g_"^name) in
  let chan = open_out (fname^".pgm") in
  (* Picture will be 2*N+1 pixels on a side *)
  let n2p1 = n*2+1 in   
  let _ = output_string chan (Format.sprintf "P5 %d %d 255\n" n2p1 n2p1) in
  let _ = 
    ffor (-n, n, 
          fun ix ->
            ffor (-n, n, 
                  fun iy ->
                    (* Convert grid locations to [-1,1] *)
                    let x = toReal(ix,n) in
                    let y = toReal(iy,n) in
                    (* Apply the given random function *)
                    let z = f (x,y) in
                    (* Convert the result to a grayscale value *)
                    let iz = toIntensity(z) in
                      (* Emit one byte for this pixel *)
                      output_char chan (char_of_int iz))) in 
    close_out chan;
    ignore(Sys.command ("convert "^fname^".pgm "^fname^".jpg"));
    ignore(Sys.command ("rm "^fname^".pgm"))

(* doRandomGray : int * int * int -> unit
   Given a depth and two seeds for the random number generator,
   create a single random expression and convert it to a
   grayscale picture with the name "art.pgm" *)

let doRandomGray (depth,seed1,seed2) =
  (* Initialize random-number generator g *)
  let g = makeRand(seed1,seed2) in
  (* Generate a random expression, and turn it into an ML function *)
  let e = build (g,depth) in
  let _ = print_string (exprToString e) in
  let f = eval_fn e in
  (* 301 x 301 pixels *)
  let n = 150 in
  (* Emit the picture *)
  let name = Format.sprintf "%d_%d_%d" depth seed1 seed2 in
    emitGrayscale (f,n,name)

(* uncomment when you have implemented `build` *)

let _ = emitGrayscale (eval_fn sampleExpr, 150, "sample") ;;


(* emitColor : (real*real->real) * (real*real->real) *
   (real*real->real) * int -> unit
   emitColor(f1, f2, f3, N) emits the values of the expressions
   f1, f2, and f3 (converted to RGB intensities) to the output
   file art.ppm for an 2N+1 by 2N+1 grid of points taken 
   from [-1,1] x [-1,1].

   See "man ppm" on turing for a full description of the file format,
   but it's essentially a one-line header followed by
   three bytes (representing red, green, and blue values in the
   range 0..255) per pixel.
*)
let emitColor (f1,f2,f3,n,name) =
  (* Open the output file and write the header *)
  let fname  = ("art_c_"^name) in
  let chan = open_out (fname^".ppm") in
  (* Picture will be 2*N+1 pixels on a side *)
  let n2p1 = n*2+1 in   
  let _ = output_string chan (Format.sprintf "P6 %d %d 255\n" n2p1 n2p1) in
  let _ = 
    ffor (-n, n, 
          fun ix ->
            ffor (-n, n, 
                  fun iy ->
                    (* Convert grid locations to [-1,1] *)
                    let x = toReal(ix,n) in
                    let y = toReal(iy,n) in
                    (* Apply the given random function *)
                    let z1 = f1 (x,y) in
                    let z2 = f2 (x,y) in
                    let z3 = f3 (x,y) in

                    (* Convert the result to a grayscale value *)
                    let iz1 = toIntensity(z1) in
                    let iz2 = toIntensity(z2) in
                    let iz3 = toIntensity(z3) in

                      (* Emit one byte per color for this pixel *)
                      output_char chan (char_of_int iz1);
                      output_char chan (char_of_int iz2);
                      output_char chan (char_of_int iz3);
                 )) in  
    close_out chan;
    ignore(Sys.command ("convert "^fname^".ppm  "^fname^".jpg"));
    ignore(Sys.command ("rm "^fname^".ppm")) 

(* doRandomColor : int * int * int -> unit
   Given a depth and two seeds for the random number generator,
   create a single random expression and convert it to a
   color picture with the name "art.ppm"  (note the different
   extension from toGray) 
*)
let doRandomColor (depth,seed1,seed2) =
  (* Initialize random-number generator g *)
  let g = makeRand (seed1,seed2) in
  (* Generate a random expression, and turn it into an ML function *)
  let e1 = build (g, depth) in
  let e2 = build (g, depth) in
  let e3 = build (g, depth) in

  let _ = Format.printf "red   = %s \n" (exprToString e1) in
  let _ = Format.printf "green = %s \n" (exprToString e2) in
  let _ = Format.printf "blue  = %s \n" (exprToString e3) in

  let f1 = eval_fn e1 in
  let f2 = eval_fn e2 in
  let f3 = eval_fn e3 in

  (* 301 x 301 pixels *)
  let n = 150 in
  (* Emit the picture *)
  let name = Format.sprintf "%d_%d_%d" depth seed1 seed2 in
    emitColor (f1,f2,f3,n,name)

(****************************************************************************************)
(*** Testing Code ***********************************************************************)
(****************************************************************************************)

type test = unit -> string

let key = "" (* change *)
let prefix130 = "130" (* change *)
let print130 s = print_string (prefix130^">>"^s)

exception ErrorCode of string

exception TestException

type result = Pass | Fail | ErrorCode of string

let score = ref 0
let max = ref 0
let timeout = 300

let runWTimeout (f,arg,out,time) = 
  try if compare (f arg) out = 0 then Pass else Fail
  with e -> (print130 ("Uncaught Exception: "^(Printexc.to_string e)); ErrorCode "exception") 

let testTest () =
  let testGood x = 1 in
  let testBad x = 0 in 
  let testException x = raise TestException in
  let rec testTimeout x = testTimeout x in
    runWTimeout(testGood,0,1,5) = Pass &&  
    runWTimeout(testBad,0,1,5) = Fail &&  
    runWTimeout(testException,0,1,5) = ErrorCode "exception" && 
    runWTimeout(testTimeout,0,1,5) = ErrorCode "timeout"

let runTest ((f,arg,out),points,name) =
  let _   = max := !max + points in
  let outs = 
    match runWTimeout(f,arg,out,timeout) with 
        Pass -> (score := !score + points; "[pass]")
      | Fail -> "[fail]"
      | ErrorCode e -> "[error: "^e^"]"  in
    name^" "^outs^" ("^(string_of_int points)^")\n"

(* explode : string -> char list *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
    _exp 0

let implode cs = 
  String.concat "" (List.map (String.make 1) cs)

let drop_paren s = 
  implode (List.filter (fun c -> not (List.mem c ['(';' ';')'])) (explode s))

let eq_real p (r1,r2) = 
  (r1 -. r2) < p || (r2 -. r1) < p

let mkTest f x y name = runTest ((f, x, y), 1, name)

let badTest () = "WARNING: Your tests are not valid!!\n"

let scoreMsg () = 
  Format.sprintf "Results: Score/Max = %d / %d \n" !score !max 

let sampleTests =
  [
    (fun () -> mkTest
                 assoc
                 (-1, "william", [("ranjit",85);("william",23);("moose",44)])
                 23
                 "sample: assoc 1"
    );
    (fun () -> mkTest 
                 assoc
                 (-1, "bob", [("ranjit",85);("william",23);("moose",44)])
                 (-1)
                 "sample: assoc 2"
    ); 
    (fun () -> mkTest 
                 removeDuplicates
                 [1;6;2;4;12;2;13;6;9]
                 [1;6;2;4;12;13;9]
                 "sample: removeDuplicates 2"
    );
    (fun () -> mkTest 
                 removeDuplicates
                 [1;1;1]
                 [1]
                 "sample: removeDuplicates 2"
    );

    (fun () -> mkTest 
                 wwhile 
                 ((fun x -> let xx = x*x*x in (xx, xx < 100)), 2) 
                 512 
                 "sample: wwhile 1"
    ); 
    (fun () -> mkTest 
                 fixpoint
                 ((fun x -> truncate (1e6 *. cos (1e-6 *. float x))), 0)
                 739085
                 "sample: fixpoint 1"
    ); 

    (fun () -> mkTest 
                 emitGrayscale
                 (eval_fn sampleExpr, 150,"sample")
                 ()
                 "sample: eval_fn 1: manual"
    ); 
    (fun () -> mkTest 
                 emitGrayscale
                 (eval_fn sampleExpr2, 150,"sample2")
                 ()
                 "sample: eval_fn 2: manual"
    );

    (fun () -> mkTest 
                 (fun () -> doRandomGray (g1 ()))
                 ()
                 ()
                 "sample: gray 1 : manual"
    );
    (fun () -> mkTest 
                 (fun () -> doRandomGray (g2 ()))
                 ()
                 ()
                 "sample: gray 2 : manual"
    );
    (fun () -> mkTest 
                 (fun () -> doRandomGray (g3 ()))
                 ()
                 ()
                 "sample: gray 3 : manual"
    );

    (fun () -> mkTest 
                 (fun () -> doRandomColor (c1 ()))
                 ()
                 ()
                 "sample: color 1 : manual"
    );
    (fun () -> mkTest 
                 (fun () -> doRandomColor (c2 ()))
                 ()
                 ()
                 "sample: color 2 : manual"
    );
    (fun () -> mkTest 
                 (fun () -> doRandomColor (c3 ()))
                 ()
                 ()
                 "sample: color 3 : manual"
    )] 

let doTest f = 
  try f () with ex -> 
    Format.sprintf "WARNING: INVALID TEST THROWS EXCEPTION!!: %s \n\n"
      (Printexc.to_string ex)

let _ =
  let report = List.map doTest sampleTests                in
  let _      = List.iter print130 (report@([scoreMsg()])) in
  let _      = print130 ("Compiled\n")                    in
    (!score, !max)
