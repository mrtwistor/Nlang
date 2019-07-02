(*The N programming language and its C dialect*)
(*Author: Jonathan Holland*)
(*Date: 09-14-2018*)
(*Version: 0*)
(*Source language: Ocaml v 4.05.0*)
(*License: GPL version 2*)

(*The purpose of the N language is to illustrate a computing language 
having only one primitive type, the natural numbers, with a single element O 
and a constructor S (successorship), and induction (recursion).  We also
include a computation of the Ackermann function, showing that N is able
to compute functions that are not primitive-recursive, and also illustrate
how to define the primitive recursion operator i, and define elementary
arithmetic in terms of i.

The code following the comments includes the interpreter and compiler 
for the N language.  The "C dialect" is the following ruleset on a C program.
 *)


(****************************RULES***************************)
(*
The rules for the C dialect are as follows:

1.   The program begins with the header

////////////N header/////////////
#define R return
#define N unsigned
#define Q(a,b,c,d) a==b?c:d;
N O;
N S(N a){R++a;}
/////////////////////////////////


2. No lower case letters or numerals can be used, nor may any of
the special characters on the following line--
    #"!&^*%$@./-|\'`='~[]<>+"
(As a consequence, no arithmetic can be performed, no bitwise operations, inequality, pointer operations, etc.)

3. The code must compile as a valid C program, with the compiler flags "-c -Wall".  

Here is a complete example of compliant code,
using all intended features of the dialect (see
note below regarding first-class functions).

/////////////////////////////////
////////////N header/////////////
#define R return
#define N unsigned
#define Q(a,b,c,d) a==b?c:d;
N O;
N S(N a){R++a;}
/////////////////////////////////
N PRED(N A,N B){R Q(A,S(B),B,PRED(A,S(B)));}
N IND(N Z,N S(N,N),N NN){
  R NN?S(PRED(NN,O),IND(Z,S,PRED(NN,O))):Z;
}
N PLUS(N A, N B){
  N M(N X, N Y){R S(Y);}
  R IND(A,M,B);
}
N TIMES(N A, N B){
  N M(N X, N Y){R PLUS(A,Y);}
  R IND(O,M,B);
}
N POWER(N A, N B){
  N M(N X, N Y){R TIMES(A,Y);}
  R IND(S(O),M,B);
}

N PLUSREC(N A,N B){
  R B?S(PLUSREC(A,PRED(B,O))):A;
}
N TIMESREC(N A,N B){
  R B?PLUSREC(TIMESREC(A,PRED(B,O)),A):O;
}
N M(N A, N B){R B?M(PRED(A,O),PRED(B,O)):A;}
N D(N A, N B){R A?S(D(M(A,B),B)):O;}
N KNUTH(N M,N A,N B){
  R M?B?KNUTH(PRED(M,O),A,KNUTH(M,A,PRED(B,O))):S(O):TIMESREC(A,B);}
/////////////////////////////
 *)


(***************NOTES****************)
(*
- Note that conditionals are still possible, but only via with the ternary conditional operator ..?..:..; 
Loops are still possible, via recursion.

- Prototypes are necessary to allow mutually recursive functions to be declared prior to their
main body.  Functions are first-class in the C dialect, but that feature is not (yet) 
implemented by the interpreter.

- I believe no rule is needed to prevent casts, because *my* compiler gives a warning on something like this: 
N A(){R O;} N B(){R (N)A;}
However, to prevent this unintended feature for sure, we have added the rule: 

Every balanced string of parentheses (...) must be immediately follwed
by a special character
?{:),;

to the sed validation script compile.sh

- To interact with the machine, it is permissible to use a main subroutine, with
standard C functions like "printf", or pretty much anything else.  But care should 
be made to ensure that no relevant calculations are done outside the N machine.  
So printf is (probably) ok, but mucking around with global variable declarations 
is not, for example.

- The intended formal semantics are given in the module N below.  A parser
for the C dialect is also available, but not included here.  


 *)
(****************Compilation instructions******************)
(*

To compile code from the abstract machine, modify the section:
module Example = 
struct
...
my_program = [

]
end
to include your program.  A simple example showing the syntax is there.

Then compile the source ocaml file with

ocamlc -o n n.ml

This should produce an executable file n.  When run, it outputs the compiled
C code.

To run the interpreter, first compile the source ocaml file with
ocamlc -c n.ml

Then launch the ocaml interactive REPL with
ocaml n.cmo

To access the modules, try (for instance)
N.N.Example.result()

 *)


module N=struct

  (*Create the hash table type of function identifiers and function
    bodies
   *)
  module FunctionIdentifier =
    struct
      type t = char
      let compare = Pervasives.compare
    end
  module Functions = Map.Make(FunctionIdentifier)

  type statement = Apply of FunctionIdentifier.t * statement list
                 | If of statement * statement * statement
                 | S of statement
                 | O
                 | Slot of int
                 | Q of statement * statement * statement * statement

  (*Lambda expressions: we don't really have *true* lambda expressions
    in v 0, so this might be wishful thinking.
   *)
  type arity = int
  type lambda_expression = Lambda of arity * statement

  (*Each line is a definition of a function, as a lambda expression:*)
  type line = Def of FunctionIdentifier.t * lambda_expression

  type program = line list

  (*Reads the program and stores the function definitions
   in the hash table:*)
  let read_line program_functions =
    function Def (f,Lambda(n,s)) ->
      Functions.add f (Lambda(n,s)) program_functions
  let rec read_program (p:program) program_functions =
    match p with
    | [] -> program_functions
    | hd :: tl -> read_program tl (read_line program_functions hd)

  module Interpreter=struct
    let rec interpret_function
              ( n : arity )
              ( arg_list : statement list )
              ( program_functions : lambda_expression Functions.t) =
      function
      | Slot r -> List.nth arg_list r
      | S x -> S ( interpret_function n arg_list program_functions x )
      | Apply ( f, fargs )
        -> 
         let Lambda (k,fdef) = (Functions.find f program_functions) in
         interpret_function k       
           (List.map
              (interpret_function n arg_list program_functions)
              fargs) program_functions
           fdef
      | O -> O
      | Q (a,b,c,d) -> let a' = (interpret_function n arg_list program_functions a)
                    and b' = (interpret_function n arg_list program_functions b) in
                    if a' = b' 
                    then c else d
      | If (a,b,c) ->
         (
           match (interpret_function n arg_list program_functions a) with
           | O -> (interpret_function n arg_list program_functions c)
           | _ -> (interpret_function n arg_list program_functions b)
         )
  end

  module Compiler=struct
    let rec range a b =
      if (a > b) 
      then []
      else a :: range (a+1) b
    let rec pad_int = function
        0 -> ""
      | n -> "O" ^ pad_int (n-1)
    let make_variables =
      function
        0 -> "()"
      | n -> (List.fold_left
                (fun ac x -> ac ^ ",N X" ^ (pad_int x))
                ("(N X")
                (range 1 (n-1))
             ) ^ ")"
           
    let rec compile_statement =
      function
      | O -> "O"
      | S x -> "S(" ^ compile_statement x ^ ")"   
      | If (a,b,c) ->   " " ^ compile_statement a
                      ^ "?" ^ compile_statement b
                      ^ ":" ^ compile_statement c
      | Slot n -> "X" ^ (pad_int n)
      | Apply (f, []) ->  (String.make 1 f) ^ "()"
      | Apply (f, h :: t) ->  
         (
           List.fold_left
             ( fun ac x -> ac ^ ", " ^ (compile_statement x) )
             ( (String.make 1 f) ^ "(" ^ (compile_statement h) )
             t 
         ) ^ ")"
      | Q (a,b,c,d) ->
         (compile_statement a) ^ "==" ^ (compile_statement b) ^ "?" ^ (compile_statement c) ^ ":" ^ (compile_statement d)
        
        
    let compile program =
      let preamble=
        "/////////////////////////////////
////////////N header/////////////
#define R return
#define N unsigned
#define Q(a,b,c,d) a==b?c:d;
N O;
N S(N a){R++a;}
/////////////////////////////////
         "
      and functions = read_program program Functions.empty
      in let fundecl =
           Functions.fold
             (fun
                key
                (Lambda (n,x))
                ac
              ->
               ac ^ "N "
                 ^ (String.make 1 key)
                 ^ (make_variables n) ^ ";"
             )
             functions
             ""
         in let body =
              Functions.fold
                (fun key (Lambda (n,x)) ac ->
                  ac ^ "N "
                  ^ (String.make 1 key)
                  ^ (make_variables n)
                  ^ "{ R " ^ (compile_statement x) ^ ";}"
                )
                functions ""
            in preamble ^ fundecl ^ body
  end

  module Util=struct
    (*utility routines to go to and from builtin ocaml types, 
    for debugging and  illustration:*)
    let rec nest f x =
      function
      | O->x
      | S n-> nest f (f x) n
      | _ -> failwith
               "Nest argument should a pure natural number, not an unevaluated statement."
    let to_int = nest succ 0
    let rec of_int = function
      | 0 -> O
      | n -> S(of_int (n-1))
  end
end

           
module Example = struct
  open N
  open Interpreter
  open Util
  let myprogram  =
    [
      Def ('E',
           Lambda (2,
                       Q (Slot 0, S(Slot 1),
                       Slot 1,
                       Apply('E', [Slot 0; S(Slot 1)]))
             )
        );
      Def ('P',
           Lambda(2,
                  If (Slot 1,
                      S ( Apply ('P',
                                 [Slot 0;
                                  Apply ('E',
                                         [Slot 1; O])
                                 ]
                        )),
                      Slot 0
        )));
      Def ('T',
           Lambda(2,
                  If(Slot 1,
                     Apply ( 'P',
                             [
                               Apply ( 'T',
                                       [
                                         Slot 0;
                                         Apply ( 'E',
                                                 [Slot 1; O]
                                           );
                                         
                                       ]
                                 );
                               Slot 0
                             ]
                       ),
                     O)));
      Def ('K',
           Lambda(3,
                  If(Slot 0,
                     If(Slot 2,
                        Apply ( 'K',
                                [
                                  Apply ('E', [Slot 0; O]);
                                  Slot 1;
                                  Apply ('K',
                                         [
                                           Slot 0;
                                           Slot 1;
                                           Apply ('E', [Slot 2; O]);
                                         ]
                                    )
                          ]),
                        S O),
                     Apply ('T', [Slot 1; Slot 2])
             )))
    ]
    
    
  let myfunctions = read_program myprogram Functions.empty

  let compiled = Compiler.compile myprogram
          
  let result () = (*2^2^2^2, but takes many hours to complete in the interpreter!*)
    to_int (interpret_function 0 [] myfunctions (Apply ('K', [S(S(S O));of_int 2;of_int 3])))

  let result () = 
    to_int (interpret_function 0 [] myfunctions (Apply ('K', [(S O);of_int 2;of_int 11])))

end
                  
let _ = begin
    (*Uncomment to test the interpreter *)
    (*    Printf.printf "//%d\n" (Example.result ());*)
    Printf.printf "%s\n" Example.compiled;
  end
