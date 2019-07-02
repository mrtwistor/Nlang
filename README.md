# Nlang
The N programming language


The purpose of the N language is to illustrate a computing language 
having only one primitive type, the natural numbers, with a single element O 
and a constructor S (successorship), and induction (recursion).  We also
include a computation of the Ackermann function, showing that N is able
to compute functions that are not primitive-recursive, and also illustrate
how to define the primitive recursion operator i, and define elementary
arithmetic in terms of i.

The code following the comments includes the interpreter and compiler 
for the N language.  The "C dialect" is the following ruleset on a C program.
 


## RULES*

The rules for the C dialect are as follows:

1.   The program begins with the header

```C
////////////N header/////////////
#define R return
#define N unsigned
#define Q(a,b,c,d) a==b?c:d;
N O;
N S(N a){R++a;}
/////////////////////////////////
```

2. No lower case letters or numerals can be used, nor may any of
the special characters on the following line--
```
    #"!&^*%$@./-|\'`='~[]<>+"
```
(As a consequence, no arithmetic can be performed, no bitwise operations, inequality, pointer operations, etc.)

3. The code must compile as a valid C program, with the compiler flags `-c -Wall`.  

Here is a complete example of compliant code,
using all intended features of the dialect (see
note below regarding first-class functions).

```C
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
```


## NOTES

* Note that conditionals are still possible, but only via with the ternary conditional operator ..?..:..; 
Loops are still possible, via recursion.

* Prototypes are necessary to allow mutually recursive functions to be declared prior to their
main body.  Functions are first-class in the C dialect, but that feature is not (yet) 
implemented by the interpreter.

* I believe no rule is needed to prevent casts, because gcc compiler gives a warning on something like this: 
`N A(){R O;} N B(){R (N)A;}`
However, to prevent this unintended feature for sure, we have added the rule: 

Every balanced string of parentheses (...) must be immediately follwed
by a special character
`?{:),;`

to the sed validation script `compile.sh`

* To interact with the machine, it is permissible to use a main subroutine, with
standard C functions like "printf", or pretty much anything else.  But care should 
be made to ensure that no relevant calculations are done outside the N machine.  
So printf is (probably) ok, but mucking around with global variable declarations 
is not, for example.

* A compiler from a language realized as an OCAML type is also included in `n.ml`
