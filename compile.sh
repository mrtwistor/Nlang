#!/bin/bash

#Compiler for the N programming language
#Version: 0.0
#Author: Jonathan Holland
#License: GPL v 2

#Usage: ./compile.sh FILENAME

#Compiler for the N language, which is defined as the language
#that this compiler compiles (see comments and gcc flags).

#To compile an executable, write a separate main function, compile with -c, and
#combine with the linker.  For example, the two included examples are fib.c
#and main-fib.c.  First, compile with
# ./compile.sh fib.c
#Then,
# gcc -o fib fib.c.N.o main-fib.c
#This last item, to build the runtime environment, will generate warnings
#because gcc needs to perform implicit function declarations.
#Ignore those warnings.


#The program may not use lower case letters, numbers or most special
#characters (outside of comments):

if [[ $(sed "s/\/\/.*$//" $1 | sed "s/##.*$//" | sed -n "/[#\"\\'@*$\-+=&^%\!_~\`\.<>\/a-z0-9]/p") ]]; then
    echo "Failure: Illegal symbols detected"
    exit 0

    #No sneaky casts (these *should* be rejected by gcc...):
elif [[ $(sed "s/\/\/.*$//" $1 | sed "s/##.*$//" | sed -n "/)[^ \n,;:?){]/p") ]]; then
    echo "Failure: Illegal syntax detected"
    exit 0

else
    #Insert header, and output C file
    #The header defines the type N, and the constructors S and O.
    # It also defines equality conditional
    # Q(a,b,c,d) that returns c if a==b and d otherwise.
    # Note this is the only way equality may be tested, since the
    # symbol '=' was rejected by sed.
    sed "s/\/\/.*$//" $1 | sed "s/##//" | sed "1s/^/\
#define R return\n\
#define Q(a,b,c,d) (a==b)?c:d\n\
int _num_succ;\n\
#define N unsigned\n\
N S(N A){_num_succ++;R++A;}\n\
N O;\n/"  > $1.N.c
    #Here _num_succ is for testing.  It is the number of times
    #the successor function was called. 

    #Now the key part, compile the emmitted C under gcc, with *absolutely no warnings*:
    gcc -c -Wall -Werror $1.N.c
    #Cheat code: the special character sequence ## allows ordinary
    #C code to be embedded.  (The rest of the line will be ignored
    #by sed, but processed by gcc.)  But it's cheating, so only use
    #for debugging.

fi

