% Prof: Chris Davis
% Course: CS 4337.001
% Programming Assignment 2 - Prolog
% Filename: Assignment2.pl
% ----------------------------------------------------------------------------

% Problem 1 - Factorials------------------------------------------------------                                                        
                                                                                
fact(0,1).                                                                      
fact(X, Factorial):-                                                            
    (                                                                           
        X>0 ->                                                                  
    (                                                                           
    X1 is X-1,                                                                  
    fact(X1, F1),                                                               
    Factorial is X*F1                                                           
    )                                                                           
    ;                                                                           
        write('try again')).
% ----------------------------------------------------------------------------

% Problem 2 - Prime Number ---------------------------------------------------
is_prime(2).
is_prime(3).
is_prime(X) :- integer(X), X > 3, X mod 2 =\= 0, \+ factor_check(X,3).

factor_check(A,B) :- A mod B =:= 0.
factor_check(A,B) :- B * B < A, B2 is B + 2, factor_check(A,B2).
% ----------------------------------------------------------------------------

% Problem 3 - Even Odd--------------------------------------------------------
segregate([], [], []).
segregate([X|X1], [X|Even], Odd):-
  0 is X mod 2,
  segregate(X1, Even, Odd).
segregate([X|X1], Even, [X|Odd]) :-
  1 is X mod 2,
  segregate(X1, Even, Odd).
% ----------------------------------------------------------------------------

% Problem 4 - Bookends--------------------------------------------------------

bookends([], [], []).
bookends(A,B,C) :- prefix(A,C), suffix(B,C).
prefix([],_).
prefix([A|B],[A|C]) :- prefix(B,C).
suffix(A,B) :- reverse(A,AR), reverse(B,BR), prefix(AR,BR).
% ----------------------------------------------------------------------------

% Problem 5 - Subslice--------------------------------------------------------

subslice(Sublist,List) :-
    append(_,EndSubList,List),
    append(Sublist,_,EndSubList).
% ----------------------------------------------------------------------------
