:- module(_,_,[assertions,regtypes]).

:- prop alumno_prode/4 #"@includedef{alumno_prode/4}".
alumno_prode('Tomas', 'Sanchez', 'Daniel', 'a180428').

:- doc(title, "Second Proyect - Pure logic programming").

:- doc(author, "Daniel Tomas Sanchez, 180428").


comprimir(Inicial , Comprimida ):-
    limpia_memo ,
    compresion_recursiva(Inicial , Comprimida ).
limpia_memo.

%% compresion_recursiva (Inicial ,Inicial).

%% compresion_recursiva (Inicial , Comprimido ) :-
%%     repeticion (Inicial , Comprimido ).

% No Compresion posible:
compresion_recursiva(Inicial ,Inicial).


:- pred len(L, N)
   #"@var{N} is the length of @var{L} if it is greater than 0. @includedef{len/2}".
len([], 0).
len([_|L], N1) :-
    len(L, N),
    N1 is N+1.
isEmpty([]).
empty_list(list) :- list = [].
%% isEmpty(L) :- len(L,X),X=:=0.

:- pred append(L1, L2, R)
   #"@var{R} is the result of appending @var{L1} to @var{L2}. @includedef{append/3}".
append([], L2, L2).
append([H|L1], L2, [H|R]) :- append(L1, L2, R).

:- pred partir(Todo, Parte1, Parte2)
   #"@var{Parte1} and @var{Parte2} are non empty subsequences of the list @var{Todo}. @includedef{partir/3}".
%% partir(Todo, [_], Todo).
%% partir(Todo, [H|Parte1], [H|Parte2]) :-
%%     partir(Todo, Parte1, Parte2).

%% partir(Todo, Parte1, Parte2) :-
%%     len(Todo, N),
%%     N >= 2,
%%     partir2(Todo, Parte2, Parte1),
%%     len(Parte1, N1),
%%     N1 >= 1,
%%     len(Parte2, N2),
%%     N2 >= 1.

partir(Todo, Parte1, Parte2) :-
    len(Todo, N),
    N >= 2,
    partir2(Todo, Parte2, Parte1).
    %% len(Parte2, N2),
    %% N2 >= 1.

partir2(Todo, TT, [TH]) :-
    Todo = [TH|TT].
partir2(_, [], _) :- !.
partir2(Todo, Parte2, Parte1_added) :-
    partir2(Todo, [H|Parte2], Parte1),
    append(Parte1, [H], Parte1_added).

%% partir2(Todo, Parte2, Parte1_added) :-
%%     len(Parte2, N),
%%     (N is 0 ->
%%         !
%%     ;
%%          partir2(Todo, [H|Parte2], Parte1),
%%          append(Parte1, [H], Parte1_added)
%%     ).
