:- module(_,_,[classic,assertions,regtypes]).

%% alumno_prode('Cabero','Blanco','Aaron','180440').

:- prop alumno_prode/4 #"@includedef{alumno_prode/4}".
alumno_prode('Tomas', 'Sanchez', 'Daniel', 'a180428').

:- doc(title, "Second Proyect - Pure logic programming").

:- doc(author, "Daniel Tomas Sanchez, 180428").

:- pred comprimir(Inicial, Comprimida)
   #"@var{Comprimida} is the compressed version with the shortest length of the list @var{Inicial}. The compression process uses memoization. First of all, clean all previous results memoizated. @includedef{comprimir/2}".
comprimir(Inicial, Comprimida):-
    limpia_memo, compresion_recursiva(Inicial, Comprimida).

:- dynamic memo /2.

:- pred limpia_memo
   #"Clean all results previously memoizated. @includedef{limpia_memo/0}".
limpia_memo :-
    retractall(memo(_,_)).

:- pred compresion_recursiva(Inicial, Comprimida)
   #"@var{Comprimida} is the compressed version with the shortest length of the list @var{Inicial}. The compression process uses memoization. @includedef{compresion_recursiva/2}".
compresion_recursiva(Inicial , Comprimida ) :-
    mejor_compresion_memo(Inicial , Comprimida ).

:- pred mejor_compresion_memo(Inicial, Comprimida)
   #"@var{Comprimida} is the compressed version with the shortest length of the list @var{Inicial}. The compression process uses memoization. @includedef{compresion_recursiva/2}".
mejor_compresion_memo(Inicial, Comprimida) :-
    memo(Inicial, Comprimida).

mejor_compresion_memo(Inicial, Comprimida) :-
    mejor_compresion(Inicial, Comprimida),
    assert(memo(Inicial, Comprimida)).


%PRELIMINARES

:- pred partir(Todo, Parte1, Parte2)
   #"@var{Parte1} and @var{Parte2} are non empty subsequences of the list @var{Todo}. @includedef{partir/3}".
partir(Todo, Parte1, Parte2):-
    append(Parte1,Parte2,Todo), Parte1 \= [], Parte2 \= [].

:- pred parentesis(Parte, Num, ParteNum)
   #"@var{ParteNum} is the result of appending @var{Num} to @var{Parte}. If @var{Parte} has two elements or more @var{Parte} is surrounded by brackets. @includedef{parentesis/3}".
parentesis([X|Parte],Num,ParteNum):- integer(Num), length([X|Parte],N), N>=2, append(['('],[X|Parte],L1), append(L1,[')',Num],ParteNum),!.
parentesis([X|Parte],Num,ParteNum):- integer(Num), append([X|Parte], [Num], ParteNum).

%% parentesis(Parte,Num,ParteNum):-  append(L1,[')',Num],ParteNum), append(['('],Parte,L1),integer(Num),  length(Parte,N), N>=2.
%% parentesis(Parte,Num,ParteNum):-  append(Parte,[Num],ParteNum), integer(Num),  length(Parte,N), N<2.


%Auxiliar de Se_repite
%% :- pred
%%    #"@var{}. @includedef{}".
copia_lista_n(_, 0, []).
copia_lista_n(L, N, R) :-
    N > 0,
    N1 is N-1,
    copia_lista_n(L, N1, R2),
    append(L, R2, R).

:- pred se_repite(Cs, Parte, N0, Num)
   #"@var{Num} is the result of adding @em{N} to @var{N0}, being @em{N} the number of repetitions of @var{Parte} which form @var{Cs} . @includedef{se_repite/4}".
se_repite([],_,_,0):-!.
se_repite(Cs,Parte,N0,Num):-length(Cs, Ls), length(Parte, La), 0 is Ls mod La, T is Ls//La, copia_lista_n(Parte,T,Cs), Num is N0 + T.

%FASE A
:- pred repeticion(Inicial, Comprimida)
   #"@var{Comprimida} is the compressed by repetition version of the list @var{Inicial}. @includedef{repeticion/2}".
repeticion(Inicial, Comprimida):-
    partir(Inicial, Parte1, _),
    se_repite(Inicial,Parte1,0,R),
    compresion_recursiva(Parte1, Comp),
    parentesis(Comp,R,Comprimida).


% FASE B
:- pred division(Inicial, Comprimida)
   #"@var{Comprimida} is the compressed by division version of the list @var{Inicial}. @includedef{division/2}".
division(Inicial, Comprimida) :-
    partir(Inicial, Parte1, Parte2),
    compresion_recursiva(Parte1, Comprimida1),
    compresion_recursiva(Parte2, Comprimida2),
    append(Comprimida1, Comprimida2, Comprimida),
    Inicial \= Comprimida.


:- pred compresion(Inicial, Comprimida)
   #"@var{Comprimida} is the compressed version of the list @var{Inicial}. @includedef{compresion/2}".
compresion(Inicial, Comprimida) :-
    repeticion(Inicial, Comprimida).

compresion(Inicial, Comprimida) :-
    division(Inicial, Comprimida).


% FASE C
:- pred mejor_compresion(Inicial, Comprimida)
   #"@var{Comprimida} is the compressed version with the shortest length of the list @var{Inicial}. @includedef{mejor_compresion/2}".
mejor_compresion(Inicial, Comprimida) :-
    findall(ComprimidaAux, compresion(Inicial, ComprimidaAux), L),
    L \= [],
    choose_minor(L, Comprimida).

mejor_compresion(Inicial, Inicial).

:- pred choose_minor(L, Comprimida)
   #"@var{Comprimida} is the sublist with the shortest length of the list @var{L}. @includedef{choose_minor/2}".
choose_minor(L, Comprimida) :-
    choose_minor2(L, _, Comprimida).


:- pred choose_minor2(L, N, ComprimidaMenor)
   #"@var{ComprimidaMenor} is the sublist with length @var{N} and the shortest length of the list @var{L}. @includedef{choose_minor2/3}".
choose_minor2([], 999, []).
choose_minor2(L, N, ComprimidaMenor) :-
    L = [H|T],
    length(H, HN),
    choose_minor2(T, ComprimidaMenor2N, ComprimidaMenor2),
    (HN < ComprimidaMenor2N ->
        ComprimidaMenor = H,
        N = HN
    ;
        ComprimidaMenor = ComprimidaMenor2,
        N = ComprimidaMenor2N
    ).
