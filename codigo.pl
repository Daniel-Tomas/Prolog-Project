:- module(_,_,[classic,assertions,regtypes]).


:- prop alumno_prode/4 #"@includedef{alumno_prode/4}".
alumno_prode('Tomas', 'Sanchez', 'Daniel', 'a180428').

:- doc(title, "Second Proyect - Pure logic programming").

:- doc(author, "Daniel Tomas Sanchez, 180428").


comprimir(Inicial, Comprimida):-
    limpia_memo, compresion_recursiva(Inicial, Comprimida).
limpia_memo.
compresion_recursiva(Inicial, Comprimida):-
    mejor_compresion(Inicial, Comprimida).

%PRELIMINARES

%Partir/3
partir(Todo, Parte1, Parte2):-append(Parte1,Parte2,Todo), length(Parte1,N1), N1>0, length(Parte2,N2), N2>0. 

%Parentesis/3
parentesis([X|Parte],Num,ParteNum):- integer(Num), length([X|Parte],N), N>=2, append(['('],[X|Parte],L1), append(L1,[')',Num],ParteNum),!.
parentesis([X|Parte],Num,ParteNum):- integer(Num), length([X|Parte],N), N<2, append([X|Parte],[Num],ParteNum),!.

%% parentesis(Parte,Num,ParteNum):-  append(L1,[')',Num],ParteNum), append(['('],Parte,L1),integer(Num),  length(Parte,N), N>=2.
%% parentesis(Parte,Num,ParteNum):-  append(Parte,[Num],ParteNum), integer(Num),  length(Parte,N), N<2.


%Se_repite/4
%Auxiliar de Se_repite
copia_lista_n(_, 0, []).
copia_lista_n(L, N, R) :-
    N > 0,
    N1 is N-1,
    copia_lista_n(L, N1, R2),
    append(L, R2, R).
se_repite([],_,_,0):-!.
se_repite(Cs,Parte,N0,Num):-length(Cs, Ls), length(Parte, La),T is Ls//La, copia_lista_n(Parte,T,Cs), Num is N0 + T.

%FASE A
repeticion(Inicial, Comprimida):-
    partir(Inicial, Parte1, _),
    se_repite(Inicial,Parte1,0,R),
    compresion_recursiva(Parte1, Comp),
    parentesis(Comp,R,Comprimida).


% FASE B
division(Inicial, Comprimida) :-
    partir(Inicial, Parte1, Parte2),
    compresion_recursiva(Parte1, Comprimida1),
    compresion_recursiva(Parte2, Comprimida2),
    append(Comprimida1, Comprimida2, Comprimida),
    Inicial \= Comprimida.


%% compresion(Inicial, Comprimida) :-
%%     division(Inicial, Comprimida).

compresion(Inicial, Comprimida) :-
    repeticion(Inicial, Comprimida).

compresion(Inicial, Comprimida) :-
    division(Inicial, Comprimida).


% FASE C
mejor_compresion(Inicial, Comprimida) :-
    findall(ComprimidaAux, compresion(Inicial, ComprimidaAux), L),
    length(L, LN),
    LN > 0,
    choose_minor(L, Comprimida).

mejor_compresion(Inicial, Inicial).

choose_minor(L, Comprimida) :-
    choose_minor2(L, _, Comprimida).

%% choose_minor2([], _, _).
%% choose_minor2(L, N, ComprimidaMenor) :-
%%     L = [H|T],
%%     length(H, HN),
%%     (HN < N ->
%%         ComprimidaMenor = H,
%%         choose_minor2(T, HN, ComprimidaMenor)
%%     ;
%%         choose_minor2(T, N, ComprimidaMenor)
%%     ).

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

mejor_compresion_memo(X,Y).
memo(X,Y).
