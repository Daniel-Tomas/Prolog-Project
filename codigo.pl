:- module(_,_,[classic,assertions,regtypes]).
alumno_prode('Cabero','Blanco','Aaron','180440').


comprimir(Inicial, Comprimida):-limpia_memo, compresion_recursiva(Inicial, Comprimida).
limpia_memo.
compresion_recursiva(Inicial, Comprimida):-repeticion(Inicial, Comprimida).
compresion_recursiva(Inicial, Inicial).

%PRELIMINARES

%Partir/3
partir(Todo, Parte1, Parte2):-append(Parte1,Parte2,Todo), length(Parte1,N1), N1>0, length(Parte2,N2), N2>0. 

%Parentesis/3
parentesis(Parte,Num,ParteNum):-length(Parte,N), N>=2, append(['('],Parte,L1), append(L1,[')',Num],ParteNum).
parentesis(Parte,Num,ParteNum):-length(Parte,N), N<2, append(Parte,[Num],ParteNum).

%Se_repite/4
%Auxiliar de Se_repite
copia_lista_n(_, 0, []).
copia_lista_n(L, N, R) :-
    N > 0,
    N1 is N-1,
    copia_lista_n(L, N1, R2),
    append(L, R2, R).
se_repite([],_,_,0):-!.
se_repite(Cs,Parte,_,T):-length(Cs, Ls), length(Parte, La),T is Ls//La, copia_lista_n(Parte,T,Cs).

%FASE A
repeticion(Inicial, Comprimida):-partir(Inicial, Parte1, _), se_repite(Inicial,Parte1,0,R), compresion_recursiva(Parte1, Comp),
    parentesis(Comp,R,Comprimida).