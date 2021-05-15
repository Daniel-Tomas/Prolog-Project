:- module(_, _, [classic, assertions, regtypes]).

:- doc(title, "Second Proyect - ISO Prolog").

:- doc(author, "Daniel Tomas Sanchez, 180428").

:- prop alumno_prode/4
   #"@includedef{alumno_prode/4}".
alumno_prode('Tomas', 'Sanchez', 'Daniel', 'a180428').


:- pred comprimir(Initial, Compressed)
   #"@var{Compressed} is the compressed version with the shortest length of the list @var{Initial}. The compression process uses memoization. First of all, clean all previous results memoizated. @includedef{comprimir/2}".
comprimir(Initial, Compressed) :-
    limpia_memo,
    compresion_recursiva(Initial, Compressed).


:- dynamic memo/2.

:- pred limpia_memo
   #"Clean all results previously memoizated. @includedef{limpia_memo/0}".
limpia_memo :-
    retractall(memo(_,_)).


:- pred compresion_recursiva(Initial, Compressed)
   #"@var{Compressed} is the compressed version with the shortest length of the list @var{Initial}. The compression process uses memoization. @includedef{compresion_recursiva/2}".
compresion_recursiva(Initial , Compressed ) :-
    mejor_compresion_memo(Initial , Compressed ).


:- pred mejor_compresion_memo(Initial, Compressed)
   #"@var{Compressed} is the compressed version with the shortest length of the list @var{Initial}. The compression process uses memoization. @includedef{mejor_compresion_memo/2}".
mejor_compresion_memo(Initial, Compressed) :-
    memo(Initial, Compressed),
    !.

mejor_compresion_memo(Initial, Compressed) :-
    mejor_compresion(Initial, Compressed),
    assert(memo(Initial, Compressed)).


:- pred partir(Whole, Part1st, Part2nd)
   #"@var{Part1st} and @var{Part2nd} are non empty subsequences of the list @var{Whole}. @includedef{partir/3}".
partir(Whole, Part1st, Part2nd) :-
    append(Part1st, Part2nd, Whole),
    Part1st \= [],
    Part2nd \= [].


:- pred parentesis(Part, Ocur, R)
   #"@var{R} is the result of appending @var{Ocur} to @var{Part}. If @var{Part} has two elements or more @var{Part} is surrounded by brackets. @includedef{parentesis/3}".
parentesis([X,Y|Part], Ocur, R) :-
    integer(Ocur),
    append(['('], [X,Y|Part], L1),
    append(L1, [')', Ocur], R),
    !.

parentesis([X], Ocur, R) :-
    integer(Ocur),
    append([X], [Ocur], R).


:- pred repeat_list(L, N, R)
   #"@var{R} is the result of repeating @var{N} times the list @var{L}. @includedef{repeat_list/3}".
repeat_list(_, 0, []).
repeat_list(L, N, R) :-
    N > 0,
    N1 is N - 1,
    repeat_list(L, N1, R1),
    append(L, R1, R).

:- pred se_repite(Cs, Part, N0, Num)
   #"@var{Num} is the result of adding @em{N} to @var{N0}, being @em{N} the number of repetitions of @var{Part} which form @var{Cs} . @includedef{se_repite/4}".
se_repite([], _, _, 0) :-
    !.

se_repite(Cs, Part, N0, Num) :-
    length(Cs, CsN),
    length(Part, PartN),
    0 is CsN mod PartN,
    Reps is CsN // PartN,
    repeat_list(Part, Reps, Cs),
    Num is N0 + Reps.


:- pred repeticion(Initial, Compressed)
   #"@var{Compressed} is the compressed by repetition version of the list @var{Initial}. @includedef{repeticion/2}".
repeticion(Initial, Compressed) :-
    partir(Initial, Part1st, _),
    se_repite(Initial, Part1st, 0, R),
    compresion_recursiva(Part1st, Comp),
    parentesis(Comp, R, Compressed).


:- pred division(Initial, Compressed)
   #"@var{Compressed} is the compressed by division version of the list @var{Initial}. @includedef{division/2}".
division(Initial, Compressed) :-
    partir(Initial, Part1st, Part2nd),
    compresion_recursiva(Part1st, Comp1st),
    compresion_recursiva(Part2nd, Comp2nd),
    append(Comp1st, Comp2nd, Compressed),
    Initial \= Compressed.


:- pred compresion(Initial, Compressed)
   #"@var{Compressed} is the compressed version of the list @var{Initial}. @includedef{compresion/2}".
compresion(Initial, Compressed) :-
    repeticion(Initial, Compressed),
    length(Initial, InitN),
    length(Compressed, CompN),
    CompN < InitN.

compresion(Initial, Compressed) :-
    division(Initial, Compressed),
    length(Initial, InitN),
    length(Compressed, CompN),
    CompN < InitN.

compresion(Initial, Initial).


:- pred mejor_compresion(Initial, Compressed)
   #"@var{Compressed} is the compressed version with the shortest length of the list @var{Initial}. @includedef{mejor_compresion/2}".
mejor_compresion(Initial, Compressed) :-
    findall(Comp, compresion(Initial, Comp), L),
    L \= [],
    choose_smallest(L, Compressed),
    !.

mejor_compresion(Initial, Initial).


:- pred choose_smallest(L, Compressed)
   #"@var{Compressed} is the sublist with the shortest length of the list @var{L}. @includedef{choose_smallest/2}".
choose_smallest(L, Compressed) :-
    choose_smallest(L, _, Compressed).

:- pred choose_smallest(L, SmCompN, SmComp)
   #"@var{SmComp} is the sublist with length @var{SmCompN} and the shortest length of the list @var{L}. @includedef{choose_smallest/3}".
choose_smallest([], 10 ** 10, []).
choose_smallest(L, SmCompN, SmComp) :-
    L = [H|T],
    length(H, HN),
    choose_smallest(T, SmCompAuxN, SmCompAux),
    (HN < SmCompAuxN ->
        SmComp = H,
        SmCompN = HN
    ;
        SmComp = SmCompAux,
        SmCompN = SmCompAuxN
    ).
