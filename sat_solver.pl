:- module(sat_solver, [solve/2]).

:- op(200, fx, ~).
:- op(500, xfy, v).

%struktura

solve(Cs, S):-
    rewrite(Cs, ListCs),
    getVars(ListCs, AVs),
    killTriv(ListCs, LCs),
    getVars2(ListCs, LCs, AVs, Vs),
    makeValues(LCs, AVs, Vs, S).

%przepisywanie klauzul w listy literalow

rewrite([], []).
rewrite([C|Cs], [LC|LCs]):-
    ctol(C, RLC), killReps(RLC, LC), rewrite(Cs, LCs).

ctol([], _):- false.
ctol(L, [L]):- literal(L).
ctol(L v C, [L|X]):- literal(L), ctol(C, X).

literal(L):- atom(L).
literal(~L):- atom(L).

%tworzenie listy zmiennych

getVars2(ListCs, LCs, AVs, AVs):-
	ListCs = LCs, !.
getVars2(_, LCs, _, Vs):-
	getVars(LCs, Vs).

getVars(LCs, Vs):-
    varsF(LCs, RVs), killReps(RVs, Vs).

killReps([], []).
killReps([Var|RVs], [Var|Vs]):-
    delete(RVs, Var, NList), killReps(NList, Vs).

varsF([], []).
varsF([C|Cs], Vs):-
    varsC(C, VC), varsF(Cs, VCs), append(VC, VCs, Vs).

varsC([], []).
varsC([L|C], [V|Vs]):-
    varL(L, V), varsC(C, Vs).

varL(L, L):- atom(L).
varL(~L, L):- atom(L).

%pozbycie sie klauzul trywialnych

killTriv([],[]).
killTriv([T|TCs],Cs):- triv(T), !, killTriv(TCs, Cs).
killTriv([T|TCs],[T|Cs]):- killTriv(TCs, Cs).

triv([P|T]):- T = [_|_], atom(P), member(~P, T), !.
triv([P|T]):- T = [_,_|_], atom(P), \+ member(~P, T), triv(T).
triv([~P|T]):- T = [_|_], atom(P), member(P, T), !.
triv([~P|T]):- T = [_,_|_], atom(P), \+ member(P, T), triv(T).

%szukanie klauzuli jednoliteralowej

oneFirst(X, [H|Y]):- H = [_], select(H, X, Y).

%wartosciowanie klauzul jednoliteralowych

makeOne(P, (P, t)):- atom(P).
makeOne(~P, (P, f)):- atom(P).

makeOneF(P, (P, f)):- atom(P).
makeOneF(~P, (P, t)):- atom(P).

%tworzenie wartosciowan
%makeValues([], []).
%makeValues([V|Vs], [(V, t)|Ws]):- makeValues(Vs, Ws).
%makeValues([V|Vs], [(V, f)|Ws]):- makeValues(Vs, Ws).

%tworzenie wartosciowan

makeValues(Cs, AVs, Vs, W):-
    makeCValues(Cs, Vs, W1),
    makeRValues(AVs, Vs, W2),
    append(W1, W2, W).

makeCValues([], [], []).
makeCValues([], [V|Vs], [(V, x)|W]):-
    makeCValues([], Vs, W).
makeCValues(Cs, Vs, [WH|W]):-
    oneFirst(Cs, [[H]|Y]), !, makeOne(H, WH),
    delincla(WH, Y, NCs), WH = (T, _),
    select(T, Vs, NVs), makeCValues(NCs, NVs, W).
makeCValues([[L|_]|Cs], Vs, [WL|W]):-
    makeOne(L, WL), delincla(WL, Cs, NCs),
    WL = (T, _),
    select(T, Vs, NVs), makeCValues(NCs, NVs, W).
makeCValues([[L|C]|Cs], Vs, [WL|W]):-
    makeOneF(L, WL), delincla(WL, Cs, NCs),
    WL = (T, _),
    select(T, Vs, NVs), makeCValues([C|NCs], NVs, W).

makeRValues([], [], []).
makeRValues([AV|AVs], Vs, [(AV, x)|W2]):-
    \+ member(AV, Vs), makeRValues(AVs, Vs, W2).
makeRValues([AV|AVs], Vs, W2):-
    select(AV, Vs, NVs), makeRValues(AVs, NVs, W2).

delincla(_, [], []).
delincla((X, t), [C|Cs], NCs):-
    member(X, C), !, delincla((X, t), Cs, NCs).
delincla((X, t), [C|Cs], [N|NCs]):-
    select(~X, C, N), !, delincla((X, t), Cs, NCs).
delincla((X, f), [C|Cs], NCs):-
    member(~X, C), !, delincla((X, f), Cs, NCs).
delincla((X, f), [C|Cs], [N|NCs]):-
    select(X, C, N), !, delincla((X, f), Cs, NCs).
delincla((X, W), [C|Cs], [C|NCs]):- delincla((X, W), Cs, NCs).



