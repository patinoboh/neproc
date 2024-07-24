% 2. domácí úloha
%
% a) Implementujte predikát flat(+List, ?Result), který zploští libovolně
% zanořený seznam seznamů List.
%
% flat([], R).
% R = [].
%
% flat([[]], R).
% R = [].
%
% flat([a,b,c], R).
% R = [a,b,c].
%
% flat([a,[[],b,[]],[c,[d]]], R).
% R = [a,b,c,d].

ex1([]).
ex2([[]]).
ex3([a,b,c]).
ex4([a,[[],b,[]],[c,[d]]]).
ex0([1,2,3]).

% consult("hw2.pl").

flat(X, R) :- flat(X, [], R).

flat([], A, A) :- !.
flat([X|XS], A, R):-!,
    flat(XS, A, B),
    flat(X, B, R).
flat(X, A, [X|A]).


allEmpty([]).
allEmpty([[]|R]) :- allEmpty(R).

tp(M, []) :- allEmpty(M), !.
tp(M, [C|R]) :- 
    splitLists(M, C, CS),
    tp(CS, R).

splitLists([],[],[]).
splitLists([[X|XS]|XSS], [X|R], [XS|RS]):-
    splitLists(XSS, R, RS).

%
% Tento predikát měl být deterministický (speciálně otestujte, že po odmítnutí
% neprodukuje duplikátní/nesprávné výsledky). Pokuste se o efektivní
% implementaci pomocí akumulátoru.
%
% b) Implementuje predikát transp(+M, ?R), který transponuje matici M (uloženou
% jako seznam seznamů). Pokud M není ve správném formátu (např. řádky mají
% různé délky), dotaz transp(M, R) by měl selhat.
%
% transp([], R).
% R = [].
%
% transp([[],[],[]], R).
% R = [].
%
% transp([[a,b],[c,d],[e,f]], R).
% R = [[a,c,e],[b,d,f]].
%
% transp([[a],[b,c],[d]], R).
% false.

mat0([]).
mat1([[],[],[]]).
mat2([[1,2,3],[4,5,6],[7,8,9]]).
mat3([[1,2,3],[4,5,6]]).
mat4([[1],[2],[3]]).

transp([],[]).



% c) (BONUSOVÁ ÚLOHA) Implementuje vkládání prvku pro AVL stromy.
%
% Použijte následující reprezentaci:
% prázdný strom: nil
% uzel: t(B,L,X,R) kde
%   L je levý podstrom,
%   X je uložený prvek,
%   R je pravý podstrom,
%   B je informace o vyvážení:
%     B = l (levý podstrom je o 1 hlubší)
%     B = 0 (oba podstromy jsou stejně hluboké)
%     B = r (pravý podstrom je o 1 hlubší)
%
% avlInsert(+X, +T, -R)
% X je vkládané číslo, T je strom před přidáním, R je strom po přidání
%
% avlInsert(1, nil, R).
% R = t(0, nil, 1, nil).
%
% avlInsert(2, t(0, nil, 1, nil), R).
% R = t(r, nil, 1, t(0, nil, 2, nil)).
%
% avlInsert(1, t(0, nil, 1, nil), R).
% R = t(0, nil, 1, nil).
