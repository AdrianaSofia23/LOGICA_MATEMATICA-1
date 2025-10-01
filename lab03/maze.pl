%-----------------------
    %PART 1
%-----------------------
edge(a,b).
edge(b,c).
edge(a,d).
edge(d,c).

%Part2
edge(c, a).

% Basic path definition
%path(X, Y):- edge(X, Y).
%path(X, Y) :- edge(X, Z), path(Z, Y).

path(X, Y) :- path(X, Y, []).
path(X, Y,_) :- edge(X,Y).
path(X, Y, Visited) :-
    edge(X,Z),
    \+member(Z, Visited),
    path(Z, Y,[X|Visited]).



%-----------------------
    %PART 4
%-----------------------

door(entrace,a).
door(entrace,c).
door(c,e).
door(e,c).
door(a,f).
door(f,g).
door(g,h).
door(h,g).
door(h,d).
door(d,b).
door(b,d).
door(h,i).
door(i,exit).


connected(X, Y) :- door(X, Y).
connected(X, Y) :- door(Y, X).

% path(Start, End, Visited, Path)
path(X, X, _, [X]).  % Caso base: estamos en el destino
path(X, Y, Visited, [X|Path]) :-
    connected(X, Z),
    \+ member(Z, Visited),  % evitar ciclos
    path(Z, Y, [Z|Visited], Path).

find_path(From, To, Path) :-
    path(From, To, [From], Path).

%Find all possible paths
all_paths(From, To, Paths) :-
    findall(Path, find_path(From, To, Path), Paths).
