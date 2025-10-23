% ============================================
% PART 1: MAZE REPRESENTATION
% ============================================


edge(entrance, c).
edge(entrance, a).
edge(c, e).
edge(e, c).
edge(e, f).
edge(a, f).
edge(f, g).
edge(g, h).
edge(h, g).
edge(h, d).
edge(d, b).
edge(b, d).
edge(h, i).
edge(i, exit).


blocked(e, f).  % Door is blocked from a to f
blocked(b, d).  % Path is blocked from b to d

% ============================================
% PART 2: REASONING RULES
% ============================================


can_move(X, Y) :- 
    edge(X, Y), 
    \+ blocked(X, Y).


reason(X, Y, 'path is open') :- 
    can_move(X, Y).

reason(X, Y, 'path is blocked') :- 
    blocked(X, Y).

reason(X, Y, 'no direct edge exists') :- 
    \+ edge(X, Y).

reason(_, exit, 'destination reached').

% ============================================
% PART 3: RECURSIVE TRAVERSAL
% ============================================


move(X, Y, Visited, [Y|Visited]) :-
    can_move(X, Y),
    \+ member(Y, Visited),
    format('Moving from ~w to ~w: ', [X, Y]),
    (Y = exit -> 
        format('destination reached!~n') 
    ; 
        format('path is open.~n')
    ).


move(X, Y, Visited, Path) :-
    can_move(X, Z),
    \+ member(Z, Visited),
    format('Exploring from ~w to ~w: path is open.~n', [X, Z]),
    move(Z, Y, [Z|Visited], Path).

% ============================================
% PART 4: MAIN PATH FINDING
% ============================================


find_path(X, Y, Path) :-
    format('~n=== Starting pathfinding from ~w to ~w ===~n', [X, Y]),
    move(X, Y, [X], RevPath),
    reverse(RevPath, Path),
    format('~n------------------------------~n'),
    format('Path found: ~w~n', [Path]),
    length(Path, Length),
    format('Total steps: ~w~n', [Length]),
    format('------------------------------~n~n').

% ============================================
% PART 5: EXTENSIONS 
% ============================================

why(X, Y) :-
    format('~nAnalyzing path from ~w to ~w:~n', [X, Y]),
    (   edge(X, Y) ->
        format('  - Edge exists: YES~n'),
        (   blocked(X, Y) ->
            format('  - Status: BLOCKED~n'),
            format('  - Reason: Path is blocked~n')
        ;   format('  - Status: OPEN~n'),
            format('  - Reason: Path is open~n')
        )
    ;   format('  - Edge exists: NO~n'),
        format('  - Reason: No direct edge exists~n')
    ).


% ==============================================================
%                        SUMMARY
% ==============================================================


% This program models a maze as a graph structure using 
% Prolog facts (edge/2 and blocked/2) and employs logical 
% reasoning to identify valid movements, justify each decision 
% made, and recursively explore routes from an initial node to 
% a specific destination. The reasoning is implemented through 
% the predicates can_move/2 (to validate available paths) and 
% reason/3 (to explain whether a route is available, blocked, 
% or leads to the goal). During traversal, move/4 uses recursion 
% and formatted output to display step-by-step reasoning, 
% preventing cycles through a list of visited nodes. Finally, 
% find_path/3 integrates these elements to locate and print a 
% complete reasoning trace for the route from entrance to exit.
