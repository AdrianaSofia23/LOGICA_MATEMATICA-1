:- use_module(library(clpfd)).

sudoku(Rows) :-

    append(Rows, Vars),
    Vars ins 1..9,
    maplist(all_different, Rows),
    transpose(Rows, Columns),
    maplist(all_different, Columns),
    blocks(Rows),
    maplist(label, Rows).

blocks([]).
blocks([A,B,C|Rest]) :-
    blocks3(A, B, C),
    blocks(Rest).

blocks3([], [], []).
blocks3([A1,A2,A3|R1],
        [B1,B2,B3|R2],
        [C1,C2,C3|R3]) :-
    all_different([A1,A2,A3,B1,B2,B3,C1,C2,C3]),
    blocks3(R1, R2, R3).

%% ========================================
%% CASOS DE PRUEBA
%% ========================================

puzzle1([
    [5,3,_,_,7,_,_,_,_],
    [6,_,_,1,9,5,_,_,_],
    [_,9,8,_,_,_,_,6,_],
    [8,_,_,_,6,_,_,_,3],
    [4,_,_,8,_,3,_,_,1],
    [7,_,_,_,2,_,_,_,6],
    [_,6,_,_,_,_,2,8,_],
    [_,_,_,4,1,9,_,_,5],
    [_,_,_,_,8,_,_,7,9]
]).

puzzle2([
    [_,2,_,6,_,8,_,_,_],
    [5,8,_,_,_,9,7,_,_],
    [_,_,_,_,4,_,_,_,_],
    [3,7,_,_,_,_,5,_,_],
    [6,_,_,_,_,_,_,_,4],
    [_,_,8,_,_,_,_,1,3],
    [_,_,_,_,2,_,_,_,_],
    [_,_,9,8,_,_,_,3,6],
    [_,_,_,3,_,6,_,9,_]
]).


test_sudoku(Puzzle) :-
    sudoku(Puzzle),
    print_grid(Puzzle).

print_grid([R1,R2,R3,R4,R5,R6,R7,R8,R9]) :-
    print_row(R1),
    print_row(R2),
    print_row(R3),
    writeln('------+-------+------'),
    print_row(R4),
    print_row(R5),
    print_row(R6),
    writeln('------+-------+------'),
    print_row(R7),
    print_row(R8),
    print_row(R9).

print_row([A,B,C,D,E,F,G,H,I]) :-
    format('~w ~w ~w | ~w ~w ~w | ~w ~w ~w~n', [A,B,C,D,E,F,G,H,I]).


unique_solution(Puzzle) :-
    sudoku(Puzzle),
    \+ (
        copy_term(Puzzle, PuzzleCopy),
        sudoku(PuzzleCopy),
        Puzzle \= PuzzleCopy
    ).



