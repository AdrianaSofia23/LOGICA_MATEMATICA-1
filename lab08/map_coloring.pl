:- use_module(library(clpfd)).

% ----------------------------------------
% Part A — Australia
% ----------------------------------------

regions_au([wa, nt, sa, q, nsw, v, t]).

edges_au([
    wa-nt, wa-sa,
    nt-sa, nt-q,
    sa-q, sa-nsw, sa-v,
    q-nsw,
    nsw-v
]).

color_names([1-red, 2-green, 3-blue, 4-yellow]).

% Core coloring predicate
map_color(Regions, Edges, K, Vars) :-
    same_length(Regions, Vars),
    Vars ins 1..K,
    apply_edges(Regions, Vars, Edges),
    labeling([ffc], Vars).

% Apply edge constraints
apply_edges(Regions, Vars, Edges) :-
    maplist(different_colors(Regions, Vars), Edges).

different_colors(Regions, Vars, A-B) :-
    nth0(IndexA, Regions, A),
    nth0(IndexB, Regions, B),
    nth0(IndexA, Vars, ColorA),
    nth0(IndexB, Vars, ColorB),
    ColorA #\= ColorB.

% Minimum colors finder
min_colors(Regions, Edges, MaxK, MinK, Vars) :-
    between(1, MaxK, K),
    map_color(Regions, Edges, K, Vars),
    MinK = K,
    !.

% Australia-specific predicates
min_colors_au(MaxK, MinK, Vars) :-
    regions_au(Rs), 
    edges_au(Es),
    min_colors(Rs, Es, MaxK, MinK, Vars).

colorize_au(K, Vars) :-
    regions_au(Rs),
    edges_au(Es),
    map_color(Rs, Es, K, Vars).

% Pretty printing
pretty_color_by_region(Regions, Vars) :-
    color_names(ColorMap),
    maplist(show_region_color(ColorMap), Regions, Vars).

show_region_color(ColorMap, Region, ColorNum) :-
    member(ColorNum-ColorName, ColorMap),
    format("~w = ~w~n", [Region, ColorName]).

% ----------------------------------------
% Part B — South America
% ----------------------------------------

regions_sa([ar, bo, br, cl, co, ec, gy, gfr, py, pe, su, uy, ve]).

edges_sa([
    ar-cl, ar-bo, ar-py, ar-uy,
    bo-br, bo-pe, bo-cl, bo-py,
    br-py, br-uy, br-ve, br-gy, br-gfr, br-su, br-pe, br-bo,
    cl-pe, cl-ar,
    co-ve, co-ec, co-pe, co-br,
    ec-pe, ec-co,
    gy-su, gy-gfr, gy-br,
    gfr-su, gfr-br, gfr-gy,
    py-uy, py-br, py-bo, py-ar,
    su-br, su-gy, su-gfr,
    uy-ar, uy-py, uy-br,
    ve-co, ve-br
]).

% South America-specific predicates
min_colors_sa(MaxK, MinK, Vars) :-
    regions_sa(Rs), 
    edges_sa(Es),
    min_colors(Rs, Es, MaxK, MinK, Vars).

colorize_sa(K, Vars) :-
    regions_sa(Rs),
    edges_sa(Es),
    map_color(Rs, Es, K, Vars).
