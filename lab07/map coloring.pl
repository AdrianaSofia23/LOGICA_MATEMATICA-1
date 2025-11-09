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

map_color(Vars, Edges, K) :-
    Vars ins 1..K,                         
    maplist(different_colors(Vars), Edges).

different_colors(Vars, A-B) :-
    nth0(_, Vars, _, _),                   
    nth0(IndexA, Vars, _, _),              
    nth0(IndexB, Vars, _, _),              
    region_index(A, IndexA),               
    region_index(B, IndexB),
    nth0(IndexA, Vars, ColorA),
    nth0(IndexB, Vars, ColorB),
    ColorA #\= ColorB.


region_index(wa, 0).
region_index(nt, 1).
region_index(sa, 2).
region_index(q, 3).
region_index(nsw, 4).
region_index(v, 5).
region_index(t, 6).


colorize_au(K, Vars) :-
    regions_au(Regions),
    length(Regions, N),
    length(Vars, N),
    edges_au(Edges),
    map_color(Vars, Edges, K),
    labeling([], Vars).

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

region_index_sa(ar,0). region_index_sa(bo,1). region_index_sa(br,2).
region_index_sa(cl,3). region_index_sa(co,4). region_index_sa(ec,5).
region_index_sa(gy,6). region_index_sa(gfr,7). region_index_sa(py,8).
region_index_sa(pe,9). region_index_sa(su,10). region_index_sa(uy,11).
region_index_sa(ve,12).


colorize_sa(K, Vars) :-
    regions_sa(Regions),
    length(Regions, N),
    length(Vars, N),
    edges_sa(Edges),
    Vars ins 1..K,
    maplist(different_colors_sa(Vars), Edges),
    labeling([], Vars).

different_colors_sa(Vars, A-B) :-
    region_index_sa(A, IA),
    region_index_sa(B, IB),
    nth0(IA, Vars, ColorA),
    nth0(IB, Vars, ColorB),
    ColorA #\= ColorB.

% ----------------------------------------
% Example Queries:
% ----------------------------------------
% ?- colorize_au(3, Vars), writeln(Vars).
% ?- colorize_au(3, Vars), regions_au(Rs), pretty_color_by_region(Rs, Vars).
% ?- regions_au(Rs), edges_au(Es), map_color(Vs, Es, 3), labeling([ffc], Vs), pretty_color_by_region(Rs, Vs).
%
% ?- colorize_sa(4, Vars), regions_sa(Rs), pretty_color_by_region(Rs, Vars).
% ?- regions_sa(Rs), edges_sa(Es), map_color(Vs, Es, 4), labeling([min], Vs), pretty_color_by_region(Rs, Vs).
