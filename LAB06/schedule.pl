:- use_module(library(clpfd)).

task(a, 3, 1).
task(b, 2, 1).
task(c, 3, 2).
task(d, 1, 2).
task(e, 4, 3).
task(f, 2, 1).
task(g, 3, 3).
task(h, 5, 2).

schedule(Tasks, Starts, Ends, Makespan) :-
    Tasks = [task(a,3,1), task(b,2,1), task(c,3,2), task(d,1,2), 
             task(e,4,3), task(f,2,1), task(g,3,3), task(h,5,2)],
    Starts = [Sa, Sb, Sc, Sd, Se, Sf, Sg, Sh], 
    Ends = [Ea, Eb, Ec, Ed, Ee, Ef, Eg, Eh],

    Sa in 0..20, Sb in 0..20, Sc in 0..20, Sd in 0..20, 
    Se in 0..20, Sf in 0..20, Sg in 0..20, Sh in 0..20,

    Ea #= Sa + 3, 
    Eb #= Sb + 2, 
    Ec #= Sc + 3, 
    Ed #= Sd + 1,
    Ee #= Se + 4, 
    Ef #= Sf + 2, 
    Eg #= Sg + 3, 
    Eh #= Sh + 5,
    

    (Ea #=< Sb) #\/ (Eb #=< Sa),
    (Ea #=< Sf) #\/ (Ef #=< Sa),
    (Eb #=< Sf) #\/ (Ef #=< Sb),
    

    (Ec #=< Sd) #\/ (Ed #=< Sc),
    (Ec #=< Sh) #\/ (Eh #=< Sc),
    (Ed #=< Sh) #\/ (Eh #=< Sd),
    

    (Ee #=< Sg) #\/ (Eg #=< Se),

    Makespan #= max(max(max(Ea, Eb), max(Ec, Ed)), max(max(Ee, Ef), max(Eg, Eh))),
    
    labeling([min(Makespan)], [Sa, Sb, Sc, Sd, Se, Sf, Sg, Sh, Makespan]),
    
    nl,
    format('Task b: recurso 1,  inicia en ~w, termina en ~w~n', [Sb, Eb]),
    format('Task c: recurso 2,  inicia en ~w, termina en ~w~n', [Sc, Ec]),
    format('Task d: recurso 2,  inicia en ~w, termina en ~w~n', [Sd, Ed]),
    format('Task a: recurso 1,  inicia en ~w, termina en ~w~n', [Sa, Ea]),
    format('Task e: recurso 3,  inicia en ~w, termina en ~w~n', [Se, Ee]),
    format('Task f: recurso 1,  inicia en ~w, termina en ~w~n', [Sf, Ef]),
    format('Task g: recurso 3,  inicia en ~w, termina en ~w~n', [Sg, Eg]),
    format('Task h: recurso 2,  inicia en ~w, termina en ~w~n', [Sh, Eh]),
    format('Makespan: ~w~n', [Makespan]),
    nl,
    !.
