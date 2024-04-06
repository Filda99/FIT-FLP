/**
* Projekt:  flp-log
* Autor:    Filip Jahn
* Login:    xjahnf00
* Datum:    04/06/2024
*/

:- dynamic edge/2.
:- dynamic node/1.


/************************************************************************/
/** Nacteni vstupu */

/** Zkopirovano z: input2.pl (https://moodle.vut.cz/pluginfile.php/848652/mod_resource/content/1/input2.pl)*/
/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).


/** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


/** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).


/************************************************************************/
/** Vytvareni hran a uzlu */

/** Vytvori hranu */
create_edges([]).
create_edges([[[X],[Y]]|T]) :- 
    (edge(X, Y); edge(Y, X)),
    create_edges(T).
create_edges([[[X],[Y]]|T]) :- 
    assertz(edge(X,Y)), assertz(edge(Y,X)), create_edges(T).

/** Vytvori uzel */
% Musime zajistit, aby vkladany uzel jeste nebyl v databazi
create_nodes([]).
% Pokud uzel existuje, tak ho nevkladame. Funkce je zde kvuli rekurzi (rekurze by se jinak zacyklila)
create_nodes([[[X],[Y]]|T]) :- 
    node(X), % Check if node(X) is true
    create_nodes(T),
    node(Y), % Check if node(Y) is true
    create_nodes(T).
% Pokud uzel neexistuje, tak ho vlozime
create_nodes([[[X],[Y]]|T]) :- 
    \+ node(X), % Check if node(X) is false
    assertz(node(X)), 
    \+ node(Y), % Check if node(Y) is false
    assertz(node(Y)),
    create_nodes(T).


/************************************************************************/
/** Hammiltonovska kruznice */

hammilton_cycle(StartingNode, CurrentNode, CurrentPath, VisitedNodes, Result) :-
    \+ are_all_nodes_visited(VisitedNodes),
    edge(CurrentNode, NextNode),
    \+ member(NextNode, VisitedNodes),
    append(CurrentPath, [CurrentNode, NextNode], NewPath),
    hammilton_cycle(StartingNode, NextNode, NewPath, [NextNode|VisitedNodes], Result).

hammilton_cycle(StartingNode, CurrentNode, CurrentPath, VisitedNodes, NewPath) :-
    are_all_nodes_visited(VisitedNodes),
    edge(CurrentNode, StartingNode),
    append(CurrentPath, [CurrentNode, StartingNode], NewPath).

are_all_nodes_visited(VisitedNodes) :-
    findall(Node, node(Node), Nodes),
    length(Nodes, TotalNodes),
    length(VisitedNodes, VisitedCount),
    TotalNodes = VisitedCount.

find_hammilton_cycle(StartNode, Result) :-
    retractall(visited_nodes(_)),
    assert(visited_nodes(StartNode)),
    hammilton_cycle(StartNode, StartNode, [], [StartNode], Result).

/************************************************************************/
/** Hlavni funkce */

main :-
    prompt(_, ''),
    read_lines(LL),
    split_lines(LL,S),
    % write(S),
    % write('\n'),

    create_edges(S),
    listing(edge),

    create_nodes(S),
    listing(node),
    
    findall(Result, find_hammilton_cycle('A', Result), Results),
    % write_solution(Results),
    write(Results),

    halt.