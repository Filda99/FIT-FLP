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
/************************************************************************/


/** Zkopirovano z: input2.pl (https://moodle.vut.cz/pluginfile.php/848652/mod_resource/content/1/input2.pl)*/

/**
    * Nacte radek ze standardniho vstupu
    *
    * @param List   seznam znaku
    * @param Char   znak
    *
    * Funkce postupne cte znaky ze standardniho vstupu a uklada je do seznamu.
    * Konci na LF nebo EOF.
*/
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).



/**
    * Testuje znak na EOF nebo LF
    *
    * @param C   znak
    *
    * Funkce testuje znak, jestli je EOF nebo LF.
*/
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


/**
    * Nacte radky ze standardniho vstupu
    *
    * @param List   seznam radku
    *
    * Funkce postupne cte radky ze standardniho vstupu a uklada je do seznamu.
    * Konci na LF nebo EOF.
*/
read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).


/** 
    * Rozdeli radek na podseznamy 
    *
    * @param List   seznam znaku
    * @param List   seznam podseznamu
    *
    * Funkce postupne prochazi seznam znaku a rozdeli jej na podseznamy
    * podle mezer a novych radku.
    * Pri rozdeleni na podseznamy se ignoruji mezery a novy radek.
*/
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


/**
    * Rozdeli radky na podseznamy
    *
    * @param List   seznam radku
    * @param List   seznam podseznamu
    *
    * Funkce postupne prochazi seznam radku a rozdeli jej na podseznamy
    * podle mezer a novych radku.
    * Pri rozdeleni na podseznamy se ignoruji mezery a novy radek.
*/
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).


/************************************************************************/
/** Vytvareni hran a uzlu */
/************************************************************************/


/** 
    * Vytvori hranu 
    *
    * @param List   seznam hran
    * @param X      uzel X
    * @param Y      uzel Y
    *
    * Funkce postupne prochazi seznam hran a vytvari hranu mezi uzly X a Y
    * a uklada je do databaze hran.
*/
create_edges([]).
create_edges([[[X],[Y]]|T]) :- 
    (edge(X, Y); edge(Y, X)),
    create_edges(T).
create_edges([[[X],[Y]]|T]) :- 
    assertz(edge(X,Y)), assertz(edge(Y,X)), create_edges(T).

/** 
    * Vytvori uzel 
    *
    * @param List   seznam uzlu
    * @param X      uzel X
    * @param Y      uzel Y
    *
    * Funkce postupne prochazi seznam uzlu a vytvari uzly X a Y
    * a uklada je do databaze uzlu.
*/
create_nodes([]).
create_nodes([[[X], [Y]] | Rest]) :-
    (   node(X), node(Y) -> true
    ;   assert(node(X)), assert(node(Y))
    ),
    create_nodes(Rest).

/**
    * Odstrani duplikaty uzlu
    *
    * Funkce zjisti vsechny uzly, ktere jsou v databazi a ulozi je do seznamu.
    * Nasledne smaze vsechny uzly z databaze a znovu je vlozi zpatky.
*/
remove_duplicates :-
    setof(X, node(X), Nodes),
    retractall(node(_)),
    assert_nodes(Nodes).

/**
    * Vlozi uzly do databaze
    *
    * @param List   seznam uzlu
*/
assert_nodes([]).
assert_nodes([Node|Rest]) :-
    assertz(node(Node)),
    assert_nodes(Rest).


/************************************************************************/
/** Hammiltonovska kruznice */
/************************************************************************/


/** 
    * Vytvori hammiltonovskou kruznici
    *
    * @param StartingNode   pocatecni uzel
    * @param CurrentNode    aktualni uzel
    * @param CurrentPath    aktualni cesta
    * @param VisitedNodes   jiz navstivene uzly
    * @param Result         vysledna cesta
    *
    * Funkce postupne prochazi graf, dokud nejsou vsechny uzly projite,
    uklada si postupne cestu a rekurzivne vola sebe sama.
*/
hammilton_cycle(StartingNode, CurrentNode, CurrentPath, VisitedNodes, Result) :-
    \+ are_all_nodes_visited(VisitedNodes),
    edge(CurrentNode, NextNode),
    \+ member(NextNode, VisitedNodes),
    append(CurrentPath, [[CurrentNode, NextNode]], NewPath),
    hammilton_cycle(StartingNode, NextNode, NewPath, [NextNode|VisitedNodes], Result).

/**
    * Posledni usek - z posledniho uzlu do pocatecniho
    *
    * @param StartingNode   pocatecni uzel
    * @param CurrentNode    aktualni uzel
    * @param CurrentPath    aktualni cesta
    * @param VisitedNodes   jiz navstivene uzly
    * @param Result         vysledna cesta
*/
hammilton_cycle(StartingNode, CurrentNode, CurrentPath, VisitedNodes, Result) :-
    are_all_nodes_visited(VisitedNodes),
    edge(CurrentNode, StartingNode),
    append(CurrentPath, [[CurrentNode, StartingNode]], Result).

/**
    * Zjisti, zda byly navstiveny vsechny uzly
    *
    * @param VisitedNodes   Seznam jiz navstivenych uzlu
*/
are_all_nodes_visited(VisitedNodes) :-
    findall(Node, node(Node), Nodes),
    length(Nodes, TotalNodes),
    length(VisitedNodes, VisitedCount),
    TotalNodes = VisitedCount.

/**
    * Najde Hamiltonovsky cyklus
    *
    * @param StartNode      Pocatecni uzel pro hledani cyklu
    * @param Result         Vysledek hledani cyklu
    *
    * Tato funkce spousti hledani Hamiltonovskeho cyklu od zadaneho pocatecniho uzlu
*/
find_hammilton_cycle(StartNode, Result) :-
    hammilton_cycle(StartNode, StartNode, [], [StartNode], Result).


/************************************************************************/
/** Razeni uzlu a hran a mazani duplikatu */
/************************************************************************/


/**
    * Radi seznam cyklu podle abecedy
    *
    * @param UnsortedList   Seznam k razeni
    * @param SortedList     Seznam serazenych cyklu
 */
sort_cycle([], []).
sort_cycle([Cycle|Rest], [SortedCycle|SortedRest]) :-
    % Pro kazdy cyklus v seznamu pouzij funkci sort na serazeni jeho prvku
    maplist(sort, Cycle, SortedCycle),
    sort_cycle(Rest, SortedRest).

/**
    * Definuje predikát pro porovnani dvou dvojic podle prvniho prvku.
    *
    * @param Comparison     Vysledek porovnani
    * @param [X1,_]         Prvni dvojice
    * @param [Y1,_]         Druha dvojice
*/
compare_first_element(<, [X1,_], [Y1,_]) :- X1 @< Y1.
compare_first_element(>, [X1,_], [Y1,_]) :- X1 @> Y1.
compare_first_element(=, [X1,_], [Y1,_]) :- X1 == Y1.


/**
    * Predikat pro razeni seznamu cest podle prvniho prvku kazde dvojice.
    *
    * @param Paths          Neusporadany seznam cest
    * @param SortedPaths    Seznam cest serazenych podle prvniho prvku kazde dvojice
*/
sort_paths_by_first_element([], []).
sort_paths_by_first_element([Path|Paths], [SortedPath|SortedPaths]) :-
    % msort k razeni cesty podle prvniho prvku kazde dvojice
    msort(Path, SortedPath),
    sort_paths_by_first_element(Paths, SortedPaths).

/**
    * Odebere duplicitni cesty ze seznamu.
    *
    * @param Paths          Seznam cest s moznyymi duplikaty
    * @param UniquePaths    Seznam cest bez duplicit
*/
remove_duplicate_paths(Paths, UniquePaths) :-
    list_to_set(Paths, UniquePaths).


/************************************************************************/
/** Vypis */
/************************************************************************/


/**
    * Konvertuje cestu reprezentovanou jako seznam hran na retezec propojenych uzlu.
    *
    * @param Path           Cesta reprezentovana seznamem hran
    * @param String         Retezec propojenych uzlu
    *
    * Funkce postupne prochazi seznam hran a vytvari retezec propojenych uzlu.
*/
path_to_string([], '').
path_to_string([[Node1, Node2]|Rest], String) :-
    % Konvertuje hranu na retezec uzlu
    atomic_list_concat([Node1, '-', Node2], '', EdgeString),
    path_to_string(Rest, RestString),
    % Spoji retezec hrany a zbytek cesty
    atomic_list_concat([EdgeString, ' ', RestString], String).

/**
    * Konvertuje seznam cest na seznam retezcu propojenych uzlu.
    *
    * @param Paths          Seznam cest
    * @param Strings        Seznam retezcu propojenych uzlu
*/
paths_to_strings([], []).
paths_to_strings([Path|Paths], [String|Strings]) :-
    % Konvertuje jednotlive cesty na retezce
    path_to_string(Path, String),
    paths_to_strings(Paths, Strings).

/**
    * Přepíše seznam cest do specifikovaného formátu.
    *
    * @param Paths          Seznam cest
    * @param Result         Přepsaný seznam cest ve specifikovaném formátu
*/
rewrite_paths(Paths, Result) :-
    % Konvertuje seznam cest na seznam retezcu propojenych uzlu
    paths_to_strings(Paths, Strings),
    % Spoji retezce cest do jednoho retezce s novymi radky
    atomic_list_concat(Strings, '\n', Result).


/************************************************************************/
/** Hlavni funkce */
/************************************************************************/

/**
    * Hlavni funkce
    *
    * Funkce nacte vstup, vytvori hrany a uzly, najde Hamiltonovskou kruznici
    * a vypise vysledek.
*/
main :-
    % Nacteni vstupu
    prompt(_, ''),
    read_lines(LL),
    split_lines(LL,S),

    % Vytvoreni hran a uzlu a odstraneni duplikatu
    create_edges(S),
    create_nodes(S),
    remove_duplicates,
    
    % Ziskani prvního uzlu z databaze uzlu
    node(StartNode),
    
    % Hledani Hamiltonovske kruznice
    findall(Result, find_hammilton_cycle(StartNode, Result), Results),
    % Razeni cest
    sort_cycle(Results, SortedPairs),
    % Razeni cest podle prvniho prvku kazde dvojice
    sort_paths_by_first_element(SortedPairs, SortedPaths),
    % Odstraneni duplicitnich cest
    remove_duplicate_paths(SortedPaths, FilteredPaths),
    % Prepsani cest do specifikovaneho formatu
    rewrite_paths(FilteredPaths, Result),
    % Vypis vysledku
    write(Result),

    halt.