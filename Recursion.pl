/*
a
*/
member(Elem, [_|Tail]) :-
	member(Elem, Tail).
member(Elem, [Elem|_]).
	
/*
b
*/
subset([], _).
subset([Head|Tail], Set) :-
	member(Head, Set),
	subset(Tail, Set).
	
/*
c
*/
path(From, From, Graph, Visited, Path) :-
	append(Visited, [From], Path).
path(From, To, Graph, Visited, Path) :-
	member(edge(From, ToX), Graph),
	\+ member(ToX, Visited),
	append(Visited, [From], NewVisited),
	path(ToX, To, Graph, NewVisited, Path).
path(From, To, Graph, Visited, Path) :-
	member(edge(ToX, From), Graph),
	\+ member(ToX, Visited),
	append(Visited, [From], NewVisited),
	path(ToX, To, Graph, NewVisited, Path).

/*
d
*/
longest_paths(From, To, Graph, Paths) :-
	findall(Path, path(From, To, Graph, [], Path), Res),
	maplist(length, Res, PathLengths),
	findall(MaxLength, max_list(PathLengths, MaxLength), [Head|Tail]),
	select_element_on_condition(Res, Paths, Head).

is_length(List, Length, Res) :-
	length(List, Length),
	Res = List.
	
condition(X, Con) :-
	length(X, N),
	N =:= Con.
	
select_element_on_condition([Head|Tail], Head, Con) :-
	condition(Head, Con).
select_element_on_condition([_|Tail], Head, Con) :-
	select_element_on_condition(Tail, Head, Con).