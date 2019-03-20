/*
a
*/
isBinaryTree(leaf(_)).
isBinaryTree(node(L, R, _)) :-
	isBinaryTree(L),
	isBinaryTree(R).
	
/*
b
*/
nnodes(leaf(_), N) :-
	N = 1.
nnodes(node(L, R, _), N) :-
	nnodes(L, N1),
	nnodes(R, N2),
	N is 1+N1+N2.

/*
d
*/
makeBinary(N, T) :-
	N < 0,
	T = false.
makeBinary(N, T) :-
	N == 0,
	T = leaf(N).
makeBinary(N, T) :-
	N > 0,
	NewN is N-1,
	makeBinary(NewN, R1),
	T = node(R1, R1, N).
	
/*
e
*/
makeTree(N, _, Tree) :-
	N = 0,
	Tree = leaf(N).
makeTree(N, NumberOfChildren, Tree) :-
	N > 0,
	NumberOfChildren > 0,
	NewN is N-1,
	makeTree(NewN, NumberOfChildren, T1),
	cryForHelp(T1, NumberOfChildren, NewLayer),
	Tree = node(NewLayer, N).
	
cryForHelp(Tree, NumberOfChildren, Children) :-
	length(Children, NumberOfChildren),
	maplist(=(Tree), Children).	
	