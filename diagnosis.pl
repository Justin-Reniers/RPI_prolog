:- [tp].
:- use_module(library(pairs)).

% Definition of logical gates, used in the examples below.
and_gate(all X:(and(X) , ~ab(X) => (in1(X), in2(X) <=> out(X)))).
or_gate( all X:(or(X)  , ~ab(X) => (in1(X) ; in2(X) <=> out(X)))).
xor_gate(all X:(xor(X) , ~ab(X) => (out(X) <=> in1(X),~in2(X);~in1(X),in2(X)))).

% Two unconnected AND gates with two inputs. It is observed that the
% inputs are true and the outputs are false.
problem1(SD, COMP, OBS) :- 
  and_gate(AND),
  SD = [ AND, and(a1), and(a2) ],
  COMP = [a1, a2],
  OBS = [in1(a1), in2(a1), ~out(a1), in1(a2), in2(a2), ~out(a2)].

% Example of wwo AND gates where the output of the first gate (a1) is
% connected to the first input (in1) of the second gate (a2). It is
% easy to see that the observations are inconsistent with the
% specification.
problem2(SD, COMP, OBS) :-
  and_gate(AND),
  SD = [ AND, and(a1), and(a2), out(a1) <=> in1(a2) ],
  COMP = [a1, a2],
  OBS = [in1(a1), ~in2(a1), out(a2)].
  
% Another wiring example, now with two AND gates and an OR gate. 
problem3(SD, COMP, OBS) :-
  and_gate(AND), or_gate(OR),
  SD = [ AND, OR, and(a1), and(a2), or(o1),
         out(a1) <=> in1(o1), out(a2) <=> in2(o1)], 
  COMP = [a1, a2, o1],
  OBS = [in1(a1), in2(a1), in1(a2), in2(a2), ~out(o1)].

% The following represents a (one-bit) full adder: a
% circuit that can be used for the addition of two bits with 
% carry-in and carry-out bits.
%
% in1(fa), in2(fa): input bits
% carryin(fa):      carry-in bit
% out(fa):          output bit
% carryout(fa):     carry-out bit
%
% returns the sum of in1(fa) + in2(fa) + carryin(fa)
% as 2 * carryout(fa) + out(fa) (i.e., as 2 bits)
fulladder(SD, COMP, OBS) :-
  and_gate(AND), or_gate(OR), xor_gate(XOR), 
  SD = [AND, OR, XOR,
	and(a1), and(a2), xor(x1), xor(x2), or(r1),
        in1(fa) <=> in1(x1), in1(fa) <=> in1(a1),
        carryin(fa) <=> in1(a2), carryin(fa) <=> in2(x2),
	out(fa) <=> out(x2), carryout(fa) <=> out(r1),
	in2(fa) <=> in2(x1), in2(fa) <=> in2(a1), 
        out(x1) <=> in2(a2), out(x1) <=> in1(x2),
        out(a2) <=> in1(r1), out(a1) <=> in2(r1) ], 
  COMP = [a1, a2, x1, x2, r1],
  OBS = [in1(fa), ~in2(fa), carryin(fa), out(fa), ~carryout(fa)]. %1+1=1?
  
 %--------------------------------------------------------------------------
  
% Makes singleton lists out of all Components in the current conflict set.
listifyCS([], ListCS) :-
	ListCS = [].
listifyCS([Head|Tail], ListCS) :-
	listifyCS(Tail, NewList),
	append([[Head]], NewList, ListCS).

% Creates a list of singleton lists of the current conflict set (listifyCS), creates a list of paths 
% (appends  
makeChildren(SD, COMP, OBS, PHS, CS, HT) :-
	listifyCS(CS, SingletonsCS),
	maplist(append(PHS), SingletonsCS, PHSes),
	maplist(makeHittingTree(SD, COMP, OBS), PHSes, HT).
	
% Creates the hitting tree for a System. If there is no conflict set given, the current node is a leaf node with
% the path towards it as PHS (previous hitting sets).
makeHittingTree(SD, COMP, OBS, PHS, HT) :-
	not(tp(SD, COMP, OBS, PHS, _)),
	HT = leaf(PHS).
makeHittingTree(SD, COMP, OBS, PHS, HT) :-
	tp(SD, COMP, OBS, PHS, CS),
	makeChildren(SD, COMP, OBS, PHS, CS, NewHT),
	HT = node(NewHT, CS, PHS).

% A slightly more efficient version of makeHittingTree. This one only has to check for conflict sets
% if there are none left, compared to the first 	
makeHittingTree2(SD, COMP, OBS, PHS, HT) :-
	tp(SD, COMP, OBS, PHS, CS),
	makeChildren(SD, COMP, OBS, PHS, CS, NewHT),
	HT = node(NewHT, CS, PHS),
	!.
makeHittingTree2(SD, COMP, OBS, PHS, HT) :-
	not(tp(SD, COMP, OBS, PHS, _)),
	HT = leaf(PHS).
	
% Peels away the outer list layer of nested lists. 
flattenOneLayer([], []).
flattenOneLayer([Head|Tail], Res) :-
	is_list(Head), 
	flattenOneLayer(Tail, Tail1),
	!,
	append(Head, Tail1, Res).
flattenOneLayer([Head|Tail], [Head|Tail1]) :-
	flattenOneLayer(Tail, Tail1).

% Gathers all the paths towards the leaf nodes of the hitting tree (the hitting sets) and appends them to the result. 
gatherDiagnoses(HT, Res) :-
	HT = leaf(PHS),	
	append([PHS], [], Res).
gatherDiagnoses(HT, Diagnoses) :-
	HT = node(SHT, _, _),
	maplist(gatherDiagnoses, SHT, SDiagnoses),
	flattenOneLayer(SDiagnoses, Res),
	Diagnoses = Res.	
	
% Credits go to https://stackoverflow.com/questions/13733496/prolog-removing-supersets-in-a-list-of-lists
% Removes all super sets from the given list. Requires the list to be ordered.
rem_super_sets([], []).
rem_super_sets([L|Ls], R) :-
	(select(T, Ls, L1),
	subset(L, T)
	->	rem_super_sets([L|L1], R)
	;	R = [L|L2],
		rem_super_sets(Ls, L2)
	).

% Creates key-value pairs for the list of hitting sets (the key being the size of the hitting set, the value being the 
% hitting set) and sorts them smallest first.
sortDiagnosesByLength(Atoms, ByLength) :-
	map_list_to_pairs(length, Atoms, Pairs),
	keysort(Pairs, Sorted),
	pairs_values(Sorted, ByLength).

% Gathers the subset-minimal diagnoses.
getMinimalDiagnoses(Diagnoses, MinimalDiagnoses) :-
	sortDiagnosesByLength(Diagnoses, SortedDiagnoses),
	rem_super_sets(SortedDiagnoses, MinimalDiagnoses).

% Gathers the subset-minimal diagnoses given a System through the use of a hitting tree.
main(SD, COMP, OBS, PHS, MinimalDiagnoses) :-
	makeHittingTree(SD, COMP, OBS, PHS, THT),
	gatherDiagnoses(THT, Diagnoses),
	getMinimalDiagnoses(Diagnoses, MinimalDiagnoses).
	
% The same as main/5, but uses makeHittingTree2/5 (the slightly more efficient version) instead of makeHittingTree/5.
main2(SD, COMP, OBS, PHS, MinimalDiagnoses) :-
	makeHittingTree2(SD, COMP, OBS, PHS, THT),
	gatherDiagnoses(THT, Diagnoses),
	getMinimalDiagnoses(Diagnoses, MinimalDiagnoses).
	
% Calculates the running time for the fulladder system using main/5.
runningTime1(Runtime) :-
	statistics(runtime,[Start|_]),
	fulladder(SD, COMP, OBS),
	main(SD, COMP, OBS, [], _),
	statistics(runtime, [Stop|_]),
	Runtime is Stop - Start.

% Calculates the running time for the fulladder system using main2/5.
runningTime2(Runtime) :-
	statistics(runtime,[Start|_]),
	fulladder(SD, COMP, OBS),
	main2(SD, COMP, OBS, [], _),
	statistics(runtime, [Stop|_]),
	Runtime is Stop - Start.
