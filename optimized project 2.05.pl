%Code for while Project by The Deadly Packet Team.

% grid_build(N,M) should succeed only if M represents a grid that is N by N such that each cell in M contains an unbound variable.

grid_build(N,M):-
	gridBuildHelper(N,N,M,[]).

gridBuildHelper(0,_,R,R).
gridBuildHelper(Counter,N,Result,Acc):-
     Counter>0,
	 length(L,N),
	 Counter1 is Counter-1,
	 gridBuildHelper(Counter1,N,Result,[L|Acc]).
	 


%num_gen(F,L,R) should succeed only if R represents a list of consecutive numbers starting from F until L.

num_gen(L,L,[L]).
num_gen(F,L,[F|R1]):-F<L, F1 is F+1 ,num_gen(F1,L,R1).


% is_subset (L,R) checks if L is subset of R (There is a predefined predicate doing same job).

is_subset([],_).
is_subset([H|T],L):-member(H,L),is_subset(T,L).

% Takes in a list of lists and returns one list with all items together in a single list.

flatten([],[]).
flatten([H|T],R):- is_list(H),flatten(H,R1),flatten(T,R2),append(R1,R2,R).
flatten([H|T],[H|R]):- \+is_list(H),flatten(T,R).

% check_num_grid(G) succeeds if G does not contain a number X unless all the numbers 1 .. X-1 are there.

check_num_grid(L):-flatten(L,FlattenedL),max_list(FlattenedL,Max),length(L,Length),Max=<Length,num_gen(1,Max,R),is_subset(R,FlattenedL),!.


% getNelement(N,L,R) is true  if R is the Nth element in L

getNelement(1,[H|_],H).
getNelement(N,[_|T],R):-N>1,N1 is N-1,getNelement(N1,T,R).


% getColoumn or Row (Number of R Or C,Grid,Result) is true if Result is the Nth coloumn or row of the Grid.

getColoumn(_,[],[]).
getColoumn(N,[H|T],[R|R1]):- getNelement(N,H,R),getColoumn(N,T,R1).

getRow(1,[H|_],H).
getRow(N,[_|T],R):-N>1,N1 is N-1,getRow(N1,T,R).


% acceptable_distribution(G) should succeed only if no row is placed in a column with the same index and no column is placed in a row with the same index.

acceptable_distribution(G):-
                          length(G,Max),
						  Max>=1,
						  acceptable_distribution(1,Max,G).

acceptable_distribution(Start,Max,_):-Start>Max.
acceptable_distribution(Start,Max,G):-
                           Start=<Max,
						   getColoumn(Start,G,C),
						   getRow(Start,G,R),
						   R\=C,
						   Start1 is Start+1,
						   acceptable_distribution(Start1,Max,G).
						   

% trans(M,M1) should succeed only if M1 represents a transposed version of the matrix M.

trans(G,Result):-length(G,Max),trans(1,Max,G,Result,[]).						   
trans(_,_,[],[],_).
trans(Counter,Max,G,R,Acc):-Max>=Counter,
getColoumn(Max,G,Coloumn),
Max1 is Max-1,
trans(Counter,Max1,G,R,[Coloumn|Acc]).
trans(Counter,Max,_,Acc,Acc):-Max<Counter.


% distinct_rows(M) should succeed only if M represents a matrix M where all rows are unique.

distinct_rows([]).
distinct_rows([H|T]):- \+member(H,T),distinct_rows(T).

% distinct_columns(M) should succeed only if M represents a matrix M where all columns are unique.

distinct_columns(G):- trans(G,R),distinct_rows(R).





%genList(Start,N,Count,R,Acc) succeed of R is a list containing numbers within start to N with length N 
%note:counter starts with 0 and Acc with []

genList(_,N,_,R,R):-length(R,X),X==N.
genList(Start,N,Count,R,Acc):-Count<N,Start=<N,Count1 is Count+1,
							genList(Start,N,Count1,R,[Start|Acc]).

genList(Start,N,Count,R,Acc):-(Count<N,Start<N,Count1 is Count+1,
							Start1 is Start+1,
					genList(Start1,N,Count1,R,[Start1|Acc]));
					(Count<N,Start<N,Start1 is Start+1,genList(Start1,N,Count,R,[])).


% succeed if L is a list with elements from 1 to N 

genList(L,N):-setof(X,genList(1,N,0,X,[]),R),member(L,R).

%listOfGenList(L,N) succeed of L is a List containing all lists with all possible combinations for numbers form 1 to N

listOfGenList(L,N):- setof(X,genList(X,N),L).




%permuteList(L,R) succeed if L is List of Lists then R is a list containing all possible combinations(without repetition) of every list in L.

permuateList([],[]).
permuateList([H|T],Result):- setof(X,permutation(H,X),HResult),permuateList(T,TResult),append(HResult,TResult,Result).

% A predicate to combine listOfGenList and permuteList in one Call succeed when  L is a list that contains lists for all possible selection and arrangement of numbers from 1 to N.
 
getAllPossibleList(N,L):- listOfGenList(X,N),permuateList(X,L).



% gridGen2 takes M as a Grid and bind every row in it with an element form L.

gridGen2([],_).
gridGen2([H|T],L):- member(H,L),gridGen2(T,L).


% acceptable_distribution(G) should succeed only if no row is placed in a column with the same index and no column is placed in a row with the same index.
						  
notCorresponding([],[]).
notCorresponding([H1|T1],[H2|T2]):-H1\=H2,notCorresponding(T1,T2).
acceptable_permutation(L,R):-permutation(L,R),notCorresponding(L,R).



%getPost(L,Target,R) succeed if R is the postion of Target in the List L.

getPos([],_,0).
getPos([Target|_],Target,1).
getPos([H|T],Target,R):- H\=Target,getPos(T,Target,R1),R is R1+1.

%getPosList(L,R) succeed if R is a list of postions of the numbers from 1 to N in the List L.

getPosList(_,Start,N,[]):-Start>N.
getPosList(L,Start,N,[R|Remain]):-Start=<N,getPos(L,Start,R),Start1 is Start+1, getPosList(L,Start1,N,Remain).
getPosList(L,R):- length(L,N),getPosList(L,1,N,R).


% Binds rowns and columns together it is a helper for finalBinding.

bindRowsColoumns([],_,[]).
bindRowsColoumns([H|T],TG,[HPos|TPos]):- getNelement(HPos,TG,Coloumn),H=Coloumn,bindRowsColoumns(T,TG,TPos).

% it takes N and M and Binds the rows and coloumns of M to each other.

finalBinding(N,M):- trans(M,TM),num_gen(1,N,RowsOrder),acceptable_permutation(RowsOrder,ColoumnsOrder),
						getPosList(ColoumnsOrder,Pos),bindRowsColoumns(M,TM,Pos).
								
% Main Predicate in project that solves/cheks solution of the puzzle.

helsinki(N,M):- grid_build(N,M),finalBinding(N,M),getAllPossibleList(N,L),gridGen2(M,L),distinct_rows(M),check_num_grid(M).

















