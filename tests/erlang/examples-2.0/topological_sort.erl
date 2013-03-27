-module(topological_sort).

%% Copyright (C) 1998, Ericsson Computer Science Laboratory

-doc([{author,'Joe Armstrong'},
      {title,"Topological sort of a partial order."},
      {keywords, [topological,sort,partial,order]},
      {date,981102}]).


-export([sort/1, test/0]).

-import(lists, [map/2, member/2, filter/2]).

%% -type([{X, X}]) -> {ok, [{X,Y}]} | {cycle, [{X,Y}]}
%% topological_sort:pairs(L) 

%% A partial order on the set S is a set of pairs {Xi,Xj} such that
%% some relation between Xi and Xj is obeyed. 

%% A topological sort of a partial order is a sequence of elements
%% [X1, X2, X3 ...] such that if whenever {Xi, Xj} is in the partial
%% order i < j

test() ->
    Pairs = [{1,2},{2,4},{4,6},{2,10},{4,8},{6,3},{1,3},
             {3,5},{5,8},{7,5},{7,9},{9,4},{9,10}],
    sort(Pairs).

%% [7,1,9,2,4,6,3,5,8,10]
%S tag1
sort(Pairs) ->
    iterate(Pairs, [], all(Pairs)).

iterate([], L, All) ->
    {ok, remove_duplicates(L ++ subtract(All, L))};
iterate(Pairs, L, All) ->
    case subtract(lhs(Pairs), rhs(Pairs)) of
	[]  -> 
	    {cycle, Pairs};
	Lhs -> 
	    iterate(remove_pairs(Lhs, Pairs), L ++ Lhs, All)
    end.

all(L) -> lhs(L) ++ rhs(L).
lhs(L) -> map(fun({X,_}) -> X end, L).
rhs(L) -> map(fun({_,Y}) -> Y end, L).

%% subtract(L1, L2) -> all the elements in L1 which are not in L2

subtract(L1, L2) ->  filter(fun(X) -> not member(X, L2) end, L1).

remove_duplicates([H|T]) ->
  case member(H, T) of
      true  -> remove_duplicates(T);
      false -> [H|remove_duplicates(T)]
  end;
remove_duplicates([]) ->
    [].

%% remove_pairs(L1, L2) -> L2' L1 = [X] L2 = [{X,Y}]
%%   removes all pairs from L2 where the first element
%%   of each pair is a member of L1

remove_pairs(L1, L2) -> filter(fun({X,Y}) -> not member(X, L1) end, L2).
%E tag1
