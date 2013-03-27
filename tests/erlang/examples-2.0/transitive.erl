-module(transitive).
%% Copyright (C) 1997, Ericsson Telecom AB

-doc([{author,'Joe Armstrong'},
      {title,"Transitive closure of a graph."},
      {keywords, [transitive,closure]},
      {date,981102}]).

%% warning slow on big graphs

-export([closure/2]).

%S tag1
closure(RootSet, Pairs) ->
    closure_list(RootSet, Pairs, RootSet).

closure(Start, [], L) ->
    L;
closure(Start, Pairs, Reachable) ->
    {Next, Rest} = next(Start, Pairs),
    closure_list(Next, Rest, Next ++ Reachable).

closure_list([], Pairs, Reachable) ->
    Reachable;
closure_list([H|T], Pairs, Reachable) ->
    Reachable1 = closure(H, Pairs, Reachable),
    closure_list(T, Pairs, Reachable1).

next(Start, Pairs) ->
    next(Start, Pairs, [], []).

next(Start, [], Reach, NoReach) ->
    {Reach, NoReach};
next(Start, [{Start,Next}|T], Reach, NoReach) ->
    next(Start, T, [Next|Reach], NoReach);
next(Start, [H|T], Reach, NoReach) ->
    next(Start, T, Reach, [H|NoReach]).
%E tag1

