-module(ermake).

-doc([{author,'Joe Armstrong'},
      {title,"Erlang make utility."},
      {keywords, [make]},
      {date,981103}]).

-export([all/0, target/1, file/1, file/2]).

-import(lists, [delete/2, filter/2, foldl/3, map/2, max/1, member/2, zf/2]).

%% all()                          -> Makes first target in EMakefile
%% target(target::string())       -> Makes Target in EMakefile
%% file(file())                   -> Makes first target in File
%% file(file(), target::string()) -> Makes Target in File

all() -> file("EMakefile").

target(T) -> file("EMakefile", T).

file(File) -> make(File, top).

file(File, Target) -> make(File, {target, Target}).

make(File, Target) ->
    Lines  = ermake_parse:parse(File),
    Rules  = filter(fun(X) -> element(1, X) == make end, Lines),
    Suffix = filter(fun(X) -> element(1, X) == suffix end, Lines),
    Rules1 = add_extra_rules(Rules, Suffix),
    case Target of
	top ->
	    case hd(Rules) of
		{make, Ts, _, _} ->
		    make_everything(Ts, Rules1);
		_ ->
		    nothing_to_do
	    end;
	{target, T} ->
	    make_everything([T], Rules1)
    end.

add_extra_rules(Rules, Suffix) ->
    %% If a dependent is mentioned and
    %% there is no explicit rule for how to make the dependent then 
    %% add an extra rule if possible
    Targets    = [T || {make,Ts,_,_} <- Rules, T <- Ts],
    Dependents = [D || {make, _, Ds, _} <- Rules, D <- Ds],
    Missing    = filter(fun(I) -> not member(I, Targets) end, Dependents),
    Missing1   = remove_duplicates(Missing),
    Extra      = zf(fun(I) -> find_suffix_rule(I, Suffix) end, Missing1),
    Rules ++ Extra.

make_everything(Targets, Rules) ->
    Ps = [{T, D} || {make, Ts, Ds, _} <- Rules, T <- Ts, D <- Ds],
    %% trace all the rules we can reach from the root set
    L0 = transitive:closure(Targets, Ps),
    L = delete(true, L0),
    %% keep those rules that are mentioned in targets or destinations
    Ps1 = filter(fun({D,T}) ->
			member(D, L) or member(T, L)
		end, Ps),
    %% reverse the order to build the bottom up tree
    Ps2 = map(fun({I,J}) -> {J, I} end, Ps1),
    %% order the result
    case topological_sort:sort(Ps2) of
	{ok, Order0} ->
	    Order = delete(true, Order0),
	    %% Order is the absolute order to build things
	    Cmds = map(fun(I) -> select_rule(I, Rules) end, Order),
	    foldl(fun do_cmd/2, [], Cmds),
	    true;
	{cycle, Cycle} ->
	    exit({makefile,contains,cycle,Cycle})
    end.

%% find which rule is needed to build Target

select_rule(Target, Rules) ->
    Matches = [{make, Ts,Ds,Fun}|| {make,Ts,Ds,Fun} <- Rules, 
	                           member(Target, Ts)],
    case length(Matches) of
	0 -> {file, Target};
	1 -> hd(Matches);
	_ -> exit({multiple,rules,to,make,Target})
    end.

%% do_cmd(cmd(), made()) -> make()'
%%   cmd() = {make, Targets, Dependents, Fun} | {file, Target}
%%   made() = [Target, time()].

do_cmd({make, Bins, Srcs, Fun}, Made) ->
    case target_time(Bins, Made) of
	none ->
	    eval(Bins, Fun);
	{missing, M} ->
	    eval(Bins, Fun);
	{max, TBin} ->
	    case target_time(Srcs, Made) of
		{missing, M} ->
		    exit({'I don\'t know how to make',M});
		{max, TSrc} when TSrc > TBin ->
		    eval(Bins, Fun);
		{max, _} ->
		    true;
		none ->
		    exit({no,src,Srcs})
	    end
    end,
    update_times(Srcs ++ Bins, this_time(), Made);
do_cmd({file,H}, Made) ->
    update_times([H], this_time(), Made).

%% target_time(Targets, Made) ->  {max, Time} | {missing,M}
%%   none
%%     if no targets found
%%   {missing, M} 
%%     if target M is missing
%%   {max, Time} 
%%     Time is the last modified time of all the targets
%%     the limes are derived from either the Made list
%%     or from the time stamp of the file.

target_time(Targets, Made) ->
    target_time(Targets, Made, []).

target_time([H|T], Made, Times) ->
    case make_time(H, Made) of
	{yes, Time} ->
	    target_time(T, Made, [Time|Times]);
	no ->
	    case is_file(H) of
		true ->
		    target_time(T, Made, [last_modified(H)|Times]);
		false ->
		    {missing, H}
	    end
    end;
target_time([], Made, []) ->
    none;
target_time([], Made, Times) ->
    {max, max(Times)}.

make_time(X, [{X,Time}|_]) -> {yes, Time};
make_time(X, [_|T])        -> make_time(X, T);
make_time(X, [])           -> no.

update_times([H|T], Now, Made) ->
    case make_time(H, Made) of
	{yes, _} -> update_times(T, Now, Made);
	no ->
	    case is_file(H) of
		true ->
		    update_times(T, Now, [{H, last_modified(H)}|Made]);
		false ->
		    update_times(T, Now, [{H, Now}|Made])
	    end
    end;
update_times([], _, Made) ->
    Made.

%% see if a suffix rule can be applied to the file D

find_suffix_rule(D, Suffix) ->
    Ext = filename:extension(D),
    find_suffix_rule(Ext, D, Suffix).

find_suffix_rule(To, D, [{suffix, [From, To], Fun}|_]) ->
    Root = filename:rootname(D),
    Fun1 = expand_cmd(Fun, Root),
    {true, {make, [D], [Root ++ From], Fun1}};
find_suffix_rule(To, D, [_|T]) ->
    find_suffix_rule(To, D, T);
find_suffix_rule(_, _, []) ->
    false.

expand_cmd([$$,$>|T], Root) ->
    Root ++ expand_cmd(T, Root);
expand_cmd([H|T], Root) ->
    [H|expand_cmd(T, Root)];
expand_cmd([], _) ->
    [].

eval(_, []) ->
    true;
eval(Target, Str) ->
    io:format("make ~p ->~n~s~n",[Target, Str]),
    case erl_scan:tokens([], "fun() -> " ++ Str ++ " end. ", 1) of
	{done, {ok, Toks, _},_} ->
	    case erl_parse:parse_exprs(Toks) of
		{ok, [Parse]} ->
		    %% io:format("Parse = ~p~n",[Parse]),
		    Env0 = erl_eval:new_bindings(),
		    Call = [{call,9999,Parse,[]}],
		    case  erl_eval:exprs(Call, Env0) of
			{value, Val, _} ->
			    Val;
			O3 ->
			    exit({eval,error,O3})
		    end;
		O1 ->
		    exit({parse,error,o1,O1})
	    end;
	O2 ->
	    exit({tokenisation,error,O2})
    end.

%% Stuff that should have been in the libraries (sigh :-)

last_modified(F) ->
    case file:file_info(F) of
	{ok, {_, _, _, _, Time, _, _}} ->
	    Time;
	_ ->
	    exit({last_modified, F})
    end.

is_file(File) ->
    case file:file_info(File) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

remove_duplicates(L) ->
    foldl(fun(I, Acc) ->
		 case member(I, Acc) of
		     true -> Acc;
		     false -> [I|Acc]
		 end
	 end, [], L).

this_time() ->
    {Y,M,D} = date(),
    {H,Min,S} = time(),
    {Y,M,D,H,Min,S}.
