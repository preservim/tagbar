-module(ermake_parse).

-doc([{author,'Joe Armstrong'},
      {title,"Parser used by ermake."},
      {keywords, [parser,make]},
      {date,981029}]).

-export([parse/1]).

-import(lists, [reverse/1, prefix/2, map/2]).

parse(File) ->
    Dir = filename:dirname(code:which(?MODULE)),
    Lines = ermake_line_reader:read_file(File, [{"MAKEDIR", Dir}]),
    map(fun parse_line/1, Lines).

parse_line({File,Line,Str}) ->
    parse_str(Str).

parse_str([$S,$u,$f,$f,$i,$x,$ |T]) ->
    %% io:format("Suffix:~s~n",[T]),
    case split("->", T) of
	{yes, Pre, Fun} ->
	    Tmp = string:tokens(Pre,". "), 
	    Tmp1 = map(fun(I) -> [$.|I] end, Tmp),
	    {suffix, Tmp1, Fun};
	no ->
	    exit({'No -> in suffix rule', T})
    end;
parse_str(Str) ->
    case split("when", Str) of
	{yes, As, BsF} ->
	    case split("->", BsF) of
		%% As when Bs -> F
		{yes, Bs, F} ->
		    {make, parse_files(As), parse_files(Bs), 
		     parse_fun(F)};
		no ->
		    %% As when Bs.
		    {make, parse_files(As), parse_files(BsF), []}
	    end;
	no ->
	    %% A -> F
	    case split("->", Str) of
		no ->
		    exit({'No "when" or "->" in rule', Str});
		{yes, As, F} ->
		    {make, parse_files(As), [true], parse_fun(F)}
	    end
    end.

%% split(Prefix, String) -> {yes, Before, After} | no
%%    splits String at Prefix

split(Prefix, L) -> split(Prefix, L, []).

split(_, [], L) ->
    no;
split(Prefix, L, L1) ->
    case prefix(Prefix, L) of
	true ->
	    {yes, reverse(L1), string:substr(L, length(Prefix)+1)};
	false ->
	    split(Prefix, tl(L), [hd(L)|L1])
    end.

parse_files(Str) -> 
    Files = string:tokens(Str, [$ ,$,,$\n,$\t]).

parse_fun([$\n|F]) -> F;
parse_fun(F)       -> F.


