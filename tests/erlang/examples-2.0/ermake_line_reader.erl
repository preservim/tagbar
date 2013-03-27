-module(ermake_line_reader).

-doc([{author,'Joe Armstrong'},
      {title,"Provide character level input for utilities such as make, lex, yacc etc. This module reads lines up to dot whitespace. Include files and named macros are expanded in place."},
      {keywords, [read,line,make,dot,whitespace,forms]},
      {date,981028}]).

%% This module provides a common *character level input*
%% For Erlang look-alike utilities such as make, yecc, lex
%% It provides
%% 1) Multiple line input terminated by dot white space
%% 2) Variables           VAR = Val, or VAR += Val
%% 3) Include files       include("File")
%% 4) comment stripping   %... are removed

-export([test/1, read_file/1, read_file/2]).

-import(lists, [keyreplace/4, keysearch/3, member/2, reverse/1, reverse/2]).

test(1) -> read_file("EMakefile");
test(2) -> read_file("test1").

read_file(File) ->
    read_file(File, []).

read_file(File, Macros) ->
    {Macros1, Lines} = read_lines(File, Macros, [File]),
    %% io:format("Macros were:~p~n",[Macros1]),
    trim(Lines).

trim([{{File,Line},Str}|T]) ->
    Leading = count_leading_nls(Str, Line),
    case trim_line(Str) of
	[] -> trim(T);
	Str1 -> [{File,Leading,Str1}|trim(T)]
    end;
trim([]) -> [].

trim_line(Str) ->
    Str1 = skip_white(Str),
    trim_end_of_line(Str1).

trim_end_of_line(Str1) ->
    case reverse(Str1) of
	[X,$.|Tmp] ->
	    reverse(Tmp);
	[] ->
	    [];
	Other ->
	    exit({oops,Other})
    end.

read_lines(File, Macros0, Stack) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    Lines = gather_lines(binary_to_list(Bin), File, 1, []),
	    %% io:format("Lines=~p~n",[Lines]),
	    expand(Lines, Macros0, Stack, []);
	_ ->
	    exit({cannot,read,file,File})
    end.

expand([{Where, H}|T], Macros, Stack, L) ->
    %% first expand any macros
    H1 = expand_macro(H, Macros, Where),
    %% now add any macro definitions
    case is_macro_defn(H1) of
	{new, Var, Val} ->
	    case keysearch(Var,1,Macros) of
		{value,{_,Val}} ->
		    %% same value no problem
		    expand(T, Macros, Stack, L);
		{value,{_,Replacement}} ->
		    %% some other value
		    exit({error, Where, cannot,redefine,macro,
			  Var,was,Replacement,is,Val});
		false ->
		    %% new definition
		    expand(T, [{Var,Val}|Macros], Stack, L)
	    end;
	{plus, Var, Val} ->
	    case keysearch(Var,1,Macros) of
		{value,{_,Old}} ->
		    %% some other value
		    Macros1 = keyreplace(Var,1,Macros,{Var,Old++Val}),
		    expand(T, Macros1, Stack, L);
		false ->
		    exit({error, Where, no,previous,defn,for,Var})
	    end;
	no ->
	    case is_include(H1, Where) of
		{yes, File} ->
		    case member(File, Stack) of
			true ->
			    exit({error, Where, recursive_include, File});
			false ->
			    {Macros1, Lines1} = read_lines(File,Macros,Stack),
			    expand(T, Macros1, Stack, reverse(Lines1, L))
		    end;
		no ->
		    expand(T, Macros, Stack, [{Where, H1}|L])
	    end
    end;
expand([], Macros, Stack, L) ->
    {Macros, reverse(L)}.

expand_macro([$$,$(|T], Macros, Where) ->
    case is_var(T) of
        {yes, Var, [$)|T1]} ->
            case keysearch(Var,1,Macros) of
                {value,{_,Replacement}} ->
                    Replacement ++ expand_macro(T1, Macros, Where);
                false ->
                    exit({error,Where,undefined,macro,Var})
            end;
        no ->
            [$$,$(|expand_macro(T, Macros, Where)]
    end;
expand_macro([H|T], Macros, Where) ->
    [H|expand_macro(T, Macros, Where)];
expand_macro([], Macros, _) ->
    [].

is_include(Line, Where) ->
    case skip_white(Line) of
	[$i,$n,$c,$l,$u,$d,$e,$(,$"|T] ->
	    {File, T1} = get_quoted([$"|T], Where),
	    case skip_white(T1) of
		[$)|_] ->
		    {yes, File};
		_ ->
		    exit({Where,bad,include,syntax})
	    end;
	_ ->
	    no
    end.

is_macro_defn(Line) ->
    Str1 = skip_white(Line),
    case is_var(Str1) of
	{yes, Var, Str2} ->
	    case skip_white(Str2) of
		[$=|T] ->
		    {new, Var, trim_end_of_line(T)};
		[$+,$=|T] ->
		    {plus, Var, trim_end_of_line(T)};
		_ ->
		    no
	    end;
	no ->
	    no
    end.

is_var([H|T]) when $A =< H, H =< $Z ->
    collect_var(T, [H]);
is_var(_) ->
    no.

collect_var([H|T], L) when $A =< H, H =< $Z ->
    collect_var(T, [H|L]);
collect_var([H|T], L) when $1 =< H, H =< $9 ->
    collect_var(T, [H|L]);
collect_var([H|T], L) when $a =< H, H =< $z ->
    collect_var(T, [H|L]);
collect_var(X, L) ->
    {yes, reverse(L), X}.

skip_white([$ |T])  -> skip_white(T);
skip_white([$\n|T]) -> skip_white(T);
skip_white([$\t|T]) -> skip_white(T);
skip_white(T)       -> T.

gather_lines([], File, N, L) ->
    reverse(L);
gather_lines(Str, File, N, L) ->
    {Line, Str1} = get_line(Str, {File, N}, []),
    Width = count_nls(Line, 0),
    gather_lines(Str1, File, N + Width, [{{File,N},Line}|L]).

count_nls([$\n|T], N) -> count_nls(T, N+1);
count_nls([_|T], N)   -> count_nls(T, N);
count_nls([], N)      -> N.
    
count_leading_nls([$\n|T], N) -> count_leading_nls(T, N+1);
count_leading_nls(_, N)       -> N.

%% get_line collects a line up to . <white>

get_line([$.,X|T], Where, L) ->
    case X of
	$\n ->
	    {reverse(L, [$.,$\n]), T};
	$ ->
	    {reverse(L, [". "]), T};
	$\t ->
	    {reverse(L, [$.,$\t]), T};
	_ ->
	    get_line(T, Where, [X,$.|L])
    end;
get_line([$"|T], Where, L) ->
    {Str, T1} = get_quoted([$"|T], Where),
    get_line(T1, Where, [$"|reverse(Str, [$"|L])]);
get_line([$'|T], Where, L) ->
    {Str, T1} = get_quoted([$'|T], Where),
    get_line(T1, Where,[$'|reverse(Str, [$'|L])]);
get_line([$%|T], Where, L) ->
    %% remove the comment
    T1 = skip_to_eol(T),
    get_line(T1, Where, L);
get_line([H|T], Where, L) ->
    get_line(T, Where, [H|L]);
get_line([], Where, L) ->
    {reverse(L), []}.

skip_to_eol([$\n|T]) -> [$\n|T];
skip_to_eol([_|T])   -> skip_to_eol(T);
skip_to_eol([])      -> [].


%% get_quoted(string(), {file(),line()}) -> {quoted(), rest()}
%% The " ' is not included

get_quoted([End|T], Where) ->
    get_quoted(T, Where, End, []).

get_quoted([End|T], Where, End, Acc) -> 
    {reverse(Acc), T};
get_quoted([$\\,C|T], Where, End, Acc) ->
    get_quoted(T, Where, End, [quoted(C)|Acc]);
get_quoted([$\n|_], {File,Line}, _, _) ->
    exit({error, file, File, line, Line,
	  "newline not allowed in string"});
get_quoted([H|T], Where, End, Acc) ->
    get_quoted(T, Where, End, [H|Acc]);
get_quoted([], {File,Line}, _, _) ->
    exit({error, file, File, line, Line,
	  "end of line not allowed in string"}).

%% Quoted characters

quoted($n) -> $\n;
quoted($t) -> $\t;
quoted($r) -> $\r;
quoted($b) -> $\b;
quoted($v) -> $\v;
quoted(C)  -> C.
