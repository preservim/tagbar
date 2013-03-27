-module(ecc_parse).

-doc([{author, 'Joe Armstrong'},
      {title, "Parser for the <b>ecc</b> language."},
      {keywords,[ecc,parser,yecc,leex]},
      {date, 891106}]).

-export([make/0, file/1]).

%% usage 
%%    ecc_parse:file(File)
%%        Converts File.ebnf -> File.xbin
%%    ecc_parse:make()
%%        Makes the parser

make() ->
    %% The parser is made from
    %% ecc.yrl and ecc.xrl
    yecc:yecc("ecc", "ecc_yecc"),
    c:c(ecc_yecc),
    leex:gen(ecc, ecc_lex),
    c:c(ecc_lex).

file(F) ->
    io:format("Parsing ~s.ecc~n", [F]),
    {ok, Stream} = file:open(F ++ ".ecc", read),
    Parse = handle(Stream, 1, [], 0),
    file:close(Stream),
    Parse.

handle(Stream, LineNo, L, NErrors) ->
    handle1(io:requests(Stream, [{get_until,foo,ecc_lex,
			  tokens,[LineNo]}]), Stream, L, NErrors).

handle1({ok, Toks, Next}, Stream, L, Nerrs) ->
    case ecc_yecc:parse(Toks) of
	{ok, Parse} ->
	    handle(Stream, Next, [Parse|L], Nerrs);
	{error, {Line, Mod, What}} ->
	    Str = apply(Mod, format_error, [What]),
	    io:format("** ~w ~s~n", [Line, Str]),
	    handle(Stream, Next, L, Nerrs+1);
	Other ->
	    io:format("Bad_parse:~p\n", [Other]),
	    handle(Stream, Next, L, Nerrs+1)
    end;
handle1({eof, _}, Stream, L, 0) ->
    {ok, lists:reverse(L)};
handle1({eof, _}, Stream, L, N) ->
    {error, N};
handle1(What, Stream, L, Nerrs) ->
    io:format("Here:~p\n", [What]),
    handle(Stream, 1, L, Nerrs+1).

first([H]) -> [];
first([H|T]) -> [H|first(T)];
first([]) -> [].

