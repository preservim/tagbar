Definitions.

Dig = [0-9]
Big = [A-Z]
Small = [a-z]
WS = [\000-\s]

COMMENT = \(\*\(*([^*)]|[^*]\)|\*[^)])*\**\*\)
STRING = "(\\\^.|\\.|[^"])*"
QUOTE = '(\\\^.|\\.|[^'])*' 

Rules.

({Small}({Small}|{Big}|{Dig}|_)*) : {token, {atom,YYline, YYtext}}.

({Big}({Small}|{Big}|{Dig}|_)*) : {token, special(YYtext, YYline)}.

({Dig}{Dig}*)	: {token, {integer, YYline, list_to_integer(YYtext)}}.

%% string

{STRING} : 		%% Strip quotes.
			S = lists:sublist(YYtext, 2, length(YYtext) - 2),
			{token,{string,YYline,string_gen(S)}}.

{QUOTE} : 		%% Strip quotes.
			S = lists:sublist(YYtext, 2, length(YYtext) - 2),
			{token,{quote,YYline,string_gen(S)}}.


{COMMENT} : .	


%%---------------------------------------------------------
%% Ignore stuff
%%---------------------------------------------------------
%% "{WHITE}". 	%% whitespace
%% "#.*".	   	%% Ignore Macro stuff for now
%% "{COMMENT}".	%% Ignore Comments

%% C   comments are /* ... */
%% Our comments are (* ... *) {we have to quote ( and * yuck
%%                             i.e. write \* and \(   }
%% 

%% COMMENT	"/\\*/*([^*/]|[^*]/|\\*[^/])*\\**\\*/".     (tobbe)
%% COMMENT	"(\\*/*([^*)]|[^*])|\\*[^)])*\\**\\*)".     (modified)
%% COMMENT	"\(\\*/*([^*\)]|[^*]\)|\\*[^\)])*\\**\\*\)". (quoted)

=  : {token, {'=', YYline}}.
\+ : {token, {'+', YYline}}.
\- : {token, {'-', YYline}}.
\; : {token, {';', YYline}}.
}  : {token, {'}', YYline}}.
{  : {token, {'{', YYline}}.
\[ : {token, {'[', YYline}}.
\] : {token, {']', YYline}}.
\( : {token, {'(', YYline}}.
\) : {token, {')', YYline}}.
\| : {token, {'|', YYline}}.
\: : {token, {':', YYline}}.

(.|\n) : skip_token.

\.[\s\t\n] : {end_token,{'$end', YYline}}.

Erlang code.

string_gen([$\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
    [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= 0, C =< $  ->
    string_gen(Cs);
string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $ ;				%\s = SPC
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.

remove_brackets([_,_|T]) ->
	[_,_|T1] = lists:reverse(T),
	lists:reverse(T1).

special("COMPILER", Line) -> {'COMPILER', Line};
special("CHARACTERS", Line) -> {'CHARACTERS', Line};
special("COMMENTS", Line) -> {'COMMENTS', Line};
special("FROM", Line) -> {'FROM', Line};
special("TO", Line) -> {'TO', Line};
special("TOKENS", Line) -> {'TOKENS', Line};
special("IGNORE", Line) -> {'IGNORE', Line};
special("PRODUCTIONS", Line) -> {'PRODUCTIONS', Line};
special("END", Line) -> {'END', Line};
special("NESTED", Line) -> {'NESTED', Line};
special("EOL", Line) -> {'EOL', Line};
special("CHR", Line) -> {'CHR', Line};
special("ANY", Line) -> {'ANY', Line};
special(Other, Line) -> {var, Line, Other}.




