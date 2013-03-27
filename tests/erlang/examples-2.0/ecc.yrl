Nonterminals

production
form lhs  factor 
nested syntax char_prods charline char_rhs char_prim 
ignore moreignore expr term.

Terminals
atom var string quote '|'  '=' '}' '{' '(' ')' '[' ']'
'COMPILER' 'CHARACTERS' 'COMMENTS' 'FROM' 'TO' 'TOKENS'
'IGNORE' 'PRODUCTIONS' 'END' 'NESTED' 'EOL' 'CHR' 'ANY' integer comment
'+' '-' ';'.


Rootsymbol form.

form -> 'COMPILER' atom			: {compiler, unwrap('$2')}.
form -> 'CHARACTERS' char_prods    	: {characters, '$2'}.
form -> 'COMMENTS' 'FROM' string 
	          'TO' string nested 	: {comments,unwrap('$3'),unwrap('$5'),
						    '$6'}.
form -> 'TOKENS' syntax		        : {tokens, '$2'}.
form -> 'IGNORE' ignore 	        : {ignore, '$2'}.
form -> 'PRODUCTIONS' syntax 		: {syntax, '$2'}.
form -> 'END' atom			: {theend, '$2'}.
form -> comment.

nested -> 'NESTED'			: nested.
nested -> 'EOL'                         : eol.
nested -> '$empty'			: not_nested.

%% Character syntax

char_prods -> charline ';' char_prods	: ['$1'|'$3'].
char_prods -> charline			: ['$1'].

charline -> atom '=' char_rhs           : {unwrap('$1'), '$3'}.

char_rhs -> char_prim '+' char_rhs	: {plus, '$1', '$3'}.
char_rhs -> char_prim '-' char_rhs	: {minus, '$1', '$3'}.
char_rhs -> char_prim			: '$1'.

char_prim -> 'CHR' '(' integer ')'	: {chr, unwrap('$3')}.
char_prim -> string			: {string, unwrap('$1')}.
char_prim -> quote			: {string, unwrap('$1')}.
char_prim -> atom			: {atom, unwrap('$1')}.
char_prim -> 'ANY'			: any.

ignore -> var moreignore		: [unwrap('$1')|'$2'].

moreignore -> '+' ignore		: '$2'.
moreignore -> '$empty'			: [].

%% The following deifinitions are taken from  [WIR82]
%% WIR82 Programming in Modular2
%% Springer Verlag 1982

%% statement	: A syntactic  form
%% expression	: A list of alternatives
%% term		: A concatination of factors
%% factor	: A single syntactoc entity or a parenthesized expression

%% Construct
%% =========
%% [ A ]      = zero or more A's
%% { A }      = any number of A's
%% "A"        = a string parse tree
%% A | B      = A or B parse tree
%% A  B       = sequence of A followed by B
%% identifier = a name

%% syntax     = {production} 
%% production = id "=" expr ";"		
%% expr       = term {"|" term}        
%% term       = factor {factor}        
%% factor     = id | string "{" expr "}

syntax -> production ';' syntax 	: ['$1'|'$3'].
syntax -> production		  	: ['$1'].

production -> lhs '=' expr 	 	: {prod, '$1', '$3'}.

lhs -> var 				: unwrap('$1').
lhs -> atom 				: unwrap('$1').

expr -> term	 			: '$1'.
expr -> term '|' expr			: {alt, '$1', '$3'}.

term -> factor  			: '$1'.
term -> factor term			: {seq, '$1', '$2'}.

factor -> atom 				: {nt, unwrap('$1')}.
factor -> var  				: {ta, unwrap('$1')}.
factor -> string 			: {ts, unwrap('$1')}.
factor -> quote 			: {tq, unwrap('$1')}.
factor -> '[' expr ']' 			: {one, '$2'}.
factor -> '{' expr '}' 			: {star, '$2'}.
factor -> '(' expr ')' 			: {bracket, '$2'}.

Erlang code.

unwrap({_,_,V}) -> V.

simplify({Tag,A,nil}) -> A;
simplify(X) -> X.
