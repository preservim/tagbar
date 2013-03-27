-module(lin).

-doc([{author,'Joe Armstrong'},
      {title,"Linear algebra utilities."},
      {keywords, [linear,algebra]},
      {date,981103}]).

-export([pow/3, inv/2, solve/2, str2int/1, int2str/1, gcd/2]).

%% pow(A, B, M) => (A^B) mod M
%% examples pow(9726,3533,11413) = 5761
%%          pow(5971,6597,11413) = 9726

pow(A, 1, M) ->
    A rem M;
pow(A, 2, M) ->
    A*A rem M;
pow(A, B, M) ->
    B1 = B div 2,
    B2 = B - B1,
    %% B2 = B1 or B1+1
    P = pow(A, B1, M),
    case B2 of
	B1 -> (P*P) rem M;
	_  -> (P*P*A) rem M
    end.

%% inv(A, B) = C | no_inverse
%%    computes C such that
%%    A*C mod B = 1
%% computes A^-1 mod B
%% examples inv(28, 75) = 67.
%%          inv(3533, 11200) = 6597
%%          inv(6597, 11200) = 3533

inv(A, B) ->
    case solve(A, B) of
	{X, Y} ->
	    if X < 0 -> X + B;
	       true  -> X
	    end;
	_ ->
	    no_inverse
    end.

%% solve(A, B) => {X, Y} | insoluble
%%   solve the linear congruence
%%   A * X - B * Y = 1

%S tag1
solve(A, B) ->
    case catch s(A,B) of
	insoluble -> insoluble;
	{X, Y} ->
	    case A * X - B * Y of
		1     -> {X, Y};
		Other -> error
	    end
    end.

s(A, 0)  -> throw(insoluble);
s(A, 1)  -> {0, -1};
s(A, -1) -> {0, 1};
s(A, B)  ->
    K1 = A div B,
    K2 = A - K1*B,
    {Tmp, X} = s(B, -K2),
    {X, K1 * X - Tmp}.
%E tag1


%% converts a string to a base 256 integer
%% converts a base 256 integer to a string

%S tag2    
str2int(Str) -> str2int(Str, 0).

str2int([H|T], N) -> str2int(T, N*256+H);
str2int([], N) -> N.

int2str(N) -> int2str(N, []).

int2str(N, L) when N =< 0 -> L;
int2str(N, L) ->
    N1 = N div 256,
    H = N - N1 * 256,
    int2str(N1, [H|L]).
%E tag2

%% greatest common denominator

gcd(A, B) when A < B -> gcd(B, A);
gcd(A, 0) -> A;
gcd(A, B) ->
    gcd(B, A rem B).
