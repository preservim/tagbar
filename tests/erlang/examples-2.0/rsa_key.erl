-module(rsa_key).

-export([make_sig/1, make_sig/2]).

make_sig(Who, Len) when Len > 79 ->
    {Public, Private} = make_sig(Len),
    file:write_file(Who ++ ".pub", term_to_binary(Public)),
    file:write_file(Who ++ ".pri", term_to_binary(Private)),
    {keyfilecreated,for,Who}.

%% The "book" says ...
%% 1. Bob generates two primes p and q
%% 2. Bob computes n = pq and phi(n) = (p-1)*(q-1)
%% 3. Bob chooses a random b(0 < b < phi(n)) such that
%%    gcd(b, phi(n)) = 1
%% 4. Bob computes a = b^(-1) mod phi(n) using the Euclidean algorithm
%% 5. Bob publishes n and  b in a directory as his public key.

%S tag1
make_sig(Len) ->
    %% generate two <Len> digit prime numbers
    P = primes:make_prime(Len),
    io:format("P = ~p~n", [P]),
    Q = primes:make_prime(Len),
    io:format("Q = ~p~n", [Q]),
    N = P*Q,
    io:format("N = ~p~n", [N]),
    Phi = (P-1)*(Q-1),
    %% now make B such that B < Phi and gcd(B, Phi) = 1
    B = b(Phi),
    io:format("Public key (B) = ~p~n", [B]),
    A = lin:inv(B, Phi),
    io:format("Private key (A) = ~p~n", [A]),
    {{B,N},{A,N}}.

b(Phi) ->
    io:format("Generating a public key B "),
    K = length(integer_to_list(Phi)) - 1,
    B = b(1, K, Phi),
    io:format("~n", []),
    B.

b(Try, K, Phi) ->
    io:format("."),
    B = primes:make(K),
    if 
	B < Phi ->
	    case lin:gcd(B, Phi) of
		1 -> B;
		_ -> b(Try+1, K, Phi)
	    end;
	true ->
	    b(Try, K, Phi)
    end.
%E tag1

