-module(sos_test3).
-doc(none).
-export([main/0]).
-import(sos, [read/0, write/1]).

main() ->
    loop().

loop() ->
    case read() of
        eof ->
            true;
        {ok, X} ->
            write([X]),
            loop()
    end.
