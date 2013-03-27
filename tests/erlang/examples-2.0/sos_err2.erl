-module(sos_err2).
-doc(none).
-export([main/0]).

main() ->
    lists:abc(123),
    sos:write("Hello world\n").
