-module(sos_test1).
-doc(none).
-export([main/0]).

main() ->
    sos:write("Hello world\n").
