-module(sos_err1).
-doc(none).
-export([main/0]).

main() ->
    listsforeaack:abc(123),
    sos:write("Hello world\n").
