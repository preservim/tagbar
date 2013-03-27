-module(sos_test4).
-doc(none).
-export([main/0]).

main() ->
    sos:write("I will crash now\n"),
    1 = 2,
    sos:write("This line will not be printed\n").
