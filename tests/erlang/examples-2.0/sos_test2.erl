-module(sos_test2).
-doc(none).
-export([main/0]).

main() ->
    X = lists:reverse("Hellow world"),
    sos:write([X,"\n"]).
