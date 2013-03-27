-module(error_handler).

-doc([{author,joe},
      {title,"Special version of error handler used by sos.erl"},
      {date,981012}]).

-export([undefined_function/3,undefined_global_name/2]).

undefined_function(sos, F, A) ->
    erlang:display({error_handler,undefined_function,
		    sos,F,A}),
    exit(oops);
undefined_function(M, F, A) ->
    case sos:load_module(M) of
	{ok, M} ->
	    case erlang:function_exported(M,F,length(A)) of
		true ->
		    apply(M, F, A);
		false ->
		    sos:stop_system({undef,{M,F,A}})
	    end;
	{ok, Other} ->
	    sos:stop_system({undef,{M,F,A}});
	already_loaded ->
	    sos:stop_system({undef,{M,F,A}});
	{error, What} ->
	    sos:stop_system({load,error,What})
    end.
undefined_global_name(Name, Message) ->
    exit({badarg,{Name,Message}}).
