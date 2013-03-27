-module(sos).

-doc([{author,joe},
      {title,"Simple OS written entirely in Erlang"},
      {keywords, [os]},
      {htmldoc, "sos.html"},
      {date,981012}]).

-export([main/0,                % starts the system
         load_module/1,         % 
         log_error/1, 
         make_server/3, 
         cast/2, 
         rpc/2, 
         change_behaviour/2, 
         keep_alive/2, 
         make_global/2,
         on_exit/2, 
         on_halt/1, 
         stop_system/1, 
         every/3, 
         spawn_fun/1,
         spawn_link_fun/1, 
         lookup/2, 
         map/2,
         reverse/1,
         read/0,
         write/1, 
         env/1,
	 make_scripts/0
        ]).

-export([internal_call/1]).

main() ->
    make_server(io,           
                fun start_io/0, fun handle_io/2),
    make_server(code,         
                const([init,erl_prim_loader]),
                fun handle_code/2),
    make_server(error_logger, 
                const(0),  fun handle_error_logger/2),
    make_server(halt_demon,   
                const([]), fun handle_halt_demon/2),
    make_server(env,
                fun start_env/0, fun handle_env/2), 
    load_module(error_handler),
    Mod = get_module_name(),
    load_module(Mod),
    run(Mod).

run(Mod) ->
    Pid = spawn_link(Mod, main, []),
    on_exit(Pid, fun(Why) -> stop_system(Why) end).

load_module(Mod) -> rpc(code, {load_module, Mod}).

handle_code({load_module, Mod}, Mods) ->
    case member(Mod, Mods) of
        true ->
            {already_loaded, Mods};
        false ->
            case primLoad(Mod) of
                ok ->
                    {{ok,Mod}, [Mod|Mods]};
                Error ->
                    {Error, Mods}
            end
    end.

primLoad(Module) ->
    Str = atom_to_list(Module),
    case erl_prim_loader:get_file(Str ++ ".jam") of
        {ok, Bin, FullName} ->
            case erlang:load_module(Module, Bin) of
                {module, Module} ->
                    ok;
                {module, _} ->
                    {error, wrong_module_in_binary};
                Other ->
                    {error, {bad_object_code, Module}}
            end;
        error ->
            {error, {cannot_locate, Module}}

    end.

log_error(Error)  ->  cast(error_logger, {log, Error}).

handle_error_logger({log, Error}, N) ->
    erlang:display({error, Error}),
    {ok, N+1}.

%S tag4
on_halt(Fun)     -> cast(halt_demon,{on_halt,Fun}).
stop_system(Why) -> cast(halt_demon,{stop_system,Why}).
%E tag4

handle_halt_demon({on_halt, Fun}, Funs) ->
    {ok, [Fun|Funs]};
handle_halt_demon({stop_system, Why}, Funs) ->
    case Why of
        normal -> true;
        _      -> erlang:display({stopping_system,Why})
    end,
    map(fun(F) -> F() end, Funs),
    erlang:halt(),
    {ok, []}.     

%S tag5
read()   -> rpc(io, read).
write(X) -> rpc(io, {write, X}).
%E tag5

start_io() -> 
    Port = open_port({fd,0,1}, [eof, binary]),
    process_flag(trap_exit, true),
    {false, Port}.

handle_io(read, {true, Port}) -> 
    {eof, {true, Port}};
handle_io(read, {false, Port}) ->
    receive
        {Port, {data, Bytes}} ->
            {{ok, Bytes}, {false, Port}};
        {Port, eof} ->
            {eof, {true,Port}};
        {'EXIT', Port, badsig} ->
            handle_io(read, {false, Port});
        {'EXIT', Port, Why} ->
            {eof, {true, Port}}
    end;
handle_io({write,X}, {Flag,Port}) -> 
    Port ! {self(), {command, X}},
    {ok, {Flag, Port}}.

env(Key) -> rpc(env, {lookup, Key}).

handle_env({lookup, Key}, Dict) -> 
    {lookup(Key, Dict), Dict}.

start_env() ->
    Env = case init:get_argument(environment) of
              {ok, [L]} -> 
                   L;
              error -> 
                   fatal({missing, '-environment ...'})
        end,
    map(fun split_env/1, Env).

split_env(Str) -> split_env(Str, []).

split_env([$=|T], L) -> {reverse(L), T};
split_env([], L)     -> {reverse(L), []};
split_env([H|T], L)  -> split_env(T, [H|L]).

make_server(Name, FunD, FunH) ->
    make_global(Name, 
                fun() -> 
                       Data = FunD(),
                       server_loop(Name, Data, FunH) 
               end).

server_loop(Name, Data, Fun) ->
    receive
        {rpc, Pid, Q} ->
            case (catch Fun(Q, Data)) of
                {'EXIT', Why} ->
                    Pid ! {Name, exit, Why},
                    server_loop(Name, Data, Fun);
                {Reply, Data1} ->
                    Pid ! {Name, Reply},
                    server_loop(Name, Data1, Fun)
            end;
        {cast, Pid, Q} ->
            case (catch Fun(Q, Data)) of
                {'EXIT', Why} ->
                    exit(Pid, Why),
                    server_loop(Name, Data, Fun);
                Data1 ->
                    server_loop(Name, Data1, Fun)
            end;            
        {eval, Fun1} ->
            server_loop(Name, Data, Fun1)
    end.

rpc(Name, Q) -> 
    Name ! {rpc, self(), Q},
    receive     
        {Name, Reply} ->
            Reply;
        {Name, exit, Why} ->
            exit(Why)
    end.

cast(Name, Q) ->
    Name ! {cast, self(), Q}.

change_behaviour(Name, Fun) ->
    Name ! {eval, Fun}.

const(C) -> fun() -> C end.

keep_alive(Name, Fun) ->
    Pid = make_global(Name, Fun),
    on_exit(Pid, 
	    fun(Exit) -> keep_alive(Name, Fun) end).

make_global(Name, Fun) ->
    case whereis(Name) of
        undefined ->
            Self = self(),
            Pid = spawn_fun(fun() ->
                               make_global(Self,Name,Fun)
                            end),
            receive
                {Pid, ack} ->
                    Pid
            end;
        Pid ->
            Pid
    end.

make_global(Pid, Name, Fun) ->
    case register(Name, self()) of
        {'EXIT', _} ->
            Pid ! {self(), ack};
        _ ->
            Pid ! {self(), ack},
            Fun()
    end.

spawn_fun({'fun',Mod,Arity,Chksum,Env}) ->
    spawn(?MODULE, internal_call, 
	  [[Mod,Arity,Chksum,[],Env]]).

spawn_link_fun({'fun',Mod,Arity,Chksum,Env}) ->
    spawn(?MODULE, internal_call, 
	  [[Mod,Arity,Chksum,[],Env]]).
 
internal_call([Mod|Args]) ->
    apply(Mod, module_lambdas, Args).

on_exit(Pid, Fun) ->
    spawn_fun(fun() -> 
		     process_flag(trap_exit, true),
		     link(Pid),
		     receive
			 {'EXIT', Pid, Why} ->
			     Fun(Why)
		     end
	      end).

every(Pid, Time, Fun) ->
    spawn_fun(fun() ->
                      process_flag(trap_exit, true),
                      link(Pid),
                      every_loop(Pid, Time, Fun)
              end).

every_loop(Pid, Time, Fun) ->
    receive
        {'EXIT', Pid, Why} ->
            true
    after Time ->
            Fun(),
            every_loop(Pid, Time, Fun)
    end.

get_module_name() ->
    case init:get_argument(load) of
        {ok, [[Arg]]} ->
            module_name(Arg);
        error ->
            fatal({missing, '-load Mod'})
    end.

%S tag7 
lookup(Key, [{Key,Val}|_]) -> {found, Val};
lookup(Key, [_|T])         -> lookup(Key, T);
lookup(Key, [])            -> not_found.

member(X, [X|_]) -> true;
member(H, [_|T]) -> member(H, T);
member(_, [])    -> false.

map(F, [H|T]) -> [F(H)|map(F, T)];
map(F, [])    -> [].

reverse(X) -> reverse(X, []).

reverse([H|T], L) -> reverse(T, [H|L]);
reverse([], L)    -> L.

module_name(Str) ->
    case (catch list_to_atom(Str)) of
        {'EXIT', _} ->
            log_error({bad_module_name,Str}),
            stop_system(bad_start_module);
        Mod -> Mod
    end.
%E tag7

fatal(Term) ->
    log_error({fatal, Term}),
    stop_system({fatal, Term}).

%S tag8
make_scripts() ->
    {ok, Cwd} = file:get_cwd(),
    Script =
	{script,{"sos","1.0"},
	 [{preLoaded,[init,erl_prim_loader]},
	  {progress,preloaded},
	  {path, 
	   [".",
	    Cwd,
	    "$ROOT/lib/" ++ lib_location(kernel) ++ "/ebin",
	    "$ROOT/lib/" ++ lib_location(stdlib) ++ "/ebin"]},
	  {primLoad,
	   [erl_open_port,
	    erlang,
	    error_handler,
	    sos
	   ]},
	  {kernel_load_completed},
	  {progress,kernel_load_completed},
	  {progress,started},
	  {apply,{sos,main,[]}}
	 ]}, 
    file:write_file("sos.boot", term_to_binary(Script)),

    {ok, Stream} = file:open("sos", write),
    io:format(Stream, "#!/bin/sh~nerl -boot ~s/sos "
	      " -environment `printenv` -load $1~n",
	      [Cwd]),
    file:close(Stream),
    unix:cmd("chmod a+x sos"),
    true.

lib_location(Lib) ->
    filename:basename(code:lib_dir(Lib)).
%E tag8
    
