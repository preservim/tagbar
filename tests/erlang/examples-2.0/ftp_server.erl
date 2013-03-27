-module(ftp_server).

%% Look in ~tony/erlang/ftpd/ftpd.erl
%% For filename stuff

-doc([{author, joe},
      {title, "FTP server in pure Erlang -- i.e. an FTP server as it 
might have been written, i.e. not according to RFC 959"},
      {keywords,[ftp, server]},
      {date, 981014}]).

-compile(export_all).

-export([start/0, internal/0, handler/1]).
-import(lists, [member/2, reverse/1]).

start() ->
    case (catch register(ftp_server, 
			 spawn(?MODULE, internal, []))) of
	{'EXIT', _} ->
	    already_started;
	Pid ->
	    ok
    end.

internal() ->
    case file:consult("users") of
	{ok, Users} ->
	    process_flag(trap_exit, true),
	    loop(Users, 0);
	_ ->
	    exit(no_users_allowed)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(Users, N) ->
  receive
    {connect, Pid, User, Password} ->
      io:format("connection request from:~p ~p ~p~n",
		[Pid, User, Password]),
      case member({User, Password}, Users) of
	true ->
	  Max = max_connections(),
          if 
	    N > Max ->
	      Pid ! {ftp_server, 
		       {error, too_many_connections}},
	      loop(Users, N);
	    true ->
	      New = spawn_link(?MODULE, handler, [Pid]),
	      Pid ! {ftp_server, {ok, New}},
	      loop(Users,  N + 1)
	  end;
	false ->
	  Pid ! {ftp_server, {error, rejected}},
	  loop(Users, N)
      end;
    {'EXIT', Pid} ->
      io:format("Handler ~p died~n", [Pid]),
      loop(Users, lists:max(N-1, 0));
    Any ->
      io:format("received:~p~n",[Any]),
      loop(Users, N)
    end.

handler(Pid) ->
    receive
	{Pid, quit} ->
	    Pid ! {ftp_server, ok};
	{Pid, Op} ->
	    io:format("got:~p ~p~n",[Pid, Op]),
	    Pid ! {ftp_server, do_op(Op)},
	    handler(Pid)
    end.

do_op({cd, Dir})        -> file:set_cwd(Dir), cwd();
do_op(ls)               -> element(2, file:list_dir(cwd()));
do_op(pwd)              -> cwd();
do_op({get_file, File}) -> file:read_file(File).

max_connections() -> 10.

cwd() -> element(2, file:get_cwd()).

%% This was taken from Tony

%%
%% Compose file/directory names
%%
rel_name(Name, Wd) ->
    case filename:pathtype(Name) of
	relative ->
	    rel_path(filename:join(Wd, Name));
	absolute ->
	    rel_path(Name);
	volumerelative ->
	    rel_path(filename:join(Wd,Name))
    end.
%%
%% We sometime need a simulated root, then call abs_name
%%
abs_name(Name) ->
    filename:join("/", Name).

%%
%% rel_path returns a relative path i.e remove
%% and root or volume relative start components
%%
rel_path(Path) ->
    rel_path(filename:split(Path),[]).

%% remove absolute or volume relative stuff
rel_path([Root|Path], RP) ->
    case filename:pathtype(Root) of
	relative -> rpath(Path, [Root|RP]);
	_ -> rpath(Path, RP)
    end.

rpath([".."|P], [_|RP]) ->  rpath(P, RP);
rpath(["."|P], RP) -> rpath(P, RP);
rpath([F|P], RP) -> rpath(P, [F|RP]);
rpath([],[]) -> "";
rpath([], RP) -> filename:join(reverse(RP)).
