-module(ftp_client).

-doc([{author, joe},
      {title, "FTP client in pure Erlang -- i.e. an FTP client as it might 
have been written, i.e. not according to RFC 959"},
      {keywords,[ftp, client]},
      {date, 981014}]).

-export([connect/3, pwd/1, cd/2, ls/1, put/2, get/2, lcd/1, lpwd/0, lls/0,
	quit/1]).

connect(Host, User, Password) ->
    {ftp_server, Host} ! {connect,self(),User,Password},
    receive
	{ftp_server, Reply} -> Reply;
	Other -> Other
    after 10000 ->
	    timeout
    end.

%S tag1
pwd(Handle)       -> remote(Handle, pwd).
cd(Handle, Dir)   -> remote(Handle, {cd, Dir}).
ls(Handle)        -> remote(Handle, ls).
get(Handle, File) -> remote(Handle, {get, File}).
quit(Handle)      -> remote(Handle, quit).
%E tag1

%S tag2
lcd(Dir)          -> file:set_cwd(Dir), lpwd().
lpwd()            -> cwd().
lls()             -> element(2, file:list_dir(cwd())).
%E tag2

cwd() -> element(2, file:get_cwd()).

remote(Handle, Op) ->
    Handle ! {self(), Op},
    receive
	{ftp_server, Any} ->
	    Any
    after 1000 ->
	    timeout
    end.

put(Handle, File) ->
    case file:read_file(File) of
	{ok, Contents} ->
	    remote(Handle, {put, File, Contents});
	Other ->
	    Other
    end.
