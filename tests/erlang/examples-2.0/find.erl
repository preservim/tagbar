-module(find).

-doc([{author,'Joe Armstrong'},
      {title,"Find all files. Find all out of date files.
<p>A find utility which finds all files (and directories)
relative to a given root directory"},
      {keywords, [find,make]},
      {api,["find:files(\".\", \"*.erl\", false)</b> finds all 
entries in the current directory.
Recursive scan of sub-directories is also allowed.",
"find:out_of_date(\".\",\".erl\",\".jam\")</b> finds all out of date
Erlang files in the current directory"]},
      {date,970203}]).

-export([files/3, out_of_date/3]).

-import(lists, [suffix/2, sublist/3, map/2, filter/2]).


%% files(Dir, ReExpr, Recursive) -> [File]
%%     Find regular files starting from Dir
%%     Which match ReExpr
%%     If Recursive is true do recursivly on all sub-directories
%%     Example find(".", "*.erl", false) will find all erlang files in the 
%%     Current directory
%%
%% out_of_date(Dir, SrcExt, ObjExt) find all "out of date files" in
%%     Dir.
%%     Example:
%%         out_of_date(".", ".erl", ".jam") 
%%             Finds all out of date files in the current directory

files(Dir, Re, Flag) -> 
    Re1 = string:re_sh_to_awk(Re),
    find_files(Dir, Re1, Flag, []).

%% +type find_files(dirname(), Regexp, bool(), [filename()]) -> [filename()]
%%      when Regexp = string().

find_files(Dir, Re, Flag, L) -> 
    case file:list_dir(Dir) of
	{ok, Files} -> find_files(Files, Dir, Re, Flag, L);
	{error, _}  -> L
    end.

%% +type find_files([filename()], dirname(), Regexp, bool(), [filename()]) -> 
%%    [filename()] when Regexp = string().

find_files([File|T], Dir, Re, Recursive, L) ->
    FullName = Dir ++  [$/|File],
    case file_type(FullName) of
	regular ->
	    case string:re_match(FullName, Re) of
		{match, _, _}  -> 
		    find_files(T, Dir, Re, Recursive, [FullName|L]);
		_ ->
		    find_files(T, Dir, Re, Recursive, L)
	    end;
	directory -> 
	    case Recursive of
		true ->
		    L1 = find_files(FullName, Re, Recursive, L),
		    find_files(T, Dir, Re, Recursive, L1);
		false ->
		    find_files(T, Dir, Re, Recursive, L)
	    end;
	error -> 
	    find_files(T, Dir, Re, Recursive, L)
    end;
find_files([], _, _, _, L) ->
    L.

%% +type file_type(string()) -> regular | directory | error.

file_type(File) ->
    case file:file_info(File) of
	{ok, Facts} ->
	    case element(2, Facts) of
		regular   -> regular;
		directory -> directory;
		_         -> error
	    end;
	_ ->
	    error
    end.


%%______________________________________________________________________
%% outofdate(Dir, InExtension, OutExtension)
%%   scans Dir for all files with the extension "InExtension"
%%   If a file with this extension is found then "OutExtension" is checked
%%
%%   returns a list of files in <Dir> where *.OutExtension is
%%   "out of date" with respect to *.InExtension
%%   in the sence of "make"

out_of_date(Dir, In, Out) ->
    case file:list_dir(Dir) of
	{ok, Files0} ->
	    Files1 = filter(fun(F) -> 
			       suffix(In, F) 
			   end, Files0),
	    Files2 = map(fun(F) -> 
			     sublist(F, 1, 
				     length(F)-length(In)) 
			end, Files1),
	    filter(fun(F) -> update(F, In, Out) end,Files2);
	_ ->
	    []
    end.

%% +type update(string(), string(), string()) -> bool().
	  
update(File, In, Out) ->
    InFile  = File ++ In,
    OutFile = File ++ Out,
    case is_file(OutFile) of
	true ->
	    case writeable(OutFile) of
		true ->
		    outofdate(InFile, OutFile);
		false ->
		    %% can't write so we can't update
		    false
	    end;
	false ->
	    %% doesn't exist
	    true
    end.

%% +type is_file(string()) -> bool().

is_file(File) ->
    case file:file_info(File) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

%% +type outofdate(string(), string()) -> bool().

outofdate(In, Out) ->
    case {last_modified(In), last_modified(Out)} of
	{T1, T2} when T1 > T2 ->
	    true;
	_ ->
	    false
    end.

%% +type last_modified(string()) -> {int(), int(),int(), int(),int(), int()} 
%%                              |  'EXIT'({last_modified, string()}).

last_modified(F) ->
    case file:file_info(F) of
	{ok, {_, _, _, _, Time, _, _}} ->
	    Time;
	_ ->
	    exit({last_modified, F})
    end.

%% +type writeable(string()) -> bool().

writeable(F) ->
    case file:file_info(F) of
	{ok, {_,_,read_write,_,_,_,_}} -> true;
	{ok, {_,_,write     ,_,_,_,_}} -> true;
	_ -> false
    end.

