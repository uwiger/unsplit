#!/usr/bin/env escript
%% -*- erlang -*-

main([Src, Target]) ->
    {ok, Cwd} = file:get_cwd(),
    App = filename:basename(Cwd),
    Branch = get_git_branch(),
    case file:read_file(Src) of
	{ok, SrcBin} ->
	    TgtBin =
		replace(
		  replace(SrcBin,
			  <<"](../test/">>,
			  list_to_binary(["](", App, "/blob/",
					  Branch, "/test/"])),
		  <<"href=\"unsplit">>,
		  list_to_binary(["href=\"", App, "/blob/",
				  Branch, "/doc/unsplit"])),
	    ok = file:write_file(Target, TgtBin)
    end.

replace(SrcBin, SrcStr, NewStr) ->
    binary:replace(SrcBin,
		   SrcStr,
		   NewStr, [global]
		  ).
    

get_git_branch() ->
    case os:cmd("git branch | awk '/\\*/ {print $2}'") of
	[_,_|_] = Res ->
	    %% trailing newline expected - remove.
	    lists:reverse(tl(lists:reverse(Res)));
	Other ->
	    io:fwrite("Could not get git branch (~s)~n", [Other]),
	    halt(1)
    end.
