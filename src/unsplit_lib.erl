%% The contents of this file are subject to the Erlang Public License,
%% Version 1.0, (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.erlang.org/license/EPL1_0.txt
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is unsplit-0.1.
%%
%% The Initial Developer of the Original Code is Erlang Solutions Ltd (ESL)
%% Portions created by ESL are Copyright (C), 2010, Erlang Solutions Ltd.
%% All Rights Reserved.
%%
%%-------------------------------------------------------------------
%% File    : unsplit_lib.erl
%% @author  : Ulf Wiger <ulf.wiger@erlang-solutions.com>
%% @end
%% Description : Predefined deconflict logic callbacks
%%-------------------------------------------------------------------
%%
%% @doc Predefined merge functions
%% 
%% This module implements a few merge functions that can be used.
%% 
%%
%% @end
-module(unsplit_lib).
-export([no_action/2,
         bag/2,
         last_modified/2,
         last_version/2,
	 vclock/2]).

-include("unsplit.hrl").

%% @spec no_action(init, State) -> stop
%% @doc Minimal merge action - does nothing
%% @end
%%
no_action(init, [Tab|_]) ->
    unsplit:log_write(error, "Will not merge table ~p~n", [Tab]),
    stop.

%% @spec last_modified(Phase, State) -> merge_ret()
%% @doc Keeps the last modified object, based on the `modified' attribute
%%
%% This function assumes that the table to be merged contains objects with 
%% a `modified' attribute.
%% @end
%%
last_modified(init, S0) ->
    last_version(init, S0 ++ [modified]);
last_modified(Other, S) ->
    last_version(Other, S).

%% @spec bag(Phase, State) -> merge_ret()
%% @doc Default `bag' merge; removes duplicate objects
bag(init, _S0) ->
    {ok, []};
bag(done, _S) ->
    stop;
bag(Objs, S) ->
    Merged = lists:usort(lists:concat([A ++ B || {A,B} <- Objs])),
    {ok, [{write, Merged}], S}.

%% @spec last_version(Phase, State) -> merge_ret()
%% @doc Picks the object with the greatest value of a given attribute
%%
%% This function assumes that an attribute name is passed as an extra argument.
%% e.g. by adding the following user property to the table:
%%
%% `{unsplit_method, {unsplit_lib, last_version, [Attr]}}'
%%
%% The function will choose the object that has the greatest value in the 
%% position given by `Attr'.
%% @end
%%
last_version(init, [Tab, Attrs, Attr]) ->
    case lists:member(Attr, Attrs) of
        false ->
            unsplit:log_write(error, "Cannot merge table ~p."
                                     "Missing ~p attribute~n", [Tab, Attr]),
            stop;
        true ->
            unsplit:log_write(normal, "Starting merge of ~p (~p)~n", [Tab, Attrs]),
            {ok, {Tab, pos(Attr, Tab, Attrs)}}
    end;
last_version(done, _S) ->
    stop;
last_version(Objs, {T, P} = S) when is_list(Objs) ->
    Actions = lists:map(fun(Obj) ->
                                last_version_entry(Obj, T, P)
                        end, Objs),
    {ok, Actions, same, S}.

vclock(init, [Tab, Attrs, Attr]) ->
    case lists:member(Attr, Attrs) of
        false ->
            unsplit:log_write(error, "Cannot merge table ~p."
                                     "Missing ~p attribute~n", [Tab, Attr]),
            stop;
        true ->
            unsplit:log_write(normal, "Starting merge of ~p (~p)~n", [Tab, Attrs]),
            {ok, {Tab, pos(Attr, Tab, Attrs)}}
    end;
vclock(done, _) ->
    stop;
vclock(Objs, {T, P} = S) ->
    Comp =
	fun(A, B) ->
		case unsplit_vclock:descends(A, B) of
		    true -> left;
		    false ->
			case unsplit_vclock:descends(B, A) of
			    true -> right;
			    false ->
				neither
			end
		end
	end,
    Actions = lists:map(fun(Obj) ->
				compare(Obj, T, P, Comp)
			end, Objs),
    {ok, Actions, same, S}.

last_version_entry(Obj, T, P) ->
    unsplit:log_write(normal, "last_version_entry(~p)~n", [Obj]),
    compare(Obj, T, P, fun(A, B) when A < B -> left;
			  (A, B) when A > B -> right;
			  (_, _) -> neither
		       end).

compare(Obj, T, P, Comp) ->
    unsplit:log_write("compare(~p)~n", [Obj]),
    case Obj of
        {A, []} -> {write, A};
        {[], B} -> {write, B};
        {[A], [B]} ->
            ModA = element(P, A),
            ModB = element(P, B),
	    case Comp(ModA, ModB) of
		left    -> {write, B};
		right   -> {write, A};
		neither -> unsplit:report_inconsistency(T, A, B)
	    end
    end.


pos(A, T, L) ->
    pos(A, T, L, 2).  % record tag is the 1st element in the tuple

pos(H, _, [H|_], P) ->
    P;
pos(H, Tab, [_|T], P) ->
    pos(H, Tab, T, P+1);
pos(A, Tab, [], _) ->
    mnesia:abort({missing_attribute, Tab, A}).
