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
%% @doc Framework for merging mnesia tables after netsplit
%% 
%% <p>...</p>
%%
%% @end
-module(unsplit_lib).
-export([no_action/2,
         bag/2,
         last_modified/2,
         last_version/2]).



no_action(start, [Tab|_]) ->
    error_logger:format("Will not merge table ~p~n", [Tab]),
    stop.


last_modified(init, S0) ->
    last_version(init, S0 ++ [modified]);
last_modified(Other, S) ->
    last_version(Other, S).

bag(init, _S0) ->
    {ok, []};
bag(done, _) ->
    ok;
bag(Objs, S) ->
    Merged = lists:usort(lists:concat(Objs)),
    {ok, [{write, Merged}], S}.

last_version(init, [Tab, Attrs, Attr]) ->
    case lists:member(Attr, Attrs) of
        false ->
            error_logger:format("Cannot merge table ~p."
                                "Missing ~p attribute~n", [Tab, Attr]),
            stop;
        true ->
            io:fwrite("Starting merge of ~p (~p)~n", [Tab, Attrs]),
            {ok, {Tab, pos(Attr, Tab, Attrs)}}
    end;
last_version(done, _) ->
    ok;
last_version(Objs, {T, P} = S) when is_list(Objs) ->
    Actions = lists:map(fun(Obj) ->
                                last_version_entry(Obj, T, P)
                        end, Objs),
    {ok, Actions, same, S}.


last_version_entry(Obj, T, P) ->
    io:fwrite("last_version_entry(~p)~n", [Obj]),
    case Obj of
        {A, []} -> {write, A};
        {[], B} -> {write, B};
        {[A], [B]} ->
            ModA = element(P, A),
            ModB = element(P, B),
            io:fwrite("ModA = ~p, ModB = ~p~n", [ModA, ModB]),
            if ModA < ModB ->
                    {write, B};
               ModA > ModB ->
                    {write, A};
               ModA == ModB ->
                    if A =/= B ->
                            mnesia:abort({undecided,T,A,B});
                       true ->
                            {write, A}
                    end
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
