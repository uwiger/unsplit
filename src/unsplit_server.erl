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
%% File    : unsplit_server.erl
%% Author  : Ulf Wiger <ulf.wiger@erlang-solutions.com>
%% Description : Coordinator for merging mnesia tables after netsplit
%%
%% Created :  1 Feb 2010 by Ulf Wiger <ulf.wiger@erlang-solutions.com>
%%-------------------------------------------------------------------
-module(unsplit_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([remote_handle_query/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).
-record(st, {module, function, extra_args = [],
             modstate,
             table, attributes,
             remote,
             chunk,
             strategy = default_strategy(),
             progress}).


-define(SERVER, ?MODULE).
-define(DEFAULT_METHOD, {unsplit_lib, no_action, []}).
-define(DEFAULT_STRATEGY, all_keys).

-define(DONE, {?MODULE,done}).

-define(LOCK, {?MODULE, stitch}).


%% @spec start_link() -> {ok, pid()}
%% @doc Starts the Unsplit server
%% @end
%%
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    mnesia:subscribe(system),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({mnesia_system_event, 
             {inconsistent_database, Context, Node}}, State) ->
    io:fwrite("inconsistency. Context = ~p; Node = ~p~n", [Context, Node]),
    Res = global:trans(
            {?LOCK, self()},
            fun() ->
                    io:fwrite("have lock...~n", []),
                    stitch_together(node(), Node)
            end),
    io:fwrite("Res = ~p~n", [Res]),
    {noreply, State};
handle_info(_Info, State) ->
    io:fwrite("Got event: ~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


stitch_together(NodeA, NodeB) ->
    case lists:member(NodeB, mnesia:system_info(running_db_nodes)) of
        true ->
            io:fwrite("~p already stitched, it seems. All is well.~n", [NodeB]),
            ok;
        false ->
            do_stitch_together(NodeA, NodeB)
    end.

do_stitch_together(NodeA, NodeB) ->
    [IslandA, IslandB] =
        [rpc:call(N, mnesia, system_info, [running_db_nodes]) ||
            N <- [NodeA, NodeB]],
    io:fwrite("IslandA = ~p;~nIslandB = ~p~n", [IslandA, IslandB]),
    TabsAndNodes = affected_tables(IslandA, IslandB),
    Tabs = [T || {T,_} <- TabsAndNodes],
    io:fwrite("Affected tabs = ~p~n", [Tabs]),
    DefaultMethod = default_method(),
    TabMethods = [{T, Ns, get_method(T, DefaultMethod)}
                  || {T,Ns} <- TabsAndNodes],
    io:fwrite("Methods = ~p~n", [TabMethods]),
    mnesia_controller:connect_nodes(
      [NodeB],
      fun(MergeF) ->
              case MergeF(Tabs) of
                  {merged,_,_} = Res ->
                      show_locks(NodeB),
                      %% For now, assume that we have merged with the right
                      %% node, and not with others that could also be
                      %% consistent (mnesia gurus, how does this work?)
                      io:fwrite("stitching: ~p~n", [TabMethods]),
                      stitch_tabs(TabMethods, NodeB),
                      Res;
                  Other ->
                      Other
              end
      end).

show_locks(OtherNode) ->
    Info = [{node(), mnesia_locker:get_held_locks()},
            {OtherNode, rpc:call(OtherNode,
                                 mnesia_locker,get_held_locks,[])}],
    io:fwrite("Held locks = ~p~n", [Info]).


stitch_tabs(TabMethods, NodeB) ->
%%    Tabs = [Tab || {Tab,_} <- TabMethods],
%%    [mnesia:write_lock_table(T) || T <- Tabs],
    [do_stitch(TM, NodeB) || TM <- TabMethods].




do_stitch({Tab, Ns, {M, F, XArgs}} = TM, Remote) ->
    io:fwrite("do_stitch(~p, ~p).~n", [TM,Remote]),
    HasCopy = lists:member(Remote, Ns),
    io:fwrite("~p has a copy of ~p? -> ~p~n", [Remote, Tab, HasCopy]),
    Attrs = mnesia:table_info(Tab, attributes),
    S0 = #st{module = M, function = F, extra_args = XArgs,
             table = Tab, attributes = Attrs,
             remote = Remote,
             chunk = get_table_chunk_factor(Tab),
             strategy = default_strategy()},
    io:fwrite("Calling ~p:~p(init, ~p)", [M,F,[Tab,Attrs|XArgs]]),
    try
        run_stitch(check_return(M:F(init, [Tab, Attrs | XArgs]), S0))
    catch
        throw:?DONE ->
            ok
    end.

-spec check_return(unsplit:merge_ret(), #st{}) -> #st{}.

check_return(Ret, S) ->
    io:fwrite(" -> ~p~n", [Ret]),
    case Ret of
        stop -> throw(?DONE);
        {ok, St} ->
            S#st{modstate = St};
        {ok, Actions, St} ->
            S1 = S#st{modstate = St},
            perform_actions(Actions, S1);
        {ok, Actions, Strategy, St} ->
            perform_actions(Actions, new_strategy(Strategy,
                                                  S#st{modstate = St}))
    end.

-spec new_strategy(unsplit:merge_strategy(), #st{}) -> #st{}.

new_strategy(same, S) ->
    S;
new_strategy({M,F}, S) ->
    S#st{strategy = {M, F}};
new_strategy(all_keys, S) ->
    S#st{strategy = all_keys}.

perform_actions(Actions, #st{table = Tab, remote = Remote} = S) ->
    local_perform_actions(Actions, Tab),
    ask_remote(Remote, {actions, Tab, Actions}),
    S.

run_stitch(#st{table = Tab,
	       module = M, function = F, modstate = MSt,
	       strategy = {Ms,Fs}, remote = Remote} = St) ->
    {ok, Objs, MSt1} = Ms:Fs(Tab, Remote, MSt),
    run_stitch(check_return(M:F(Objs, MSt1), St));
run_stitch(#st{table = Tab, 
               module = M, function = F, modstate = MSt,
               strategy = all_keys, remote = Remote} = St) ->

    RemoteKeys = rpc:call(Remote, mnesia, dirty_all_keys, [Tab]),
    LocalKeys = mnesia:dirty_all_keys(Tab),
%%    Keys = LocalKeys ++ (RemoteKeys -- LocalKeys),
    Keys = mnesia_lib:uniq(
      LocalKeys ++ RemoteKeys
%%      lists:merge(lists:sort(LocalKeys), lists:sort(RemoteKeys))
    ),

    lists:foldl(
      fun(K, Sx) ->
              A = mnesia:read({Tab,K}),  % assert that A is non-empty
              B = get_remote_obj(Remote, Tab, K),
              if A == B ->
                      Sx;
                 true ->
                      check_return(M:F([{A, B}], MSt), Sx)
              end
      end, St, Keys).

get_remote_obj(Remote, Tab, Key) ->
    ask_remote(Remote, {get_obj, Tab, Key}).


%% As it works now, we run inside the mnesia_schema:merge_schema transaction,
%% telling it to lock the tables we're interested in. This gives us time to
%% do this, but replication will not be active until the transaction has been
%% committed, so we have to write dirty explicitly to both copies.
write_result(Data, Tab) when is_list(Data) ->
    [mnesia:dirty_write(Tab, D) || D <- Data];
%%    [mnesia:write(Tab, D, write) || D <- Data];
write_result(Data, Tab) ->
    mnesia:dirty_write(Tab, Data).
%%    mnesia:write(Tab, Data, write).


ask_remote(Remote, Q) ->
    rpc:call(Remote, ?MODULE, remote_handle_query, [Q]).


remote_handle_query(Q) ->
    case Q of
        {get_obj, Tab, Key} ->
            mnesia:dirty_read({Tab, Key});
        {write, Tab, Data} ->
            write_result(Data, Tab);
        {actions, Tab, Actions} ->
            local_perform_actions(Actions, Tab)
    end.


local_perform_actions(Actions, Tab) ->
    lists:foreach(
      fun({write, Data}) ->
              write_result(Data, Tab);
         ({delete, Data}) when is_list(Data) ->
              [mnesia:dirty_delete({Tab,D}) || D <- Data]
      end, Actions).



affected_tables(IslandA, IslandB) ->
    Tabs = mnesia:system_info(tables) -- [schema],
    lists:foldl(
      fun(T, Acc) ->
              Nodes = lists:concat(
                        [mnesia:table_info(T, C) ||
			    C <- backend_types()]),
              io:fwrite("nodes_of(~p) = ~p~n", [T, Nodes]),
              case {intersection(IslandA, Nodes), 
                    intersection(IslandB, Nodes)} of 
                  {[_|_], [_|_]} ->
                      [{T, Nodes}|Acc];
                  _ ->
                      Acc
              end
      end, [], Tabs).

backend_types() ->
    try mnesia:system_info(backend_types)
    catch
	exit:_ ->
	    [ram_copies, disc_copies, disc_only_copies]
    end.

intersection(A, B) ->
    A -- (A -- B).


default_method() ->
    get_env(default_method, ?DEFAULT_METHOD).

default_strategy() ->
    get_env(default_strategy, ?DEFAULT_STRATEGY).

get_env(K, Default) ->
    case application:get_env(K) of
        undefined ->
            Default;
        {ok, undefined} ->
            Default;
        {ok, Value} ->
	    Value
    end.

get_method(T, Def) ->
    try mnesia:read_table_property(T, unsplit_method) of
        {unsplit_method,Method} -> Method
    catch
        exit:_ ->
            Def
    end.

get_table_chunk_factor(_) ->
    %% initially use 1 for testing. 100 might be a better default,
    %% and it should be made configurable per-table.
    1.
