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
%% File    : unsplit.erl
%% @author  : Ulf Wiger <ulf.wiger@erlang-solutions.com>
%% @end
%% Description : Application callback and supervisor for unsplit framework.
%%-------------------------------------------------------------------
%%
%% @doc Framework for merging mnesia tables after netsplit
%% 
%% <p>...</p>
%%
%% @end
-module(unsplit).

-behaviour(application).
-behaviour(supervisor).

-export([get_reporter/0,
         report_inconsistency/4, report_inconsistency/5]).


%% application start/stop API
-export([start/2, stop/1]).
-export([init/1]).


%% @spec get_reporter() -> module()
%% @doc Look up the predefined callback module for reporting inconsistencies
%%
get_reporter() ->
    {ok, R} = application:get_env(unsplit, reporter),
    R.

%% @spec report_inconsistency(Table, Key, ObjectA, ObjectB) -> ok
%% @doc Report an inconcistency to the predefined reporter
%%
report_inconsistency(Tab, Key, ObjA, ObjB) ->
    report_inconsistency(get_reporter(), Tab, Key, ObjA, ObjB).

%% @spec report_inconsistency(Reporter, Table, Key, ObjectA, ObjectB) -> ok
%% @doc Report an inconsistency to Reporter (an unsplit_reporter behaviour)
%%
report_inconsistency(Reporter, Tab, Key, ObjA, ObjB) ->
    Reporter:inconsistency(Tab, Key, ObjA, ObjB).


%% @spec start(Type, Arg) -> {ok, pid()}
%% @doc Application start callback
%%
start(_, _) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

%% @spec stop(State) -> ok
%% @doc Application stop callback
%%
stop(_) ->
    ok.

%% Supervisor callback:    

%% @hidden
init([]) ->
    Children = [{unsplit_server, {unsplit_server, start_link, []},
                 permanent, 3000, worker, [unsplit_server]} |
                reporting_channel()],
    {ok, {{one_for_one, 3, 10}, Children}}.


reporting_channel() ->
    {ok, R} = application:get_env(unsplit, reporter),
    case R:childspec() of
        ignore ->
            [];
        {_Name, {_M,_F,_A},_,_,_,_} = Child ->
            [Child]
    end.
