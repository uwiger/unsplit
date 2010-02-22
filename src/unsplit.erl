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

-export([start/2, stop/1]).


-export([init/1]).

start(_, _) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

stop(_) ->
    ok.

%% Supervisor callback:    

init([]) ->
    Children = [{unsplit_server, {unsplit_server, start_link, []},
                 permanent, 3000, worker, [unsplit_server]}],
    {ok, {{one_for_one, 3, 10}, Children}}.
