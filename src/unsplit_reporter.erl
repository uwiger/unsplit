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
%% File    : unsplit_reporter.erl
%% Author  : Ulf Wiger <ulf.wiger@erlang-solutions.com>
%% Description : Behaviour for reporting inconsistencies from merge
%%
%% Created :  2 Mar 2010 by Ulf Wiger <ulf.wiger@erlang-solutions.com>
%%-------------------------------------------------------------------

%% @doc Unsplit Inconsistency Reporter Behaviour
%%
%% This module implements a basic behaviour for reporting inconsistencies 
%% encountered during the merge procedure.
%%
%% @end
-module(unsplit_reporter).

-export([childspec/0,
         inconsistency/4]).

-export([behaviour_info/1]).


behaviour_info(callbacks) ->
    [{childspec, 0},
     {inconsistency, 4}];
behaviour_info(_) ->
    undefined.

%% @spec childspec() -> ignore | supervisor:child_spec()
%% @doc Return a child start specification for the pre-defined reporter
%%
%% See {@link supervisor}.
%% Use `ignore' if no process should be started.
%% @end
%%
childspec() ->
    %% no need for a process in this case.
    ignore.

%% @spec inconsistency(Table, Key, ObjectA, ObjectB) -> ok
%% @doc Report an inconsistency encountered during the merge
%% 
%% The default implementation raises an alarm via the SASL alarm_handler
%% @end
%%
inconsistency(Table, Key, ObjA, ObjB) ->
    alarm_handler:set_alarm({unsplit, inconsistency,
                             [Table, Key, ObjA, ObjB]}).
