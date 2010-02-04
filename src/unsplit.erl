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
