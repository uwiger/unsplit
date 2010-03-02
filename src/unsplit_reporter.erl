-module(unsplit_reporter).

-export([childspec/0,
         inconsistency/4]).

-export([behaviour_info/1]).


behaviour_info(callbacks) ->
    [{childspec, 0},
     {inconsistency, 4}];
behaviour_info(_) ->
    undefined.



childspec() ->
    %% no need for a process in this case.
    ignore.


inconsistency(Table, Key, ObjA, ObjB) ->
    alarm_handler:set_alarm({unsplit, inconsistency,
                             [Table, Key, ObjA, ObjB]}).
