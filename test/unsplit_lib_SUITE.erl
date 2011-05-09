%%% @doc
%%% Test suite for unsplit
%%% @copyright Nimbuzz B.V.
%%% @author Ahmed Omar <omar@nimbuzz.nl>
-module(unsplit_lib_SUITE).
%%% @end
-include_lib("eunit/include/eunit.hrl").
%%% Include files
-include_lib("common_test/include/ct.hrl").

%%% External exports
-compile(export_all).
-define(TABLE, test1).
-define(SNODE, mn7).
-record(?TABLE,{key,modified=erlang:now(),value}).
%%% Macros

all() ->
    [split].


init_per_suite(Conf) ->
    {ok, Host} = inet:gethostname(),
    ct_slave:start(list_to_atom(Host), ?SNODE, [{erl_flags, "-pa ../../../ebin/"}]),
    Conf.

end_per_suite(_Conf) ->
    {ok, Host} = inet:gethostname(),
    ct_slave:stop(list_to_atom(Host), ?SNODE),
    ok.

init_per_testcase(Case, Conf) ->
    ct:print("Test case ~p started", [Case]),
    Conf.

end_per_testcase(Case, Conf) ->
    ct:print("Test case ~p finished", [Case]),
    Conf.

split()->
    [{userdata, [{doc, "Tests split network"}]}].
split(_Conf)->
    RemoteNode = hd(nodes()),
    mnesia:delete_schema([node()|nodes()]),
    ok = mnesia:start(),
    ok = application:start(unsplit),
    ok = rpc:call(RemoteNode, mnesia, start,[]),
    ok = rpc:call(RemoteNode, application, start, [unsplit]),
    mnesia:create_schema([node()|nodes()]),
    mnesia:change_config(extra_db_nodes, nodes()),
    mnesia:delete_table(?TABLE),
    mnesia:create_table(?TABLE ,[{ram_copies,[node()|nodes()]},
                                 {attributes,[key,modified,value]},
                                 {user_properties,[{unsplit_method,{unsplit_lib,last_modified,[]}}]}]),
    ct:print("inserting records~n"),
    mnesia:transaction(fun() -> mnesia:write(#?TABLE{key=1,value=a}) end),
    mnesia:transaction(fun() -> mnesia:write(#?TABLE{key=2,value=a}) end),
    ct:print("disconnecting nodes~n"),
    disconnect_node(RemoteNode),
    mnesia:transaction(fun() -> mnesia:write(#?TABLE{key=4,value=b}) end),
    mnesia:transaction(fun() -> mnesia:write(#?TABLE{key=5,value=b}) end),
    ct:print("inserting records on one node, while the other one is disconnected~n"),
    timer:sleep(4000),
    ct:print("reconnecting nodes~n"),
    net_kernel:connect_node(RemoteNode),
    timer:sleep(5000),
    true = compare_table_size(RemoteNode, test),
    mnesia:delete_table(?TABLE).


compare_table_size(RemoteNode, Table)->
    rpc:call(RemoteNode, mnesia, table_info,[Table, size])==
        mnesia:table_info(Table, size).
