%%% @doc
%%% Test suite for unsplit
%%% @copyright Nimbuzz B.V.
%%% @author Ahmed Omar <omar@nimbuzz.nl>
-module(unsplit_SUITE).
%%% @end
-include_lib("eunit/include/eunit.hrl").
%%% Include files
-include_lib("common_test/include/ct.hrl").

%%% External exports
-compile(export_all).
%% -define(ERL_FLAGS, "-kernel dist_auto_connect once -pa ../../ -pa ../../../ebin/").
-define(ERL_FLAGS, "-kernel dist_auto_connect once").
-define(TABLE, test1).
-define(NODES, ['mn1', 'mn2']).
-define(DISCONNECT_TIME, 100).
-define(UNSPLIT_TIMEOUT, 500).
-record(?TABLE, {key, modified=erlang:timestamp(), value}).
%%% Macros

all() ->
    [split1].


init_per_suite(Conf) ->
    net_kernel:start(['test@127.0.0.1', longnames]),

    % Grabbed this from phoenix_pubsub test and it seems necessary
    erl_boot_server:start(['127.0.0.1']),

    Nodes = ct:get_config(nodes, ?NODES),
    DisconnectTime = ct:get_config(disconnect_time, ?DISCONNECT_TIME),
    UnsplitTimeout = ct:get_config(unsplit_timeout, ?UNSPLIT_TIMEOUT),
    Host = '127.0.0.1',

    StartNode = fun(Node)->
                        ct:print("starting node ~p, on host ~p ~n",[Node, Host]),
                        {ok, StartedNode} = slave:start(Host, Node, "-loader inet -hosts 127.0.0.1 -setcookie " ++ atom_to_list(erlang:get_cookie())),
                        StartedNode
                end,

    NodeNames = lists:map(StartNode, Nodes),

    ct:print("started things ~n ~p",[NodeNames]),
    [{disconnect_time, DisconnectTime},
     {unsplit_timeout, UnsplitTimeout},
     {nodes, NodeNames}|Conf].

end_per_suite(_Conf) ->
    Nodes = ct:get_config(nodes, ?NODES),
    StopNode = fun(Node)->
                       ok = slave:stop(Node)
               end,
    lists:map(StopNode, Nodes),
    ok.

init_per_testcase(Case, Conf) ->
    ct:print("Test case ~p started", [Case]),
    init_nodes(get_conf(nodes, Conf)),
    Conf.

end_per_testcase(Case, Conf) ->
    ct:print("Test case ~p finished", [Case]),
    terminate_nodes(get_conf(nodes, Conf)),
    Conf.

split1()->
    [{userdata, [{doc, "Tests split network of 2 nodes"}]}].
split1(Conf)->
    DisconnectTime = get_conf(disconnect_time, Conf),
    UnsplitTimeout = get_conf(unsplit_timeout, Conf),
    Nodes = [M, S|_Rest] = get_conf(nodes, Conf),
    ct:print("Initial tables~n"),
    print_tables(Nodes, ?TABLE),
    ct:print("inserting records~n"),
    {atomic, ok} = write(M, [#?TABLE{key=1, value=a}, #?TABLE{key=2, value=a}]),
    print_tables(Nodes, ?TABLE),
    ct:print("disconnecting node ~p from ~p", [M, S]),
    disconnect(M, S),
    ct:print("updating records on one node, while the other one is disconnected~n"),
    timer:sleep(DisconnectTime),
    {atomic, ok} = write(M, [#?TABLE{key=1, modified=erlang:timestamp() ,value=c}, #?TABLE{key=2, modified=erlang:timestamp(), value=d}]),
    print_tables(Nodes, ?TABLE),
    ct:print("reconnecting nodes~n"),
    connect(M, S),
    timer:sleep(UnsplitTimeout),
    {MasterTable, SlaveTable} = print_tables(Nodes, ?TABLE),
    true.
    % MasterTable = SlaveTable.

table_size(Node, Table)->
    rpc:call(Node, mnesia, table_info,[Table, size]).

print_tables([M,S|_], Table)->
    MasterTable = rpc:call(M, ets, tab2list, [Table]),
    SlaveTable = rpc:call(S, ets, tab2list, [Table]),
    ct:print("master table = ~p~n", [MasterTable]),
    ct:print("slave table = ~p~n", [SlaveTable]),
    {MasterTable, SlaveTable}.

get_conf(Key, Conf)->
    proplists:get_value(Key, Conf).


terminate_nodes(Nodes)->
    rpc:multicall(Nodes, application, stop, [sasl]),
    rpc:multicall(Nodes, application, stop, [unsplit]),
    rpc:multicall(Nodes, mnesia, stop, []),
    rpc:call(hd(Nodes), mnesia, delete_schema, [Nodes]).

init_nodes(Nodes)->
    % Load this module on new nodes
    {Mod, Bin, File} = code:get_object_code(?MODULE),
    rpc:multicall(Nodes, code, load_binary, [Mod, File, Bin]),

    FirstNode = hd(Nodes),
    rpc:call(FirstNode, mnesia, create_schema, [Nodes]),

    rpc:multicall(Nodes, application, start, [sasl]),
    rpc:multicall(Nodes, mnesia, start, []),
    rpc:multicall(Nodes, application, start, [unsplit]),

    rpc:call(FirstNode, mnesia, create_table, [?TABLE,
      [{ram_copies, Nodes},
        {attributes, [key, modified, value]},
        {user_properties,
        [{unsplit_method, {unsplit_lib, last_modified, []}}]
      }]
    ]).

disconnect(Master, Slave)->
    true = rpc:call(Master, erlang, disconnect_node, [Slave]),
    NodeList = rpc:call(Master, erlang, nodes, []),
    ct:print("node list is now ~p", [NodeList]).

connect(Master, Slave)->
    true = rpc:call(Master, net_kernel, connect_node, [Slave]),
    NodeList = rpc:call(Master, erlang, nodes, []),
    ct:print("node list is now ~p", [NodeList]).

write(Node, Records)->
    rpc:call(Node, ?MODULE, write, [Records]).

write(Records)->
    Trans = fun()->
                    lists:foreach(fun(Record)->
                                          mnesia:write(Record)
                                  end, Records)
            end,
    mnesia:transaction(Trans).

get_path_flags() ->
    [ [[" -",atom_to_list(K)," ",D] || D <- V]
      || {K,V} <- init:get_arguments(),
	 K == pa orelse K == pz].
