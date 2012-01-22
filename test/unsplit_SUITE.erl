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
-define(NODES, ['mn1@localhost', 'mn2@localhost']).
-define(DISCONNECT_TIME, 4000).
-define(UNSPLIT_TIMEOUT, 5000).
-record(?TABLE,{key,modified=erlang:now(),value}).
%%% Macros

all() ->
    [split1].


init_per_suite(Conf) ->
    Nodes = ct:get_config(nodes, ?NODES),
    DisconnectTime = ct:get_config(disconnect_time, ?DISCONNECT_TIME),
    UnsplitTimeout = ct:get_config(unsplit_timeout, ?UNSPLIT_TIMEOUT),
    Host = get_host(),
    ErlFlags = lists:flatten([?ERL_FLAGS,
			      get_path_flags(),
			     " -pa ", filename:absname(
					filename:dirname(code:which(?MODULE)))]),
    ct:print("ErlFlags = ~p~n", [ErlFlags]),
    StartNode = fun(Node)->
                        ct:print("starting node ~p, on host ~p ~n",[Node, Host]),
                        {ok, NodeName} = ct_slave:start(Host, Node,
                                                        [{erl_flags, ErlFlags}]),
                        NodeName
                end,

    NodeNames = lists:map(StartNode, Nodes),
    [{disconnect_time, DisconnectTime},
     {unsplit_timeout, UnsplitTimeout},
     {nodes, NodeNames}|Conf].

end_per_suite(_Conf) ->
    Nodes = ct:get_config(nodes,?NODES),
    Host = get_host(),
    StopNode = fun(Node)->
                       {ok, _NodeName} = ct_slave:stop(Host, Node)
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
    ct:print("Initial table size~n"),
    print_table_size(Nodes, ?TABLE),
    ct:print("inserting records~n"),
    {atomic, ok} = write(M, [#?TABLE{key=1, value=a}, #?TABLE{key=2, value=a}]),
    print_table_size(Nodes, ?TABLE),
    ct:print("disconnecting nodes~n"),
    disconnect(M, S),
    ct:print("inserting records on one node, while the other one is disconnected~n"),
    {atomic, ok} = write(M, [#?TABLE{key=3, value=b}, #?TABLE{key=4, value=b}]),
    print_table_size(Nodes, ?TABLE),
    timer:sleep(DisconnectTime),
    ct:print("reconnecting nodes~n"),
    connect(S, M),
    timer:sleep(UnsplitTimeout),
    print_table_size(Nodes, ?TABLE),
    true = compare_table_size(Nodes, ?TABLE).


compare_table_size([Node1, Node2|_], Table)->
    table_size(Node1, Table) == table_size(Node2, Table).

table_size(Node, Table)->
    rpc:call(Node, mnesia, table_info,[Table, size]).

print_table_size([M,S|_], Table)->
    ct:print("master size = ~p~n",[table_size(M, Table)]),
    ct:print("slave size = ~p~n",[table_size(S, Table)]).

get_conf(Key, Conf)->
    proplists:get_value(Key, Conf).


terminate_nodes(Nodes)->
    Terminate = fun(Node)->
                        rpc:call(Node, application, stop, [unsplit]),
                        rpc:call(Node, mnesia, stop,[]),
                        rpc:call(Node, application, stop, [sasl])
                end,
    lists:foreach(Terminate, Nodes).


init_nodes(Nodes)->
    Init = fun(Node)->
                          rpc:call(Node, mnesia, delete_schema,[Node]),
                          rpc:call(Node, application, start, [sasl]),
                          rpc:call(Node, mnesia, start,[]),
                          rpc:call(Node, application, start, [unsplit]),
                          rpc:call(Node, mnesia, create_schema, [Nodes]),
                          rpc:call(Node, mnesia, change_config, [extra_db_nodes, Nodes--[Node]]),
                          rpc:call(Node, mnesia, delete_table, [?TABLE]),
                          rpc:call(Node, mnesia, create_table, [?TABLE,
                                                                [{ram_copies,Nodes},
                                                                 {attributes,[key,modified,value]},
                                                                 {user_properties,
                                                                  [{unsplit_method,{unsplit_lib,last_modified,[]}}]}]])
                  end,
    lists:foreach(Init, Nodes).

disconnect(Master, Slave)->
    rpc:call(Master, erlang, disconnect_node, [Slave]).
connect(Master, Slave)->
    rpc:call(Master, net_kernel, connect_node, [Slave]).

write(Node, Records)->
    rpc:call(Node, ?MODULE, write, [Records]).

write(Records)->
    Trans = fun()->
                    lists:foreach(fun(Record)->
                                          mnesia:write(Record)
                                  end, Records)
            end,
    mnesia:transaction(Trans).

get_host()->
    [_, H] = re:split(atom_to_list(node()),"@",[{return,list}]),
    list_to_atom(H).
    %% {ok, HostS} = inet:gethostname(),
    %% list_to_atom(HostS).

get_path_flags() ->
    [ [[" -",atom_to_list(K)," ",D] || D <- V]
      || {K,V} <- init:get_arguments(),
	 K == pa orelse K == pz].
