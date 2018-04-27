-module(unsplit_2nodes_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1]).
-export([all/0]).
-export([insert_new_records/1, change_existing_records/1]).
-export([write/1, read/1]).

-define(ERL_FLAGS, "-kernel dist_auto_connect once").
-define(TABLE, test1).
-define(NODES, ['mn1', 'mn2']).

-record(?TABLE, {key, modified = erlang:timestamp(), value}).

all() ->
  [
    insert_new_records,
    change_existing_records
  ].


init_per_suite(Conf) ->
  Nodes = ct:get_config(nodes, ?NODES),

  StartNode =
    fun(Node) ->
      {ok, NodeName} = ct_slave:start(Node, [{erl_flags, make_erl_flags()}]),
      NodeName
    end,

  NodeNames = lists:map(StartNode, Nodes),

  init_nodes(NodeNames),

  [{nodes, NodeNames} | Conf].

end_per_suite(_Conf) ->
  Nodes = ct:get_config(nodes, ?NODES),
  terminate_nodes(Nodes),
  StopNode =
    fun(Node) ->
      {ok, _NodeName} = ct_slave:stop(Node)
    end,
  lists:map(StopNode, Nodes),
  ok.

insert_new_records(Conf) ->
  Nodes = [Node1, Node2 | _Rest] = ?config(nodes, Conf),

  Element1 = #?TABLE{key = 1, value = a},
  Element2 = #?TABLE{key = 2, value = a},
  Element3 = #?TABLE{key = 3, value = b},
  Element4 = #?TABLE{key = 4, value = b},
  Element5 = #?TABLE{key = 5, value = c},

  0 = table_size(Node1, ?TABLE),
  0 = table_size(Node2, ?TABLE),

  {atomic, ok} = write(Node1, [Element1, Element2]),
  ct:sleep(100),
  2 = table_size(Node1, ?TABLE),
  2 = table_size(Node2, ?TABLE),

  disconnect(Node1, Node2),

  {atomic, ok} = write(Node1, [Element3, Element4]),
  {atomic, ok} = write(Node2, [Element5]),

  4 = table_size(Node1, ?TABLE),
  3 = table_size(Node2, ?TABLE),

  connect(Node2, Node1),

  5 = table_size(Node1, ?TABLE),
  5 = table_size(Node2, ?TABLE),

  true = compare_table_size(Nodes, ?TABLE).

change_existing_records(Conf) ->
  [Node1, Node2 | _Rest] = ?config(nodes, Conf),

  K1 = 11,
  Element1V1 = #?TABLE{key = K1, value = av1, modified = 1},
  Element1V2 = #?TABLE{key = K1, value = av2, modified = 2},

  K2 = 21,
  Element2V1 = #?TABLE{key = K2, value = bv1, modified = 1},
  Element2V2 = #?TABLE{key = K2, value = bv2, modified = 2},

  {atomic, ok} = write(Node1, [Element1V1]),
  {atomic, ok} = write(Node2, [Element2V1]),
  ct:sleep(100),

  check_element(Node1, K1, Element1V1),
  check_element(Node2, K1, Element1V1),
  check_element(Node1, K2, Element2V1),
  check_element(Node2, K2, Element2V1),

  disconnect(Node1, Node2),

  {atomic, ok} = write(Node1, [Element1V2]),
  {atomic, ok} = write(Node2, [Element2V2]),

  check_element(Node1, K1, Element1V2),
  check_element(Node2, K1, Element1V1),
  check_element(Node1, K2, Element2V1),
  check_element(Node2, K2, Element2V2),

  connect(Node1, Node2),

  check_element(Node1, K1, Element1V2),
  check_element(Node2, K1, Element1V2),
  check_element(Node1, K2, Element2V2),
  check_element(Node2, K2, Element2V2),

  ok.

check_element(Node, Key, Element) ->
  {atomic, [Element]} = read(Node, Key).

compare_table_size([Node1, Node2 | _], Table) ->
  table_size(Node1, Table) == table_size(Node2, Table).

table_size(Node, Table) ->
  ct_rpc:call(Node, mnesia, table_info, [Table, size]).

terminate_nodes(Nodes) ->
  Terminate = fun(Node) ->
    ct_rpc:call(Node, application, stop, [unsplit]),
    ct_rpc:call(Node, mnesia, stop, []),
    ct_rpc:call(Node, application, stop, [sasl])
              end,
  lists:foreach(Terminate, Nodes).

init_nodes([FirstNode | _] = Nodes) ->
  Init =
    fun(Node) ->
      {ok, [mnesia, unsplit]} = ct_rpc:call(Node, application, ensure_all_started,
                                            [unsplit]),
      {ok, _} = ct_rpc:call(Node, mnesia, change_config,
                            [extra_db_nodes, Nodes--[Node]])
    end,
  CreateTable =
    fun(Node) ->
      Tables = [?TABLE, [
        {ram_copies, Nodes},
        {attributes, [key, modified, value]},
        {user_properties, [{unsplit_method, {unsplit_lib, last_modified, []}}]}
      ]],
      {atomic, ok} = ct_rpc:call(Node, mnesia, create_table, Tables)
    end,
  WaitForTables =
    fun(Node) ->
      ok = ct_rpc:call(Node, mnesia, wait_for_tables, [[?TABLE], 1000])
    end,
  lists:foreach(Init, Nodes),
  CreateTable(FirstNode),
  lists:foreach(WaitForTables, Nodes).

disconnect(Node1, Node2) ->
  ct_rpc:call(Node1, erlang, disconnect_node, [Node2]).
connect(Node1, Node2) ->
  ct_rpc:call(Node1, net_kernel, connect_node, [Node2]),
  ct:sleep(500).

write(Node, Records) ->
  ct_rpc:call(Node, ?MODULE, write, [Records]).

write(Records) ->
  mnesia:transaction(fun() -> lists:foreach(fun mnesia:write/1, Records) end).

read(Node, Key) ->
  ct_rpc:call(Node, ?MODULE, read, [Key]).

read(Key) ->
  mnesia:transaction(fun() -> mnesia:read(?TABLE, Key) end).

make_erl_flags() ->
  ThisDir = filename:dirname(code:which(?MODULE)),
  TestPath = filename:absname(ThisDir),
  EbinPath = filename:absname(ThisDir ++ "/../ebin"),
  lists:flatten([?ERL_FLAGS, get_path_flags(),
                 " -pa ", EbinPath,
                 " -pa ", TestPath]).

get_path_flags() ->
  [[[" -", atom_to_list(K), " ", D] || D <- V]
   || {K, V} <- init:get_arguments(),
   K == pa orelse K == pz].
