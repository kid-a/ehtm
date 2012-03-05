-module (utils).

-export ([
	  configuration_file/0,
	  read_network_structure/1,
	  table_lookup/3
	 ]).

configuration_file () -> "/home/loris/ehtm/priv/two-layers-network.conf".

read_network_structure (Path) ->
    {ok, [NetworkStructure]} = file:consult (Path),
    NetworkStructure.
    

table_lookup (TableName, Key, Default) ->
    case ets:lookup (TableName, Key) of
	[] -> Default;
	[{Key, Value}] -> Value
    end.
