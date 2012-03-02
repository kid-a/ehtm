-module (utils).

-export ([
	  read_network_structure/1,
	  table_lookup/3
	 ]).

read_network_structure (Path) ->
    {ok, [NetworkStructure]} = file:consult (Path),
    NetworkStructure.
    

table_lookup (TableName, Key, Default) ->
    case ets:lookup (TableName, Key) of
	[] -> Default;
	[{Key, Value}] -> Value
    end.
