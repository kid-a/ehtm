-module (utils).

-export ([
	  configuration_file/0,
	  read_network_structure/1,
	  table_lookup/3,
	  norm/3
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

%% -----------------------------------------------------------------------------
%% Func: norm
%% @doc Compute the Euclidean norm between two binaries, dividing them 
%% into chunks of Chunk bits each.
%%
%% Parameters:
%%   C1 :: binary ()
%%   C2 :: binary ()
%%   ChunkSize :: int ()
%%
%% Reply:
%%   Norm :: float ()
%% -----------------------------------------------------------------------------
norm (C1, C2, ChunkSize) ->
    norm ([], C1, C2, ChunkSize).

norm (Acc, <<>>, <<>>, _) ->
    math:sqrt (lists:sum (Acc));

norm (Acc, C1, C2, ChunkSize) ->
    <<E1:ChunkSize, R1/binary >> = C1,
    <<E2:ChunkSize, R2/binary >> = C2,
    norm ([ math:pow ( E1 - E2, 2) | Acc ], R1, R2, ChunkSize).
