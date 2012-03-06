-module (utils).

-export ([
	  configuration_file/0,
	  read_network_structure/1,
	  table_lookup/3,
	  norm/3,
	  extract_widx/1
	 ]).

configuration_file () -> "/home/loris/ehtm/priv/two-layers-network.conf".

%% !FIXME doc here
read_network_structure (Path) ->
    {ok, [NetworkStructure]} = file:consult (Path),
    NetworkStructure.
    
%% !FIXME doc here
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


%% -----------------------------------------------------------------------------
%% Func: extract_widx
%% @doc Given an input message, returns the list of winning temporal groups for
%% each child, that is, the temporal groups with the highest activation level
%% in that child.
%%
%% Parameters:
%%   LambdaMinus :: [ { child :: atom (), 
%%                      lambda :: [ { temporal_group_name :: atom (),
%%                                    density :: float () ] }
%% Reply:
%%   Widx :: [ {child :: atom (), winning_temporal_group :: atom () } ]
%% -----------------------------------------------------------------------------
extract_widx (LambdaMinus) ->
    extract_widx (LambdaMinus, []).

extract_widx ([], Acc) -> lists:reverse (Acc);

extract_widx ([{Child, Lambda} | Rest], Acc) ->
    extract_widx (Rest, [{Child, winning_group (Lambda)} | Acc]).

winning_group ([{T, D}|Rest]) ->
    winning_group (Rest, {T, D}).

winning_group ([], {WinningGroup, _}) -> WinningGroup;

winning_group ([{T, D}| Rest], {OldT, OldD}) ->
    if D > OldD -> winning_group (Rest, {T, D});
       true -> winning_group (Rest, {OldT, OldD})
    end.
	    
    
    
