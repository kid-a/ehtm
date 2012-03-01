-module(entry_node).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include ("node.hrl").

-record (state, { 
	   name,
	   parent,
	   data
	  }).

-define (DEF_SIGMA, 1.0).

%% -----------------------------------------------------------------------------
%% Func: start_link/0
%% @doc Starts an entry_node process.
%%
%% Parameters: 
%%  ProcessName :: atom ()
%%  Params :: [ { name, string () }, 
%%              { layer, string () }, 
%%              { parent, string () },
%%              { sigma, float () } ]
%% -----------------------------------------------------------------------------
start_link(ProcessName, Params) ->
    gen_server:start_link({local, ProcessName}, ?MODULE, [Params], []).

%% -----------------------------------------------------------------------------
%% Func: init/1
%% @doc Starts an entry_node process.
%%
%% Parameters:
%%   Params :: [ { name, string () }, 
%%               { layer, string () }, 
%%               { parent, string () },
%%               { sigma, float () } ]
%% -----------------------------------------------------------------------------
init([Params]) ->
    NodeName = proplists:get_value (name, Params),
    LayerName = proplists:get_value (layer, Params),
    ProcessName = node:make_process_name (LayerName, NodeName),
    EtsTableName = node:make_ets_name (ProcessName),
    
    %% create a table for process data
    ets:new (EtsTableName, [set,
			    named_table,
			    protected, %% other processes can read
			    {read_concurrency, true}
			   ]),
    
    %% initialize some parameters
    Sigma = proplists:get_value (sigma, Params, ?DEF_SIGMA),
    ets:insert (EtsTableName, {sigma, Sigma}),   
    
    %% initialize the process state
    ParentName = proplists:get_value (parent, Params),
    UpperLayerName = node:get_upper_layer (LayerName),
    ParentProcessName = node:make_process_name (UpperLayerName, ParentName),
    State = #state {
      name = ProcessName,
      parent = ParentProcessName,
      data = EtsTableName
     },
    {ok, State}.


%% callbacks
handle_call (read_state, _From, State) ->
    EtsTableName = State#state.data,
    Reply = make_snapshot (EtsTableName),
    {reply, Reply , State};

handle_call (_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast ({feed, Data}, State) ->
    EtsTableName = State#state.data,
    ets:insert(EtsTableName, {current_input, Data}),
    {noreply, State};

handle_cast (inference, State) ->
    EtsTableName = State#state.data,
    inference (EtsTableName),
    propagate (EtsTableName, State),
    {noreply, State};

handle_cast ({set_state, S}, State) ->
    EtsTableName = State#state.data,
    set_state (EtsTableName, S),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%% ancillary functions
%% -----------------------------------------------------------------------------
%% Func: inference
%% @doc Perfoms an inference steps. Takes as input the name of an ETS table
%% containing the node state.
%%
%% Parameters:
%%   Data :: atom ()
%% -----------------------------------------------------------------------------
inference (Data) ->
    %% read the state
    [{_, Input}] = ets:lookup (Data, current_input),
    [{_, Coincidences}] = ets:lookup (Data, coincidences),
    [{_, Sigma}] = ets:lookup (Data, sigma),
    [{_, TemporalGroups}] = ets:lookup (Data, temporal_groups),
    [{_, PCG}] = ets:lookup (Data, pcg),

    %% perform inference
    Y = compute_density_over_coincidences (Coincidences, Input, Sigma),
    LambdaPlus = compute_density_over_groups (Y, PCG, TemporalGroups),

    %% update the state
    ets:insert (Data, [{y, Y}, 
		       {lambda_plus, LambdaPlus}]).
    

%% -----------------------------------------------------------------------------
%% Func: compute_density_over_coincidences
%% @doc Compute the density of an input vector over a set of coincidences.
%%
%% Parameters:
%%   S :: [#coincidences ()]
%%   I :: #entry_node_input ()
%%   Sigma :: float ()
%%
%% Reply:
%%   Densities :: [ { coincidence_name :: atom (),
%%                    density :: float () } ]
%% -----------------------------------------------------------------------------
compute_density_over_coincidences (S, I, Sigma) ->
    compute_density_over_coincidences ([], S, I, Sigma).

compute_density_over_coincidences (Acc, [], _Input, _Sigma) ->
    lists:reverse (Acc);

compute_density_over_coincidences (Acc, StoredCoinc, Input, Sigma) ->
    [First|Rest] = StoredCoinc,
    Distance = compute_distance (First, Input, Sigma),
    NewAcc = [Distance|Acc],
    compute_density_over_coincidences (NewAcc, Rest, Input, Sigma).


%% -----------------------------------------------------------------------------
%% Func: compute_distance 
%% @doc Compute the distance between a coincidence and an input vector according
%% to the following formula: d = exp ( - ||c - i||^2 / sigma^2 )
%%
%% Parameters:
%%   Coincidence :: #coincidence ()
%%   Input :: #entry_node_input ()
%%   Sigma :: float ()
%%
%% Reply:
%%   Distance :: [{ coincidence_name :: atom (),
%%                  value :: float () }]
%% -----------------------------------------------------------------------------
compute_distance (Coincidence, Input, Sigma) ->
    InputData = Input#entry_node_input.binary_data,
    ChunkSize = Input#entry_node_input.chunk_size,
    CoincidenceName = Coincidence#coincidence.name,
    CoincidenceData = Coincidence#coincidence.data,
    
    Norm = norm (CoincidenceData, InputData, ChunkSize),
    Distance = math:exp ( - math:pow ( (Norm / Sigma), 2 )),
    
    {CoincidenceName, Distance}.


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
%% Func: compute_density_over_groups
%% @doc Given the vector Y of densities over coincidences, computes the vector
%% of density over coincidences.
%% 
%%
%% Parameters:
%%   Y :: [ { coincidence_name :: atom (), y :: float } ]
%%   PCG :: [ { coincidence_name :: atom (), 
%%              temporal_group_name :: atom (), 
%%              probability :: float () } ]
%%   TemporalGroups = [#temporal_group ()]
%%
%% Reply:
%%   Densities :: [ { temporal_group_name :: atom (), 
%%                    density :: float () } ]
%% -----------------------------------------------------------------------------
compute_density_over_groups (Y, PCG, TemporalGroups) ->
    compute_density_over_groups ([], TemporalGroups, Y, PCG).

compute_density_over_groups (Acc, [], _Y, _PCG) ->
    lists:reverse (Acc);

compute_density_over_groups (Acc, TemporalGroups, Y, PCG) ->
    [First|Rest] = TemporalGroups,
    Density = compute_density_over_group (First, Y, PCG),
    NewAcc = [Density|Acc],
    compute_density_over_groups (NewAcc, Rest, Y, PCG).


%% -----------------------------------------------------------------------------
%% Func: compute_density_over_group
%% @doc Given the vector Y of densities over coincidences and a temporal group,
%% computes the density of that group.
%% 
%%
%% Parameters:
%%   Y :: [ { coincidence_name :: atom (), y :: float } ]
%%   PCG :: [ { coincidence_name :: atom (), 
%%              temporal_group_name :: atom (), 
%%              probability :: float () } ]
%%   Group :: #temporal_group ()
%%
%% Reply:
%%   Density :: { temporal_group_name :: atom (), 
%%                density :: float () } 
%% -----------------------------------------------------------------------------
compute_density_over_group (Group, Y, PCG) ->
    GroupName = Group#temporal_group.name,
    Probabilities = 
	lists:foldl (fun (Entry, Acc) ->
			     case Entry of
				 {CoincName, GroupName, Value} ->
				     [{CoincName, Value}|Acc];
				 _ -> Acc
			     end
		     end,
		     [],
		     PCG),
    Densities = 
	lists:foldl (fun ({CoincName, Yi}, Acc) ->
			     %% if no probability is found, suppose
			     %% it is zero
			     Prob = 
				 proplists:get_value (CoincName, 
						      Probabilities, 
						      0.0),
			     [Yi * Prob | Acc]
		     end,
		     [],
		     Y),

    {GroupName, lists:sum (Densities)}.						 


%% -----------------------------------------------------------------------------
%% Func: make_snapshot
%% @doc Given the name of the ETS table containing the node's data,
%% makes a snapshot of the current state and returns it.
%% 
%%
%% Parameters:
%%   Data :: atom ()
%%
%% Reply:
%%   Snapshot :: #entry_node_state ()
%% -----------------------------------------------------------------------------
make_snapshot (Data) ->
    LambdaMinus = case table_lookup (Data, current_input, undefined) of
		      undefined -> undefined;
		      Entry -> Entry#entry_node_input.binary_data
		  end,
    LambdaPlus = table_lookup (Data, lambda_plus, []),
    Sigma = table_lookup (Data, sigma, undefined),
    Coincidences = table_lookup (Data, coincidences, []),
    CoincidencesOccurrences = table_lookup (Data, coincidences_occurrences, []),
    Y = table_lookup (Data, y, []),
    T = table_lookup (Data, t, []),
    TemporalGroups = table_lookup (Data, temporal_groups, []),
    PCG = table_lookup (Data, pcg, []),
    
    #entry_node_state { lambda_minus = LambdaMinus,
			lambda_plus = LambdaPlus,
			sigma = Sigma,
			coincidences = Coincidences,
			coincidences_occurrences = CoincidencesOccurrences,
			y = Y,
			t = T,
			temporal_groups = TemporalGroups,
			pcg = PCG
		      }.

table_lookup (TableName, Key, Default) ->
    case ets:lookup (TableName, Key) of
	[] -> Default;
	[{Key, Value}] -> Value
    end.


set_state (Data, State) ->
    LambdaMinus = State#entry_node_state.lambda_minus,
    LambdaPlus = State#entry_node_state.lambda_plus,
    Sigma = State#entry_node_state.sigma,
    Coincidences = State#entry_node_state.coincidences,
    CoincidencesOccurrences = State#entry_node_state.coincidences_occurrences,
    Y = State#entry_node_state.y,
    T = State#entry_node_state.t,
    TemporalGroups = State#entry_node_state.temporal_groups,
    PCG = State#entry_node_state.pcg,
    
    ets:insert (Data, [{lambda_minus, #entry_node_input { 
			  chunk_size = undefined,
			  binary_data = LambdaMinus
			 }},
		       {lambda_plus, LambdaPlus},
		       {sigma, Sigma},
		       {coincidences, Coincidences},
		       {coincidences_occurrences, CoincidencesOccurrences},
		       {y, Y},
		       {t, T},
		       {temporal_groups, TemporalGroups},
		       {pcg, PCG}]).


%% -----------------------------------------------------------------------------
%% Func: propagate
%% @doc Propagate the output message to the parent node.
%%
%% Parameters:
%%   Data :: atom ()
%%   State :: #state
%% -----------------------------------------------------------------------------
propagate (Data, State) ->
    [{_, LambdaPlus}] = ets:lookup (Data, lambda_plus),
    Parent = State#state.parent,    
    node:feed (Parent, LambdaPlus).
    
		   
%% tests
norm_test () ->
    I1 = <<1,1,1>>,
    I2 = <<2,2,2>>,
    ChunkSize = 8,
    
    ?assertEqual ( norm(I1, I1, ChunkSize), 0.0 ),
    ?assertEqual ( norm(I1, I2, ChunkSize), math:sqrt(3) ).


compute_distance_test () ->
    Coincidence = #coincidence {
      name = c1, 
      data = <<1,1,1>>
     },
    Input = #entry_node_input {
      chunk_size = 8,
      binary_data = <<2,2,2>>
     },
    Sigma = 1.0,
    Result = compute_distance (Coincidence, Input, Sigma),

    ?assertEqual ({c1, math:exp (- math:pow (math:sqrt(3), 2))}, Result).

compute_density_over_coincidences_test () ->
    Coincidences = [#coincidence {name = c1, data = <<1,1,1>>},
		    #coincidence {name = c2, data = <<2,2,2>>}],
    Input = #entry_node_input {chunk_size = 8, binary_data = <<1,1,1>>},
    Sigma = 1.0,
    Result = compute_density_over_coincidences (Coincidences, Input,Sigma),
    
    ?assertEqual ([{c1, 1.0},
		   {c2, math:exp (- math:pow (math:sqrt(3), 2))}],
		  Result).

compute_density_over_group_test () ->
    Group1 = #temporal_group {name = g1, coincidences = [c1,c2]},
    Group2 = #temporal_group {name = g2, coincidences = [c1]},
    Y = [{c1, 0.5}, {c2, 1}],
    PCG = [{c1, g1, 0.4},
	   {c1, g2, 1.0},
	   {c2, g1, 0.6}],
    Result1 = compute_density_over_group (Group1, Y, PCG),
    Result2 = compute_density_over_group (Group2, Y, PCG),
    
    ?assertEqual ({g1, 0.5 * 0.4 + 0.6}, Result1),
    ?assertEqual ({g2, 0.5}, Result2).

compute_density_over_groups_test () ->
    TemporalGroups =
	[#temporal_group {name = g1, coincidences = [c1,c2]},
	 #temporal_group {name = g2, coincidences = [c1]}],
    Y = [{c1, 0.5}, {c2, 1}],
    PCG = [{c1, g1, 0.4},
	   {c1, g2, 1.0},
	   {c2, g1, 0.6}],
    
    Result = compute_density_over_groups (Y, PCG, TemporalGroups),
    
    ?assertEqual ([{g1, 0.5 * 0.4 + 0.6}, {g2, 0.5}], Result).

create_entry_node_test () ->
    Name = "node1",
    Layer = "0",
    Parent = "node5",
    ProcessName = node:make_process_name (Layer, Name),
    Params = [ {name, Name},
	       {layer, Layer},
	       {parent, Parent},
	       {sigma, 1.0} ],
    
    start_link (ProcessName, Params).

read_state_test () ->
    Name = "node1",
    Layer = "0",
    Parent = "node5",
    ProcessName = node:make_process_name (Layer, Name),
    Params = [ {name, Name},
	       {layer, Layer},
	       {parent, Parent},
	       {sigma, 1.0} ],
    
    start_link (ProcessName, Params),
    
    State = node:read_state (ProcessName),
    
    ?assertEqual (State#entry_node_state.sigma, 1.0),
    ?assertEqual (State#entry_node_state.lambda_minus, undefined).


feed_test () ->
    Name = "node1",
    Layer = "0",
    BinaryData = <<1,1,1>>,
    ProcessName = node:make_process_name (Layer, Name),
    node:feed (ProcessName, 
	       #entry_node_input
	       {
		 chunk_size = 8,
		 binary_data = BinaryData
	       }),
    State = node:read_state (ProcessName),
    
    ?assertEqual (State#entry_node_state.lambda_minus,
		  BinaryData).


set_state_test () ->
    Name = "node1",
    Layer = "0",
    BinaryData = <<1,1,1>>,
    ProcessName = node:make_process_name (Layer, Name),
    node:set_state (ProcessName, 
		    #entry_node_state { lambda_minus = BinaryData }),
    
    State = node:read_state (ProcessName),
    
    ?assertEqual (State#entry_node_state.lambda_minus,
    		  BinaryData).

%% !FIXME maybe everything should be a call and not a cast
%% otherways some tests could fail
%% an inference test here
