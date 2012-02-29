-module(entry_node).

-behaviour(gen_server).

-export([start_link/0]).

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

start_link() ->
    %% !FIXME make process name
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(Params) ->
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
    UpperLayerName = utils:get_upper_layer (LayerName),
    ParentProcessName = utils:make_process_name (UpperLayerName, ParentName),
    State = #state {
      name = ProcessName,
      parent = ParentProcessName,
      data = EtsTableName
     },
    {ok, [State]}.


%% callbacks
handle_call(hello, _From, State) ->
    io:format("Hello from server!~n", []),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({feed, Data}, State) ->
    EtsTableName = State#state.data,
    ets:insert(EtsTableName, {current_input, Data}),
    {noreply, State};

handle_cast(inference, State) ->
    EtsTableName = State#state.data,
    inference (EtsTableName),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ancillary functions
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
    ets:insert (Data, {y, Y}),
    ets:insert (Data, {lambda_plus, LambdaPlus}).
    

compute_density_over_coincidences (S, I, Sigma) ->
    compute_density_over_coincidences ([], S, I, Sigma).

compute_density_over_coincidences (Acc, [], _Input, _Sigma) ->
    lists:reverse (Acc);

compute_density_over_coincidences (Acc, StoredCoinc, Input, Sigma) ->
    [First|Rest] = StoredCoinc,
    Distance = compute_distance (First, Input, Sigma),
    NewAcc = [Distance|Acc],
    compute_density_over_coincidences (NewAcc, Rest, Input, Sigma).

compute_distance (Coincidence, Input, Sigma) ->
    InputData = Input#entry_node_input.binary_data,
    ChunkSize = Input#entry_node_input.chunk_size,
    CoincidenceName = Coincidence#coincidence.name,
    CoincidenceData = Coincidence#coincidence.data,
    
    Norm = norm (CoincidenceData, InputData, ChunkSize),
    Distance = math:exp ( - math:pow ( (Norm / Sigma), 2 )),
    
    {CoincidenceName, Distance}.


norm (C1, C2, ChunkSize) ->
    norm ([], C1, C2, ChunkSize).

norm (Acc, <<>>, <<>>, _) ->
    math:sqrt (lists:sum (Acc));

norm (Acc, C1, C2, ChunkSize) ->
    <<E1:ChunkSize, R1/binary >> = C1,
    <<E2:ChunkSize, R2/binary >> = C2,
    norm ([ math:pow ( E1 - E2, 2) | Acc ], R1, R2, ChunkSize).

    
compute_density_over_groups (Y, PCG, TemporalGroups) ->
    compute_density_over_groups ([], TemporalGroups, Y, PCG).

compute_density_over_groups (Acc, [], Y, PCG) ->
    lists:reverse (Acc);

compute_density_over_groups (Acc, TemporalGroups, Y, PCG) ->
    [First|Rest] = TemporalGroups,
    Density = compute_density_over_group (First, Y, PCG),
    NewAcc = [Density|Acc],
    compute_density_over_groups (NewAcc, Rest, Y, PCG).

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
	[Group1 = #temporal_group {name = g1, coincidences = [c1,c2]},
	 Group2 = #temporal_group {name = g2, coincidences = [c1]}],
    Y = [{c1, 0.5}, {c2, 1}],
    PCG = [{c1, g1, 0.4},
	   {c1, g2, 1.0},
	   {c2, g1, 0.6}],
    
    Result = compute_density_over_groups (Y, PCG, TemporalGroups),
    
    ?assertEqual ([{g1, 0.5 * 0.4 + 0.6}, {g2, 0.5}], Result).
