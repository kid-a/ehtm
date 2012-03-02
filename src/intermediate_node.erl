-module(intermediate_node).

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

%% -----------------------------------------------------------------------------
%% Func: start_link/0
%% @doc Starts an entry_node process.
%%
%% Parameters: 
%%  ProcessName :: atom ()
%%  Params :: [ { name, string () }, 
%%              { layer, string () }, 
%%              { parent, string () }]
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
%%               { parent, string () } ]
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

handle_call ({feed, Data}, _From, State) ->
    EtsTableName = State#state.data,
    LambdaMinus = utils:table_lookup (EtsTableName, lambda_minus, []),
    ets:insert (EtsTableName, {lambda_minus, 
			       [Data | LambdaMinus]}),
    {reply, ok, State};

handle_call ({set_state, S}, _From, State) ->
    EtsTableName = State#state.data,
    set_state (EtsTableName, S),
    {reply, ok, State};

handle_call (_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast (inference, State) ->
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
    [{_, Input}] = ets:lookup (Data, lambda_minus),
    [{_, Coincidences}] = ets:lookup (Data, coincidences),
    [{_, TemporalGroups}] = ets:lookup (Data, temporal_groups),
    [{_, PCG}] = ets:lookup (Data, pcg),

    %% perform inference
    Y = compute_density_over_coincidences (Coincidences, Input),
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
%%   I :: [ {child :: atom (), {temporal_group_name :: atom (),
%%                              density :: float () } ]
%%
%% Reply:
%%   Densities :: [ { coincidence_name :: atom (),
%%                    density :: float () } ]
%% -----------------------------------------------------------------------------
compute_density_over_coincidences (S, I) ->
    compute_density_over_coincidences ([], S, I).

compute_density_over_coincidences (Acc, [], _Input) ->
    lists:reverse (Acc);

compute_density_over_coincidences (Acc, StoredCoinc, Input) ->
    [First|Rest] = StoredCoinc,
    Distance = compute_density_over_coincidence (First, Input),
    NewAcc = [Distance|Acc],
    compute_density_over_coincidences (NewAcc, Rest, Input).


%% -----------------------------------------------------------------------------
%% Func: compute_density_over_coincidence
%% @doc Compute the density of an input vector over a given coincidence.
%%
%% Parameters:
%%   Coincidence :: #coincidence ()
%%   Input :: [ {child :: atom (), { temporal_group_name :: atom (),
%%                                   density :: float () } ]
%%
%% Reply:
%%   Density :: [{ coincidence_name :: atom (),
%%                 value :: float () }]
%% -----------------------------------------------------------------------------
compute_density_over_coincidence (Coincidence, Input) ->
    CoincidenceName = Coincidence#coincidence.name,
    CoincidenceData = Coincidence#coincidence.data,
    
    GroupsActivationValues =
	lists:foldl (fun ({ChildName, Group}, Acc) ->
			     Message = proplists:get_value (ChildName, Input),
			     GroupActivationValue = proplists:get_value (Group, Message),
			     [GroupActivationValue | Acc]
		     end,
		     [],
		     CoincidenceData),
    
    %% multiply all elements in the list
    Density = lists:foldl (fun (V, Prod) -> V * Prod end, 1.0, GroupsActivationValues),

    {CoincidenceName, Density}.

    
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
%%   Snapshot :: #intermediate_node_state ()
%% -----------------------------------------------------------------------------
make_snapshot (Data) ->
    LambdaMinus = utils:table_lookup (Data, lambda_minus, []),
    LambdaPlus = utils:table_lookup (Data, lambda_plus, []),
    Coincidences = utils:table_lookup (Data, coincidences, []),
    CoincidencesOccurrences = utils:table_lookup (Data, coincidences_occurrences, []),
    Y = utils:table_lookup (Data, y, []),
    T = utils:table_lookup (Data, t, []),
    TemporalGroups = utils:table_lookup (Data, temporal_groups, []),
    PCG = utils:table_lookup (Data, pcg, []),
    
    #intermediate_node_state { lambda_minus = LambdaMinus,
			       lambda_plus = LambdaPlus,
			       coincidences = Coincidences,
			       coincidences_occurrences = CoincidencesOccurrences,
			       y = Y,
			       t = T,
			       temporal_groups = TemporalGroups,
			       pcg = PCG
			     }.


set_state (Data, State) ->
    LambdaMinus = State#intermediate_node_state.lambda_minus,
    LambdaPlus = State#intermediate_node_state.lambda_plus,
    Coincidences = State#intermediate_node_state.coincidences,
    CoincidencesOccurrences = State#intermediate_node_state.coincidences_occurrences,
    Y = State#intermediate_node_state.y,
    T = State#intermediate_node_state.t,
    TemporalGroups = State#intermediate_node_state.temporal_groups,
    PCG = State#intermediate_node_state.pcg,
    
    ets:insert (Data, [{lambda_minus, LambdaMinus},
		       {lambda_plus, LambdaPlus},
		       {coincidences, Coincidences},
		       {coincidences_occurrences, CoincidencesOccurrences},
		       {y, Y},
		       {t, T},
		       {temporal_groups, TemporalGroups},
		       {pcg, PCG}]).
		   
%% tests
compute_density_over_coincidences_test () ->
    Coincidences = [#coincidence {name = c1, 
				  data = [{'layer0.node1', g1},
					  {'layer0.node2', g1}]},
		    #coincidence {name = c2, 
				  data = [{'layer0.node1', g2},
					  {'layer0.node2', g1}]}],
    
    Input = [{'layer0.node1', [{g1, 0.6}, {g2, 1.0}]},
	     {'layer0.node2', [{g1, 0.6}, {g2, 1.0}, {g3, 0.2}]}],
    
    Result = compute_density_over_coincidences (Coincidences, Input),
    
    ?assertEqual ([{c1, 0.6 * 0.6}, {c2, 1.0 * 0.6}], Result).

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

create_intermediate_node_test () ->
    {Name, Layer, Parent} = {"node1", "1", "node5"},
    ProcessName = node:make_process_name (Layer, Name),
    Params = [ {name, Name},
	       {layer, Layer},
	       {parent, Parent}],
    
    start_link (ProcessName, Params).

read_state_test () ->
    {Name, Layer, Parent} = {"node2", "1", "node5"},
    ProcessName = node:make_process_name (Layer, Name),
    Params = [ {name, Name},
	       {layer, Layer},
	       {parent, Parent}],
    
    start_link (ProcessName, Params),
   
    State = node:read_state (ProcessName),
    
    ?assertEqual (State#intermediate_node_state.lambda_minus, []),
    ?assertEqual (State#intermediate_node_state.lambda_plus, []),
    ?assertEqual (State#intermediate_node_state.coincidences, []),
    ?assertEqual (State#intermediate_node_state.coincidences_occurrences, []),
    ?assertEqual (State#intermediate_node_state.y, []),
    ?assertEqual (State#intermediate_node_state.t, []),
    ?assertEqual (State#intermediate_node_state.temporal_groups, []),
    ?assertEqual (State#intermediate_node_state.pcg, []).

feed_test () ->
    {Name, Layer, Parent} = {"node3", "1", "node5"},
    ProcessName = node:make_process_name (Layer, Name),
    Params = [ {name, Name},
	       {layer, Layer},
	       {parent, Parent}],
    Input = {'layer0.node1', [{g1, 0.6}, {g2, 1.0}]},
    start_link (ProcessName, Params),
    node:feed (ProcessName, Input),
    State = node:read_state (ProcessName),

    ?assertEqual ([Input], State#intermediate_node_state.lambda_minus).

set_state_test () ->
    {Name, Layer, Parent} = {"node4", "1", "node5"},
    Input = [{'layer0.node1', [{g1, 0.6}, {g2, 1.0}]},
	     {'layer0.node2', [{g1, 0.6}, {g2, 1.0}, {g3, 0.2}]}],    
    ProcessName = node:make_process_name (Layer, Name),
    Params = [ {name, Name},
	       {layer, Layer},
	       {parent, Parent}],
    start_link (ProcessName, Params),
    node:set_state (ProcessName, #intermediate_node_state {lambda_minus =  Input}),
    State = node:read_state (ProcessName),
    
    ?assertEqual (Input, State#intermediate_node_state.lambda_minus).

%% !FIXME refactor, some code is duplicated between intermediate and entry
%% nodes
