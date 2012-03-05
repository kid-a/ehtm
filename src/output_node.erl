-module(output_node).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include ("node.hrl").

-record (state, { 
	   name,
	   data,
	   children
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
%%               { layer, string () } ]
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
    State = #state {
      name = ProcessName,
      data = EtsTableName,
      children = []
     },
    {ok, State}.


%% callbacks
handle_call (read_state, _From, State) ->
    EtsTableName = State#state.data,
    Reply = make_snapshot (EtsTableName),
    {reply, Reply , State};

handle_call ({set_state, S}, _From, State) ->
    EtsTableName = State#state.data,
    set_state (EtsTableName, S),
    {reply, ok, State};

handle_call (_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast ({feed, Data}, State) ->
    EtsTableName = State#state.data,
    LambdaMinus = utils:table_lookup (EtsTableName, lambda_minus, []),
    ets:insert (EtsTableName, {lambda_minus, 
			       [Data | LambdaMinus]}),
    
    if length ([Data | LambdaMinus]) == length (State#state.children) ->
	    inference (EtsTableName),
	    ets:insert (EtsTableName, {lambda_minus, []}),
	    {noreply, State};
       true ->
	    {noreply, State}
    end;

handle_cast ({register_child, Child}, State) ->
    ChildrenList = State#state.children,
    NewChildrenList = [Child | ChildrenList],
    NewState = State#state {children = NewChildrenList},
    {noreply, State};

%% handle_cast (inference, State) ->
%%     EtsTableName = State#state.data,
%%     inference (EtsTableName),
%%     {noreply, State};

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
    [{_, Classes}] = ets:lookup (Data, classes),
    [{_, PriorProbabilities}] = ets:lookup (Data, prior_probabilities),
    [{_, PCW}] = ets:lookup (Data, pcw),

    %% perform inference
    Y = compute_density_over_coincidences (Coincidences, Input),
    Densities = compute_density_over_classes (Classes, Y, PCW),
    LambdaPlus = compute_class_posterior_probabilities (Densities, PriorProbabilities),

    %% update the state
    ets:insert (Data, [{y, Y},
		       {densities_over_classes, Densities},
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
%% Func: compute_density_over_classes
%% @doc Given the vector Y of densities over coincidences, computes the vector
%% of density over classes.
%% 
%%
%% Parameters:
%%   Y :: [ { coincidence_name :: atom (), y :: float } ]
%%   PCW :: [ { coincidence_name :: atom (), 
%%              class_name :: atom (), 
%%              probability :: float () } ]
%%   Classes = [#class]
%%
%% Reply:
%%   Densities :: [ { class_name :: atom (), 
%%                    density :: float () } ]
%% -----------------------------------------------------------------------------
compute_density_over_classes (Classes, Y, PCW) ->
    compute_density_over_classes ([], Classes, Y, PCW).

compute_density_over_classes (Acc, [], _Y, _PCW) ->
    lists:reverse (Acc);

compute_density_over_classes (Acc, Classes, Y, PCW) ->
    [First|Rest] = Classes,
    Density = compute_density_over_class (First, Y, PCW),
    NewAcc = [Density|Acc],
    compute_density_over_classes (NewAcc, Rest, Y, PCW).


%% -----------------------------------------------------------------------------
%% Func: compute_density_over_class
%% @doc Given the vector Y of densities over coincidences and a decision class,
%% computes the density of that class
%% 
%%
%% Parameters:
%%   Y :: [ { coincidence_name :: atom (), y :: float } ]
%%   PCW :: [ { coincidence_name :: atom (), 
%%              class_name :: atom (), 
%%              probability :: float () } ]
%%   Class :: #class ()
%%
%% Reply:
%%   Density :: { class_name :: atom (), 
%%                density :: float () } 
%% -----------------------------------------------------------------------------
compute_density_over_class (Class, Y, PCW) ->
    ClassName = Class#class.name,
    Probabilities = 
	lists:foldl (fun (Entry, Acc) ->
			     %% !FIXME this case is no more needed
			     case Entry of
				 {CoincName, ClassName, Value} ->
				     [{CoincName, Value}|Acc];
				 _ -> Acc
			     end
		     end,
		     [],
		     PCW),
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

    {ClassName, lists:sum (Densities)}.


%% -----------------------------------------------------------------------------
%% Func: compute_class_posterior_probabilities
%% @doc Given the vector Y of densities over classes and the prior probabilities,
%% computes each class' posterior probability by means of Bayes' inversion
%% formula
%% 
%%
%% Parameters:
%%   DensitiesOverClasses :: [ { class_name :: atom (),
%%                               density :: float () } ]
%%   PriorProbabilities :: [ { class_name :: atom (),
%%                             probability :: float () } ]
%%
%% Reply:
%%   PosteriorProbabilities :: { class_name :: atom (), 
%%                               probability :: float () } 
%% -----------------------------------------------------------------------------
compute_class_posterior_probabilities (DensitiesOverClasses, PriorProbabilities) ->
    %% first, compute the total probability
    TotalProbability = 
	lists:sum ( lists:foldl (fun ({ClassName, Density}, Acc) ->
					 PriorProbability = 
					     proplists:get_value (ClassName, 
								  PriorProbabilities),

					 [PriorProbability * Density | Acc]
				 end,
				 [],
				 DensitiesOverClasses)),
    
    PosteriorProbabilities = 
	lists:foldl (fun ({ClassName, Density}, Acc) ->
			     PriorProbability = 
				 proplists:get_value (ClassName, 
						      PriorProbabilities),
			     PosteriorProbability = 
				 Density * PriorProbability / TotalProbability,
			     
			     [{ClassName, PosteriorProbability} | Acc]
		     end,
		     [],
		     DensitiesOverClasses).
		    

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
%%   Snapshot :: #output_node_state ()
%% -----------------------------------------------------------------------------
make_snapshot (Data) ->
    LambdaMinus = utils:table_lookup (Data, lambda_minus, []),
    LambdaPlus = utils:table_lookup (Data, lambda_plus, []),
    Coincidences = utils:table_lookup (Data, coincidences, []),
    CoincidencesOccurrences = utils:table_lookup (Data, coincidences_occurrences, []),
    Y = utils:table_lookup (Data, y, []),
    T = utils:table_lookup (Data, t, []),
    Classes = utils:table_lookup (Data, classes, []),
    PriorProbabilities = utils:table_lookup (Data, prior_probabilities, []),
    PCW = utils:table_lookup (Data, pcw, []),
    
    #output_node_state { lambda_minus = LambdaMinus,
			 lambda_plus = LambdaPlus,
			 coincidences = Coincidences,
			 coincidences_occurrences = CoincidencesOccurrences,
			 y = Y,
			 t = T,
			 classes = Classes,
			 prior_probabilities = PriorProbabilities,
			 pcw = PCW
		       }.


set_state (Data, State) ->
    LambdaMinus = State#output_node_state.lambda_minus,
    LambdaPlus = State#output_node_state.lambda_plus,
    Coincidences = State#output_node_state.coincidences,
    CoincidencesOccurrences = State#output_node_state.coincidences_occurrences,
    Y = State#output_node_state.y,
    T = State#output_node_state.t,    
    Classes = State#output_node_state.classes,
    PriorProbabilities = State#output_node_state.prior_probabilities,
    PCW = State#output_node_state.pcw,
    
    ets:insert (Data, [{lambda_minus, LambdaMinus},
		       {lambda_plus, LambdaPlus},
		       {coincidences, Coincidences},
		       {coincidences_occurrences, CoincidencesOccurrences},
		       {y, Y},
		       {t, T},
		       {classes, Classes},
		       {prior_probabilities, PriorProbabilities},
		       {pcw, PCW}]).
		   
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

compute_density_over_classes_test () ->
    Classes = [#class{name = cl1}, #class{name = cl2}],
    Y = [{c1, 0.5}, {c2, 1}],
    PCW = [{c1, cl1, 0.4},
    	   {c1, cl2, 1.0},
    	   {c2, cl1, 0.6}],
    
    Result = compute_density_over_classes (Classes, Y, PCW),
    
    ?assertEqual ([{cl1, 0.5 * 0.4 + 0.6}, {cl2, 0.5}], Result).

create_output_node_test () ->
    {Name, Layer} = {node1, 2},
    ProcessName = node:make_process_name (Layer, Name),
    Params = [ {name, Name},
	       {layer, Layer}],
    
    start_link (ProcessName, Params).

read_state_test () ->
    {Name, Layer} = {node2, 2},
    ProcessName = node:make_process_name (Layer, Name),
    Params = [ {name, Name},
	       {layer, Layer}],
    
    start_link (ProcessName, Params),
   
    State = node:read_state (ProcessName),
    
    ?assertEqual (State#output_node_state.lambda_minus, []),
    ?assertEqual (State#output_node_state.lambda_plus, []),
    ?assertEqual (State#output_node_state.coincidences, []),
    ?assertEqual (State#output_node_state.coincidences_occurrences, []),
    ?assertEqual (State#output_node_state.y, []),
    ?assertEqual (State#output_node_state.t, []),
    ?assertEqual (State#output_node_state.classes, []),
    ?assertEqual (State#output_node_state.prior_probabilities, []),
    ?assertEqual (State#output_node_state.pcw, []).

feed_test () ->
    {Name, Layer} = {node3, 2},
    Input = [{'layer1.node1', [{g1, 0.6}, {g2, 1.0}]},
	     {'layer1.node2', [{g1, 0.6}, {g2, 1.0}, {g3, 0.2}]}],    
    ProcessName = node:make_process_name (Layer, Name),
    Params = [ {name, Name},
	       {layer, Layer}],
    start_link (ProcessName, Params),

    node:feed (ProcessName, Input),
    State = node:read_state (ProcessName),
    
    ?assertEqual (State#output_node_state.lambda_minus, Input).

set_state_test () ->
    {Name, Layer} = {node4, 2},
    Input = [{'layer1.node1', [{g1, 0.6}, {g2, 1.0}]},
	     {'layer1.node2', [{g1, 0.6}, {g2, 1.0}, {g3, 0.2}]}],
    ProcessName = node:make_process_name (Layer, Name),
    Params = [ {name, Name},
	       {layer, Layer}],
    start_link (ProcessName, Params),
    node:set_state (ProcessName, #output_node_state {lambda_minus =  Input}),
    State = node:read_state (ProcessName),
    
    ?assertEqual (Input, State#output_node_state.lambda_minus).

%% !FIXME refactor, some code is duplicated between intermediate and entry
%% nodes
