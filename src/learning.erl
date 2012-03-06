%%
%% maltoni.erl
%%
-module (learning).

-export([train_entry_node/1,
	 train_intermediate_node/1,
	 train_output_node/2]).

-include ("node.hrl").
-include_lib("eunit/include/eunit.hrl").

-define (THRESHOLD, 10).
-define (WIDX_THRESHOLD(X), 30 / 100 * X).

train_entry_node (Node) ->
    State = node:read_state (Node),
    {C, Distance} = get_closest_coincidence (State#entry_node_state.lambda_minus,
					     State#entry_node_state.coincidences),
    
    NewState = 
	if Distance > ?THRESHOLD -> 
		NewCoincidence = make_coincidence (State#entry_node_state.coincidences,
						   State#entry_node_state.lambda_minus),
		NewSeen = update_seen ( add_coincidence_to_seen (
					  State#entry_node_state.seen,
					  NewCoincidence),
					NewCoincidence),

		%% !FIXME not taking into account temporal gap
		%% should update T here
		
		State#entry_node_state { 
		  coincidences = lists:append (State#entry_node_state.coincidences,
					       [NewCoincidence]),
		  seen = NewSeen,
		  last_seen = NewCoincidence#coincidence.name
		 };
	   
	   true ->
		NewSeen = update_seen (State#entry_node_state.seen, C),
		
		%% !FIXME not taking into account temporal gap
		%% should update T here

		State#entry_node_state {
		  seen = NewSeen,
		  last_seen = C#coincidence.name
		 }
	end,

    node:set_state (Node, NewState).


train_intermediate_node (Node) ->
    State = node:read_state (Node),    
    Widx = utils:extract_widx (State#intermediate_node_state.lambda_minus),
    {C, Distance} = get_closest_coincidence_to_widx (Widx,
						     State#intermediate_node_state.coincidences),

    NewState = 
	if Distance > ?WIDX_THRESHOLD(length (Widx)) ->
		NewCoincidence = make_coincidence (State#intermediate_node_state.coincidences,
						   Widx),
		NewSeen = update_seen ( add_coincidence_to_seen (
					  State#intermediate_node_state.seen,
					  NewCoincidence),
					NewCoincidence),
		
		%% !FIXME not taking into account temporal gap
		%% should update T here
		
		State#intermediate_node_state { 
		  coincidences = lists:append (State#intermediate_node_state.coincidences,
					       [NewCoincidence]),
		  seen = NewSeen,
		  last_seen = NewCoincidence#coincidence.name
		 };
	   
	   true ->
		NewSeen = update_seen (State#intermediate_node_state.seen, C),
		
		%% !FIXME not taking into account temporal gap
		%% should update T here
		
		State#entry_node_state {
		  seen = NewSeen,
		  last_seen = C#coincidence.name
		 }
	end,
    
    node:set_state (Node, NewState).
		

train_output_node (Node, Class) ->
    State = node:read_state (Node),    
    Widx = utils:extract_widx (State#output_node_state.lambda_minus),
    {C, Distance} = get_closest_coincidence_to_widx (Widx,
						     State#output_node_state.coincidences),
    NewState = 
	if Distance > ?WIDX_THRESHOLD(length (Widx)) ->
		NewCoincidence = make_coincidence (State#output_node_state.coincidences,
						   Widx),
		NewSeen = update_seen ( add_coincidence_to_seen (
					  State#output_node_state.seen,
					  NewCoincidence),
					NewCoincidence),

		NewPCW = update_pcw (State#output_node_state.pcw, 
				     NewCoincidence,
				     Class),

		%% !FIXME not taking into account temporal gap
		%% should update T here

		State#output_node_state { 
		  coincidences = lists:append (State#output_node_state.coincidences,
					       [NewCoincidence]),
		  seen = NewSeen,
		  last_seen = NewCoincidence#coincidence.name,
		  pcw = NewPCW
		 };

	   true ->
		NewSeen = update_seen (State#output_node_state.seen, C),
		NewPCW = update_pcw (State#output_node_state.pcw, C, Class),
		
		%% !FIXME not taking into account temporal gap
		%% should update T here
		
		State#output_node_state {
		  seen = NewSeen,
		  last_seen = C#coincidence.name,
		  pcw = NewPCW
		 }
	end,

    node:set_state (Node, NewState).


%% -----------------------------------------------------------------------------
%% Func: get_closest_coincidence_to_widx
%% @doc Get the closest coindicence to a given widx vector. Each coincidence is
%% matched against the widx vector and the one that differs the least is returned.
%%
%% Parameters:
%%   Coincidences :: [#coincidence]
%%   Widx :: [ {child :: atom (), winning_temporal_group :: atom () } ]
%%
%% Reply:
%%   Coincidence :: #coincidence 
%% -----------------------------------------------------------------------------
get_closest_coincidence_to_widx (Widx, Coincidences) ->
    [Coincidence|Rest] = Coincidences,
    Distance =  compute_distance (Widx, Coincidence),
    get_closest_coincidence_to_widx (Widx, Rest, {Coincidence, Distance}).

get_closest_coincidence_to_widx (Widx, [], {C,_}) -> C;

get_closest_coincidence_to_widx (Widx, [Coincidence|Rest], {OldC, OldD}) ->
    Distance = compute_distance (Widx, Coincidence),
    if Distance < OldD -> 
	    get_closest_coincidence_to_widx (Widx, Rest, {Coincidence,
							  Distance});
       true ->
	    get_closest_coincidence_to_widx (Widx, Rest, {OldC, OldD})
    end.

compute_distance (Widx, Coincidence) ->
    compute_distance (Widx, Coincidence, 0).

compute_distance ([], _Coincidence, Acc) -> Acc;

compute_distance ([{Child, WinningGroup}|Rest], Coincidence, Acc) ->
    StoredTemporalGroup = proplists:get_value (Child, Coincidence#coincidence.data, nil),
    
    if StoredTemporalGroup == WinningGroup ->
	    compute_distance (Rest, Coincidence, Acc);
       StoredTemporalGroup == nil -> 
	    %% !FIXME the coincidence was made when the 
	    %% temporal group was not defined yet,
	    %% suppose the pattern is different ?
	    compute_distance (Rest, Coincidence, Acc + 1);
       true ->
	    compute_distance (Rest, Coincidence, Acc)
    end.
	    
%% !FIXME missing doc
get_closest_coincidence (LambdaMinus, Coincidences) ->
    [First|Rest] = Coincidences,
    Distance = utils:norm (LambdaMinus#entry_node_input.binary_data,
			   (First#coincidence.data)#entry_node_input.binary_data,
			   LambdaMinus#entry_node_input.chunk_size),
    
    get_closest_coincidence (LambdaMinus, Rest, 
			     {First#coincidence.name, Distance}).

get_closest_coincidence (_LambdaMinus, [], Distance) -> Distance;
get_closest_coincidence (LambdaMinus, [First|Rest], {Name, Distance}) -> 
    NewDistance = utils:norm (LambdaMinus#entry_node_input.binary_data,
			      (First#coincidence.data)#entry_node_input.binary_data,
			      LambdaMinus#entry_node_input.chunk_size),
    
    if NewDistance =< Distance ->
	    get_closest_coincidence (LambdaMinus, Rest, 
				     {First#coincidence.name, NewDistance});
       true ->
	    get_closest_coincidence (LambdaMinus, Rest, 
				     {Name, Distance})
    end.

%% !FIXME missing doc
make_coincidence (Coincidences, LambdaMinus) ->
    L = length (Coincidences),
    Name = list_to_atom (lists:concat (["c", erlang:integer_to_list (L + 1)])),
    Data = LambdaMinus,
    #coincidence { name = Name, data = Data}.

%% !FIXME missing doc
add_coincidence_to_seen (Seen, Coincidence) ->
    CoincidenceName = Coincidence#coincidence.name,
    lists:append (Seen, [{CoincidenceName, 0}]).

%% !FIXME missing doc
update_seen (Seen, Coincidence) ->
    CoincidenceName = Coincidence#coincidence.name,
    CoincidenceSeen = proplists:get_value (CoincidenceName, Seen),
    lists:keyreplace (CoincidenceName, 1, Seen,
		      {CoincidenceName, CoincidenceSeen + 1}).

%% !FIXME missing doc
update_pcw (PCW, Coincidence, Class) ->
    Key = {Class, Coincidence#coincidence.name},
    Value = proplists:get_value (Key, PCW, 0),
    case Value of 0 ->
	    lists:append (PCW, [{Key, Value + 1}]);
	_ ->
	    lists:keyreplace (Key, 1, PCW, {Key, Value + 1})
    end.




update_temporal_activation_matrix () -> ok.
%% tests 
