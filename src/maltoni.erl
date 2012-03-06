%%
%% maltoni.erl
%%
-module (maltoni).

-export([train_entry_node/1,
	 train_intermediate_node/1,
	 train_output_node/1]).

-include ("node.hrl").

-define (THRESHOLD, 10).

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

    node:set_state (NewState).


make_coincidence (Coincidences, LambdaMinus) ->
    L = length (Coincidences),
    Name = list_to_atom (lists:concat (["c", erlang:integer_to_list (L + 1)])),
    Data = LambdaMinus,
    #coincidence { name = Name, data = Data}.

add_coincidence_to_seen (Seen, Coincidence) ->
    CoincidenceName = Coincidence#coincidence.name,
    lists:append (Seen, [{CoincidenceName, 0}]).

update_seen (Seen, Coincidence) ->
    CoincidenceName = Coincidence#coincidence.name,
    CoincidenceSeen = proplists:get_value (CoincidenceName, Seen),
    lists:keyreplace (CoincidenceName, 1, Seen,
		      {CoincidenceName, CoincidenceSeen + 1}).


update_temporal_activation_matrix () -> ok.

%% !FIXME the above functions are the same for all nodes, duplicated code?
%% use the is_record predicate

%% !FIXME to implement
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

train_intermediate_node (Node) ->
    ok.

train_output_node (Node) ->
    ok.
