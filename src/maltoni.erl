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
    {C, Distance} = get_closest_coincidence (State),
    
    NewState = 
	if Distance > ?THRESHOLD -> 
		NewCoincidence = make_coincidence (State, entry_node),
		S1 = add_new_coincidence (NewCoincidence, State, entry_node),
		seen (NewCoincidence, S1, entry_node);
	   true ->
		seen (C, State, entry_node)
	end,
    
    %% !FIXME not taking into account temporal gap
    %% should update T here
    node:set_state (NewState).


make_coincidence (State, entry_node) ->
    NumberOfCoincidences = length (State#entry_node_state.coincidences),
    Name = list_to_atom (lists:concat (["c", 
					erlang:integer_to_list( 
					  length (NumberOfCoincidences) + 1)])),
    Coincidence = State#entry_node_state.lambda_minus,
    
    #coincidence { name = Name, data = Coincidence}.


add_new_coincidence (Coincidence, State, entry_node) ->
    CoincidenceName = Coincidence#coincidence.name,
    Coincidences = State#entry_node_state.coincidences,
    Occurrences = State#entry_node_state.coincidences_occurrences,
    
    State#entry_node_state 
	{ coincidences = lists:append (Coincidences, [CoincidenceName]),
	  coincidences_occurrences = lists:append (Occurrences, [{CoincidenceName, 0}]) }.


seen (Coincidence, State, entry_node) ->
    CoincidenceName = Coincidence#coincidence.name,
    ListOfOccurrences = State#entry_node_state.coincidences_occurrences,
    Occurrences = proplists:get_value (CoincidenceName, ListOfOccurrences),
    NewListOfOccurrences = lists:keyreplace (CoincidenceName, 1, 
					     ListOfOccurrences, 
					     {CoincidenceName, Occurrences + 1}),
    
    State#entry_node_state { coincidences_occurrences = NewListOfOccurrences }.		

%% !FIXME the above functions are the same for all nodes, duplicated code?
%% use the is_record predicate

%% !FIXME to implement
get_closest_coincidence (State) ->
    {nil, nil}.

train_intermediate_node (Node) ->
    ok.

train_output_node (Node) ->
    ok.
