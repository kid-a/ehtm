%%
%% learning.erl
%%
-module (learning).

-export([train_entry_node/1,
	 train_intermediate_node/1,
	 train_output_node/2,
	 expand_TAM/2
	]).

-include ("node.hrl").
-include_lib("eunit/include/eunit.hrl").

-define (THRESHOLD, 10).
-define (WIDX_THRESHOLD(X), 30 / 100 * X).


%% -----------------------------------------------------------------------------
%% Func: train_entry_node/1
%% @doc Train an entry node.
%%
%% Parameters:
%%   Node :: atom ()
%% -----------------------------------------------------------------------------
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

    %% !FIXME forget about rare coincidences here
    %% !FIXME compute coincidence priors here

    node:set_state (Node, NewState).


%% -----------------------------------------------------------------------------
%% Func: train_intermediate_node/1
%% @doc Train an intermediate node.
%%
%% Parameters:
%%   Node :: atom ()
%% -----------------------------------------------------------------------------
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

    %% !FIXME forget about rare coincidences here
    %% !FIXME compute coincidence priors here
    
    node:set_state (Node, NewState).
		

%% -----------------------------------------------------------------------------
%% Func: train_output_node/2
%% @doc Train an outputnode over a set of classes.
%%
%% Parameters:
%%   Node :: atom ()
%% -----------------------------------------------------------------------------
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

    %% !FIXME forget about rare coincidences here
    %% !FIXME compute coincidence priors here

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


%% -----------------------------------------------------------------------------
%% Func: make_coincidence/2
%% @doc Make a new coindicence out of the LambdaMinus passed as argument.
%% The name of the new coindicence is determined by incrementing the 
%% the size of the coincidences list passed as first argument.
%%
%% Parameters:
%%   Coincidences :: [#coincidence]
%%   LambdaMinus :: term ()
%%
%% Reply:
%%   Coincidence :: #coincidence
%% -----------------------------------------------------------------------------
make_coincidence (Coincidences, LambdaMinus) ->
    L = length (Coincidences),
    Name = list_to_atom (lists:concat (["c", erlang:integer_to_list (L + 1)])),
    Data = LambdaMinus,
    #coincidence { name = Name, data = Data}.


%% -----------------------------------------------------------------------------
%% Func: add_coincidence_to_seen/2
%% @doc Given a Seen vector, add a new entry concerning the coincidence
%% passed as second parameter.
%%
%% Parameters:
%%   Seen :: [ { Coincidence :: atom (), Times :: integer () } ]
%%   Coincidence :: #coindicence
%%
%% Reply:
%%   NewSeen :: [ { Coincidence :: atom (), Times :: integer () } ]
%% -----------------------------------------------------------------------------
add_coincidence_to_seen (Seen, Coincidence) ->
    CoincidenceName = Coincidence#coincidence.name,
    lists:append (Seen, [{CoincidenceName, 0}]).


%% -----------------------------------------------------------------------------
%% Func: update_seen/2
%% @doc Given a Seen vector, increment the counter for the coincidence passed
%% as a second argument.
%%
%% Parameters:
%%   Seen :: [ { Coincidence :: atom (), Times :: integer () } ]
%%   Coincidence :: #coindicence
%%
%% Reply:
%%   NewSeen :: [ { Coincidence :: atom (), Times :: integer () } ]
%% -----------------------------------------------------------------------------
update_seen (Seen, Coincidence) ->
    CoincidenceName = Coincidence#coincidence.name,
    CoincidenceSeen = proplists:get_value (CoincidenceName, Seen),
    lists:keyreplace (CoincidenceName, 1, Seen,
		      {CoincidenceName, CoincidenceSeen + 1}).



%% -----------------------------------------------------------------------------
%% Func: update_pcw/3
%% @doc Updates the PCW matrix, given that Coincidence has been active in the 
%% context of Class.
%%
%% Parameters:
%%   PCW :: [ { { class_name :: atom (), 
%%                coincidence_name :: atom (), } 
%%            probability :: float () } ]
%%   Coincidence :: #coindicence
%%   Class :: atom ()
%%
%% Reply:
%%   NewPCW :: [ { { class_name :: atom (), 
%%                coincidence_name :: atom (), } 
%%            probability :: float () } ]
%% -----------------------------------------------------------------------------
update_pcw (PCW, Coincidence, Class) ->
    Key = {Class, Coincidence#coincidence.name},
    Value = proplists:get_value (Key, PCW, 0),
    case Value of 0 ->
	    lists:append (PCW, [{Key, Value + 1}]);
	_ ->
	    lists:keyreplace (Key, 1, PCW, {Key, Value + 1})
    end.


%% -----------------------------------------------------------------------------
%% Func: compute_coincidence_priors/2
%% @doc Computes the a priori probability of the occurrence of each Coincidence
%% passed as first argument.
%%
%% Parameters:
%%   Coincidences :: [ #coindicence ] 
%%   Seen :: [ { Coincidence :: atom (), Times :: integer () } ]
%%
%% Reply:
%%   Priors :: [ { Coincidence :: atom (), PriorProbability :: float () } ]
%% -----------------------------------------------------------------------------
compute_coincidence_priors (Coincidences, Seen) ->
    SeenList = lists:map (fun ({_, O}) -> O end, Seen),
    TotalSeen = list:sum (SeenList),
    compute_coincidence_priors (Coincidences, Seen, TotalSeen,[]).

compute_coincidence_priors ([], _S, _T, Acc) -> lists:reverse (Acc);
compute_coincidence_priors ([Concidence|Rest], Seen, TotalSeen, Acc) ->
    Key = Concidence#coincidence.name,
    SeenC = proplists:get_value (Key, Seen),
    Probability = {Key, SeenC / TotalSeen}, %% according to eq. 7
    compute_coincidence_priors (Rest, Seen, TotalSeen, [Probability|Acc]).


%% -----------------------------------------------------------------------------
%% Func: compute_class_priors/2
%% @doc Computes the a priori probability of the occurrence of a pattern belonging
%% to a certain class.
%%
%% Parameters:
%%   Classes :: [ #class () ]
%%   PCW :: [ { { class_name :: atom (), 
%%                coincidence_name :: atom (), } 
%%            probability :: float () } ] 
%%
%% Reply:
%%   Priors :: [ { Class :: atom (), PriorProbability :: float () } ]
%% -----------------------------------------------------------------------------
compute_class_priors (Classes, PCW) ->
    Sum = lists:sum (lists:map (fun ({{_,_} V}) -> V end, PCW)),
    compute_class_priors (Classes, PCW, Sum, []).

compute_class_priors ([], _PCW, Sum, Priors ) -> Priors;
compute_class_priors ([Class|Rest], PCW, Sum, Acc) ->
    P = temporal_pooler:sum_over_column (PCW, Class) / Sum,
    compute_class_priors (Rest, PCW, Sum [{Class, P} | Acc]).


%% -----------------------------------------------------------------------------
%% Func: make_symmetric/1
%% @doc Given a temporal activation matrix, makes it symmetric.
%%
%% Parameters:
%%   T :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%              Occurrences :: integer () } ]
%%
%% Reply:
%%   SymmetricT :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%                    Occurrences :: integer () } ]
%% -----------------------------------------------------------------------------
make_symmetric (T) ->
    lists:foldl (fun ({{C1, C2}, Value1}, Acc) ->
			 Value2 = proplists:get_value ({C2, C1}, T, undefined),
			 if Value2 == undefined ->
				 S = Value1 + 0,
				 [{{C1, C2}, S}, {{C2, C1}, S} | Acc];
			    true -> 
				 S = Value1 + Value2,
				 [{{C1, C2}, S} | Acc]
			 end
		 end,
		 [],
		 T).


%% -----------------------------------------------------------------------------
%% Func: normalize_ovew_rows/1
%% @doc Given a temporal activation matrix, normalizes it over rows.
%%
%% Parameters:
%%   T :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%          Occurrences :: integer () } ]
%%
%% Reply:
%%   NewT :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%             Occurrences :: integer () } ]
%% -----------------------------------------------------------------------------
normalize_over_rows (T) ->
    normalize_over_rows (T, T, []).

normalize_over_rows ([], _M, Acc) -> Acc;
normalize_over_rows  (T, Matrix, Acc) ->
    [{{C1, C2}, Value} | Rest] = T,

    RowSum = sum_over_row (Matrix, C1),
    P = Value / RowSum,

    normalize_over_rows (Rest, Matrix, [{{C1, C2}, P} | Acc]).


%% -----------------------------------------------------------------------------
%% Func: sum_over_row
%% @doc Given a matrix, returns the sum of the element of a given row.
%%
%% Parameters:
%%   T :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%          Occurrences :: integer () } ]
%%
%% Reply:
%%   Sum :: integer ()
%% -----------------------------------------------------------------------------
sum_over_row (T, Row) ->
    %% get all items from Row
    R = lists:filter (fun ({{C1, C2}, V}) ->
			      if C1 == Row -> true;
				 true -> false
			      end
		      end,
		      T),
    Values = [ V || {{C1, C2}, V} <- R],
    lists:sum (Values).
    

%% -----------------------------------------------------------------------------
%% Func: compute_temporal_connections/3
%% @doc Compute the Temporal Connections vector.
%%
%% Parameters:
%%   Coincidences :: [ #coincidence ]
%%   CoincidencePriors :: [ {Coincidence :: atom (), Probability :: float () } ]
%%   TAM :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%                Occurrences :: integer () } ]
%%
%% Reply:
%%   TC :: [ { Coincidence :: atom (), Probability :: atom () } ]
%% -----------------------------------------------------------------------------
compute_temporal_connections (Coincidences, CoincidencePriors, TAM) ->
    compute_temporal_connections (Coincidences, CoincidencePriors, TAM, []).

compute_temporal_connections ([], _CP, _TAM, Acc) -> Acc;
compute_temporal_connections ([Coincidence|Rest], CoincidencePriors, TAM , Acc) -> 
    Name = Coincidence#coincidence.name,

    Column = lists:filter (fun ({{C1, C2}, _}) ->
				   if C2 == Name -> true;
				      true -> false
				   end
			   end,
			   TAM),
    
    TemporalConnection = { Name, 
			   lists:foldl ( 
			     fun ({{C1, C2}, V}, Acc) ->
				     Prior = 
					 proplists:get_value (C1, CoincidencePriors, 0),
				     
				     Acc +  Prior * V
			     end,
			     0,
			     Column) },
    
    compute_temporal_connections (Rest, CoincidencePriors, 
				  TAM, [TemporalConnection|Acc]).


%% -----------------------------------------------------------------------------
%% Func: expand_TAM/2
%% @doc Expand the TAM matrix, filling it with zeros in empty places.
%%
%% Parameters:
%%   TAM :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%            Occurrences :: integer () } ]
%%   Coincidences :: [ #coincidence ]
%%
%% Reply:
%%   NewTAM :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%                Occurrences :: integer () } ]
%% -----------------------------------------------------------------------------
expand_TAM (TAM, Coincidences) ->    
    lists:append ([TAM, expand_TAM(TAM, Coincidences, Coincidences, [])]).

expand_TAM (_TAM, [], _C, Acc) -> Acc;
expand_TAM (TAM, [Coincidence|Rest], Coincidences, Acc) ->
    R = lists:foldl (fun (OtherCoincidence, A) ->			     
			     case proplists:is_defined ({Coincidence, OtherCoincidence}, 
							lists:append (TAM, Acc)) of
				 true -> A;
				 _ ->
				     if Coincidence == OtherCoincidence ->
					     [{{Coincidence, OtherCoincidence}, 0}| A];
					true ->
					     [{{Coincidence, OtherCoincidence}, 0},
					      {{OtherCoincidence, Coincidence}, 0} | A]
				     end
			     end
		     end,
		     [],
		     Coincidences),
    
    expand_TAM (TAM, Rest, Coincidences, 
		lists:append ([R, Acc])).
    
    
%% tests 
make_symmetric_test () ->
    A = [{{c1, c2}, 5}, {{c1, c3}, 6}],
    B = [{{c1, c3}, 6}, {{c3, c1}, 6}, {{c1, c2}, 5}, {{c2, c1}, 5}],

    A1 = [{{c1, c2}, 5}, {{c2, c1}, 6}, {{c1, c3}, 8}],
    B1 = [{{c1, c3}, 8}, {{c3, c1}, 8}, {{c2, c1}, 11}, {{c1, c2}, 11}],

    ?assertEqual (B, make_symmetric (A)),
    ?assertEqual (B1, make_symmetric (A1)).

sum_over_rows_test () ->
    A = [{{c1, c3}, 6}, {{c3, c1}, 6}, {{c1, c2}, 5}, {{c2, c1}, 5}],
    A1 = [{{c1, c3}, 8}, {{c3, c1}, 8}, {{c2, c1}, 11}, {{c1, c2}, 11}],

    ?assertEqual (11, sum_over_row (A, c1)),
    ?assertEqual (6, sum_over_row (A, c3)),
    ?assertEqual (5, sum_over_row (A, c2)),

    ?assertEqual (19, sum_over_row (A1, c1)),
    ?assertEqual (8, sum_over_row (A1, c3)),
    ?assertEqual (11, sum_over_row (A1, c2)).


normalize_over_row_test () ->
    A = [{{c1, c3}, 6}, {{c3, c1}, 6}, {{c1, c2}, 5}, {{c2, c1}, 5}],
    B = normalize_over_rows (A),
    

    ?assertEqual (1.0, proplists:get_value ({c2, c1}, B)),
    
    ?assertEqual (proplists:get_value ({c1, c2}, A) / sum_over_row (A, c1), 
		  proplists:get_value ({c1, c2}, B)),

    ?assertEqual (proplists:get_value ({c1, c3}, A) / sum_over_row (A, c1), 
		  proplists:get_value ({c1, c3}, B)),

    %% ?assertEqual (8/19, proplists:get_value ({c1, c3}, B)),
    ?assertEqual (1.0, proplists:get_value ({c3, c1}, B)).


compute_temporal_connections_test () ->
    C1 = #coincidence {name = c1},
    C2 = #coincidence {name = c2},
    C3 = #coincidence {name = c3},
   
    C1Prior = {c1, 0.1},
    C2Prior = {c2, 0.6},
    C3Prior = {c3, 0.3},
    
    TAM = [{{c1, c3}, 6/11}, {{c3, c1}, 1}, {{c1, c2}, 5/11}, {{c2, c1}, 1}],
    
    TC = compute_temporal_connections ([C1,C2,C3], [C1Prior, C2Prior, C3Prior],
				       TAM),
    
    ?assertEqual (0.6 + 0.3 , proplists:get_value (c1, TC)),
    ?assertEqual (5/11 * 0.1 , proplists:get_value (c2, TC)),
    ?assertEqual (6/11 * 0.1 , proplists:get_value (c3, TC)).

expand_TAM_test () ->
    TAM = [{{c1, c3}, 6/11}, {{c3, c1}, 1}, {{c1, c2}, 5/11}, {{c2, c1}, 1}],
    NewTAM = expand_TAM (TAM, [c1, c2, c3]),
    
    ?assertEqual (5/11 , proplists:get_value ({c1, c2}, NewTAM)),
    ?assertEqual (6/11 , proplists:get_value ({c1, c3}, NewTAM)),
    ?assertEqual (1 , proplists:get_value ({c2, c1}, NewTAM)),
    ?assertEqual (0 , proplists:get_value ({c2, c3}, NewTAM)),
    ?assertEqual (1 , proplists:get_value ({c3, c1}, NewTAM)),
    ?assertEqual (0 , proplists:get_value ({c3, c2}, NewTAM)),
    ?assertEqual (9, length (NewTAM)).
    
%% !FIXME to be implemented
compute_class_prior_test () ->
    ok.
