%%
%% temporal_pooler
%%

-module (temporal_pooler).

-export ([default_temporal_cluster/3,
	 growing_group/3]).

-include ("node.hrl").
-include_lib("eunit/include/eunit.hrl").

-define (TOPNEIGHBOURS, 3).
-define (MAX_GROUP_SIZE, 10).

%% -----------------------------------------------------------------------------
%% Func: default_temporal_cluster/2
%% @doc Implements the default temporal clustering algorithm.
%% Takes as input Temporal Connection (TC) vector and the Temporal Activation
%% Matrix (TAM) and returns a list of temporal groups.
%%
%% Parameters:
%%   TC :: [ { CoincidenceName :: atom (), Probability :: float () } ]
%%   TAM :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%            Occurrences :: integer () } ]
%%
%% Reply:
%%   TemporalGroups :: [#temporal_group ()]
%% -----------------------------------------------------------------------------
default_temporal_cluster (TC, TAM) ->
    default_temporal_cluster (TC, TAM, []).

default_temporal_cluster ([], _TAM, Groups) -> Groups;
default_temporal_cluster (TC, TAM, Groups) -> 
    [FirstTC | RestTC] = TC,
    {MostProbableCoincidence, _} = 
	lists:foldl (fun ({C, V}, {OldC, OldV}) ->
			     if V > OldV -> C;
				true -> OldC
			     end
		     end,
		     FirstTC,
		     TC),
    
    G = make_temporal_group (MostProbableCoincidence, Groups),
    {NewGroup, NewTC} = growing_group (G, 
				       lists:keydelete (MostProbableCoincidence, 
							1, 
							TC), 
				       TAM),
    
    default_temporal_cluster (NewTC, TAM, [NewGroup|Groups]).


%% -----------------------------------------------------------------------------
%% Func: make_temporal_group/2
%% @doc Makes a new group, and fills it with the Coincidence name passed as
%% first argument. The name of the group is determined by incrementing 
%% the number of groups passed as second argument.
%%
%% Parameters:
%%   FirstElement :: atom ()
%%   OtherGroups :: [#coindicence ()]
%%
%% Reply:
%%   NewGroup :: #temporal_group
%% -----------------------------------------------------------------------------
make_temporal_group (FirstElement, OtherGroups) ->
    L = length (OtherGroups),
    Name = 
	erlang:list_to_atom ( lists:concat (["g", erlang:integer_to_list (L + 1)])),
    
    #temporal_group { name = Name, coincidences = [FirstElement]}.


%% -----------------------------------------------------------------------------
%% Func: growing_group/3
%% @doc Given a temporal group, the Temporal Connection vector (TC) and the 
%% Temporal Activation Matrix (TAM), enlarge the group by adding coincidences
%% that are likely to occur close in time with respect to coincidences already
%% present in the group. Maximum number of allowed coincidences whithin a group
%% is defined via the MAX_GROUP_SIZE macro.
%% Returns the enlarged group plus a filtered TC vector, where entries related
%% to temporal groups that have been clustered have been deleted.
%%
%% Parameters:
%%   Group :: #temporal_group
%%   TC :: [ { CoincidenceName :: atom (), Probability :: float () } ]
%%   TAM :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%                Occurrences :: integer () } ]
%%
%% Reply:
%%   {EnlargedGroup :: #temporal_group,
%%    NeWTC :: [ { CoincidenceName :: atom (), Probability :: float () } ] }
%% -----------------------------------------------------------------------------
growing_group (Group, TC, TAM) ->
    {Coincidences, NewTC} =
	growing_group (Group#temporal_group.coincidences,
		       TC,
		       TAM,
		       1,
		       []),
    
    {Group#temporal_group {coincidences = Coincidences}, NewTC}.

growing_group (Coincidences, TC, _TAM, GroupSize, Accumulator) 
  when (Coincidences == []) or (GroupSize == ?MAX_GROUP_SIZE) ->
    io:format ("1Coincidences: ~p ~n TC: ~p ~n", [Coincidences, TC]),
    {lists:reverse(Accumulator), TC};

growing_group (Coincidences, TC, TAM, GroupSize, Acc) -> 
    io:format ("2Coincidences: ~p ~n TC: ~p ~n", [Coincidences, TC]),
    [First|Rest] = Coincidences,
    
    case lists:member (First, Acc) of true -> %% discard coincidences already taken
	    growing_group (Rest, TC, TAM, GroupSize, Acc);
	
	_ ->
	    NewTC = lists:keydelete (First, 1, TC),
	    io:format ("NewTC: ~p ~n", [NewTC]),
	    MostConnected = top_most_connected (First, NewTC, TAM),
	    io:format ("MostConnected: ~p ~n~n", [MostConnected]),
	    
	    growing_group (lists:append (Rest, MostConnected),
			   NewTC,
			   TAM, 
			   GroupSize + 1,
			   [First|Acc])
    end.


%% -----------------------------------------------------------------------------
%% Func: compute_PCG
%% @doc Given Coincidence names, their prior probabilities and the Temporal Groups,
%% computes the PCG matrix.
%%
%% Parameters:
%%   Coincidences :: [ atom () ]
%%   Priors :: [ { CoincidenceName :: atom (), PriorProbability :: float () } ]
%%   Groups :: [ #temporal_group ]
%%
%% Reply:
%%   PCG :: [ { { Coincidence :: atom (), Group :: atom ()},
%%                Probability :: float () } ]
%% -----------------------------------------------------------------------------
compute_PCG (Coincidences, Priors, Groups) ->
    PCGTemp = assign_priors (Coincidences, Priors, Groups),
    normalize_PCG (PCGTemp).

assign_priors (Coincidences, Priors, Groups) ->
    assign_priors (Coincidences, Groups, Priors, []).

assign_priors ([], _G, _P, PCG) -> PCG;
assign_priors ([Coincidence|Rest], Groups, Priors, Acc) -> 
    Prior = proplists:get_value (Coincidence, Priors),
    P = lists:foldl (fun (Group, A) ->
			     GroupCoincidences = Group#temporal_group.coincidences,
			     GroupName = Group#temporal_group.name,
			     case lists:member (Coincidence, GroupCoincidences) of
				 true -> [{{Coincidence, GroupName}, Prior} | A];
				 _ -> [{{Coincidence, GroupName}, 0} | A]
			     end
		     end,
		     [],
		     Groups),
    
    assign_priors (Rest, Groups, Priors, lists:append (P, Acc)).
    
				 
%% -----------------------------------------------------------------------------
%% Func: normalize_PCG
%% @doc Normalize the columns of a PCG matrix.
%%
%% Parameters:
%%   PCG :: [ { { Coincidence :: atom (), Group :: atom ()},
%%                Probability :: float () } ]
%%
%% Reply:
%%   NormalizedPCG :: [ { { Coincidence :: atom (), Group :: atom ()},
%%                          Probability :: float () } ]
%% -----------------------------------------------------------------------------
normalize_PCG (PCG) ->
    normalize_PCG (PCG, PCG, []).

normalize_PCG ([], PCG, NewPGC) -> NewPGC;
normalize_PCG ([Entry|Rest], PCG, Acc) -> 
    {{C1, C2}, Value} = Entry,
    Sum = sum_over_column (PCG, C2),
    NewValue = Value / Sum,
    normalize_PCG (Rest, PCG, [{{C1, C2}, NewValue} | Acc]).


%% -----------------------------------------------------------------------------
%% Func: sum_over_colum
%% @doc Given a matrix, returns the sum of the elements of a column.
%%
%% Parameters:
%%   Matrix :: [ { { Row :: atom (), Column :: atom ()},
%%                   Value :: float () } ]
%%
%% Reply:
%%   Sum :: float ()
%% -----------------------------------------------------------------------------
sum_over_column (M, Column) ->
    %% get all items from Row
    R = lists:filter (fun ({{C1, C2}, V}) ->
			      if C2 == Column -> true;
				 true -> false
			      end
		      end,
		      M),
    Values = [ V || {{C1, C2}, V} <- R],
    lists:sum (Values).			     


%% -----------------------------------------------------------------------------
%% Func: top_most_connected
%% @doc Given a coincidence, returns the three (or less) coincidences that are
%% most likely to occur close in time.
%%
%% Parameters:
%%   Coincidence :: atom ()
%%   TC :: [ { CoincidenceName :: atom (), Probability :: float () } ]
%%   TAM :: [ { { Coincidence1 :: atom (), Coincidence2 :: atom () },
%%                Occurrences :: integer () } ]
%%
%% Reply:
%%   TopMostConnectedList :: [Coincidence :: atom ()]
%% -----------------------------------------------------------------------------
top_most_connected (Coincidence, TC, TAM) ->
    Neighbours = [ {{C1, C2}, Value} || {{C1, C2}, Value} <- TAM,
					C1 == Coincidence,
					proplists:is_defined (C2, TC) ],
    
    OrderedN = 
	lists:sort (fun ({_, V1}, {_, V2}) ->
			    if V1 > V2 -> true;
			       true -> false
			    end
		    end,
		    Neighbours),
    
    case OrderedN of 
	[] -> [];
	[{{_, N1}, _}] -> [N1];
	[{{_, N1}, _}, {{_, N2}, _}] -> [N1, N2];
	[{{_, N1}, _}, {{_, N2}, _}, {{_, N3}, _} | Rest] -> [N1, N2, N3]
    end.
    

%% tests
top_most_connected_test () ->
    TAM = learning:expand_TAM([{{c1, c3}, 6/11}, {{c3, c1}, 1}, {{c1, c2}, 5/11}, {{c2, c1}, 1}],
			      [c1,c2,c3]),
    TC = [{c2, 5/11 * 0.1}, {c3, 6/11 * 0.1}],
    Coincidence = c1,
    
    Result = top_most_connected (Coincidence, TC, TAM),
    ?assertEqual ([c3, c2], Result).


growing_group1_test () ->
    TAM = learning:expand_TAM([{{c1, c3}, 6/11}, {{c3, c1}, 1}, {{c1, c2}, 5/11}, {{c2, c1}, 1}],
			      [c1,c2,c3]),
    TC = [{c1, 0.9}, {c2, 5/11 * 0.1}, {c3, 6/11 * 0.1}],
    
    Group = #temporal_group { name = g1,
			      coincidences = [c1]
			    },

    {NewGroup, NewTC} = growing_group (Group, TC, TAM),
    ?assertEqual ([], NewTC),
    ?assertEqual ([c1, c3, c2], NewGroup#temporal_group.coincidences). 


growing_group2_test () ->
    TAM = learning:expand_TAM([{{c1, c3}, 6/11}, {{c3, c1}, 1}, {{c1, c2}, 5/11}, {{c2, c1}, 1}],
			      [c1,c2,c3,c4,c5,c6,c7,c8, c9,c10,c11]),
    TC = [{c1, 0.9}, {c2, 5/11 * 0.1}, {c3, 6/11 * 0.1},
	  {c4, 0}, {c5, 0}, {c6, 0}, {c7, 0}, {c8, 0}, {c9, 0}, {c10,0}, {c11,0}],
    
    Group = #temporal_group { name = g1,
			      coincidences = [c1]
			    },

    {NewGroup, NewTC} = growing_group (Group, TC, TAM),
    ?assertEqual ([{c10, 0}, {c11,0}], NewTC),
    ?assertEqual ([c1, c3, c2, c4, c5, c6, c7, c8, c9], 
		  NewGroup#temporal_group.coincidences).


assign_priors_test () ->
    Coincidences = [c1, c2, c3],
    Priors = [{c1, 0.6}, {c2, 0.3}, {c3, 0.1}],
    Groups = [#temporal_group { name = g1,
				coincidences = [c1, c3]
			      },
	      #temporal_group { name = g2,
				coincidences = [c2]
			      }],
    
    Result = assign_priors (Coincidences, Priors, Groups),
        
    ?assertEqual (0.6, proplists:get_value ({c1, g1}, Result)),
    ?assertEqual (0, proplists:get_value ({c1, g2}, Result)),
    ?assertEqual (0, proplists:get_value ({c2, g1}, Result)),
    ?assertEqual (0.3, proplists:get_value ({c2, g2}, Result)),
    ?assertEqual (0.1, proplists:get_value ({c3, g1}, Result)),
    ?assertEqual (0, proplists:get_value ({c3, g2}, Result)).


normalize_PCG_test () ->
    Coincidences = [c1, c2, c3],
    Priors = [{c1, 0.6}, {c2, 0.3}, {c3, 0.1}],
    Groups = [#temporal_group { name = g1,
				coincidences = [c1, c3]
			      },
	      #temporal_group { name = g2,
				coincidences = [c2]
			      }],
    TempPCG = assign_priors (Coincidences, Priors, Groups),
    Result = normalize_PCG (TempPCG),
    
    ?assertEqual (0.6/0.7, proplists:get_value ({c1, g1}, Result)),
    ?assertEqual (0.0, proplists:get_value ({c1, g2}, Result)),
    ?assertEqual (0.0, proplists:get_value ({c2, g1}, Result)),
    ?assertEqual (1.0, proplists:get_value ({c2, g2}, Result)),
    ?assertEqual (0.1/0.7, proplists:get_value ({c3, g1}, Result)),
    ?assertEqual (0.0, proplists:get_value ({c3, g2}, Result)).    
