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

%% !FIXME missing doc
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


%% !FIXME missing doc
make_temporal_group (FirstElement, OtherGroups) ->
    L = length (OtherGroups),
    Name = 
	erlang:list_to_atom ( lists:concat (["g", erlang:integer_to_list (L + 1)])),
    
    #temporal_group { name = Name, coincidences = [FirstElement]}.


%% !FIXME missing doc
growing_group (Group, TC, TAM) ->
    {Coincidences, NewTC} =
	growing_group (Group#temporal_group.coincidences,
		       TC,
		       TAM,
		       1,
		       []),
    
    {Group#temporal_group {coincidences = Coincidences}, NewTC}.


%% !FIXME missing doc
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


%% !FIXME missing doc
top_most_connected (Coincidence, TC, TAM) ->
    io:format ("TAM: ~p ~n", [TAM]),
    Neighbours = [ {{C1, C2}, Value} || {{C1, C2}, Value} <- TAM,
					C1 == Coincidence,
					proplists:is_defined (C2, TC) ],
    
    io:format ("~p~n", [Neighbours]),
    
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

