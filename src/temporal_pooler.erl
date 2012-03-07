%%
%% temporal_pooler
%%

-module (temporal_pooler).

-export ([default_temporal_cluster/3]).

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
    {TC, Accumulator};

growing_group (Coincidences, TC, GroupSize, TAM, Acc) -> 
    [First|Rest] = Coincidences,
    MostConnected = top_most_connected (First, TAM),
    NewTC = lists:keydelete (First, 1, TC),
    
    growing_group (lists:append (Rest, MostConnected),
		   NewTC,
		   GroupSize + 1,
		   TAM,
		   [First|Acc]).


%% !FIXME missing doc
top_most_connected (Coincidence, TAM) ->
    Neighbours = lists:filter (fun ({{C1, C2}, Value}) ->
				       if C1 == Coincidence -> true;
					  true -> false
				       end
			       end,
			       TAM),
    
    OrderedN = lists:sort (fun ({_, V1}, {_, V2}) ->
				   if V1 > V2 -> false;
				      true -> true
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
