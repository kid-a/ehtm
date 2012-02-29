%%
%% node.erl
%%

-module (node).

-export ([ make_process_name/2,
	   make_ets_name/1,
	   get_upper_layer/1,	   
	   feed/2,
	   inference/1
	 ]).

%% -----------------------------------------------------------------------------
%% Func: make_process_name/2
%% @doc Given a node name and the layer it belongs to, returns the fully 
%% qualified name of its process, e.g. 'layer.node'.
%%
%% Parameters:
%%   LayerName :: string ()
%%   NodeName :: string ()
%%
%% Reply:
%%   ProcessName :: atom()
%% -----------------------------------------------------------------------------
make_process_name (LayerName, NodeName) ->
    list_to_atom ( lists:concat ([LayerName, ".", NodeName]) ).    


%% -----------------------------------------------------------------------------
%% Func: make_ets_name
%% @doc Given a process name, returns a fully qualified name for the
%% ETS table representing the process state.
%%
%% Parameters:
%%   ProcessName :: atom ()
%%
%% Reply:
%%   TableName :: atom ()
%% -----------------------------------------------------------------------------
make_ets_name (ProcessName) ->
    list_to_atom ( lists:concat ([ atom_to_list (ProcessName), ".state" ])).


%% -----------------------------------------------------------------------------
%% Func: get_upper_layer
%% @doc Given a layer name, returns the name of the layer immediately
%% higher level in the network hierarchy.
%%
%% Parameters:
%%   LayerName :: string ()
%%   NodeName :: string ()
%%
%% Reply:
%%   ProcessName :: atom()
%% -----------------------------------------------------------------------------
get_upper_layer (LayerName) -> 
    erlang:integer_to_list (erlang:list_to_integer (LayerName) + 1).


%% -----------------------------------------------------------------------------
%% Func: feed/2
%% @doc Feed data to a node process via an async request.
%%
%% Parameters:
%%   ProcessName :: atom ()
%%   Data :: term ()
%% -----------------------------------------------------------------------------
feed (ProcessName, Data) ->
    gen_server:cast (ProcessName, {feed, Data}).


%% -----------------------------------------------------------------------------
%% Func: inference
%% @doc Tells a node process to perform an inference step.
%%
%% Parameters:
%%   ProcessName :: atom ()
%% -----------------------------------------------------------------------------
inference (ProcessName) ->
    gen_server:cast (ProcessName, inference).
