%%
%% node.erl
%%

-module (node).

-export ([ make_process_name/2,
	   make_ets_name/1,
	   get_upper_layer/1,	   
	   feed/2,
	   inference/1,
	   read_state/1,
	   set_state/2
	 ]).

%% -----------------------------------------------------------------------------
%% Func: make_process_name/2
%% @doc Given a node name and the layer it belongs to, returns the fully 
%% qualified name of its process, e.g. 'layer.node'.
%%
%% Parameters:
%%   LayerName :: integer ()
%%   NodeName :: atom ()
%%
%% Reply:
%%   ProcessName :: atom()
%% -----------------------------------------------------------------------------
make_process_name (LayerName, NodeName) ->
    list_to_atom ( lists:concat ([erlang:integer_to_list (LayerName),
				  ".",
				  erlang:atom_to_list (NodeName)])).


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
%%   LayerName :: integer ()
%%
%% Reply:
%%   upper_layer :: integer ()
%% -----------------------------------------------------------------------------
get_upper_layer (LayerName) ->  LayerName + 1.


%% -----------------------------------------------------------------------------
%% Func: feed/2
%% @doc Feed data to a node process via an async request.
%%
%% Parameters:
%%   ProcessName :: atom ()
%%   Data :: term ()
%% -----------------------------------------------------------------------------
feed (ProcessName, Data) ->
    gen_server:call (ProcessName, {feed, Data}).


%% -----------------------------------------------------------------------------
%% Func: inference
%% @doc Tells a node process to perform an inference step.
%%
%% Parameters:
%%   ProcessName :: atom ()
%% -----------------------------------------------------------------------------
inference (ProcessName) ->
    gen_server:cast (ProcessName, inference).


%% -----------------------------------------------------------------------------
%% Func: read_state
%% @doc Read the state of a node.
%%
%% Parameters:
%%   ProcessName :: atom ()
%%
%% Reply:
%%   State :: #entry_node_state () | #intermediate_node_state () | #output_node_state ()
%% -----------------------------------------------------------------------------
read_state (ProcessName) ->
    gen_server:call (ProcessName, read_state).

%% -----------------------------------------------------------------------------
%% Func: set_state
%% @doc Set the state of a node.
%%
%% Parameters:
%%   ProcessName :: atom ()
%%   State :: #entry_node_state () | #node_state () | #output_node_state ()
%% -----------------------------------------------------------------------------
set_state (ProcessName, State) ->
    gen_server:call (ProcessName, {set_state, State}).


