%%
%% node.erl
%%

-module (node).

-export ([ make_process_name/2,
	   feed/2 ]).

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
    atom_to_list ( lists:concat ([ list_to_atom (ProcessName), ".state" ])).


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
