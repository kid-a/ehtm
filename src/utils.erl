-module (utils).

-export ([
	  get_upper_layer/1,
	  make_process_name/2,
	  make_ets_name/1
	 ]).

-include ("node.hrl").


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
    integer_to_list (list_to_integer (LayerName) + 1).


%% -----------------------------------------------------------------------------
%% Func: make_process_name/1
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

%% %% -----------------------------------------------------------------------------
%% %% Func: make_datafield_name
%% %% @doc Given a process name and the name of one of its data fields, returns
%% %% the fully qualified name of the data field, e.g. 'process.data'.
%% %%
%% %% Parameters:
%% %%   ProcessName :: atom ()
%% %%   FieldName   :: atom ()
%% %%
%% %% Reply:
%% %%   FQName :: atom ()
%% %% -----------------------------------------------------------------------------
%% make_datafield_name (ProcessName, FieldName ->
%%     list_to_atom ( lists:concat ([ atom_to_list (ProcessName), ".", 
%% 				   atom_to_list (FieldName) ])).


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
