%%
%% layer_sup.erl
%%
-module(layer_sup).

-behaviour(supervisor).

%% API
-export([start_link/2,
	make_process_name/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
 %% !FIXME doc missing
start_link(ProcessName, LayerSpec) ->
    io:format("Supervisor starting... ~p ~n", [ProcessName]),
    supervisor:start_link({local, ProcessName}, ?MODULE, [LayerSpec]).

 %% !FIXME doc missing
make_process_name (LayerName) ->
    erlang:list_to_atom (erlang:integer_to_list(LayerName)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LayerSpec]) ->
    {LayerName, Nodes} = LayerSpec,
    RestartStrategy = {one_for_one, 5, 10},
    NodeProcesses =
	if length (Nodes) == 1 -> make_nodes (LayerSpec, [], output); %% output level
	   LayerName == 0   -> make_nodes (LayerSpec, [], entry); %% input level
	   true -> make_nodes (LayerSpec, [], intermediate) %% !FIXME not implemented
	end,

    {ok, {RestartStrategy, NodeProcesses}}.


make_nodes ({_LayerName, []}, Acc, _) -> Acc;

make_nodes ({LayerName, [{NodeName, NodeSpec}]}, _, output) ->
    %% !FIXME other node parameters could be passed
    io:format ("Creating child node... ~p ~n", [NodeName]),
    ProcName = node:make_process_name (LayerName, NodeName),
    Params = [{name, NodeName},
	      {layer, LayerName}],
    
    ProcessSpec = {ProcName, 
		    {output_node, start_link, [ProcName, Params]},
		    permanent, brutal_kill, worker, [output_node]},
    [ProcessSpec];


make_nodes ({LayerName, [{NodeName, NodeSpec}|Rest]}, Acc, entry) ->
    %% !FIXME other node parameters could be passed
    io:format ("Creating child node... ~p ~n", [NodeName]),
    ProcName = node:make_process_name (LayerName, NodeName),
    Params = [{name, NodeName},
	      {layer, LayerName},
	      {parent, proplists:get_value (parent, NodeSpec)},
	      {sigma, proplists:get_value (sigma, NodeSpec)}
	     ],
    
    ProcessSpec = {ProcName, 
		   {entry_node, start_link, [ProcName, Params]},
		   permanent, brutal_kill, worker, [entry_node]},
    make_nodes ({LayerName, Rest}, [ProcessSpec|Acc], entry).
