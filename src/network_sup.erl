
-module(network_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    %% !FIXME conffile hardcoded here
    ConfFile = utils:configuration_file (),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ConfFile]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ConfFile]) ->
    io:format ("Starting Network Supervisor...~n"),
    RestartStrategy = {one_for_one, 5, 10},
    NetworkDescr = utils:read_network_structure (ConfFile),
    NetworkStructure = proplists:get_value (layers, NetworkDescr),
    LayersSupervisors = make_supervision_tree (NetworkStructure, []),
    {ok, {RestartStrategy, LayersSupervisors}}.
    

%% -------------------------------------------------------------------
make_supervision_tree ([], Acc) -> Acc;
%%
make_supervision_tree ([Layer | Rest], Acc) ->
    {LayerName, _} = Layer,
    ProcName = layer_sup:make_process_name (LayerName),
    LayerSupervisor = {ProcName,
		       {layer_sup, start_link, [ProcName, Layer]},
		       permanent, brutal_kill, supervisor, [layer_sup]},
    make_supervision_tree (Rest, [LayerSupervisor | Acc]).


