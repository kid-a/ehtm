-module(entry_node).

-behaviour(gen_server).

-export([start_link/0, say_hello/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include ("node.hrl").

-record (state, { 
	   name,
	   parent,
	   data
	  }).

-define (DEF_SIGMA, 1.0).

start_link() ->
    %% !FIXME make process name
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Params) ->
    NodeName = proplists:get_value (name, Params),
    LayerName = proplists:get_value (layer, Params),
    ProcessName = utils:make_process_name (LayerName, NodeName),
    EtsTableName = utils:make_ets_name (ProcessName),
    
    %% create a table for process data
    ets:new (EtsTableName, [set,
			    named_table,
			    protected, %% other processes can read
			    {read_concurrency, true}
			   ]),

    %% initialize some parameters
    Sigma = proplists:get_value (sigma, Params, ?DEF_SIGMA),
    ets:insert (EtsTableName, {sigma, Sigma}),   
    
    %% initialize the process state
    ParentName = proplists:get_value (parent, Params),
    UpperLayerName = utils:get_upper_layer (LayerName),
    ParentProcessName = utils:make_process_name (UpperLayerName, ParentName),
    State = #state {
      name = ProcessName,
      parent = ParentProcessName,
      data = EtsTableName
     },
    {ok, [State]}.

say_hello() ->
    gen_server:call(?MODULE, hello).

%% callbacks
handle_call(hello, _From, State) ->
    io:format("Hello from server!~n", []),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ancillary functions

