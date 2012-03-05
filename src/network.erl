%%
%% network.erl
%%

-module (network).

-behaviour (gen_server). %% what about a gen_event?

-export ([ make_process_name/1,
	   start_link/2,
	   feed/2,
	   inference/1,
	   read_output/1
	 ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (state, 
	 {
	   name,
	   layers
	 }).

%% -----------------------------------------------------------------------------
%% Func: make_process_name/1
%% @doc Given the name of the network, returns a process name for it.
%%
%% Parameters:
%%   NetworkName :: string ()
%%
%% Reply:
%%   ProcessName :: atom()
%% -----------------------------------------------------------------------------
make_process_name (NetworkName) ->
    list_to_atom (NetworkName).


%% -----------------------------------------------------------------------------
%% Func: start_link/2
%% @doc Starts the network process.
%%
%% Parameters: 
%%  ProcessName :: atom ()
%%  Params :: ??
%% -----------------------------------------------------------------------------
start_link(ProcessName, Params) ->
    gen_server:start_link({local, ProcessName}, ?MODULE, [Params], []).

feed (ProcessName, Input) ->
    gen_server:cast (ProcessName, {feed, Input}).

inference (ProcessName) ->
    gen_server:cast (ProcessName, inference).

read_output (ProcessName) ->
    gen_server:call (ProcessName, read_output).


%% -----------------------------------------------------------------------------
%% Func: init/1
%% @doc Starts an entry_node process.
%%
%% Parameters:
%%   Params :: ??
%% -----------------------------------------------------------------------------
init([Params]) ->
    Name = proplists:get_value (name, Params),
    Layers = proplists:get_value (layers, Params),			   
    State = #state { name = Name, layers = Layers},
    {ok, State}.

%% !FIXME to be implemented
handle_call (read_output, _From, State) ->
    Reply = nil,
    {reply, Reply, State};

handle_call (_Request, _From, State) ->
    Reply = nil,
    {reply, Reply, State}.

%% !FIXME to be implemented
handle_cast({feed, Input}, State) ->
    io:format ("Feeding Input ~p ~n to Network~n",[Input]),
    InputLayer = proplists:get_value (0, State#state.layers),
    feed_input (Input, InputLayer),
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format ("Received unknown message.~n"),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%% ancillary functions
%% -----------------------------------------------------------------------------
feed_input (Input, InputLayer) ->
    {InputData, InputSize, ChunkSize} = Input,
    NumerOfNodes = length (InputLayer),
    {FirstNodeRF, RF} = make_receptive_field_size (InputSize, NumerOfNodes, ChunkSize),
    
    <<E1:FirstNodeRF, 
      RestOfData/binary >> = InputData,

    [{FirstNodeName, _} | RestOfNodes] = InputLayer,
    node:feed (node:make_process_name (0, FirstNodeName), E1),
    feed_input (RestOfData, RestOfNodes, RF).

feed_input (<<>>, [], RF) -> 
    io:format ("~n"),
    ok;

feed_input (Data, [{NodeName, _}|RestOfNodes], RF) ->
    io:format ("..."),
    <<Chunk:RF, RestOfData/binary >> = Data,
    node:feed (node:make_process_name (0, NodeName), Chunk),
    feed_input (RestOfData, RestOfNodes, RF).
    


%%inference_step (Layers) ->
    
    


%% -----------------------------------------------------------------------------
%% Func: make_receptive_field_size/2
%% @doc Given the number of components of the input and the number of nodes 
%% of the input layer, computes the size of the receptive field of each 
%% input node.
%%
%% Parameters:
%%   InputSize :: integer ()
%%   NumberOfNodes :: integer ()
%%
%% Reply:
%%   {Size :: integer ()
%%    Rest :: integer ()}
%% -----------------------------------------------------------------------------
make_receptive_field_size (InputSize, NumberOfNodes, ChunkSize) ->
    Size = InputSize div NumberOfNodes,
    Rest = InputSize - (Size * NumberOfNodes),
    {(Size + Rest) * ChunkSize, Size * ChunkSize}.
