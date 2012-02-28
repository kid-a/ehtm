-module(entry_node).

-behaviour(gen_server).

-export([start_link/0, say_hello/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (state,
	 {
	   lambda_minus = [],       %% input vector (a proplist?)
	   sigma = 1.0,             %% the sigma param
	   y = [],                  %% y vector (a proplist?)
	   lambda_plus = [],        %% output vector (a proplist?)
	   
	   coincidence_occurrences, %% k_seen
	   last_active_coincidence, %% k
	   coincidences,            %% list of coincidences
	   t,                       %% coincidences transition matrix (an ets table?)
	   temporal_groups,         %% the temporal groups
	   pcg                      %% the PCG matrix (an ets table?)
	 }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = #state {},
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
