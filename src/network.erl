%%
%% network.erl
%%

-module (network).

-behaviour (gen_server). %% what about a gen_event?

-export ([ make_process_name/1,
	   start_link/2,
	   feed/1,
	   inference/0,
	   read_output/0
	 ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (state, {}).

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

feed (Input) ->
    ok.

inference () ->
    ok.

read_output () ->
    ok.



%% -----------------------------------------------------------------------------
%% Func: init/1
%% @doc Starts an entry_node process.
%%
%% Parameters:
%%   Params :: ??
%% -----------------------------------------------------------------------------
init([Params]) ->
    State = #state{},
    {ok, State}.

handle_call (_Request, _From, State) ->
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
