-module(entry_node).

-behaviour(gen_server).

-export([start_link/0, say_hello/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include ("node.hrl").

-define (DEF_SIGMA, 1.0).

-define (NODE_FIELD_NAMES, 
	 [lambda_minus,             
	  y,
	  lambda_plus,
	  coincidence_occurrences,
	  coincidences,
	  t,
	  temporal_groups,         
	  pcg
	 ]).

-record (state,
	 {
	   lambda_minus,            %% input vector (a proplist?)
	   sigma = 1.0,             %% the sigma param
	   y,                       %% y vector (a proplist?)
	   lambda_plus = [],        %% output vector (a proplist?)
	   
	   coincidence_occurrences, %% k_seen
	   last_active_coincidence, %% k
	   coincidences,            %% list of coincidences
	   t,                       %% coincidences transition matrix (an ets table?)
	   temporal_groups,         %% the temporal groups
	   pcg                      %% the PCG matrix (an ets table?)
	 }).

start_link() ->
    %% !FIXME make process name
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Params) ->
    Sigma = proplists:get_value (sigma, Params, ?DEF_SIGMA), 
    NodeName = proplists:get_value (name, Params),
    EtsFieldNames = lists:map (
		      fun (FieldName) -> make_fq_datafield_name (NodeName, FieldName) end,
		      ?NODE_FIELD_NAMES ),
    
    lists:foreach (fun (FieldName) ->
			   ets:new(FieldName, 
				   [set, 
				    named_table,
				    private, 
				    {read_concurrency, true}
				   ])
		   end,
		   EtsFieldNames),
    
    State = [],
    %% State = #state { lambda_minus = 
    %%   sigma = 1.0,             %% the sigma param
    %% 		     y,                       %% y vector (a proplist?)
    %%   lambda_plus = [],        %% output vector (a proplist?)
      
    %%   coincidence_occurrences, %% k_seen
    %%   last_active_coincidence, %% k
    %%   coincidences,            %% list of coincidences
    %%   t,                       %% coincidences transition matrix (an ets table?)
    %%   temporal_groups,         %% the temporal groups
    %%   pcg                      %% the PCG matrix (an ets table?)
    %%  },
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
%% -----------------------------------------------------------------------------
%% Func: make_fq_datafield_name
%% @doc Given a process name and a data field name, returns the fully qualified
%%      data field name.
%%
%% Parameters:
%%   ProcessName :: string ()
%%   DataField   :: string ()
%%
%% Reply:
%%   FQName :: atom ()
%% -----------------------------------------------------------------------------
make_fq_datafield_name (ProcessName, DataField) ->
    list_to_atom (ProcessName ++ atom_to_list (DataField)).

%% -----------------------------------------------------------------------------
%% Func: make_process_name/1
%% @doc Given a node name, returns the name of its process.
%%
%% Parameters:
%%   NodeName :: string ()
%%
%% Reply:
%%   ProcessName :: atom()
%% -----------------------------------------------------------------------------
make_process_name (NodeName) ->
    list_to_atom (NodeName).
