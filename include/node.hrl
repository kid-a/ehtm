%%
%% node.hrl
%%

%%
%% entry_node_input { chunk_size :: int ()
%%                    binary_data :: binary ()
%%                   
%%
-record (entry_node_input, {
	   chunk_size,
	   binary_data
	   }).


%%
%% coincidence { name :: atom ()
%%               data :: [float ()] | binary() }
%%                   
%%
-record (coincidence, {
	   name,
	   data
	  }).


%%
%% temporal_group { name :: atom ()
%%                  coincidences :: [atom ()] }
%%                   
%%
-record (temporal_group, {
	   name,
	   coincidences
	  }).

%%
%% entry_node_state { lambda_minus :: [ { origin :: atom (), 
%%                                        lambda :: float () } ] 
%%                                  | binary ()
%%
%%                    lambda_plus :: [ { temporal_group_name :: atom (), 
%%                                       density :: float () } ]
%%
%%                    sigma :: float ()
%%
%%                    coincidences :: #coincidence ()
%%
%%                    coincidences_occurrences :: [ { coincidence_name :: atom (), 
%%                                                    occurrences :: int() } ]
%%
%%                    y :: [ { coincidence_name :: atom (), 
%%                             density:: float ()]
%%
%%                    t :: ??
%%
%%                    temporal_groups :: #temporal_group ()
%%
%%                    pcg :: [ { group_name :: atom (), 
%%                               coincidence_name :: atom (), 
%%                               probability :: float () } ]
%%                  }
-record (entry_node_state, {
	   lambda_minus,
	   lambda_plus,
	   
	   sigma,
	   coincidences,
	   coincidences_occurrences,
	   y,
	   t,
	   temporal_groups,
	   pcg
	  }).


-record (node_state, {
	   lambda_minus,
	   lambda_plus,
	   
	   coincidences,
	   coincidences_occurrences,
	   y,
	   t,
	   temporal_groups,
	   pcg
	  }).

-record (output_node_state, {
	   lambda_minus,
	   outputs,
	   
	   coincidences,
	   coincidences_occurrences,
	   y,
	   t,
	   prior_probabilities,
	   pcw
	   }).
