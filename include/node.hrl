%%
%% node.hrl
%%
-record (entry_node_input, {
	   chunk_size,
	   binary_data
	   }).

-record (coincidence, {
	   name,
	   data
	  }).


-record (node_state, {
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
