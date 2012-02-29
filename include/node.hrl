%%
%% node.hrl
%%

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
	 
