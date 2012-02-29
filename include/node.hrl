%%
%% node.hrl
%%

%% -define (NODE_FIELDS,
%% 	 [ lambda_minus,
%% 	   lambda_plus,  
%% 	   sigma,
%% 	   coincidences,
%% 	   coincidences_occurrences,
%% 	   y,
%% 	   t,
%% 	   temporal_groups,
%% 	   pcg 
%% 	 ]
%% 	).


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


%% -define (OUT_NODE_FIELDS,
%% 	 [ lambda_minus,
%% 	   outputs,
%% 	   sigma,
%% 	   coincidences,
%% 	   coincidences_occurrences,
%% 	   y,
%% 	   t,
%% 	   prior_probabilities,
%% 	   pcw
%% 	 ]
%% 	).


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
	 
