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
%%               data :: [{child :: atom (), temporal_group_name :: atom ()}] 
%%                     | binary() }
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
%% class { name :: atom ()
%%         data :: term () }
%%                   
%%
-record (class, {
	   name,
	   data
	  }).

%%
%% entry_node_state { lambda_minus :: binary (),
%%                    lambda_plus :: [ { temporal_group_name :: atom (), 
%%                                       density :: float () } ]
%%                    sigma :: float ()
%%                    coincidences :: #coincidence ()
%%                    coincidences_occurrences :: [ { coincidence_name :: atom (), 
%%                                                    occurrences :: int() } ]
%%                    y :: [ { coincidence_name :: atom (), 
%%                             density:: float () } ]
%%                    t :: ??
%%                    temporal_groups :: #temporal_group ()
%%                    pcg :: [ { group_name :: atom (), 
%%                               coincidence_name :: atom (), 
%%                               probability :: float () } ]
%%                  }
-record (entry_node_state, { lambda_minus,
			     lambda_plus,
			     sigma,
			     coincidences,
			     coincidences_occurrences,
			     y,
			     t,
			     temporal_groups,
			     pcg
			   }).


%%
%% intermediate_node_state { lambda_minus :: [ { origin :: atom (), 
%%                                               lambda :: [ { temporal_group_name :: atom (),
%%                                                             density :: float () ] } ] 
%%                           lambda_plus :: [ { temporal_group_name :: atom (), 
%%                                              density :: float () } ]
%%                           coincidences :: #coincidence ()
%%                           coincidences_occurrences :: [ { coincidence_name :: atom (), 
%%                                                    occurrences :: int() } ]
%%                           y :: [ { coincidence_name :: atom (), 
%%                                    density:: float () } ]
%%                           t :: ??
%%                           temporal_groups :: #temporal_group ()
%%                           pcg :: [ { group_name :: atom (), 
%%                                      coincidence_name :: atom (), 
%%                                      probability :: float () } ]
%%                          }
-record (intermediate_node_state, { lambda_minus,
				    lambda_plus,
				    coincidences,
				    coincidences_occurrences,
				    y,
				    t,
				    temporal_groups,
				    pcg
				  }).


%%
%% output_node_state { lambda_minus :: [ { origin :: atom (), 
%%                                         lambda :: [ { temporal_group_name :: atom (),
%%                                                       density :: float () ] } ] 
%%                     lambda_plus :: [ { class_name :: atom (),
%%                                        probability :: density } ]
%%                     coincidences :: #coincidence ()
%%                     coincidences_occurrences :: [ { coincidence_name :: atom (), 
%%                                                     occurrences :: int() } ]
%%                     y :: [ { coincidence_name :: atom (), 
%%                              density:: float () }]
%%                     t :: ??
%%                     classes :: #class
%%                     prior_probabilities :: [ { class_name :: atom (), 
%%                                                probability :: float () } ]
%%                     densities_over_classes :: [ { class_name :: atom (),
%%                                                   density :: float () } ]
%%                     posterior_probabilities :: [ { class_name :: atom (),
%%                                                    density :: float () } ]
%%                     pcw :: [ { class_name :: atom (), 
%%                                coincidence_name :: atom (), 
%%                                probability :: float () } ]
%%                   }
-record (output_node_state, { lambda_minus,
			      lambda_plus,
			      coincidences,
			      coincidences_occurrences,
			      y,
			      t,
			      classes,
			      prior_probabilities,
			      densities_over_classes,
			      posterior_probabilities,
			      pcw
			    }).
