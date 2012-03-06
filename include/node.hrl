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
%%                     | #entry_node_input }
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
%% entry_node_state { lambda_minus :: {chunk_size :: integer (), data :: binary ()}
%%                    lambda_plus :: [ { temporal_group_name :: atom (), 
%%                                       density :: float () } ]
%%                    sigma :: float ()
%%                    coincidences :: [#coincidence ()]
%%                    seen :: [ { coincidence_name :: atom (), 
%%                                occurrences :: int() } ]
%%                    last_seen :: atom ()
%%                    y :: [ { coincidence_name :: atom (), 
%%                             density:: float () } ]
%%                    t :: [ { coincidence1_name :: atom (),
%%                             coincidence2_name :: atom (),
%%                             value :: integer () } ]
%%                    temporal_groups :: #temporal_group ()
%%                    pcg :: [ { group_name :: atom (), 
%%                               coincidence_name :: atom (), 
%%                               probability :: float () } ]
%%                  }
-record (entry_node_state, { lambda_minus,
			     lambda_plus,
			     sigma,
			     coincidences,
			     seen,
			     last_seen,
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
%%                           coincidences :: [#coincidence ()]
%%                           seen :: [ { coincidence_name :: atom (), 
%%                                       occurrences :: int() } ]
%%                           last_seen :: atom ()
%%                           y :: [ { coincidence_name :: atom (), 
%%                                    density:: float () } ]
%%                           t :: [ { coincidence1_name :: atom (),
%%                                    coincidence2_name :: atom (),
%%                                    value :: integer () } ]
%%                           temporal_groups :: #temporal_group ()
%%                           pcg :: [ { group_name :: atom (), 
%%                                      coincidence_name :: atom (), 
%%                                      probability :: float () } ]
%%                          }
-record (intermediate_node_state, { lambda_minus,
				    lambda_plus,
				    coincidences,
				    seen,
				    last_seen,
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
%%                     coincidences :: [#coincidence ()]
%%                     seen :: [ { coincidence_name :: atom (), 
%%                                 occurrences :: int() } ]
%%                     last_seen :: atom ()
%%                     y :: [ { coincidence_name :: atom (), 
%%                              density:: float () }]
%%                     t :: [ { coincidence1_name :: atom (),
%%                                    coincidence2_name :: atom (),
%%                                    value :: integer () } ]
%%                     classes :: #class
%%                     prior_probabilities :: [ { class_name :: atom (), 
%%                                                probability :: float () } ]
%%                     densities_over_classes :: [ { class_name :: atom (),
%%                                                   density :: float () } ]
%%                     posterior_probabilities :: [ { class_name :: atom (),
%%                                                    density :: float () } ]
%%                     pcw :: [ { { class_name :: atom (), 
%%                                 coincidence_name :: atom (), } 
%%                                probability :: float () } ]
%%                   }
-record (output_node_state, { lambda_minus,
			      lambda_plus,
			      coincidences,
			      seen,
			      last_seen,
			      y,
			      t,
			      classes,
			      prior_probabilities,
			      pcw
			    }).

%% !FIXME maybe values should be initialized to undefined
%% to allow selective set of parameters?
