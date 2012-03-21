%% -*- mode: octave -*-
function NODE = finalize_node_training (NODE, CLASS, LEVEL)
  switch LEVEL

      case "output"
	%% compute class priors
	s = sum (NODE.PCW, 1); %% column-wise sum
	total = sum ( sum (NODE.PCW));
    
	if (total == 0)
	  NODE.class_prior_prob = [1];
	else
	  NODE.class_prior_prob = s / total;
	endif
    
	%% normalize the PCW
	NODE.PCW = normalize_over_columns (NODE.PCW);
	

      otherwise %% entry or intermediate

	%% compute coincidence priors
	total_seen = sum (NODE.seen);
	c_priors = NODE.seen / total_seen;
	
	%% make TAM symmetric
	TAM = make_symmetric (NODE.TAM);
	
	%% normalize TAM
	TAM = normalize_over_rows (TAM);
	
	%% compute temporal connections
	TC = c_priors * TAM;
	
	%%temporal groups learning
	NODE.temporal_groups = do_temporal_clustering (TC, TAM);
	
	%% PCG matrix computation
	NODE.PCG = do_compute_PCG (c_priors, NODE.temporal_groups);

    endswitch
endfunction
