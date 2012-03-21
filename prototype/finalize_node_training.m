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
	# [h, _] = size (TAM);
	# for i = 1 : h
	#   if ( sum (TAM(i,:)) != 0)
	#     TAM(i,:) = TAM (i,:) / sum (TAM(i,:));
	#   endif
	# endfor
	
	%% compute temporal connections
	TC = c_priors * TAM;
	
	# TC = [];
	# coinc_count = length (c_priors);
	# for j = 1 : coinc_count
	#   TC(j) = 0;
	#   for k = 1 : coinc_count
	#     TC(j) += TAM(k, j) * c_priors (k);
	#   endfor
	# endfor
	

	%%temporal groups learning
	NODE.temporal_groups = do_temporal_clustering (TC, TAM);
	
	%% PCG matrix computation
	NODE.PCG = do_compute_PCG (c_priors, NODE.temporal_groups);

    endswitch
	

  # if (strcmp(LEVEL, "entry"))
  #   %% compute coincidence priors
  #   c_priors = [];
  #   total_seen = sum (NODE.seen);
    
  #   printf("Here");
  #   fflush(stdout);
  #   for i = 1 : length (NODE.seen)
  #     c_priors(i) = NODE.seen (i) / total_seen;
  #   endfor
       

  #   printf("Here1");
  #   fflush(stdout);
  #   %% make TAM symmetric
  #   %%node.TAM %%!FIXME
  #   TAM = make_symmetric (NODE.TAM);

  #   printf("Here2");
  #   fflush(stdout);
  #   %% normalize TAM
  #   [h, _] = size (TAM);
  #   for i = 1 : h
  #     if ( sum (TAM(i,:)) != 0)
  # 	TAM(i,:) = TAM (i,:) / sum (TAM(i,:));
  #     endif
  #   endfor

  #   printf("Here2");
  #   fflush(stdout);
  #   %% compute temporal connections
  #   TC = [];
  #   coinc_count = length (c_priors);
  #   for j = 1 : coinc_count
  #     TC(j) = 0;
  #     for k = 1 : coinc_count
  # 	TC(j) += TAM(k, j) * c_priors (k);
  #     endfor
  #   endfor
    
  #   printf("Here3");
  #   fflush(stdout);
  #   %%temporal groups learning
  #   NODE.temporal_groups = do_temporal_clustering (TC, TAM);

  #   printf("Here4");
  #   fflush(stdout);
  #   %% PCG matrix computation
  #   NODE.PCG = do_compute_PCG (c_priors, NODE.temporal_groups);
    
  # elseif (strcmp(LEVEL, "output"))
  #   %% compute class priors
  #   s = sum (NODE.PCW, 1); %% column-wise sum
  #   total = sum ( sum (NODE.PCW));
    
  #   if (total == 0)
  #     NODE.CLASS_prior_prob = [1];
  #   else
  #     NODE.CLASS_prior_prob = s / total;
  #   endif
    
  #   %% normalize the PCW
  #   NODE.PCW = normalize_over_columns (NODE.PCW);
    
  # else %% intermediate node
  # endif
endfunction
