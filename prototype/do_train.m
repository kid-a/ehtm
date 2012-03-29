%% -*- mode: octave -*-
%% do_train(network, training_sequence)
function NETWORK = do_train(NETWORK, PATTERNS, SIGMA = 25.0, NODE_SHARING = [1 0 0])
  entry_ind  = 1;
  output_ind = length(NETWORK);

  %% reset the network
  NETWORK = reset (NETWORK);

  %% make the training sequences
  sequences = {};
  count = [0 0 0];
  
  for i = 1 : length(PATTERNS)
    s = make_train_seq(PATTERNS{i}{1}, "entry", [4 4]);
    s{3} = PATTERNS{i}{2}; %%copy the class
    sequences{1}{i} = s;
    [_,_,l] = size(s{1});
    count(1) += l;
    
    s = make_train_seq(PATTERNS{i}{1}, "intermediate");
    s{3} = PATTERNS{i}{2}; %%copy the class
    sequences{2}{i} = s;
    [_,_,l] = size(s{1});
    count(2) += l;
    
    s = make_train_seq(PATTERNS{i}{1}, "output");
    s{3} = PATTERNS{i}{2}; %%copy the class
    sequences{3}{i} = s;
    [_,_,l] = size(s{1});
    count(3) += l;
  endfor

  %% training routine
  tic
  
  %% for each level
  for i = 1 : output_ind
    
    %% do actual training
    printf("Training level %d\n", i);
    fflush(stdout);

    %% determine current layer
    switch i
      case entry_ind
	cur_level = "entry";
	seq_index = 1;
      case output_ind 
	cur_level = "output";
	seq_index = 2;
      otherwise
	cur_level = "intermediate";
	seq_index = 3;
    endswitch
    
    seq = sequences{seq_index};
    
    %% for each input pattern
    for j = 1 : length(seq)
      [_,_,k] = size(seq{j}{1});
      temporal_gaps = seq{j}{2}; %% get the indexes of temporal gaps
      cls = seq{j}{3}; %% get the class
      
      for l = 1 : k
	if (i != entry_ind)
	  NETWORK{1} = do_expose (NETWORK{1}, seq{j}{1}(:,:,l));
	endif

	%% do inference on input
	%% (it is not executed when training layer 1)
	for m = 1 : i
	  if (m == i)
	    continue;
	  endif
	  
	  switch m  
	    case output_ind
	      NETWORK{m} = do_inference (NETWORK{m}, "output");
	    
	    case entry_ind
	      NETWORK{m} = do_inference (NETWORK{m}, "entry", SIGMA);
 	      NETWORK{m + 1} = do_propagate (NETWORK{m}, NETWORK{m + 1});
	      
	    otherwise
 	      NETWORK{m} = do_inference (NETWORK{m}, "intermediate");
 	      NETWORK{m + 1} = do_propagate (NETWORK{m}, NETWORK{m + 1});
 	  endswitch
	endfor
	
	%% determine if the current pattern is the first after
	%% a temporal gap
	temporal_gap = !all((temporal_gaps - l));
	if (temporal_gap)
	  printf("Temporal gap detected.\n");
	endif

	fflush(stdout);

	NETWORK{i} = do_train_layer(NETWORK{i}, cur_level, 
				    seq{j}{1}(:,:,l), cls, temporal_gap, 
				    NODE_SHARING(i));
	
      endfor
    endfor

    NETWORK{i} = do_finalize_training(NETWORK{i}, cur_level, 
				      cls, NODE_SHARING(i));

  endfor
  toc
  
  printf("\n\n");
  printf("*** Summary ***\n");
  printf("Number of patterns: %d\n", length(PATTERNS));
  printf("Size of training sequences:\n");
  printf("Q1: %d\n", count(1));
  printf("Q2: %d\n", count(2));
  printf("Q3: %d\n", count(3));
  fflush(stdout);

  printf("\n");
  printf("Cardinality of coincidences and temporal groups:\n");
  for i = 1 : length(NETWORK)

    printf("Layer %d:", i);
    [h, w] = size(NETWORK{i});
    coinc_count = 0;
    temp_groups_count = 0;

    k = 0;
    
    for j = 1 : h
      for l = 1 : w
	k += 1;
	coinc_count += length(NETWORK{i}{j,l}.coincidences);
	if (i != output_ind)
	  temp_groups_count += length(NETWORK{i}{j,l}.temporal_groups);
	endif
      endfor
    endfor

    if (i != output_ind)
      printf("%f, %f\n", coinc_count/k, temp_groups_count/k);
    else
      printf("%f\n", coinc_count/k);
    endif
    fflush(stdout);
  endfor

endfunction
