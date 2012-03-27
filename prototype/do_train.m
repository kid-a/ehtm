%% -*- mode: octave -*-
%% do_train(network, training_sequence)
function NETWORK = do_train(NETWORK, PATTERNS, L=1)
  entry_ind  = 1;
  output_ind = length(NETWORK);

  %% reset nodes
  for i = 1 : output_ind
    [layer_h, layer_w] = size(NETWORK{i});
    
    for j = 1 : layer_h
      for k = 1 : layer_w
	if (i == output_ind)
	  (NETWORK{i}){j, k}.in_msg = [];
	  (NETWORK{i}){j, k}.coincidences = [];
	  (NETWORK{i}){j, k}.TAM = [];
	  (NETWORK{i}){j, k}.seen = [];
	  (NETWORK{i}){j, k}.k_prev = 0;
	  (NETWORK{i}){j, k}.PCG = [];
	  (NETWORK{i}){j, k}.PCW = [];
	  (NETWORK{i}){j, k}.out_msg = [];
	else
	  (NETWORK{i}){j, k}.in_msg = [];
	  (NETWORK{i}){j, k}.coincidences = [];
	  (NETWORK{i}){j, k}.TAM = [];
	  (NETWORK{i}){j, k}.seen = [];
	  (NETWORK{i}){j, k}.k_prev = 0;
	  (NETWORK{i}){j, k}.PCG = [];
	  (NETWORK{i}){j, k}.temporal_groups = [];
	  (NETWORK{i}){j, k}.out_msg = [];
	endif
      endfor
    endfor
  endfor

  %% training
  tic
  for i = 1 : output_ind
    
    %% find out what is the current level
    switch (i)
      case output_ind
	cur_level = "output";
      case entry_ind
	cur_level = "entry";
      otherwise
	cur_level = "intermediate";
    endswitch

    printf("\n\n");
    printf("Training layer %d\n", i);
    fflush(stdout);
    cls = PATTERNS{1}{2};
    
    for k = 1 : length (PATTERNS)
      %% from now on,
      %% PATTERNS{k}{1} is the bitmap
      %% PATTERNS{k}{2} is the class

      %% check whether the current one is a temporal gap
      temporal_gap = 0;
      
      new_cls = PATTERNS{k}{2};
      if (new_cls != cls)
	temporal_gap = 1
      endif
      
      cls = new_cls;
      
      NETWORK{1} = do_expose (NETWORK{1}, PATTERNS{k}{1});
      
      %% inference and propagation of messages
      for j = 1 : (i - 1)
	# if (j == i)
	#   break;
	# endif
	
	printf("Doing inference on level %d\n", j);
	fflush(stdout);
	switch (j)
	    case output_ind
	      NETWORK{j} = do_inference (NETWORK{j}, "output");
	      
	    case entry_ind
	      NETWORK{j} = do_inference (NETWORK{j}, "entry", 200.0);
	      NETWORK{j + 1} = do_propagate (NETWORK{j}, NETWORK{j + 1});
	      
	    otherwise
	      NETWORK{j} = do_inference (NETWORK{j}, "intermediate");
	      NETWORK{j + 1} = do_propagate (NETWORK{j}, NETWORK{j + 1});
	  endswitch
	  
      endfor
      
      %% actual training
      printf("Training level %d\n", i);
      fflush(stdout);
      switch i
	case entry_ind
	  training_seq = make_train_seq (PATTERNS{k}{1}, [4 4]);
	  NETWORK{i} = do_train_layer (NETWORK{i}, cls, cur_level, temporal_gap, 1, training_seq);
	  
	otherwise
	  NETWORK{i} = do_train_layer (NETWORK{i}, cls, cur_level, temporal_gap);
      endswitch  
    endfor

    %% finalize training
    printf("Finalizing training on %d\n", i);
    fflush(stdout);
    switch i
      case entry_ind
	NETWORK{i} = do_finalize_training (NETWORK{i}, cls, cur_level, 1);
      otherwise
	NETWORK{i} = do_finalize_training (NETWORK{i}, cls, cur_level);
    endswitch

  endfor
  toc
endfunction
