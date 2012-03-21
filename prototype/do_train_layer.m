%% -*- mode: octave -*-
%% do_train_layer (l, "entry" | "intermediate" | "output")
function layer = do_train_layer (layer, class, level, temporal_gap, node_sharing = 0, training_seq = [])
  
  switch node_sharing

      case 1
	printf("Node sharing active.\n");
	printf("Training master node.\n");
	fflush (stdout);

	[layer_h, layer_w] = size (layer);
	
	for i = 1 : length(training_seq)
	  layer{1,1}.in_msg = double(vec(training_seq(:,:,i))');
	  fflush(stdout);
	  layer{1,1} = train_node (layer{1,1}, class, temporal_gap, level);
	endfor
	
	%% clone the master node's content
	C = layer{1,1}.coincidences;
	PCG = layer{1,1}.PCG;
	G = layer{1,1}.temporal_groups;
	TAM =  layer{1,1}.TAM;
	seen = layer{1,1}.seen;
	k_prev = layer{1,1}.k_prev;
	
	for i = 1 : layer_h
	  for j = 1 : layer_w
	    
	    printf("Setting state of node %d,%d\n", i, j);
	    layer{i,j}.coincidences = C;
	    layer{i,j}.PCG = PCG;
	    layer{i,j}.temporal_groups = G;
	    layer{i,j}.TAM = TAM;
	    layer{i,j}.seen = seen;
	    layer{i,j}.k_prev = k_prev;
	    fflush (stdout);
	    
	  endfor
	endfor

      otherwise
	
	[layer_h, layer_w] = size (layer);
	for i = 1 : layer_h
	  for j = 1 : layer_w
	    
	    printf("Training node %d,%d\n", i, j);
	    fflush(stdout);
	    layer{i, j} = train_node (layer{i, j}, class, temporal_gap, level);
	    
	  endfor
	endfor
	
    endswitch
endfunction
