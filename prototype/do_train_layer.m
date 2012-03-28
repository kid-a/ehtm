%% -*- mode: octave -*-
function layer = do_train_layer (layer, level, pattern, class, temporal_gap, node_sharing)
  
  switch node_sharing

      case 1
	%%printf("Node sharing active.\n");
	%%printf("Training just the master node.\n");
	%%fflush (stdout);

	%% FIXME here should distinguish between entry and intermediate nodes
	layer{1,1}.in_msg = double(vec(pattern)');
	layer{1,1} = train_node (layer{1,1}, class, temporal_gap, level);

      otherwise
	
	[layer_h, layer_w] = size (layer);
	for i = 1 : layer_h
	  for j = 1 : layer_w
	    
	    printf("Training node (%d,%d)\n", i, j);
	    fflush(stdout);
	    layer{i, j} = train_node (layer{i, j}, class, temporal_gap, level);
	    
	  endfor
	endfor
	
    endswitch
endfunction
