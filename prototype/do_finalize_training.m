%% -*- mode: octave -*-
function LAYER = do_finalize_training (LAYER, class, level, node_sharing = 0)
  switch LAYER
    case "output"
      LAYER{1, 1} = finalize_node_training (LAYER{1,1}, class, level);
      
    otherwise %% intermediate || entry
      
      switch node_sharing

	  case 0
	    [layer_h, layer_w] = size (LAYER);
	    for i = 1 : layer_h
	      for j = 1 : layer_w
		LAYER{i, j} = finalize_node_training (LAYER{i,j}, class, level);
	      endfor
	    endfor 
	    
	  case 1
	    LAYER{1, 1} = finalize_node_training (LAYER{1,1}, class, level);
	    [layer_h, layer_w] = size (LAYER);
	    for i = 1 : layer_h
	      for j = 1 : layer_w
		LAYER{i,j}.temporal_groups = LAYER{1,1}.temporal_groups;
		LAYER{i,j}.PCG = LAYER{1,1}.PCG;
	      endfor
	    endfor 
	endswitch
  endswitch
endfunction
