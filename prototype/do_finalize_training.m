%% -*- mode: octave -*-
function LAYER = do_finalize_training (LAYER, class, level)
  switch LAYER
    case "output"
      LAYER{1, 1} = finalize_node_training (LAYER{1,1}, class, level);
      
    otherwise %% intermediate || entry
      [layer_h, layer_w] = size (LAYER);
      for i = 1 : layer_h
	for j = 1 : layer_w
	  LAYER{i, j} = finalize_node_training (LAYER{i,j}, class, level);
	endfor
      endfor 
      
  endswitch
endfunction
