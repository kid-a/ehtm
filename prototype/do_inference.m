%% -*- mode: octave -*-
%% do_inference (LAYER, "entry" | "intermediate" | "output", SIGMA)
function LAYER = do_inference(LAYER, LEVEL = "intermediate", SIGMA = 1.0)

  switch LEVEL

    case "output"      
      node = LAYER{1,1};
            
      y = dens_over_coinc (node.coincidences, node.in_msg, LEVEL, SIGMA);
      z = dens_over_classes (y, node.PCW);
      p = class_post_prob (z, node.class_prior_prob);
      
      LAYER{1,1}.out_msg = p;
      
    otherwise %% "entry" or "intermediate"
      [layer_height, layer_width] = size(LAYER);
      for i = 1 : layer_height
	for j = 1 : layer_width  
  	  node = LAYER{i,j};
	  
  	  y = dens_over_coinc (node.coincidences, node.in_msg, LEVEL, SIGMA);
  	  z = dens_over_groups (y, node.PCG);
	  
	  LAYER{i,j}.out_msg = z;
	endfor
      endfor
      
  endswitch

endfunction
