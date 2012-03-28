%% -*- mode: octave -*-
function NETWORK = reset (NETWORK)
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
	  (NETWORK{i}){j, k}.PCW = [];
	  (NETWORK{i}){j, k}.class_prior_prob = [];
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
  
endfunction
