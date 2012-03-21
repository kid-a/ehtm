%% -*- mode: octave -*-
%% propagate output messages from LAYER1 up to LAYER2
function LAYER2 = do_propagate (LAYER1, LAYER2)
  [layer1_height, layer1_width] = size(LAYER1);
  [layer2_height, layer2_width] = size(LAYER2);
  
  %% reset all the input messages in layer 2
  for k = 1: layer2_height
    for m = 1: layer2_width
      LAYER2{k, m}.in_msg = {};
    endfor
  endfor
  
  %% propagate the messages
  for i = 1 : layer1_height
    upper_i = idivide (i, layer2_height);
    
    for j = 1 : layer1_width
      upper_j = idivide (j, layer2_width);
      
      msg = LAYER1{i, j}.out_msg;
      
      for k = 1 : layer2_height
	for m = 1 : layer2_width
	  [_, w] = size (LAYER2{k, m}.in_msg);
	  LAYER2{k, m}.in_msg{w + 1} = msg;
	endfor
      endfor

    endfor
  endfor
endfunction
