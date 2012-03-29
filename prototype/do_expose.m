%% -*- mode: octave -*-
%% do_expose
function LAYER = do_expose (LAYER, INPUT)
  [input_height, input_width] = size(INPUT);
  [layer_height, layer_width] = size(LAYER);
  patch_height = input_height / layer_height;
  patch_width = input_width / layer_width;
  
  starting_point_h = 1;
  starting_point_w = 1;
  
  for i = 1 : layer_height
    for j = 1 : layer_width
      node = LAYER{i,j};
      patch = INPUT (starting_point_h : starting_point_h + patch_height - 1, 
		     starting_point_w : starting_point_w + patch_width - 1)
      
      node.in_msg = double (vec(patch)');
      LAYER{i,j} = node;

      starting_point_w = (j * patch_width) + 1;
    endfor
    starting_point_w = 1;
    starting_point_h = (i * patch_height) + 1;
  endfor
  
endfunction
