%% -*- mode: octave -*-p
function i = pad_input (input, desired_size = [300 300])
  [height, width] = size(input);
  h_add = desired_size(1) - height;
  w_add = desired_size(2) - width;
  i = impad (input, [w_add 0], [h_add 0], "ones");
endfunction
