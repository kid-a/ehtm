%% -*- mode: octave -*-
%% make_layer ("entry" | "intermediate" | "output", SIZE)
function LAYER = make_layer (_type, SIZE = [1 1])
  height = SIZE(2);
  width = SIZE(1);
  LAYER = {};
  for i = 1 : height
    for j = 1 : width
      node = make_node (_type);
      LAYER (i,j) = node;
    endfor
  endfor
endfunction
