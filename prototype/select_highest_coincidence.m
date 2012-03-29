%% -*- mode: octave -*-
%% select_highest_coincidence
%% in support of the temporal clustering algorithm
function k = select_highest_coincidence (TC, ASSIGNED, N=1)
  [v,o] = sort(TC, "descend");
  
  for i = o
    if (!ASSIGNED(i))
      k = i;
      break;
    endif
  endfor
endfunction