%% -*- mode: octave -*-
%% select_highest_coincidence
%% in support of the temporal clustering algorithm
function k = select_highest_coincidence (TC, COINCIDENCES)
  [v,o] = sort(TC, "descend");
  
  for i = 1 : length(o)
    mask = abs(COINCIDENCES - o(i));
    
    if (!all(mask))
      [_,p] = sort(mask);
      k = COINCIDENCES(p(1));
      break;
    endif    
  endfor
endfunction
