%% -*- mode: octave -*-
%% returns the top_most_connected, unassigned coincidences
function most_connected = top_most_connected (k, COINCIDENCES, ASSIGNED, TAM)
  most_connected = [];

  %% remove coincidence k
  COINCIDENCES = COINCIDENCES(COINCIDENCES != k);
  
  [v,o] = sort(TAM(k, COINCIDENCES), "descend");

  p = 1;
  
  # while ((length(most_connected) < 3) && (p <= length(o)))
  while ((p <= 3) && (p <= length(o)))
    index = COINCIDENCES(o(p));
    
    if (!ASSIGNED(index))
      most_connected = [most_connected index];
    endif
    p += 1;
  endwhile
endfunction
