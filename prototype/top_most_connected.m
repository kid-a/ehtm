%% -*- mode: octave -*-
%% returns the top_most_connected, unassigned coincidences
function [most_connected, COINCIDENCES] = top_most_connected (k, COINCIDENCES, TAM)
  most_connected = [];

  %% remove coincidence k
  COINCIDENCES = COINCIDENCES(COINCIDENCES != k);
  
  [v,o] = sort(TAM(k, COINCIDENCES), "descend");

  p = 1;
  
  while ((length(most_connected) < 3) && (p <= length(o)))
    index = COINCIDENCES(o(p));
    most_connected = [most_connected index];
    p += 1;
  endwhile

  for i = most_connected
    COINCIDENCES = COINCIDENCES(COINCIDENCES != i);
  endfor



  



  # [v,o] = sort(TAM(k,:));
  
  # p = 1;
  
  # while ((length(most_connected) < 5) && (p < length(o)))
  #   index = o(p);
  #   if (!assigned(index) && !(k == index))
  #     most_connected = [most_connected index];
  #   endif
    
  #   p += 1;
  # endwhile
  
endfunction
