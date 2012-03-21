%% -*- mode: octave -*-
%% returns the top_most_connected, unassigned coincidences
function most_connected = top_most_connected (k, assigned, TAM)
  most_connected = [];
  
  [v,o] = sort(TAM(k,:));
  
  p = 1;
  
  while ((length(most_connected) < 5) && (p < length(o)))
    index = o(p);
    if (!assigned(index) && !(k == index))
      most_connected = [most_connected index];
    endif
    
    p += 1;
  endwhile
  
endfunction
