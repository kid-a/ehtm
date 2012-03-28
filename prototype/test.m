%% -*- mode: octave -*-
function ratio = test (network, patterns)
  count = 0;
  k = 0;
  
  for i = 1 : length(patterns)
    [c, network] = classify(network, patterns{i}{1});
    [m,s] = max(c);
    
    k += 1;
    if (s == patterns{i}{2})
      count = count + 1;
    endif		 
  endfor
  
  ratio = count / k;
endfunction
