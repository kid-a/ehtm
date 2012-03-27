%% -*- mode: octave -*p-
function count = test (network, patterns)
  count = 0;
  
  for i = 1 : length(patterns)
    [c, network] = classify(network, patterns{i}{1});
    [m,s] = max(c);
    if (s == patterns{i}{2})
      count = count + 1;
    endif		 
  endfor
endfunction
