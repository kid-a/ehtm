%% -*- mode: octave -*-
%% select_highest_coincidence
%% in support of the temporal clustering algorithm
function k = select_highest_coincidence (TC, assigned)
  %%tic
  [v,o] = sort(TC, "descend");
  p = 1;
  while (1)
    if (spdiag(assigned(o(p))))
      p++;
      continue;
    else
      k = p;
      break;
    endif
  endwhile
  %%toc
endfunction
