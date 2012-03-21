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
  
  



  # for i = 1 : length (assigned)
  #   if ( ! assigned(i) )
  #     k = i;
  #     j = i;
  #     highest_temporal_connection = TC(i);
  #     break;
  #   endif
  # endfor
  
  # for i = (j + 1) : length (assigned)
  #   if (assigned (i)) 
  #     continue;

  #   else
  #     tc = TC(i);
  #     if (tc > highest_temporal_connection)
  # 	k = i;
  # 	highest_temporal_connection = tc;
  #     endif

  #   endif
    
  # endfor
endfunction
