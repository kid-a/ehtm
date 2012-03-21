%% check whether an element belongs to a set
function B = is_in_set (S, ITEM)
  if ( ! all (S - ITEM) )
    B = 1;
  else 
    B = 0;
  endif
endfunction
