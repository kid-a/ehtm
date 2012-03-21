%% insert into set
function S = insert_into_set (S, ITEM)
  if ( ! all (S - ITEM) ) %% if the element is already in the set
    S = S;
  else
    S = [S ITEM];
  endif
endfunction
