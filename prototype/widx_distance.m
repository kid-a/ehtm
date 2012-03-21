function distance = widx_distance (coincidence, widx)
  diff = coincidence - widx;
  distance = length ( find (diff));
endfunction
