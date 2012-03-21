function widx = compute_widx (msg)
  widx = [];

  for i = 1 : length (msg)
    [_, p] = max (msg{i});
    widx(i) = p;
  endfor

endfunction
