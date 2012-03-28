%% -*- mode: octave -*-
function PCG = do_compute_PCG (coinc_priors, temporal_groups)
  PCG = zeros (length (coinc_priors), length (temporal_groups));
  
  %% !FIXME maybe it can be optimized
  for i = 1 : length(coinc_priors)
    for j = 1 : length (temporal_groups)
      if is_in_set (temporal_groups{j}, i)
	PCG(i, j) = coinc_priors (i);
      endif
    endfor
  endfor

  %% normalize
  PCG = normalize_over_columns (PCG);
  %%PCG = normalize_over_columns (PCG);
endfunction
