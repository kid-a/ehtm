%% %-*- mode: octave -*-
%% dens_over_coinc
function Y = dens_over_coinc(COINCIDENCES, I, LEVEL = "intermediate", SIGMA = 1)
  switch LEVEL
    case "entry"
      Y = norm(bsxfun(@minus, COINCIDENCES, I), OPT="rows")';
      Y = exp(-Y.^2/SIGMA^2);
      
    otherwise
      [rows, columns] = size (COINCIDENCES);

      %% !FIXME maybe can be optimized?
      for i = 1 : rows %% iterate over coincidences
	selected_coincidences = [];
	for j = 1 : columns %% iterate over input vector
	  selected_coincidences(j) = I{j}(COINCIDENCES(i,j));
	endfor
	Y(i) = prod(selected_coincidences);
      endfor
  endswitch
endfunction
