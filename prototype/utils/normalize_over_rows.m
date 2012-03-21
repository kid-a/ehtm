%% -*- mode: octave -*-
%% normalize_over_rows
function M = normalize_over_rows(M)
  M = bsxfun(@rdivide, M, sum(M, 2));
endfunction
