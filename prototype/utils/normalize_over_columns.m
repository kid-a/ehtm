%% -*- mode: octave -*-p
%% normalize over cols
function M = normalize_over_columns(M)
  M = bsxfun(@rdivide, M, sum(M, 1));
endfunction
