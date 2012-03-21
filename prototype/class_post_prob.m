%% -*- mode: octave -*-
%% class_post_prob
function P = class_post_prob (Z, CLASS_PRIOR_PROB) 
  total = Z * CLASS_PRIOR_PROB';
  P = Z .* CLASS_PRIOR_PROB / total;
endfunction
