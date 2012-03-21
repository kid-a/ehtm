%% -*- mode: octave -*-
function matrix = make_symmetric (M)
  matrix = tril ( tril (M, -1)  + triu (M, 1)', -1) + \
           triu ( tril (M, -1)' + triu (M, 1),   1) + \
           ( M .* eye ( length ( M ) ) );
endfunction
