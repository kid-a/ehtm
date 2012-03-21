%% -*- mode: octave -*-
%% make_network
function NETWORK = make_network ()
  l1 = make_layer("entry", [4 4]);
  l2 = make_layer("intermediate", [2 2]);
  l3 = make_layer("output");
  NETWORK = {l1 l2 l3};

  # l1 = make_layer("entry", [30 30]);
  # %%l2 = make_layer("intermediate", [15 15]);
  # l3 = make_layer("output");
  # NETWORK = {l1 l3};
endfunction
