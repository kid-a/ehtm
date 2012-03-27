%% -*- mode: octave -*-
function [out, network] = classify (network, in)
  network{1} = do_expose (network{1}, in);
  network{1} = do_inference (network{1}, "entry", 250.0);
  network{2} = do_propagate (network{1}, network{2});
  network{2} = do_inference (network{2}, "intermediate");
  network{3} = do_propagate (network{2}, network{3});
  network{3} = do_inference (network{3}, "output");
  out = network{3}{1,1}.out_msg;
endfunction
