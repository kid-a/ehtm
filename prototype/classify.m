%% -*- mode: octave -*-
function out = classify (network, in)
  network{1} = do_expose (network{1}, in);
  network{1} = do_inference (network{1}, "entry");
  network{2} = do_propagate (network{1}, network{2});
  network{2} = do_inference (network{2}, "output");
  out = network{2}{1,1}.out_msg;
endfunction
