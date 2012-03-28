%% -*- mode: octave -*-
%% make_node ("entry" | "intermediate" | "output")
function NODE = make_node (T)
  switch T

    case "output"
      NODE.in_msg = [];
      NODE.coincidences = [];
      NODE.TAM = [];
      NODE.seen = [];
      NODE.k_prev = 0;
      NODE.PCW = [];
      NODE.class_prior_prob = [];
      NODE.out_msg = [];
      
    otherwise
      NODE.in_msg = [];
      NODE.coincidences = [];
      NODE.seen = [];
      NODE.TAM = [];
      NODE.k_prev = 0;
      NODE.temporal_groups = [];
      NODE.PCG = [];
      NODE.out_msg = [];

  endswitch

endfunction
