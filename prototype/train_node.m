%% -*- mode: octave -*-
%% train_node
function NODE = train_node (NODE, CLASS, TEMPORAL_GAP, LEVEL)

  [k, NODE.coincidences, NODE.seen, NODE.TAM] = \
      select_active_coinc (LEVEL, NODE.coincidences, NODE.in_msg, NODE.seen, NODE.TAM);

  printf("Node has %d coincidences\n", length(NODE.coincidences));
  fflush(stdout);
  %% increment the temporal activation matrix cell
  %% for k_prev, k
  if (!TEMPORAL_GAP)
    if (NODE.k_prev != 0) %% skip if first pattern
      NODE.TAM (NODE.k_prev, k) += 1;
    endif
  endif
  
  NODE.k_prev = k;
  
  if (strcmp(LEVEL, "output"))

    printf("Setting class %d", CLASS);

    %% increment the PCW matrix
    try 
      NODE.PCW(k, CLASS) += 1;
    catch
      NODE.PCW(k, CLASS) = 1;
    end_try_catch
    
    NODE.PCW
    fflush(stdout);

  endif

endfunction