%% -*- mode: octave -*-
%% train_node
function NODE = train_node (NODE, CLASS, TEMPORAL_GAP, LEVEL)

  switch LEVEL
    case "entry"
      transition_memory = 2;
    case "intermediate"
      transition_memory = 5;
    case "output"
      transition_memory = 1;
  endswitch


  [k, NODE.coincidences, NODE.seen, NODE.TAM] = \
      select_active_coinc (LEVEL, NODE.coincidences, NODE.in_msg, NODE.seen, NODE.TAM);
    
  printf("Node has %d coincidences\n", length(NODE.coincidences));
  fflush(stdout);
  %% increment the temporal activation matrix cell
  %% for k_prev, k
  if (NODE.k_prev == 0) %% if first pattern
    NODE.k_prev = [];
  else
    for t = 1 : length(NODE.k_prev)
      NODE.TAM(NODE.k_prev(t), k) = NODE.TAM(NODE.k_prev(t), k) + 1 + transition_memory - t;
    endfor
  endif

  
  NODE.k_prev = [NODE.k_prev k];
  while (length(NODE.k_prev) > transition_memory)
    NODE.k_prev = NODE.k_prev(2:length(NODE.k_prev));
  endwhile
  
  
  # if (!TEMPORAL_GAP)
  #   if (NODE.k_prev != 0) %% skip if first pattern
  #     NODE.TAM (NODE.k_prev, k) += 1;
  #   endif
  # endif
  
  # NODE.k_prev = k;
  
  if (strcmp(LEVEL, "output"))

    %%printf("Setting class %d", CLASS);

    %% increment the PCW matrix
    try 
      NODE.PCW(k, CLASS) += 1;
    catch
      NODE.PCW(k, CLASS) = 1;
    end_try_catch
    
    %%fflush(stdout);

  endif

endfunction