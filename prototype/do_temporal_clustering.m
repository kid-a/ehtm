%% -*- mode: octave -*-
%% temporal clustering
function G = do_temporal_clustering (TC, TAM, MAX_GROUP_SIZE = 10, MIN_GROUP_SIZE = 4)
  G = {};
  
  coincidences = 1 : length(TC);
  assigned = zeros(1, length(TC));
  
  Omega = [];

  printf("Clustering %d coincidences...\n", length(TC));
  
  while (!all(assigned))
    k = select_highest_coincidence (TC, assigned);
    assigned(k) = 1;
    Omega = [Omega k];
    pos = length(Omega);
    
    while ((pos <= length (Omega)) && (pos < MAX_GROUP_SIZE))
      k = Omega (pos);
      most_connected = top_most_connected (k, coincidences, assigned, sparse(TAM));
      
      %% !FIXME insert into set not needed anymore
      for i = 1 : length (most_connected)
	Omega = insert_into_set (Omega, most_connected( i ));
      endfor
      
      pos = pos + 1;
    endwhile

    if (length(Omega) < MIN_GROUP_SIZE)
      printf("Continue\n");
      continue;
    endif

    printf("Coincidences assigned: %d\n", nnz(assigned));
    fflush(stdout);    

    g = [];
    
    for j = Omega
      g = insert_into_set (g, j);
    endfor
    
    G (length (G) + 1) = g;
    assigned(Omega) = 1;

    Omega = [];
  endwhile
endfunction
