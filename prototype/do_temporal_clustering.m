%% -*- mode: octave -*-
%% temporal clustering
function G = do_temporal_clustering (TC, TAM, MAX_GROUP_SIZE = 20)
  G = {};
  assigned = zeros (1, length (TC));
  counter = 0;

  printf("Clustering %d coincidences...\n", length(TC));
  
  while ( ! all ( assigned ) )
    counter += 1;
    fflush(stdout);

    k = select_highest_coincidence (TC, assigned);
    Omega = [k];
    pos = 1;
    
    while ((pos <= length (Omega)) && (length (Omega) < MAX_GROUP_SIZE)) %% max_group_size
      k = Omega (pos);
      most_connected = top_most_connected (k, assigned, TAM);

      for i = 1 : length (most_connected)
	Omega = insert_into_set (Omega, most_connected( i ));
      endfor

      pos = pos + 1;
    endwhile
    
    g = [];
    
    for j = Omega
      g = insert_into_set (g, j);
      assigned (j) = 1;
    endfor
    
    G (length (G) + 1) = g;

  endwhile
endfunction
