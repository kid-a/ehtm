%% -*- mode: octave -*-
%% temporal clustering
function G = do_temporal_clustering (TC, TAM, MAX_GROUP_SIZE = 10)
  G = {};
  coincidences = 1 : length(TC);

  printf("Clustering %d coincidences...\n", length(TC));
  
  while (length(coincidences) > 0)
    length(coincidences)
    fflush(stdout);

    k = select_highest_coincidence (TC, coincidences);
    Omega = [k];
    pos = 1;
    
    while ((pos <= length (Omega)) && (length (Omega) < MAX_GROUP_SIZE)) %% max_group_size
      k = Omega (pos);
      [most_connected, coincidences] = top_most_connected (k, coincidences, sparse(TAM));
      
      %% !FIXME insert into set not needed anymore
      for i = 1 : length (most_connected)
	Omega = insert_into_set (Omega, most_connected( i ));
      endfor

      pos = pos + 1;
    endwhile
    
    g = [];
    
    for j = Omega
      g = insert_into_set (g, j);
    endfor
    
    G (length (G) + 1) = g;

  endwhile
endfunction
