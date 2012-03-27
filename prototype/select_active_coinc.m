%% -*- mode: octave -*-
function [K, COINC, SEEN, TAM] = select_active_coinc (LEVEL, COINC, IN_MSG, SEEN, TAM)
  
  switch length(COINC)

    %% if no coincidences seen,
    %% make a coincidence from the first input
    case 0
      
      switch LEVEL
	case "entry" 
	  COINC = [IN_MSG];
	otherwise    
	  widx = compute_widx (IN_MSG);
	  COINC = [widx];
      endswitch
      
      K = 1;
      SEEN = [1];
      TAM = [0];

      
    %% else, 
    %% search for the closest coincidence    
    otherwise      
      %% make the _distances_ vector, then find the minimum
      [coinc_count, _] = size(COINC);

      switch LEVEL
	case "entry"
	  distances = sqrt(sum(bsxfun(@minus, COINC, IN_MSG).^2, 2));
	  %%distances = bsxfun(@minus, COINC, IN_MSG);

	otherwise
	  distances = [];

	  for j = 1 : coinc_count
      	    widx = compute_widx(IN_MSG);
      	    distances(j) = widx_distance(COINC(j, :), widx);
	  endfor
	  
      endswitch
            
      [distance,K] = min(distances);
    
      switch LEVEL
	case "entry"
	  thr = 225.0;
	otherwise
	  thr = 0.0;
      endswitch

      # printf("Distance is %f.\n", distance);
      # fflush(stdout);
      %% then, if the closest coincidence is not close enough,
      if (distance > thr)
	%% make new coinc
	switch LEVEL
	  case "entry"
	    COINC = [COINC; IN_MSG]; 
	  otherwise
	    COINC = [COINC; widx];
	endswitch

	K = coinc_count + 1;
	
	%% add a new element to seen
	SEEN = [SEEN 0]; 
	
	%% resize TAM matrix
	[T_h, T_w] = size(TAM);
	TAM = resize(TAM, T_h + 1, T_w + 1); 
      endif
      
      SEEN(K) += 1;

  endswitch
endfunction
