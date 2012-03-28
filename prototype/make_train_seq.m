%% -*- mode: octave -*-p
function scans = make_train_seq (I, S, LEVEL = "entry")
  scans = [];

  switch LEVEL

      case "entry"	
	I(:,!any(double(I) - 255)) = [];
	I(!any(double(I) - 255, 2),:) = [];
	
	%% now, pad the image
	window_h = S(1);
	window_w = S(2);
	I = impad(I, [window_w window_w], [(window_h - 1) (window_h -1)], "constant", 255);
	
	%% ready to perform the scans
	[image_h, image_w] = size(I);
	
	l = fliplr(1:(image_w - window_w + 1));
	%%reversed = sort(1:(image_w - window_w + 1), "descend");
	
	k = 1;
	for i = 1 : (image_h - window_h + 1)
	  for j = l
	    scans(:,:,k) = I(i:i+window_h - 1, j:j+window_w - 1);
	    k += 1;
	  endfor
	  l = fliplr(l);
	endfor

      case "intermediate"

	%% crop the foreground object, then put it in the lower-sx part of the image
	[image_h, image_w] = size(I);
	I(:,!any(double(I) - 255)) = [];
	I(!any(double(I) - 255, 2),:) = [];
	[crop_image_h, crop_image_w] = size(I);
	I = impad(I, [(image_w - crop_image_w) 0], 
		  [image_h - crop_image_h 0], "constant", 255);
	Ib = I;
	
	%% perform the horizontal scan
	k = 1;
	w_dir = 1; %% can be either 1 or -1

	while(1)
	  while(1)
%%	    printf("Blah\n");
	    
	    scans(:,:,k) = I;
	    
	    %% moving the object left
	    if (w_dir == 1)
	      if (abs(sum(double(I(:,1)) - 255)) > 0)
		break;
	      endif
	    
	      I = [I(:,2:length(I)) 255*ones(length(I),1)];
	      k += 1;
	      
	    %% moving the object right
	    elseif (w_dir == -1)
	      
	      if (abs(sum(double(I(:,length(I))) - 255)) > 0)
		break;
	      endif
	      
	      I = [255*ones(length(I),1) I(:,1:length(I)-1) ];
	      k += 1;
	      
	    endif
	  endwhile

	  if (abs(sum(double(I(1,:)) - 255)) > 0)
	    break;
	  endif

	  I = [I(2:length(I),:); 255*ones(1,length(I))];
	  k += 1;
	  w_dir = - w_dir;

	endwhile

	k += 1;

	%% put the foreground object again in the lower-sx part of the image
	I = Ib;

	%% perform the vertical scan
	h_dir = 1; %% can be either 1 or -1
	while (1)
	  while (1)
	    
	    scans(:,:,k) = I;
	    
	    %% moving the object up
	    if (h_dir == 1)
	      if (abs(sum(double(I(1,:)) - 255)) > 0)
		break;
	      endif
	    
	      I = [I(2:length(I),:); 255*ones(1,length(I))];
	      k += 1;
	      
	    %% moving the object down
	    elseif (h_dir == -1)
	      
	      if (abs(sum(double(I(length(I),:)) - 255)) > 0)
		break;
	      endif
	      
	      I = [255*ones(1,length(I)); I(1:length(I)-1,:)];
	      k += 1;
	      
	    endif
	  endwhile

	  if (abs(sum(double(I(:,1)) - 255)) > 0)	  
	    break;
	  endif

	  I = [I(:,2:length(I)) 255*ones(length(I),1)];	  
	  k += 1;
	  h_dir = - h_dir;  
	endwhile

      case "output"
	%% crop the foreground object, then put it in the lower-sx part of the image
	[image_h, image_w] = size(I);
	I(:,!any(double(I) - 255)) = [];
	I(!any(double(I) - 255, 2),:) = [];
	[crop_image_h, crop_image_w] = size(I);
	I = impad(I, [(image_w - crop_image_w) 0], 
		  [image_h - crop_image_h 0], "constant", 255);
	Ib = I;
	
	%% perform the horizontal scan
	k = 1;
	w_dir = 1; %% can be either 1 or -1

	while(1)
	  while(1)
%%	    printf("Blah\n");
	    
	    scans(:,:,k) = I;
	    
	    %% moving the object left
	    if (w_dir == 1)
	      if (abs(sum(double(I(:,1)) - 255)) > 0)
		break;
	      endif
	    
	      I = [I(:,2:length(I)) 255*ones(length(I),1)];
	      k += 1;
	      
	    %% moving the object right
	    elseif (w_dir == -1)
	      
	      if (abs(sum(double(I(:,length(I))) - 255)) > 0)
		break;
	      endif
	      
	      I = [255*ones(length(I),1) I(:,1:length(I)-1) ];
	      k += 1;
	      
	    endif
	  endwhile

	  if (abs(sum(double(I(1,:)) - 255)) > 0)
	    break;
	  endif

	  I = [I(2:length(I),:); 255*ones(1,length(I))];
	  k += 1;
	  w_dir = - w_dir;

	endwhile

    endswitch
  
endfunction