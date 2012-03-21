%% -*- mode: octave -*-
function in = inputs (W, S = -1)
  in = {};
  counters = zeros(1,10);

  if (S != -1)
    images = read_usps (W,S * 100);
  else
    images = read_usps (W);
  endif

  [_,_,images_count] = size(images);
  
  k = 1;
  j = 1;
  while (1)
    fflush(stdout);
    if (S != -1)
      if (counters(images(:,:,k)(1) + 1) == S)
	fflush(stdout);
	k += 1;
	continue;
      endif
    endif
    
    printf("Making pattern of class %d...\n", images(:,:,k)(1) + 1);
    fflush(stdout);

    %% rescale from [-1,1] to [0,255]
    in{j} = {floor((reshape(images(:,:,k)(2:257), 16, 16)' + 1) / 2 * 255) , images(:,:,k)(1) + 1};
    counters(images(:,:,k)(1) + 1) += 1;
    
    if (S != -1)
      if (!any(counters - S))
	break;
      endif
      
    elseif (k == images_count)
      break;
    endif

    k += 1;
    j += 1;
  endwhile

endfunction
  
  
  
  
  
	
  



  # result = {};
  # paths = {"/home/loris/101_ObjectCategories/accordion/",
  # 	   "/home/loris/101_ObjectCategories/brontosaurus/",
  # 	   "/home/loris/101_ObjectCategories/airplanes/"};
  # classes = {"accordion", "brontosaurus", "airplanes"};
  # %%classes = {"accordion"};
  # i = {};
  # for j = 1 : length (classes)
  #   files = (readdir (paths(j)));
  #   files = files(3 : length (files));
  #   %%l = length (files);
  #   l = 1;

  #   for k = 1 : l

  #     in = imread ([paths{j} files{k}]);

  #     try
  # 	in = rgb2gray (in);
  #     catch
  # 	in = imread ([paths{j} files{k}]);
  #     end
      
  #     [h,w] = size (in);
  #     if ((h > 300) || (w > 300))
  # 	break;
  #     endif

  #     in = pad_input (in);
      
  #     len = length (i);
  #     i{len + 1} = {in, j};
  #   endfor
  # endfor
  
  # printf ("Loaded %d images from %d classes.\n", length(i), length(classes));
endfunction
