%% -*- mode: octave -*-p
function scans = make_train_seq (I, S)
  scans = [];
  
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
  
endfunction