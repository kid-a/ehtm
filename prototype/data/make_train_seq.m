%% -*- mode: octave -*-p
function seq = make_train_seq (I, S)
  scans = [];
  %% extract the minimal bounding box
  I = bwborder(I);
  I(:,!any(I)) = [];
  I(!any(I,2),:) = [];

  %% now, pad the image
				# window_h = S(1);
				# window_w = S(2);
  I = impad(I, [window_w window_w], [window_h - 1 window_h -1], "zeros");
  
  %% ready to perform the scans
  [image_h, image_w] = size(I);
  
  reversed = sort(1:(image_w - window_w + 1), "descend");

  k = 1;
  for i = 1 : (image_h - window_h + 1)
    for j = reversed
      scans(:,:,k) = I(i:i+window_h - 1, j:j+window_w - 1);
      k += 1;
    endfor
  endfor
  
endfunction