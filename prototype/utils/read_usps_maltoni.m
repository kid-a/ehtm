%% -*- mode: octave -*-p
function images = read_usps_maltoni (W)
  classes = [0 1 2 3 4 5 6 7 8 9];
  images = {};

  switch W
      
    case "train"
      k = 1;
      for class = classes
	for j = 1 : 10	  
	  image = imread(["data/train100/" num2str(class) "/" num2str(j) ".bmp"]);
	  images{k} = {image, class + 1};
	  k += 1;
	endfor
      endfor

      %% !FIXME "test" not implemented
    case "test"
      k = 1;
      image_number = [359 264 198 166 200 160 170 147 166 177];
      for class = classes
	for j = 1 : image_number(class + 1) 
	  image = imread(["data/test/" num2str(class) "/" num2str(j) ".bmp"]);
	  images{k} = {image, class + 1};
	  k += 1;
	endfor
      endfor
      
  endswitch

endfunction