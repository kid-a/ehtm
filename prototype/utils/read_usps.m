%% -*- mode: octave -*-p
function images = read_usps (W, S = -1)
  images = [];

  switch W
      
    case "train"      
      fid = fopen("data/zip.train");
      i = 1;
      while (!feof(fid))
	printf("Reading train pattern % d from file...\n", i); 
	fflush(stdout);
	line = fgets (fid);
	images(:,:,i) = str2num(line);
	i += 1;

	if (S != -1)
	  if (i > S) 
	    break;
	  endif
	endif
      endwhile
      
      fclose (fid);
      
    otherwise
      fid = fopen("data/zip.test");
      i = 1;
      while (!feof(fid))
	printf("Reading test %d pattern from file...\n", i);
	fflush(stdout);
	line = fgets (fid);	
	images(:,:,i) = str2num(line);
	i += 1;
      endwhile
      
      fclose (fid);
      
  endswitch

endfunction