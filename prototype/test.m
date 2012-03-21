function test (net)
  seq = inputs ();
  l = length(seq);
  for i = 1 : l
    printf("Classifying pattern of class: %d.\n", seq{i}{2});
    classify (net, seq{i}{1})
  endfor
endfunction